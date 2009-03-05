/*
 * Copyright (C) 2009 - Stepan Zastupov
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
 */
#include <unistd.h>
#include <errno.h>
#include "memory.h"
#include "heap.h"

/** 
 * @file heap.c
 * @brief Heap managment inplementation
 * @author Stepan Zastupov
 *
 * This is the implementation of Cheney's Algorithm
 * Not, that memory forwarding via old memory overwrite,
 * it may be changed inf future
 */


#define balign 7
#define align_up(s)	(((s)+balign) & ~balign)
#define pad align_up(BHDR_SIZE)-BHDR_SIZE

static void copy_heap_reset(copy_heap_t *heap)
{
	heap->pos = heap->mem + pad;
	heap->free_mem = heap->size - pad;
	heap->blocks = 0;
}

static void copy_heap_init(copy_heap_t *heap, void *mem, int size)
{
	heap->size = size;
	heap->mem = mem;

	copy_heap_reset(heap);
}

/*
 * All pointers should be aligned by wordsize
 * Assume that pos is always aligned address - BHDR_SIZE
 */
static void* copy_heap_alloc(copy_heap_t *heap, size_t size, int type_id)
{
	size_t minimal = size+BHDR_SIZE;
	if (minimal > heap->free_mem) {
		LOG_DBG("out of free mem on heap (need %zd)\n", minimal);
		return NULL;
	}

	size_t required = align_up(minimal)-BHDR_SIZE;

	block_hdr_t *hdr = heap->pos;
	hdr->forward = 0;
	hdr->type_id = type_id;
	heap->pos += BHDR_SIZE;
	heap->free_mem -= BHDR_SIZE;
	/*
	 * Align size to required if we have enaught memory
	 */
	if (required <= heap->free_mem) {
		hdr->size = required;
		heap->free_mem -= required;
	} else {
		hdr->size = size;
		heap->free_mem -= size;
	}
	void *res = heap->pos;

	heap->pos += hdr->size;
	heap->blocks++;

	return res;
}

static void* copy_heap_copy(copy_heap_t *heap, void *p, size_t size)
{
	if (size > heap->free_mem)
		FATAL("Totaly fucking out of memory");
	void *res = heap->pos;
	memcpy(res, p, size);
	heap->pos += size;
	heap->free_mem -= size;
	heap->blocks++;

	return res;
}

static int copy_heap_own(copy_heap_t *heap, void *p)
{
	if (p < heap->mem || p > (heap->mem + heap->size))
		return 0;
	return 1;
}

static void heap_swap(heap_t *heap)
{
	copy_heap_reset(heap->from);
	copy_heap_t *tmp = heap->from;
	heap->from = heap->to;
	heap->to = tmp;
}

/** 
 * @brief Incremental scan of object referencies
 * 
 * @param heap 
 */
static void heap_scan_references(heap_t *heap)
{
	LOG_DBG("Survived root objects %d\n", heap->to->blocks);
	int i;
	block_hdr_t *hdr = heap->to->mem + pad;
	for (i = 0; i < heap->to->blocks; i++) {
		const type_t *type = &type_table[hdr->type_id];
		LOG_DBG("Survived %d %s\n", i, type_table[hdr->type_id].name);

		/*
		 * If type provide visit function - call it
		 */
		if (type->visit)
			type->visit(&heap->visitor, BHDR_SIZE + (void*)hdr);

		hdr = hdr->size + BHDR_SIZE + (void*)hdr;
	}
	LOG_DBG("Total survived objects %d\n", heap->to->blocks);
}

void* heap_alloc(heap_t *heap, int size, int type_id)
{
	void *res = copy_heap_alloc(heap->from, size, type_id);
	if (!res) {
		LOG_DBG("!!!Starting garbage collection, DON'T PANIC!!!!\n");
		heap->vm_get_roots(&heap->visitor, heap->vm);
		heap_scan_references(heap);
		heap_swap(heap);
		res = copy_heap_alloc(heap->from, size, type_id);
		if (!res)
			FATAL("Totaly fucking out of memory");
	}
	LOG_DBG("allocated %d:%p\n", size, res);
	return res;
}

void* heap_alloc0(heap_t *heap, int size, int type_id)
{
	void *ptr = heap_alloc(heap, size, type_id);
	memset(ptr, 0, size);
	return ptr;
}

static void heap_mark(visitor_t *visitor, obj_t *obj)
{
	printf("heap_mark(%p)\n", obj);
	heap_t *heap = visitor->user_data;

	if (!obj->ptr || obj->tag != id_ptr)
		return;

	ptr_t ptr = { .ptr = obj->ptr };

	void *p = PTR_GET(ptr);
	if (!copy_heap_own(heap->from, p)) {
		if (copy_heap_own(heap->to, p)) {
			LOG_DBG("%p belong to `to' space, skip...\n", p);
			return;
		} else
			FATAL("Trying to mark %p which doesn't belong to this heap\n", p);
	}

	void **forward = p;
	p -= BHDR_SIZE;
	block_hdr_t *hdr = p;

	/*
	 * Use forward pointer if object already moved
	 */
	if (hdr->forward) {
		LOG_DBG("Forwarding to %p\n", *forward);
		PTR_SET(ptr, *forward);
		obj->ptr = ptr.ptr;
		return;
	}

	/*
	 * Copy object to the second heap and update pointer
	 */
	void *new_pos = copy_heap_copy(heap->to, p, hdr->size+BHDR_SIZE);

	hdr->forward = 1;
	*forward = new_pos + BHDR_SIZE;
	LOG_DBG("Forward set to %p\n", *forward);

	hdr = new_pos;
	hdr->forward = 0;
	new_pos += BHDR_SIZE;
	PTR_SET(ptr, new_pos);
	obj->ptr = ptr.ptr;
}

void heap_init(heap_t *heap, visitor_fun vm_get_roots, void *vm)
{
	heap->from = &heap->heaps[0];
	heap->to = &heap->heaps[1];

	static int sc_page_size = 0;
	if (!sc_page_size)
		sc_page_size = sysconf(_SC_PAGE_SIZE);

	heap->page_size = sc_page_size*2;
	heap->page = mem_alloc(heap->page_size);

	copy_heap_init(heap->from, heap->page, sc_page_size);
	copy_heap_init(heap->to, heap->page+sc_page_size, sc_page_size);

	heap->visitor.visit = heap_mark;
	heap->visitor.user_data = heap;
	heap->vm_get_roots = vm_get_roots;
	heap->vm = vm;
}

void heap_destroy(heap_t *heap)
{
	mem_free(heap->page);
}

void heap_stat(heap_t *heap)
{
	LOG_DBG("mem used %d\n", heap->from->size - heap->from->free_mem);
}
