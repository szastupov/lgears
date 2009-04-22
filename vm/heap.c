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
#include <sys/time.h>
#include "memory.h"
#include "heap.h"

/**
 * @file heap.c
 * @brief Heap managment inplementation
 * @author Stepan Zastupov
 *
 * This is the implementation of Cheney's Algorithm
 * Note, that old memory overwrited on forwarding,
 * it may be changed in future
 */

#define align_up(s, a) (((s)+((a)-1)) & ~((a)-1))
#define mem_idx(shift, block) (align_up(shift+1, block)/block-1)

static void space_reset(space_t *space)
{
	space->pos = space->mem;
	space->free_mem = space->size;
	space->blocks = 0;

	memset(space->mem, 0, space->size); // Fore debug purposes
}

static void space_init(space_t *space, void *mem, int size)
{
	space->size = size;
	space->mem = mem;

	space_reset(space);
}

/*
 * All pointers should be aligned by wordsize
 * Assume that pos is always aligned address - BHDR_SIZE
 */
static void* space_alloc(space_t *space, size_t size, int type_id)
{
	size = align_up(size, 8);
	size_t need = size+BHDR_SIZE;

	if (need > space->free_mem) {
		LOG_DBG("out of free mem on space (need %zd)\n", need);
		return NULL;
	}


	block_hdr_t *hdr = space->pos;
	hdr->forward = 0;
	hdr->generation = 0;
	hdr->modified = 0;
	hdr->type_id = type_id;
	hdr->size = size;

	space->pos += BHDR_SIZE;
	void *res = space->pos;
	space->pos += hdr->size;
	space->free_mem -= need;
	space->blocks++;

	return res;
}


static void* space_copy(space_t *space, void *p, size_t size)
{
	LOG_DBG("COPYING\n");
	if (size > space->free_mem)
		FATAL("Totaly fucking out of memory (need %zd)\n", size);

	void *res = space->pos;
	memcpy(res, p, size);
	space->pos += size;
	space->free_mem -= size;
	space->blocks++;

	return res;
}

static int space_enough(space_t *space, size_t size)
{
	return space->free_mem > size+BHDR_SIZE;
}

static void heap_swap_survived(heap_t *heap)
{
	space_reset(heap->survived);
	space_t *tmp = heap->future_survived;
	heap->future_survived = heap->survived;
	heap->survived = tmp;
}

void remember(heap_t *heap, block_hdr_t *hdr)
{
	remembered_t *new = new0(remembered_t);
	new->next = heap->remembered;
	new->hdr = hdr;
	heap->remembered = new;
}

static void forget(remembered_t *cur)
{
	remembered_t *next;
	while (cur) {
		next = cur->next;
		mem_free(cur);
		cur = next;
	}
}

typedef struct {
	block_hdr_t *hdr;
	int i;
} scan_cont_t;

/**
 * @brief Incremental scan of object references
 */
static void heap_scan_references(heap_t *heap, scan_cont_t *cont)
{
	space_t *space = heap->future_survived;
	int i = cont->i;
	block_hdr_t *hdr = cont->hdr;
	for (; i < space->blocks; i++,
			 hdr = BHDR_SIZE + hdr->size + (void*)hdr)
	{
		const type_t *type = &type_table[hdr->type_id];
		LOG_DBG("Survived %s %d\n",
				type_table[hdr->type_id].name, i);

		/*
		 * If type provide visit function - call it
		 */
		if (type->visit)
			type->visit(&heap->visitor, BHDR_SIZE + (void*)hdr);
	}
	cont->i = i;
	cont->hdr = hdr;
}

static void visit_hdr(heap_t *heap, block_hdr_t *hdr)
{
	const type_t *type = &type_table[hdr->type_id];
	if (type->visit)
		type->visit(&heap->visitor, BHDR_SIZE + (void*)hdr);
}

static void heap_scan_remebered(heap_t *heap)
{
	remembered_t *cur = heap->remembered;
	heap->remembered = NULL;
	remembered_t *root = cur;
	while (cur) {
		visit_hdr(heap, cur->hdr);
		cur = cur->next;
	}
	forget(root);
}

static void heap_gc(heap_t *heap)
{
	LOG_DBG("!!!Starting garbage collection, DON'T PANIC!!!!\n");
	struct timeval tv1, tv2;
	gettimeofday(&tv1, NULL);

	heap->vm_get_roots(&heap->visitor, heap->vm);

	scan_cont_t cont = {
		.hdr = heap->future_survived->mem,
		.i = 0
	};

	heap_scan_references(heap, &cont);
	while (heap->remembered) {
		heap_scan_remebered(heap);
		if (cont.i < heap->future_survived->blocks)
			heap_scan_references(heap, &cont);
	}

	space_reset(&heap->fresh);
	heap_swap_survived(heap);
	if (heap->vm_after_gc)
		heap->vm_after_gc(&heap->visitor, heap->vm);

	gettimeofday(&tv2, NULL);
	printf("\nGC time: %ld seconds, %ld microseconds\n",
			tv2.tv_sec-tv1.tv_sec, tv2.tv_usec-tv1.tv_usec);
}

void* heap_alloc(heap_t *heap, int size, int type_id)
{
	void *res = space_alloc(&heap->fresh, size, type_id);
	if (!res) {
		heap_gc(heap);
		res = space_alloc(&heap->fresh, size, type_id);
		if (!res)
			FATAL("Totaly fucking out of memory");
	}
	LOG_DBG("allocated %d:%p\n", size, res);
	return res;
}

void heap_require(heap_t *heap, int size)
{
	if (!space_enough(&heap->fresh, size))
		heap_gc(heap);
}

void* heap_alloc0(heap_t *heap, int size, int type_id)
{
	void *ptr = heap_alloc(heap, size, type_id);
	memset(ptr, 0, size);
	return ptr;
}

static int in_space(space_t *space, void *p)
{
	return (p >= space->mem) && (p < space->mem+space->size);
}

static void heap_mark(visitor_t *visitor, obj_t *obj)
{
	heap_t *heap = visitor->user_data;

	if (!obj->ptr || obj->tag != id_ptr)
		return;

	ptr_t ptr = { .ptr = obj->ptr };
	void *p = PTR_GET(ptr);
	if (p < heap->mem || p > heap->mem+heap->mem_size)
		FATAL("Trying to mark %p which doesn't belong to this heap\n", p);

	if (in_space(&heap->old, p)
		|| in_space(heap->future_survived, p))
	{
		LOG_DBG("%p is already copied\n", p);
		return;
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
	size_t ts = hdr->size+BHDR_SIZE;
	void *new_pos;
	if (hdr->generation == 3) {
		LOG_DBG("Got old object\n");
		new_pos = space_copy(&heap->old, p, ts);
		remember(heap, new_pos);
	} else
		new_pos = space_copy(heap->future_survived, p, hdr->size+BHDR_SIZE);

	hdr->forward = 1;
	*forward = new_pos + BHDR_SIZE;

	hdr = new_pos;
	hdr->forward = 0;
	hdr->generation++;
	new_pos += BHDR_SIZE;
	PTR_SET(ptr, new_pos);
	obj->ptr = ptr.ptr;
}

/* TODO: tune it */
#define FRESH_SIZE 8192
#define SURVIVED_SIZE 4096
#define OLD_SIZE 16384

void heap_init(heap_t *heap, visitor_fun vm_get_roots,
		visitor_fun vm_after_gc, void *vm)
{
	heap->mem_size = FRESH_SIZE+SURVIVED_SIZE*2+OLD_SIZE;
	heap->mem = mem_alloc(heap->mem_size);

	void *memp = heap->mem;

#define INIT_SPACE(space, size)					\
	space_init(space, memp, size);				\
	memp += size;

	heap->survived = &heap->survived_spaces[0];
	heap->future_survived = &heap->survived_spaces[1];

	INIT_SPACE(&heap->fresh, FRESH_SIZE);
	INIT_SPACE(&heap->survived_spaces[0], SURVIVED_SIZE);
	INIT_SPACE(&heap->survived_spaces[1], SURVIVED_SIZE);
	INIT_SPACE(&heap->old, OLD_SIZE);

	heap->visitor.visit = heap_mark;
	heap->visitor.user_data = heap;
	heap->vm_get_roots = vm_get_roots;
	heap->vm_after_gc = vm_after_gc;
	heap->vm = vm;
	heap->remembered = NULL;
}

void heap_destroy(heap_t *heap)
{
	mem_free(heap->mem);
	forget(heap->remembered);
}
