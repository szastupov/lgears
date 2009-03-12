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
 * Note, that old memory overwrited on forwarding,
 * it may be changed in future
 */

#define align_up(s, a) (((s)+((a)-1)) & ~((a)-1))
#define card_idx(shift) (align_up(shift+1, 512)/512-1)

static void copy_heap_reset(copy_heap_t *heap)
{
	heap->pos = heap->mem;
	heap->free_mem = heap->size;
	heap->cur_card = 0;
	memset(heap->cards, 0, sizeof(heap->cards));

	memset(heap->mem, 0, heap->size); // Fore debug purposes
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
	size = align_up(size, 8);
	size_t need = size+BHDR_SIZE;

	if (need > heap->free_mem) {
		LOG_DBG("out of free mem on heap (need %zd)\n", need);
		return NULL;
	}


	block_hdr_t *hdr = heap->pos;
	hdr->forward = 0;
	hdr->modified = 0;
	hdr->type_id = type_id;
	hdr->size = size;

	heap->pos += BHDR_SIZE;
	void *res = heap->pos;
	heap->pos += hdr->size;

	heap->free_mem -= need;

	return res;
}


static void* copy_heap_copy(copy_heap_t *heap, void *p, size_t size)
{
	LOG_DBG("COPYING\n");
	if (size > heap->free_mem)
		FATAL("Totaly fucking out of memory (need %zd)\n", size);

	int card = card_idx((heap->pos-heap->mem)+size);
	if (card > heap->cur_card) {
		heap->cur_card++;
		heap->pos = heap->mem + card*512;
	}

	void *res = heap->pos;
	memcpy(res, p, size);
	heap->pos += size;
	heap->free_mem -= size;
	heap->cards[card].blocks++;

	return res;
}

static int copy_heap_own(copy_heap_t *heap, void *p)
{
	if (p < heap->mem || p > (heap->mem + heap->size))
		return 0;
	return 1;
}

static int copy_heap_enough(copy_heap_t *heap, size_t size)
{
	return heap->free_mem > size+BHDR_SIZE;
}

void heap_swap(heap_t *heap)
{
	copy_heap_reset(heap->from);
	copy_heap_t *tmp = heap->from;
	heap->from = heap->to;
	heap->to = tmp;
}

/** 
 * @brief Incremental scan of object references
 */

static void heap_scan_card(heap_t *heap, copy_heap_t *ch,
		int card, int only_dirty)
{
	int i;
	block_hdr_t *hdr = ch->mem + card*512;

	for (i = 0; i < ch->cards[card].blocks; i++,
			hdr = BHDR_SIZE + hdr->size + (void*)hdr)
	{
		const type_t *type = &type_table[hdr->type_id];
		LOG_DBG("Survived %s %d\n",
				type_table[hdr->type_id].name, i);

		/*
		 * If type provide visit function - call it
		 */
		if (!only_dirty || hdr->modified) {
			if (type->visit)
				type->visit(&heap->visitor, BHDR_SIZE + (void*)hdr);
		}
		hdr->modified = 0; // Refresh modified flag
	}

	ch->cards[card].dirty = 0;
}

static void heap_scan_references(heap_t *heap, int from_card)
{
	int i;
	for (i = from_card; i <= heap->to->cur_card; i++) {
		LOG_DBG("scan_refs %d\n", i);
		heap_scan_card(heap, heap->to, i, 0);
	}
}

static void heap_scan_old(heap_t *heap, int to_card)
{
	copy_heap_t *ch = heap->to;
	int i;
	for (i = 0; i < to_card; i++) {
		if (ch->cards[i].dirty) {
			LOG_DBG("scan old %d\n", i);
			heap_scan_card(heap, heap->to, i, 1);
		}
	}
}

static void heap_gc(heap_t *heap)
{
	LOG_DBG("!!!Starting garbage collection, DON'T PANIC!!!!\n");
	if (!heap->to) {
		heap->to = &heap->heaps[heap->count];
		copy_heap_init(heap->to, mem_alloc(heap->page_size), heap->page_size);
		heap->count++;
	}
	int card_was = heap->to->cur_card;
	heap->vm_get_roots(&heap->visitor, heap->vm);

	heap_scan_old(heap, card_was);
	heap_scan_references(heap, card_was);

	copy_heap_reset(heap->from);
	if (heap->vm_after_gc)
		heap->vm_after_gc(&heap->visitor, heap->vm);
}

void* heap_alloc(heap_t *heap, int size, int type_id)
{
	void *res = copy_heap_alloc(heap->from, size, type_id);
	if (!res) {
		heap_gc(heap);
		res = copy_heap_alloc(heap->from, size, type_id);
		if (!res)
			FATAL("Totaly fucking out of memory");
	}
	LOG_DBG("allocated %d:%p\n", size, res);
	return res;
}

void heap_require(heap_t *heap, int size)
{
	if (!copy_heap_enough(heap->from, size))
		heap_gc(heap);
}

void* heap_alloc0(heap_t *heap, int size, int type_id)
{
	void *ptr = heap_alloc(heap, size, type_id);
	memset(ptr, 0, size);
	return ptr;
}

int heap_own(heap_t *heap, void *p)
{
	int i;
	int found = -1;
	for (i = 0; i < heap->count; i++)
		if (copy_heap_own(&heap->heaps[i], p)) {
			found = i;
			break;
		}

	return found;
}

static void heap_mark(visitor_t *visitor, obj_t *obj)
{
	heap_t *heap = visitor->user_data;

	if (!obj->ptr || obj->tag != id_ptr)
		return;

	ptr_t ptr = { .ptr = obj->ptr };
	void *p = PTR_GET(ptr);


	/*
	 * TODO:
	 * determine destination heap
	 * In case pointer belong to destination:
	 *	If destination heap is full - move objects to next heap
	 *	If destination isn't full - skip
	 * In case pointer belong to 0-heap move object to destination
	 */

	int idx = heap_own(heap, p);
	if (idx == -1) {
		FATAL("Trying to mark %p which doesn't belong to this heap\n", p);
	} else if (idx == heap->count-1) {
		LOG_DBG("%p belong to destination space, skip...\n", p);
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
	void *new_pos = copy_heap_copy(heap->to, p, hdr->size+BHDR_SIZE);

	hdr->forward = 1;
	*forward = new_pos + BHDR_SIZE;
	//	LOG_DBG("Forward set to %p\n", *forward);

	hdr = new_pos;
	hdr->forward = 0;
	new_pos += BHDR_SIZE;
	PTR_SET(ptr, new_pos);
	obj->ptr = ptr.ptr;
}

void heap_mark_modified(heap_t *heap, void *ptr)
{
	int idx = heap_own(heap, ptr);
	if (idx == -1)
		FATAL("Trying to modify pointer %p which doesn't belong to any heap\n", ptr);

	copy_heap_t *ch = &heap->heaps[idx];
	unsigned long shift = ptr-ch->mem;
	int i = card_idx(shift);
	LOG_DBG("mark card %d\n", i);
	HTYPE(ptr)->modified = 1;
	ch->cards[i].dirty = 1;
}

void heap_init(heap_t *heap, visitor_fun vm_get_roots,
		visitor_fun vm_after_gc, void *vm)
{
	heap->from = &heap->heaps[0];
	heap->count = 1;
	heap->to = NULL;

	static int sc_page_size = 0;
	if (!sc_page_size)
		sc_page_size = sysconf(_SC_PAGE_SIZE);

	heap->page_size = sc_page_size;

	copy_heap_init(heap->from, mem_alloc(heap->page_size), sc_page_size);

	heap->visitor.visit = heap_mark;
	heap->visitor.user_data = heap;
	heap->vm_get_roots = vm_get_roots;
	heap->vm_after_gc = vm_after_gc;
	heap->vm = vm;
}

void heap_destroy(heap_t *heap)
{
	int i;
	for (i = 0; i < heap->count; i++)
		mem_free(heap->heaps[i].mem);
}

void heap_stat(heap_t *heap)
{
	LOG_DBG("mem used on heap 1 %zd\n", heap->to->size - heap->to->free_mem);
}
