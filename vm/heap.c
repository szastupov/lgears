/*
 * This file is part of lGears scheme system
 * Copyright (C) 2009 Stepan Zastupov <redchrom@gmail.com>
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 3 of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General
 * Public Licens along with this program, if not; see
 * <http://www.gnu.org/licenses>.
 */
#include <errno.h>
#include <sys/time.h>
#include "vm.h"

#define align_up(s, a) (((s)+((a)-1)) & ~((a)-1))
//#define mem_idx(shift, block) (align_up(shift+1, block)/block-1)

/* TODO: tune it */
#define FRESH_SIZE 1024*8
#define SURVIVED_SIZE 1024*16
#define OLD_SIZE 1024*512

#define HEAP_DBG printf

static void space_reset(space_t *space)
{
	space->pos = space->mem;
	space->free_mem = space->size;
	space->blocks = 0;
}

static void space_reset0(space_t *space)
{
	space_reset(space);
	memset(space->mem, 0, space->size); // Fore debug purposes
}

static void space_init(space_t *space, void *mem, int size)
{
	space->size = size;
	space->mem = mem;

	space_reset0(space);
}

/*
 * All pointers should be aligned by wordsize
 * Assume that pos is always aligned address - BHDR_SIZE
 */
static void* space_alloc(space_t *space, size_t size, int type_id)
{
	size = align_up(size, 8);
	size_t need = size+BHDR_SIZE;

	if (need > space->free_mem)
		return NULL;


	block_hdr_t *hdr = space->pos;
	hdr->forward = 0;
	hdr->generation = 0;
	hdr->remembered = 0;
	hdr->type_id = type_id;
	hdr->size = size;

	space->pos += BHDR_SIZE;
	void *res = space->pos;
	space->pos += hdr->size;
	space->free_mem -= need;
	space->blocks++;

	return res;
}

typedef void (*put_func)(void *dest, const void *src, size_t n);

static void* space_put(space_t *space, void *p, size_t size, put_func put)
{
	if (size > space->free_mem)
		FATAL("Totaly fucking out of memory (need %zd)\n", size);

	void *res = space->pos;
	put(res, p, size);
	space->pos += size;
	space->free_mem -= size;
	space->blocks++;

	return res;
}

static void* space_copy(space_t *space, void *p, size_t size)
{
	return space_put(space, p, size, (put_func)memcpy);
}

static void* space_move(space_t *space, void *p, size_t size)
{
	return space_put(space, p, size, (put_func)memmove);
}

static int space_enough(space_t *space, size_t size)
{
	return space->free_mem > size+BHDR_SIZE;
}

static void heap_swap_survived(heap_t *heap)
{
	space_reset0(heap->survived);
	space_t *tmp = heap->future_survived;
	heap->future_survived = heap->survived;
	heap->survived = tmp;
}

static void append_remembered(remembered_set_t *rs,
							  remembered_t *new)
{
	if (rs->tail)
		rs->tail->next = new;
	rs->tail = new;
	if (!rs->head)
		rs->head = new;
}

void heap_remember(heap_t *heap, block_hdr_t *hdr)
{
	remembered_t *new = new0(remembered_t);
	new->hdr = hdr;
	append_remembered(&heap->remembered_set, new);
}

static void forget(remembered_set_t *rs)
{
	remembered_t *cur = rs->head;
	remembered_t *next;
	while (cur) {
		next = cur->next;
		mem_free(cur);
		cur = next;
	}
	rs->head = NULL;
	rs->tail = NULL;
}

typedef struct {
	block_hdr_t *hdr;
	int i;
} scan_cont_t;

static inline void visit_hdr(heap_t *heap, block_hdr_t *hdr)
{
	const type_t *type = &type_table[hdr->type_id];
	if (type->visit)
		type->visit(&heap->visitor, BHDR_SIZE + (void*)hdr);
}

static void heap_scan_references(heap_t *heap, scan_cont_t *cont)
{
	space_t *space = heap->future_survived;
	int i = cont->i;
	block_hdr_t *hdr = cont->hdr;
	for (; i < space->blocks; i++,
			 hdr = BHDR_SIZE + hdr->size + (void*)hdr)
	{
		visit_hdr(heap, hdr);
	}
	cont->i = i;
	cont->hdr = hdr;
}

static void heap_scan_remebered(heap_t *heap)
{
	remembered_t *cur = heap->remembered_set.head;
	while (cur) {
		visit_hdr(heap, cur->hdr);
		cur = cur->next;
	}
	forget(&heap->remembered_set);
}

/*
 * 1. Collect all reached objects
 * 2. Reset the old space
 * 3. Move obects to the begining of the old space, creating mappings
 */
static void heap_gc_old(heap_t *heap)
{
	int i, live_count, dead_count;
	space_t *space = &heap->old;
	remembered_set_t live_set = {0};
	live_count = dead_count = 0;

	block_hdr_t *hdr = space->mem;
	for (i = 0; i < space->blocks;
		 i++, hdr = BHDR_SIZE + hdr->size + (void*)hdr)
	{
		if (hdr->remembered) {
			remembered_t *new = new0(remembered_t);
			new->hdr = hdr;
			append_remembered(&live_set, new);
			live_count++;
		} else
			dead_count++;
	}
	space_reset(space);

	remembered_t *cur = live_set.head;
	hash_table_t *tbl = new0(hash_table_t);
	hash_table_init(tbl, direct_hash, direct_equal);
	void *new_pos;
	while (cur) {
		cur->hdr->remembered = 0;
		new_pos = space_move(space, cur->hdr, BHDR_SIZE+cur->hdr->size);
		//printf("moved %p -> %p\n", cur->hdr, new_pos);
		hash_table_insert(tbl, cur->hdr, new_pos);
		heap_remember(heap, new_pos); /* Schedule it for rescan, because
									   * old objects may contains pointers
									   * to collected memory too */
		cur = cur->next;
	}
	heap->old_map = tbl;

	forget(&live_set);

	printf("live %d, dead %d, free old space %zd\n",
		   live_count, dead_count, space->free_mem);
}

static void heap_gc_main(heap_t *heap)
{
	thread_get_roots(&heap->visitor, heap->thread);

	scan_cont_t cont = {
		.hdr = heap->future_survived->mem,
		.i = 0
	};

	heap_scan_references(heap, &cont);
	while (heap->remembered_set.head) {
		heap_scan_remebered(heap);
		if (cont.i < heap->future_survived->blocks)
			heap_scan_references(heap, &cont);
	}
}

static void heap_gc(heap_t *heap)
{
	HEAP_DBG("!!!Starting garbage collection, DON'T PANIC!!!!\n");
	struct timeval tv1, tv2;
	gettimeofday(&tv1, NULL);

	heap_gc_main(heap);

	if (heap->full_gc) {
		HEAP_DBG("Full GC\n");
		heap_gc_old(heap);
		heap->full_gc = 0;
		heap_gc_main(heap);
		// TODO dont allocate table each time
		hash_table_destroy(heap->old_map);
		mem_free(heap->old_map);
		heap->old_map = NULL;
	}

	space_reset0(&heap->fresh);
	heap_swap_survived(heap);
	thread_after_gc(&heap->visitor, heap->thread);

	if (heap->old.free_mem < SURVIVED_SIZE)
		heap->full_gc = 1;		/* Tell to perform full gc next time */

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
	return res;
}

void heap_require(heap_t *heap, int size)
{
	if (!space_enough(&heap->fresh, size))
		heap_gc(heap);
}

void heap_require_blocks(heap_t *heap, int size, int count)
{
	size = align_up(size, 8)+BHDR_SIZE;
	if (!space_enough(&heap->fresh, size*count))
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
	block_hdr_t *hdr;

	if (!obj->ptr || obj->tag != id_ptr)
		return;

	ptr_t ptr = { .ptr = obj->ptr };
	void *p = PTR_GET(ptr);
	if (p < heap->mem || p > heap->mem+heap->mem_size)
		FATAL("Trying to mark %p which doesn't belong to this heap\n", p);

	if (in_space(heap->future_survived, p)) {
		//HEAP_DBG("%p is already copied to survived\n", p);
		return;
	}

	if (in_space(&heap->old, p)) {
		hdr = HTYPE(p);
		if (heap->full_gc && !hdr->remembered) {
			/* Mark old object as reached */
			hdr->remembered = 1;
			heap_remember(heap, hdr);
		} else if (heap->old_map) {
			/* Update pointer on old object */
			void *res = hash_table_lookup(heap->old_map, hdr);
			if (res) {
				p = res+BHDR_SIZE;
				//HEAP_DBG("Forwarding old to %p\n", res);
				PTR_SET(ptr, p);
				obj->ptr = ptr.ptr;
			} else {
				//HEAP_DBG("%p not in map\n", hdr);
			}
		}
		return;
	}

	void **forward = p;
	p -= BHDR_SIZE;
	hdr = p;

	/*
	 * Use forward pointer if object already moved
	 */
	if (hdr->forward) {
		//HEAP_DBG("Forwarding to %p\n", *forward);
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
		if (heap->full_gc) {
			//HEAP_DBG("postpoinig\n");
			heap_remember(heap, hdr);
			return; //Postpone copying
		}
		//HEAP_DBG("Got old object\n");
		new_pos = space_copy(&heap->old, p, ts);
		heap_remember(heap, new_pos);
	} else {
		//HEAP_DBG("copying to survived\n");
		new_pos = space_copy(heap->future_survived, p, ts);
	}

	hdr->forward = 1;
	*forward = new_pos + BHDR_SIZE;

	hdr = new_pos;
	hdr->forward = 0;
	hdr->generation++;
	new_pos += BHDR_SIZE;
	PTR_SET(ptr, new_pos);
	obj->ptr = ptr.ptr;
}

static void* heap_allocator_alloc(allocator_t *al, size_t size, int type_id)
{
	heap_t *heap = container_of(al, heap_t, allocator);
	return heap_alloc(heap, size, type_id);
}

void heap_init(heap_t *heap, void *thread)
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
	heap->thread = thread;
	heap->allocator.alloc = heap_allocator_alloc;
	heap->allocator.id = id_ptr;
}

void heap_destroy(heap_t *heap)
{
	mem_free(heap->mem);
	forget(&heap->remembered_set);
}
