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
#ifndef HEAP_H
#define HEAP_H

#include "allocator.h"

#define IS_OLD(hp, ptr) ((void*)ptr >= (hp)->old.mem)

#define MARK_MODIFIED(hp, ptr)							\
	if (!HTYPE(ptr)->remembered && IS_OLD(hp, ptr)) {	\
		heap_remember(hp, HTYPE(ptr));					\
		HTYPE(ptr)->remembered  = 1;					\
	}

/* Heap space */
typedef struct {
	void *mem;					/* Memory */
	void *pos;					/* Current position */
	size_t size;				/* Total size of space */
	size_t free_mem;			/* Free memory */
	int blocks;					/* Allocated blocks */
} space_t;

/* Node in remembered set */
typedef struct remembered_s {
	block_hdr_t *hdr;			/* Pointer on object */
	struct remembered_s *next;	/* Next remembered */
} remembered_t;

typedef struct {
	remembered_t *head, *tail;
} remembered_set_t;

/* Main heap interface */
typedef struct {
	visitor_t visitor;			/* Visitor */
	void *mem;					/* Memory allocated for the heap */
	size_t mem_size;			/* Size of memory */
	space_t fresh;				/* Space for fresh objects  */
	space_t survived_spaces[2];	/* Spaces for survived objects */
	space_t *survived;
	space_t *future_survived;
	space_t old;				/* Old generation */
	remembered_set_t remembered_set;
	unsigned full_gc:1;			/* Full gc flag */
	hash_table_t *old_map;		/* Map used to update pointers to OLD objects */
	vm_thread_t *thread;		/* Thread pointer */
	allocator_t allocator;		/* Provide allocator interface */
} heap_t;

void heap_init(heap_t *heap, vm_thread_t *thread);
void heap_destroy(heap_t *heap);
void* heap_alloc(heap_t *heap, int size, int type_id);
void* heap_alloc0(heap_t *heap, int size, int type_id);
void heap_require(heap_t *heap, int size);
void heap_require_blocks(heap_t *heap, int size, int count);
void heap_remember(heap_t *heap, block_hdr_t *hdr);

#endif /* HEAP_H */
