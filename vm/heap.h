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
#ifndef HEAP_H
#define HEAP_H
#include "types.h"
#include "hash.h"

/**
 * @file heap.h
 * @brief Heap manager defination
 * @author Stepan Zastupov
 */

/**
 * @brief Header attached to each object on heap
 */
typedef struct {
	unsigned size;		/**< Size of block (with padding if need) */
	unsigned type_id:4;	/**< Type id @see type_table */
	unsigned forward:1;	/**< Indicate that pointer should be forwarded */
	unsigned generation:2;	/**< Generation */
	unsigned modified:1;
} block_hdr_t;

#define BHDR_SIZE sizeof(block_hdr_t)
#define HTYPE(ptr) ((block_hdr_t*)((void*)ptr-BHDR_SIZE))
#define HTYPE_TAG(ptr) HTYPE(ptr)->type_id
#define IS_TYPE(obj, tid) \
	((obj).tag == id_ptr && HTYPE_TAG(PTR(obj)) == tid)

#define IS_OLD(hp, ptr) ((void*)ptr >= (hp)->old.mem)

#define MARK_MODIFIED(hp, ptr)						\
	if (!HTYPE(ptr)->modified && IS_OLD(hp, ptr)) {	\
		remember(hp, HTYPE(ptr));					\
		LOG_DBG("marked as modified");				\
	}

/**
 * @brief Copying heap
 */
typedef struct {
	void *mem;		/**< Memory */
	void *pos;		/**< Current position */
	size_t size;		/**< Total size of space */
	size_t free_mem;	/**< Free memory */
	int blocks;
} space_t;

typedef struct remembered_s {
	block_hdr_t *hdr;
	struct remembered_s *next;
} remembered_t;

/**
 * @brief Heap interface
 */
typedef struct {
	visitor_t visitor;
	void *mem;
	size_t mem_size;
	space_t fresh;
	space_t survived_spaces[2];
	space_t *survived;
	space_t *future_survived;
	space_t old;
	visitor_fun vm_get_roots;
	visitor_fun vm_after_gc;
	remembered_t *rem_head;
	remembered_t *rem_tail;
	unsigned full_gc:1;
	hash_table_t *old_map;
	void *vm;
} heap_t;

/**
 * @brief Init heap manager
 */
void heap_init(heap_t *heap, visitor_fun vm_get_roots,
		visitor_fun vm_after_gc, void *vm);

/**
 * @brief Destroy heap
 *
 * @param heap
 */
void heap_destroy(heap_t *heap);

/**
 * @brief Alloc memory form heap
 *
 * @param heap
 * @param size
 *
 * @return ponter to allocated memory
 */
void* heap_alloc(heap_t *heap, int size, int type_id);
void* heap_alloc0(heap_t *heap, int size, int type_id);
void heap_require(heap_t *heap, int size);
void heap_mark_modified(heap_t *heap, void *ptr);
void remember(heap_t *heap, block_hdr_t *hdr);

#endif /* HEAP_H */
