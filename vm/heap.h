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
	unsigned modified:1;	/**< Modification flag */
} block_hdr_t;

#define BHDR_SIZE sizeof(block_hdr_t)
#define HTYPE(ptr) ((block_hdr_t*)((void*)ptr-BHDR_SIZE))
#define HTYPE_TAG(ptr) HTYPE(ptr)->type_id
#define IS_TYPE(obj, tid) \
	(obj.tag == id_ptr && HTYPE_TAG(PTR(obj)) == tid)
#define MARK_MODIFIED(heap, ptr) if (!HTYPE(ptr)->modified)	\
		heap_mark_modified(heap, ptr);

/** 
 * @brief Check object type and return pointer
 * 
 * @param obj Object
 * @param type_id Valid type id
 */
static inline void* get_typed(obj_t obj, int type_id)
{
	ASSERT(obj.ptr != NULL);
	if (obj.tag != id_ptr) {
		printf("expected ptr but got %d\n", obj.tag);
		return NULL;
	}
	void *res = PTR(obj);

	block_hdr_t *bhdr = res-BHDR_SIZE;
	if (bhdr->type_id != type_id) {
		printf("expected type %s but got %s\n", type_table[type_id].name, type_table[bhdr->type_id].name);
		return NULL;
	}
	return res;
}

typedef struct {
	uint8_t blocks:7;
	uint8_t dirty:1;
} card_t;

/** 
 * @brief Copying heap
 */
typedef struct {
	void *mem;		/**< Memory */
	void *pos;		/**< Current position */
	size_t size;		/**< Total size of space */
	size_t free_mem;	/**< Free memory */
	int cur_card;
	card_t cards[8];
} copy_heap_t;

/** 
 * @brief Heap interface
 */
typedef struct {
	visitor_t visitor;			/**< VM visitor */
	int page_size;				/**< Size of page */
	int count;					/**< Count of heaps */
	copy_heap_t heaps[5];		/**< Copying heaps */
	copy_heap_t *from, *to;	
	visitor_fun vm_get_roots;	/**< Callback to procedure which mark root objects */
	visitor_fun vm_after_gc;
	void *vm;					/**< VM pointer */
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

#endif /* HEAP_H */
