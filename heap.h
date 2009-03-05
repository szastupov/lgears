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
} block_hdr_t;
#define BHDR_SIZE sizeof(block_hdr_t)
#define HTYPE_TAG(ptr) ((block_hdr_t*)(ptr-BHDR_SIZE))->type_id

#define IS_TYPE(obj, tid) \
	(obj.tag == id_ptr && HTYPE_TAG(PTR(obj)) == tid)

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

/** 
 * @brief Copying heap
 */
typedef struct {
	void *mem;		/**< Memory */
	void *pos;		/**< Current position */
	int size;		/**< Total size of space */
	int free_mem;	/**< Free memory */
	int blocks;		/**< Used blocks */
} copy_heap_t;

/** 
 * @brief Heap interface
 */
typedef struct {
	visitor_t visitor;			/**< VM visitor */
	void *page;					/**< Managed page (actualy memory received from malloc() */
	int page_size;				/**< Size of page */
	copy_heap_t heaps[2];		/**< Two copying heaps */
	copy_heap_t *from, *to;	
	visitor_fun vm_get_roots;	/**< Callback to procedure which mark root objects */
	void *vm;					/**< VM pointer */
} heap_t;

/** 
 * @brief Init heap manager
 * 
 * @param heap heap structure
 * @param vm_get_roots inspect function
 * @param vm vm pointer
 */
void heap_init(heap_t *heap, visitor_fun vm_get_roots, void *vm);

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

/** 
 * @brief Print heap statistics
 * 
 * @param heap 
 */
void heap_stat(heap_t *heap);

#endif /* HEAP_H */
