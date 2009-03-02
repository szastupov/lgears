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
 * @date 2009-01-06
 */


typedef struct {
	unsigned size;
	unsigned type_id:4;
	unsigned forward:1;
} block_hdr_t;
#define BHDR_SIZE sizeof(block_hdr_t)

static inline void* get_typed(obj_t obj, int type_id)
{
	ptr_t ptr = { .ptr = obj.ptr };
	if (ptr.tag != id_ptr) {
		printf("expected ptr but got %d\n", ptr.tag);
		return NULL;
	}
	void *res = PTR_GET(ptr);

	block_hdr_t *bhdr = res-BHDR_SIZE;
	if (bhdr->type_id != type_id) {
		printf("expected type %s\n", type_table[type_id].name);
		return NULL;
	}
	return res;
}

typedef struct {
	void *mem;
	void *pos;
	int size;
	int free_mem;
	int blocks;
} copy_heap_t;

typedef struct {
	visitor_t visitor;
	void *page;
	int page_size;
	copy_heap_t heaps[2];
	copy_heap_t *from, *to;
	visitor_fun vm_get_roots;
	void *vm;
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

void heap_stat(heap_t *heap);

#endif /* HEAP_H */
