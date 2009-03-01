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
	unsigned short size;
	void *forward;
} block_hdr_t;

typedef struct {
	void *mem;
	void *pos;
	int size;
	int blocks;
	int free_mem;
} copy_heap_t;

typedef struct {
	visitor_t visitor;
	void *page;
	int page_size;
	copy_heap_t heaps[2];
	copy_heap_t *from, *to;
	visitor_fun vm_inspect;
	void *vm;
} heap_t;

/** 
 * @brief Init heap manager
 * 
 * @param heap heap structure
 * @param vm_inspect inspect function
 * @param vm vm pointer
 */
void heap_init(heap_t *heap, visitor_fun vm_inspect, void *vm);

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
void* heap_alloc(heap_t *heap, int size);
void* heap_alloc0(heap_t *heap, int size);

void heap_stat(heap_t *heap);

#endif /* HEAP_H */
