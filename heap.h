#ifndef HEAP_H
#define HEAP_H 
#include "types.h"

/*
 * Heap exports
 */

typedef struct {
	unsigned short reached:1;
	unsigned short size:15;
} block_hdr_t;

typedef struct {
	void *page;
	void *pos;
	int page_size;
	int blocks;
	int free_mem;
} copy_heap_t;

typedef struct {
	visitor_t visitor;
	copy_heap_t heaps[2];
	copy_heap_t *from, *to;
	visitor_fun vm_inspect;
	void *vm;
} heap_t;

void heap_init(heap_t *heap, visitor_fun vm_inspect, void *vm);
void heap_destroy(heap_t *heap);
void* heap_alloc(heap_t *heap, int size);
void heap_swap(heap_t *heap);

#endif /* HEAP_H */
