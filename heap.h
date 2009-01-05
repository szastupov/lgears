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
} copy_heap_t;

typedef struct {
	copy_heap_t heaps[2];
	copy_heap_t *from, *to;
} heap_t;

void heap_init(heap_t *heap);
void heap_destroy(heap_t *heap);
void* heap_alloc(heap_t *heap, int size);
void heap_swap(heap_t *heap);
void heap_mark(heap_t *heap, obj_t *obj);

typedef struct {
	const char *name;
	void (*destructor)(void*);
	void (*visit)(heap_t*, void*);
} heap_type_t;

#endif /* HEAP_H */
