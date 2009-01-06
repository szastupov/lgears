#include <sys/mman.h>
#include <unistd.h>
#include <errno.h>
#include "memory.h"
#include "heap.h"

static void copy_heap_init(copy_heap_t *heap)
{
	heap->page_size = sysconf(_SC_PAGE_SIZE);
	heap->page = mmap(NULL, heap->page_size,
			PROT_READ|PROT_WRITE, MAP_PRIVATE|MAP_ANONYMOUS, 0, 0);
	if (heap->page == MAP_FAILED)
		FATAL("failed to mmap page: %s\n", strerror(errno));

	heap->pos = heap->page;
	heap->blocks = 0;
}

static void copy_heap_destroy(copy_heap_t *heap)
{
	munmap(heap->page, heap->page_size);
}

static void* copy_heap_alloc(copy_heap_t *heap, size_t size)
{
	block_hdr_t *hdr = heap->pos;
	hdr->reached = 0;
	hdr->size = size;
	heap->pos += sizeof(block_hdr_t);

	void *res = heap->pos;
	heap->pos += size;
	heap->blocks++;

	return res;
}

static void* copy_heap_copy(copy_heap_t *heap, void *p, size_t size)
{
	void *res = heap->pos;
	memcpy(res, p, size);
	heap->pos += size;
	heap->blocks++;
	return res;
}

static void copy_heap_clear(copy_heap_t *heap)
{
	heap->pos = heap->page;
	heap->blocks = 0;
}

void heap_swap(heap_t *heap)
{
	copy_heap_clear(heap->from);
	copy_heap_t *tmp = heap->from;
	heap->from = heap->to;
	heap->to = tmp;
}

void* heap_alloc(heap_t *heap, int size)
{
	return copy_heap_alloc(heap->from, size);
}

static void heap_mark(visitor_t *visitor, obj_t *obj)
{
	heap_t *heap = visitor->user_data;

	if (obj->tag != id_ptr)
		return;

	ptr_t ptr;
	ptr.ptr = obj->ptr;

	void *p = ptr_get(&ptr);
	p -= sizeof(block_hdr_t);
	block_hdr_t *hdr = p;
	hdr->reached = 1;

	void *new_pos = copy_heap_copy(heap->to, p, hdr->size+sizeof(block_hdr_t));
	ptr_set(&ptr, (unsigned long)new_pos);
	obj->ptr = ptr.ptr;
}

void heap_init(heap_t *heap)
{
	heap->from = &heap->heaps[0];
	heap->to = &heap->heaps[1];
	copy_heap_init(heap->from);
	copy_heap_init(heap->to);

	heap->visitor.visit = heap_mark;
	heap->visitor.user_data = heap;
}

void heap_destroy(heap_t *heap)
{
	copy_heap_destroy(heap->from);
	copy_heap_destroy(heap->to);
}
