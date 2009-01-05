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

void heap_init(heap_t *heap)
{
	heap->from = &heap->heaps[0];
	heap->to = &heap->heaps[1];
	copy_heap_init(heap->from);
	copy_heap_init(heap->to);
}

void heap_destroy(heap_t *heap)
{
	copy_heap_destroy(heap->from);
	copy_heap_destroy(heap->to);
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

void heap_mark(heap_t *heap, obj_t *obj)
{
	if (obj->tag != id_ptr)
		return;
	num_t num;
	num.ptr = obj->ptr;

	void *p = (void*)(unsigned long)num_get(num);
	p -= sizeof(block_hdr_t);
	block_hdr_t *hdr = p;
	hdr->reached = 1;

	void *new_pos = copy_heap_copy(heap->to, p, hdr->size+sizeof(block_hdr_t));
	num_set(&num, (unsigned long)new_pos);
	obj->ptr = num.ptr;
}

#if 0
int main()
{
	heap_t heap;
	heap_init(&heap);

	int i;
	num_t n;
	for (i = 0; i < 30; i++) {
		void *p = heap_alloc(&heap, 4+i);
		num_set(&n, (unsigned long)p);
		printf("%p\n", p);
	}
	obj_t obj;
	obj.ptr = n.ptr;
	printf("Was %p\n", (void*)num_get(n));
	heap_mark(&heap, &obj);
	n.ptr = obj.ptr;
	printf("Become %p\n", (void*)num_get(n));
	heap_swap(&heap);

	heap_destroy(&heap);

	return 0;
}
#endif
