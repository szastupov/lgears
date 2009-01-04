#include "memory.h"
#include <sys/mman.h>
#include <unistd.h>
#include <errno.h>

typedef struct {
	void *page;
	void *pos;
	int page_size;
	int blocks;
} fresh_heap_t;

typedef struct {
	short reached:1;
	short size:15;
} block_hdr_t;

void fresh_heap_init(fresh_heap_t *heap)
{
	heap->page_size = sysconf(_SC_PAGE_SIZE);
	heap->page = mmap(NULL, heap->page_size,
			PROT_READ|PROT_WRITE, MAP_PRIVATE|MAP_ANONYMOUS, 0, 0);
	if (heap->page == MAP_FAILED)
		FATAL("failed to mmap page: %s\n", strerror(errno));

	heap->pos = heap->page;
	heap->blocks = 0;
}

void* fresh_heap_alloc(fresh_heap_t *heap, size_t size)
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

void fresh_heap_traverse(fresh_heap_t *heap)
{
	int i;
	void *p = heap->page;
	for (i = 0; i < heap->blocks; i++) {
		block_hdr_t *hdr = p;
		p += sizeof(*hdr);
		printf("Block %d, reached %d, size %d, memory %p\n",
				i, hdr->reached, hdr->size, p);
		p += hdr->size;
	}
}

int main()
{
	/*
	fresh_heap_t heap;
	fresh_heap_init(&heap);
`
	int i;
	for (i = 0; i < 30; i++)
		printf("%p\n", fresh_heap_alloc(&heap, 4+i));

	fresh_heap_traverse(&heap);
	*/
	return 0;
}
