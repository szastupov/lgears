#include <sys/mman.h>
#include <unistd.h>
#include <errno.h>
#include "memory.h"
#include "heap.h"

#if __WORDSIZE == 64
#define balign 7
#else
#define balign 3
#endif
#define align_up(s)	(((s)+balign) & ~balign)

#define BHDR_ZIZE sizeof(block_hdr_t)

static void copy_heap_reset(copy_heap_t *heap)
{
	int pad = align_up(BHDR_ZIZE)-BHDR_ZIZE;
	heap->pos = heap->page + pad;
	heap->free_mem = heap->page_size - pad;
	heap->blocks = 0;
}

static void copy_heap_init(copy_heap_t *heap)
{
	heap->page_size = sysconf(_SC_PAGE_SIZE);
	heap->page = mmap(NULL, heap->page_size,
			PROT_READ|PROT_WRITE, MAP_PRIVATE|MAP_ANONYMOUS, 0, 0);
	if (heap->page == MAP_FAILED)
		FATAL("failed to mmap page: %s\n", strerror(errno));

	copy_heap_reset(heap);
}

static void copy_heap_destroy(copy_heap_t *heap)
{
	munmap(heap->page, heap->page_size);
}

static void* copy_heap_alloc(copy_heap_t *heap, size_t size)
{
	size_t need = align_up(size+BHDR_ZIZE);
	if (need > heap->free_mem) {
		printf("out of free mem on heap (need %ld)\n", need);
		return NULL;
	}

	block_hdr_t *hdr = heap->pos;
	hdr->reached = 0;
	hdr->size = size;
	heap->pos += sizeof(block_hdr_t);
	void *res = heap->pos;

	heap->pos += need-BHDR_ZIZE;
	heap->blocks++;
	heap->free_mem -= need;

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

void heap_swap(heap_t *heap)
{
	copy_heap_reset(heap->from);
	copy_heap_t *tmp = heap->from;
	heap->from = heap->to;
	heap->to = tmp;
}

void* heap_alloc(heap_t *heap, int size)
{
	void *res = copy_heap_alloc(heap->from, size);
	if (!res) {
		heap->vm_inspect(&heap->visitor, heap->vm);
		heap_swap(heap);
		res = copy_heap_alloc(heap->from, size);
	}
	return res;
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

void heap_init(heap_t *heap, visitor_fun vm_inspect, void *vm)
{
	heap->from = &heap->heaps[0];
	heap->to = &heap->heaps[1];
	copy_heap_init(heap->from);
	copy_heap_init(heap->to);

	heap->visitor.visit = heap_mark;
	heap->visitor.user_data = heap;
	heap->vm_inspect = vm_inspect;
	heap->vm = vm;
}

void heap_destroy(heap_t *heap)
{
	copy_heap_destroy(heap->from);
	copy_heap_destroy(heap->to);
}
