#include "memory.h"

void* _mem_alloc(const char *file, int line,
		const char *func, size_t size)
{
	void *res = malloc(size);
	if (!res)
		FATAL("Failed to allocate %ld bytes memory at %s:%d, func %s\n",
				size, file, line, func);
	return res;
}

void* _mem_realloc(const char *file, int line,
		const char *func, void *p, size_t size)
{
	void *res = realloc(p, size);
	if (!res)
		FATAL("Failed to reallocate %ld bytes memory at %s:%d, func %s\n",
				size, file, line, func);
	return res;
}

void _mem_free(const char *file, int line,
		const char *func, void *p)
{
	if (!p)
		FATAL("Attempt to free pointer 0x%lx at %s:%d, func %s\n",
				(unsigned long)p, file, line, func);
	free(p);
}


void* _mem_calloc(const char *file, int line,
		const char *func, size_t nmemb, size_t size)
{
	void *res = calloc(nmemb, size);
	if (!res)
		FATAL("Failed to callocate %ld objects by %ld bytes at %s:%d, func %s\n",
				nmemb, size, file, line, func);

	return res;
}
