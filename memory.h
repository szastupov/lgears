/*
 *  (c) 2008 Stepan Zastupov <redchrom@gmail.com>
 */

#ifndef MEMORY_H
#define MEMORY_H

#include <stdlib.h>
#include <string.h>
#include <stdio.h>

#define container_of(ptr, type, member) ({ \
	const typeof( ((type *)0)->member ) *__mptr = (ptr); \
	(type *)( (char *)__mptr - offsetof(type,member) );})

#if defined(__GNUC__)  && __GNUC__ >= 4
#define offsetof(TYPE, MEMBER) __builtin_offsetof(TYPE, MEMBER)
#else
#define offsetof(TYPE, MEMBER) ((size_t) &((TYPE *)0)->MEMBER)
#endif


#define FATAL(...)	\
{										\
	fprintf(stderr, "%s:%d `%s': ",		\
			__FILE__, __LINE__, __FUNCTION__);	\
	fprintf(stderr, __VA_ARGS__);		\
	abort();							\
}

void* _mem_alloc(const char *file, int line,
		const char *func, size_t size);
#define mem_alloc(size) _mem_alloc(__FILE__, __LINE__, __FUNCTION__, size)

void* _mem_realloc(const char *file, int line,
		const char *func, void *p, size_t size);
#define mem_realloc(p, size) _mem_realloc(__FILE__, __LINE__, __FUNCTION__, p, size)

void _mem_free(const char *file, int line,
		const char *func, void *p);
#define mem_free(p) _mem_free(__FILE__, __LINE__, __FUNCTION__, p)

void* _mem_calloc(const char *file, int line,
		const char *func, size_t nmemb, size_t size);
#define mem_calloc(nmemb, size) _mem_calloc(__FILE__, __LINE__, __FUNCTION__, nmemb, size)

#define type_alloc(type) mem_calloc(1, sizeof(type))

#endif
