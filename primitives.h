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
#ifndef PRIMITIVES_H
#define PRIMITIVES_H 

#include "vm.h"

enum { RC_OK, RC_ERROR, RC_EXIT };

typedef struct {
	func_hdr_t hdr;
	void *fp;
	int arity;
	const char *name;
} native_t;

int native_call(vm_thread_t *thread, native_t *native, obj_t *argv, int argc);
typedef int (*native_nullary)(vm_thread_t*);
typedef int (*native_unary)(vm_thread_t*, obj_t);
typedef int (*native_binary)(vm_thread_t*, obj_t, obj_t);
typedef int (*native_ternary)(vm_thread_t*, obj_t, obj_t, obj_t);
typedef int (*native_variadic)(vm_thread_t*, obj_t*, int);

#define MAKE_NATIVE(func, farity, fargc, fswallow) \
	const native_t func##_nt = { \
		.hdr.type = func_native, \
		.hdr.argc = fargc+1, \
		.arity = farity, \
		.hdr.swallow = fswallow, \
		.fp = func, \
		.name = #func \
	}

#define MAKE_NATIVE_VARIADIC(func, fargc, fswallow) \
	MAKE_NATIVE(func, -1, fargc, fswallow)

#define MAKE_NATIVE_NULLARY(func) \
	MAKE_NATIVE(func, 0, 0, 0)

#define MAKE_NATIVE_UNARY(func) \
	MAKE_NATIVE(func, 1, 1, 0)

#define MAKE_NATIVE_BINARY(func) \
	MAKE_NATIVE(func, 2, 2, 0)

#define MAKE_NATIVE_TERNARY(func) \
	MAKE_NATIVE(func, 3, 3, 0)

#define RESULT_FIXNUM(num) \
	FIXNUM_INIT(*(fixnum_t*)&thread->tramp.arg[0], num); \
	return RC_OK;

/** 
 * @brief Terminate thread if assertion failed
 * 
 * @param e condition
 */
#define SAFE_ASSERT(e) if (!(e)) { \
	fprintf(stderr, "Assertion failed %s\n", #e); \
	return RC_ERROR; \
}

#define RESULT_ERROR(msg...) { \
	fprintf(stderr, msg); \
	return RC_ERROR; \
}

void ns_install_native(hash_table_t *tbl,
		char *name, const native_t *nt);

void ns_install_primitives(hash_table_t *tbl);
void pair_repr(void *ptr);
void pair_visit(visitor_t *vs, void *data);

typedef struct {
	char *str;
	int size;
	unsigned copy:1;
} string_t;

void* _string(heap_t *heap, char *str, int copy);
void string_repr(void *ptr);

#endif /* PRIMITIVES_H */
