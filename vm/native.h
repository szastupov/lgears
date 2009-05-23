/*
 * This file is part of lGears scheme system
 * Copyright (C) 2009 Stepan Zastupov <redchrom@gmail.com>
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 3 of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General
 * Public Licens along with this program, if not; see
 * <http://www.gnu.org/licenses>.
 */
#ifndef NATIVE_H
#define NATIVE_H
#include "vm.h"

enum { RC_OK, RC_ERROR, RC_EXIT };

typedef struct {
	func_hdr_t hdr;
	void *fp;
	int arity;
	const char *name;
} native_func_t;

int native_call(vm_thread_t *thread, native_func_t *native, obj_t *argv, int argc);
typedef int (*native_nullary)(vm_thread_t*);
typedef int (*native_unary)(vm_thread_t*, obj_t*);
typedef int (*native_binary)(vm_thread_t*, obj_t*, obj_t*);
typedef int (*native_ternary)(vm_thread_t*, obj_t*, obj_t*, obj_t*);
typedef int (*native_variadic)(vm_thread_t*, obj_t*, int);

#define MAKE_NATIVE(func, farity, fargc, fswallow)					\
	const native_func_t __attribute__((aligned(8))) func##_nt = {	\
		.hdr.type = func_native,									\
		.hdr.argc = fargc+1,										\
		.hdr.swallow = fswallow,									\
		.arity = farity,											\
		.fp = func,													\
		.name = #func												\
	}

#define MAKE_NATIVE_VARIADIC(func, fargc)		\
	MAKE_NATIVE(func, -1, fargc, 1)

#define MAKE_NATIVE_NULLARY(func)				\
	MAKE_NATIVE(func, 0, 0, 0)

#define MAKE_NATIVE_UNARY(func)					\
	MAKE_NATIVE(func, 1, 1, 0)

#define MAKE_NATIVE_BINARY(func)				\
	MAKE_NATIVE(func, 2, 2, 0)

#define MAKE_NATIVE_TERNARY(func)				\
	MAKE_NATIVE(func, 3, 3, 0)

#define RETURN_OBJ(obj)							\
	STACK_PUSH(obj);							\
	return RC_OK;

#define RETURN_FIXNUM(num) {					\
		fixnum_t fx; FIXNUM_INIT(fx, num);		\
		RETURN_OBJ(fx.obj);						\
	}

#define RETURN_CHAR(chr) {						\
		char_t c; CHAR_INIT(c, chr);			\
		RETURN_OBJ(c.obj);						\
	}

#define RETURN_BOOL(b)							\
	RETURN_OBJ(CIF(b).obj);

#if FATAL_SAFE_ASSERT
#define FAIL_ASSERT FATAL
#else
#define FAIL_ASSERT(msg...) fprintf(stderr, msg);
#endif

#define SAFE_ASSERT(e) if (!(e)) {					\
		FAIL_ASSERT("Assertion failed %s\n", #e);	\
		return RC_ERROR;							\
	}

#define RETURN_ERROR(msg...) {					\
		fprintf(stderr, msg);					\
		return RC_ERROR;						\
	}

void ns_install_native(hash_table_t *tbl,
		char *name, const native_func_t *nt);
void print_obj(obj_t obj);

#endif /* NATIVE_H */
