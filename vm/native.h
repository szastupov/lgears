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

typedef struct native_func_s {
	func_hdr_t hdr;				/* Function header */
	void *fp;					/* Real function */
	/* Arity wrapper */
	int (*call)(vm_thread_t *thread,
				struct native_func_s *native,
				obj_t *argv,
				int argc);
	const char *name;			/* Name */
} native_func_t;

typedef struct {
	void (*init)();
	void (*cleanup)();
} native_module_t;

int native_call(vm_thread_t *thread, native_func_t *native, obj_t *argv, int argc);
typedef int (*native_nullary)(vm_thread_t*);
typedef int (*native_unary)(vm_thread_t*, obj_t*);
typedef int (*native_binary)(vm_thread_t*, obj_t*, obj_t*);
typedef int (*native_ternary)(vm_thread_t*, obj_t*, obj_t*, obj_t*);
typedef int (*native_variadic)(vm_thread_t*, obj_t*, int);

int native_call_variadic(vm_thread_t *thread,
						 native_func_t *native,
						 obj_t *argv,
						 int argc);
int native_call_nullary(vm_thread_t *thread,
						native_func_t *native,
						obj_t *argv,
						int argc);
int native_call_unary(vm_thread_t *thread,
					  native_func_t *native,
					  obj_t *argv,
					  int argc);
int native_call_binary(vm_thread_t *thread,
					   native_func_t *native,
					   obj_t *argv,
					   int argc);
int native_call_ternary(vm_thread_t *thread,
						native_func_t *native,
						obj_t *argv,
						int argc);

#define MAKE_NATIVE(func, fcall, fargc, fswallow)					\
	const native_func_t __attribute__((aligned(8))) func##_nt = {	\
		.hdr.type = func_native,									\
		.hdr.argc = fargc+1,										\
		.hdr.swallow = fswallow,									\
		.call = fcall,												\
		.fp = func,													\
		.name = #func												\
	}

#define MAKE_NATIVE_VARIADIC(func, fargc)				\
	MAKE_NATIVE(func, native_call_variadic, fargc, 1)

#define MAKE_NATIVE_NULLARY(func)					\
	MAKE_NATIVE(func, native_call_nullary, 0, 0)

#define MAKE_NATIVE_UNARY(func)					\
	MAKE_NATIVE(func, native_call_unary, 1, 0)

#define MAKE_NATIVE_BINARY(func)				\
	MAKE_NATIVE(func, native_call_binary, 2, 0)

#define MAKE_NATIVE_TERNARY(func)					\
	MAKE_NATIVE(func, native_call_ternary, 3, 0)

#define PROTECT_LOCAL1(x)						\
	local_roots_t __lr = {						\
		.count = 1,								\
		.objects[0] = &x,						\
	};											\
	thread->local_roots = &__lr;

#define PROTECT_LOCAL2(x, y)					\
	local_roots_t __lr = {						\
		.count = 2,								\
		.objects[0] = &x,						\
		.objects[1] = &y,						\
	};											\
	thread->local_roots = &__lr;

#define DEFINE_LOCAL1(x)						\
	obj_t x = cvoid;							\
	PROTECT_LOCAL1(x)

#define DEFINE_LOCAL2(x, y)						\
	obj_t x = cvoid;							\
	obj_t y = cvoid;							\
	PROTECT_LOCAL2(x, y);

#define RETURN_OBJ(obj)							\
	STACK_PUSH(obj);							\
	return RC_OK;

#define RETURN_FIXNUM(num) RETURN_OBJ(MAKE_FIXNUM(num))

#define RETURN_CHAR(chr) RETURN_OBJ(MAKE_CHAR(chr))

#define RETURN_BOOL(b) RETURN_OBJ(CIF(b));

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
		const char *name, const native_func_t *nt);
void ns_install_global(const char *name, const native_func_t *nt);
void print_obj(obj_t obj);

#endif /* NATIVE_H */
