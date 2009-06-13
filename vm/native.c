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
#include "native.h"

int native_call_variadic(vm_thread_t *thread,
						 native_func_t *native,
						 obj_t *argv,
						 int argc)
{
	return ((native_variadic)native->fp)(thread, argv, argc);
}

int native_call_nullary(vm_thread_t *thread,
						native_func_t *native,
						obj_t *argv,
						int argc)
{
	return ((native_nullary)native->fp)(thread);
}

int native_call_unary(vm_thread_t *thread,
					  native_func_t *native,
					  obj_t *argv,
					  int argc)
{
	return ((native_unary)native->fp)(thread, &argv[1]);
}

int native_call_binary(vm_thread_t *thread,
					   native_func_t *native,
					   obj_t *argv,
					   int argc)
{
	return ((native_binary)native->fp)(thread, &argv[1], &argv[2]);
}

int native_call_ternary(vm_thread_t *thread,
						native_func_t *native,
						obj_t *argv,
						int argc)
{
	return ((native_ternary)native->fp)(thread, &argv[1], &argv[2], &argv[3]);
}

static void print_const(obj_t obj)
{
	const char *descr[] = {
		"()",
		"#t",
		"#f",
		"<void>",
		"<eof>"
	};
	static int max_id = sizeof(descr)/sizeof(char*)-1;
	int id = FIXNUM(obj);
	if (id < 0 || id > max_id)
		FATAL("wrong const id %d\n", id);

	printf("%s", descr[id]);
}

static void print_ptr(obj_t obj)
{
	void *ptr = PTR(obj);
	const type_t *type = &type_table[HTYPE_TAG(ptr)];

	if (type->repr)
		type->repr(ptr);
	else
		printf("<ptr:%s>", type->name);
}

static void print_func(obj_t obj)
{
	void *ptr = PTR(obj);
	native_func_t *native;
	func_t *interp;
	func_hdr_t *fhdr = ptr;
	switch (fhdr->type) {
	case func_inter:
		interp = ptr;
		printf("<lambda/%d>", interp->hdr.argc-1);
		break;
	case func_native:
		native = ptr;
		printf("<native %s/%d>", native->name, native->hdr.argc-1);
		break;
	default:
		printf("<unknown func>");
	}
}

void print_obj(obj_t obj)
{
	switch (GET_TAG(obj)) {
	case TAG_CONST_PTR:
	case TAG_PTR:
		print_ptr(obj);
		break;
	case TAG_FIXNUM:
		printf("%ld", FIXNUM(obj));
		break;
	case TAG_CHAR:
		printf("#\\%lc", CHAR(obj));
		break;
	case TAG_FUNC:
		print_func(obj);
		break;
	case TAG_CONST:
		print_const(obj);
		break;
	default:
		printf("unknown obj");
	}
}
