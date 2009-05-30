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
#include "primitives.h"

/*
 * Structure used by vectors and records (and anything else which
 * needs O(1) access to fields). To distinguish types (i.e. record
 * vs. vector), a programmer should use type_name field (accessible
 * via struct-type procedure).
 */

int t_struct;

static void struct_repr(void *ptr)
{
	struct_t *st = ptr;
	printf("#<%s: ", (char*)PTR(st->type_name));
	int i;
	for (i = 0; i < st->size; i++) {
		print_obj(st->fields[i]);
		if (i < st->size-1)
			printf(" ");
	}
	printf(">");
}

static void struct_visit(visitor_t *vs, void *data)
{
	struct_t *st = data;
	st->fields = data+sizeof(struct_t);
	int i;
	for (i = 0; i < st->size; i++)
		vs->visit(vs, &st->fields[i]);
}

struct_t* struct_new(allocator_t *al, obj_t *type_name, int size)
{
	void *mem = allocator_alloc(al, sizeof(struct_t)+sizeof(obj_t)*size, t_struct);
	struct_t *st = mem;
	st->fields = mem+sizeof(struct_t);
	st->size = size;
	st->type_name = *type_name;

	return st;
}

static int make_struct(vm_thread_t *thread, obj_t *argv, int argc)
{
	SAFE_ASSERT(IS_SYMBOL(argv[1]));
	int count = argc-2;
	struct_t *st = struct_new(&thread->heap.allocator, &argv[1], count);

	int i;
	for (i = 0; i < count; i++)
		st->fields[i] = argv[i+2];

	RETURN_OBJ(MAKE_HEAP_PTR(st));
}
MAKE_NATIVE_VARIADIC(make_struct, 0);

static int alloc_struct(vm_thread_t *thread, obj_t *type_name,
						obj_t *count, obj_t *fill)
{
	SAFE_ASSERT(IS_SYMBOL(*type_name));
	SAFE_ASSERT(IS_FIXNUM(*count));

	struct_t *st = struct_new(&thread->heap.allocator, type_name, FIXNUM(*count));
	int i;
	for (i = 0; i < st->size; i++)
		st->fields[i] = *fill;

	RETURN_OBJ(MAKE_HEAP_PTR(st));
}
MAKE_NATIVE_TERNARY(alloc_struct);

static int struct_size(vm_thread_t *thread, obj_t *obj)
{
	SAFE_ASSERT(IS_STRUCT(*obj));
	struct_t *st = PTR(*obj);

	RETURN_FIXNUM(st->size);
}
MAKE_NATIVE_UNARY(struct_size);

static int struct_set(vm_thread_t *thread, obj_t *obj, obj_t *opos, obj_t *val)
{
	SAFE_ASSERT(IS_STRUCT(*obj));
	SAFE_ASSERT(IS_CONST_PTR(*obj));
	struct_t *st = PTR(*obj);
	int pos = FIXNUM(*opos);
	SAFE_ASSERT(st->size > pos);

	st->fields[pos] = *val;
	MARK_MODIFIED(&thread->heap, st);

	RETURN_OBJ(cvoid);
}
MAKE_NATIVE_TERNARY(struct_set);

static int struct_ref(vm_thread_t *thread, obj_t *obj, obj_t *opos)
{
	SAFE_ASSERT(IS_STRUCT(*obj));
	SAFE_ASSERT(IS_FIXNUM(*opos));
	struct_t *st = PTR(*obj);
	int pos = FIXNUM(*opos);
	SAFE_ASSERT(st->size > pos);

	RETURN_OBJ(st->fields[pos]);
}
MAKE_NATIVE_BINARY(struct_ref);

static int struct_type(vm_thread_t *thread, obj_t *obj)
{
	SAFE_ASSERT(IS_STRUCT(*obj));
	struct_t *st = PTR(*obj);
	RETURN_OBJ(st->type_name);
}
MAKE_NATIVE_UNARY(struct_type);

static int struct_to_list(vm_thread_t *thread, obj_t *obj)
{
	SAFE_ASSERT(IS_STRUCT(*obj));

	/* GC magic */
	struct_t *st = PTR(*obj);
	heap_require_blocks(&thread->heap, sizeof(pair_t), st->size);
	st = PTR(*obj);

	RETURN_OBJ(_list(thread, st->fields, st->size));
}
MAKE_NATIVE_UNARY(struct_to_list);

void struct_init()
{
	t_struct = register_type("struct", struct_repr, struct_visit);
	ns_install_global("make-struct", &make_struct_nt);
	ns_install_global("alloc-struct", &alloc_struct_nt);
	ns_install_global("struct-size", &struct_size_nt);
	ns_install_global("struct-set!", &struct_set_nt);
	ns_install_global("struct-ref", &struct_ref_nt);
	ns_install_global("struct-type", &struct_type_nt);
	ns_install_global("struct->list", &struct_to_list_nt);
}
