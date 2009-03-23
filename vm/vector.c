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
#include "native.h"
#include "vector.h"
#include "primitives.h"

void vector_repr(void *ptr)
{
	vector_t *vec = ptr;
	printf("#(");
	int i;
	for (i = 0; i < vec->size; i++) {
		print_obj(vec->objects[i]);
		if (i < vec->size-1)
			printf(" ");
	}
	printf(")");
}

void vector_visit(visitor_t *vs, void *data)
{
	vector_t *vec = data;
	vec->objects = data+sizeof(vector_t);
	int i;
	for (i = 0; i < vec->size; i++)
		vs->visit(vs, &vec->objects[i]);
}

static vector_t* vector_new(heap_t *heap, int size)
{
	void *mem = heap_alloc(heap,
			sizeof(vector_t)+sizeof(obj_t)*size, t_vector);
	vector_t *vec = mem;
	vec->objects = mem+sizeof(vector_t);
	vec->size = size;

	return vec;
}

static int vector(vm_thread_t *thread, obj_t *argv, int argc)
{
	int count = argc-1;
	vector_t *vec = vector_new(&thread->heap, count);

	int i;
	for (i = 0; i < count; i++)
		vec->objects[i] = argv[i+1];

	RESULT_PTR(make_ptr(vec, id_ptr));
}
MAKE_NATIVE_VARIADIC(vector, 0);

static int make_vector(vm_thread_t *thread, obj_t *count)
{
	SAFE_ASSERT(count->tag == id_fixnum);

	vector_t *vec = vector_new(&thread->heap, FIXNUM(*count));
	int i;
	for (i = 0; i < vec->size; i++)
		vec->objects[i] = cvoid.obj;

	RESULT_PTR(make_ptr(vec, id_ptr));
}
MAKE_NATIVE_UNARY(make_vector);

static int vector_length(vm_thread_t *thread, obj_t *obj)
{
	SAFE_ASSERT(IS_TYPE(*obj, t_vector));
	vector_t *vec = get_typed(*obj, t_vector);

	RESULT_FIXNUM(vec->size);
}
MAKE_NATIVE_UNARY(vector_length);

static int is_vector(vm_thread_t *thread, obj_t *obj)
{
	RESULT_BOOL(IS_TYPE(*obj, t_vector));
}
MAKE_NATIVE_UNARY(is_vector);

static int vector_set(vm_thread_t *thread, obj_t *obj, obj_t *opos, obj_t *val)
{
	SAFE_ASSERT(IS_TYPE(*obj, t_vector));
	vector_t *vec = get_typed(*obj, t_vector);
	int pos = FIXNUM(*opos);
	SAFE_ASSERT(vec->size > pos);

	vec->objects[pos] = *val;
	MARK_MODIFIED(&thread->heap, vec);

	RESULT_OBJ(cvoid.obj);
}
MAKE_NATIVE_TERNARY(vector_set);

static int vector_ref(vm_thread_t *thread, obj_t *obj, obj_t *opos)
{
	SAFE_ASSERT(IS_TYPE(*obj, t_vector));
	SAFE_ASSERT(opos->tag == id_fixnum);
	vector_t *vec = get_typed(*obj, t_vector);
	int pos = FIXNUM(*opos);
	SAFE_ASSERT(vec->size > pos);

	RESULT_OBJ(vec->objects[pos]);
}
MAKE_NATIVE_BINARY(vector_ref);

static int vector_to_list(vm_thread_t *thread, obj_t *obj)
{
	SAFE_ASSERT(IS_TYPE(*obj, t_vector));
	vector_t *vec = get_typed(*obj, t_vector);
	void *res = _list(&thread->heap, vec->objects, vec->size);
	RESULT_PTR(res);
}
MAKE_NATIVE_UNARY(vector_to_list);

void ns_install_vector(hash_table_t *tbl)
{
	ns_install_native(tbl, "vector", &vector_nt);
	ns_install_native(tbl, "vector?", &is_vector_nt);
	ns_install_native(tbl, "vector-length", &vector_length_nt);
	ns_install_native(tbl, "vector-set!", &vector_set_nt);
	ns_install_native(tbl, "vector-ref", &vector_ref_nt);
	ns_install_native(tbl, "vector->list", &vector_to_list_nt);
	ns_install_native(tbl, "make-vector", &make_vector_nt);
}
