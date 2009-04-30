/*
 * This file is part of lGears scheme system
 * Copyright (C) 2009 Stepan Zastupov
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
#include "bytevector.h"

void bv_repr(void *ptr)
{
	bytevector_t *bv = ptr;
	printf("#vu8(");
	int i;
	for (i = 0; i < bv->size; i++) {
		printf("%d", bv->data[i]);
		if (i < bv->size-1)
			printf(" ");
	}
	printf(")");
}

void bv_visit(visitor_t *vs, void *data)
{
	bytevector_t *bv = data;
	bv->data = data+sizeof(bytevector_t);
}

static bytevector_t* bv_new(heap_t *heap, int size)
{
	void *mem = heap_alloc(heap, sizeof(bytevector_t)+size, t_bytevector);
	bytevector_t *bv = mem;
	bv->data = mem+sizeof(bytevector_t);
	bv->size = size;

	return bv;
}

static int make_bytevector(vm_thread_t *thread, obj_t *count, obj_t *fill)
{
	SAFE_ASSERT(IS_FIXNUM(*count));
	SAFE_ASSERT(IS_FIXNUM(*fill));

	bytevector_t *bv = bv_new(&thread->heap, FIXNUM(*count));
	memset(bv->data, FIXNUM(*fill), bv->size);

	RESULT_PTR(make_ptr(bv, id_ptr));
}
MAKE_NATIVE_BINARY(make_bytevector);

static int bytevector_length(vm_thread_t *thread, obj_t *obj)
{
	SAFE_ASSERT(IS_BYTEVECTOR(*obj));
	bytevector_t *bv = PTR(*obj);

	RESULT_FIXNUM(bv->size);
}
MAKE_NATIVE_UNARY(bytevector_length);

static int is_bytevector(vm_thread_t *thread, obj_t *obj)
{
	RESULT_BOOL(IS_BYTEVECTOR(*obj));
}
MAKE_NATIVE_UNARY(is_bytevector);

static int bytevector_8_set(vm_thread_t *thread, obj_t *obv, obj_t *opos, obj_t *oval)
{
	SAFE_ASSERT(IS_BYTEVECTOR(*obv));
	SAFE_ASSERT(IS_FIXNUM(*opos));
	SAFE_ASSERT(IS_FIXNUM(*opos));

	bytevector_t *bv = PTR(*obv);
	int pos = FIXNUM(*opos);
	int val = FIXNUM(*oval);
	SAFE_ASSERT(pos >= 0 && pos < bv->size);
	SAFE_ASSERT(val > 0 && val < 256);
	bv->data[pos] = val;

	RESULT_OBJ(cvoid.obj);
}
MAKE_NATIVE_TERNARY(bytevector_8_set);

void ns_install_bytevector(hash_table_t *tbl)
{
	ns_install_native(tbl, "make-bytevector", &make_bytevector_nt);
	ns_install_native(tbl, "bytevector?", &is_bytevector_nt);
	ns_install_native(tbl, "bytevector-u8-set!", &bytevector_8_set_nt);
}
