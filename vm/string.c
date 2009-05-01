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
#include "string.h"

void string_repr(void *ptr)
{
	string_t *string = ptr;
	printf("%s", string->str);
}

void string_visit(visitor_t *vs, void *data)
{
	string_t *str = data;
	if (str->copy)
		str->str = data+sizeof(string_t);
}

void* _string(allocator_t *al, char *str, int copy)
{
	int hsize = sizeof(string_t);
	int ssize = strlen(str)+1;
	if (copy)
		hsize += ssize;

	void *mem = allocator_alloc(al, hsize, t_string);
	string_t *string = mem;
	string->size = ssize;
	if (copy) {
		string->str = mem + sizeof(string_t);
		memcpy(string->str, str, ssize);
		string->copy = 1;
	} else {
		string->copy = 0;
		string->str = str;
	}

	return make_ptr(string, al->id);
}

static int string_concat(vm_thread_t *thread, obj_t *oa, obj_t *ob)
{
	SAFE_ASSERT(IS_STRING(*oa));
	SAFE_ASSERT(IS_STRING(*ob));

	//FIXME: it's not gc safe
	string_t *a = PTR(*oa);
	string_t *b = PTR(*ob);

	size_t new_size = a->size + b->size - 1;
	void *mem = heap_alloc(&thread->heap, new_size+sizeof(string_t), t_string);
	string_t *new_str = mem;
	new_str->str = mem + sizeof(string_t);
	new_str->size = new_size;

	int sep = a->size-1;
	memcpy(new_str->str, a->str, sep);
	memcpy(new_str->str+sep, b->str, b->size);

	RESULT_PTR(make_ptr(new_str, id_ptr));
}
MAKE_NATIVE_BINARY(string_concat);

static int symbol_to_string(vm_thread_t *thread, obj_t *sym)
{
	SAFE_ASSERT(IS_SYMBOL(*sym));
	char *str = PTR(*sym);

	RESULT_PTR(_string(&thread->heap.allocator, str, 0));
}
MAKE_NATIVE_UNARY(symbol_to_string);

static int string_ref(vm_thread_t *thread, obj_t *ostr, obj_t *opos)
{
	SAFE_ASSERT(IS_STRING(*ostr));
	SAFE_ASSERT(IS_FIXNUM(*opos));
	string_t *str = PTR(*ostr);
	int pos = FIXNUM(*opos);
	SAFE_ASSERT(pos < str->size-1);

	RESULT_CHAR(str->str[pos]);
}
MAKE_NATIVE_BINARY(string_ref);

static int string_length(vm_thread_t *thread, obj_t *ostr)
{
	SAFE_ASSERT(IS_STRING(*ostr));
	string_t *str = PTR(*ostr);

	RESULT_FIXNUM(str->size-1);
}
MAKE_NATIVE_UNARY(string_length);

static int is_string(vm_thread_t *thread, obj_t *obj)
{
	RESULT_BOOL(IS_STRING(*obj));
}
MAKE_NATIVE_UNARY(is_string);

static int string_eq(vm_thread_t *thread, obj_t *a, obj_t *b)
{
	SAFE_ASSERT(IS_STRING(*a));
	SAFE_ASSERT(IS_STRING(*b));
	string_t *sa = PTR(*a);
	string_t *sb = PTR(*b);

	RESULT_BOOL(strcmp(sa->str, sb->str) == 0);
}
MAKE_NATIVE_BINARY(string_eq);

void ns_install_string(hash_table_t *tbl)
{
	ns_install_native(tbl, "symbol->string", &symbol_to_string_nt);
	ns_install_native(tbl, "string-ref", &string_ref_nt);
	ns_install_native(tbl, "string-length", &string_length_nt);
	ns_install_native(tbl, "string?", &is_string_nt);
	ns_install_native(tbl, "string=?", &string_eq_nt);
	ns_install_native(tbl, "string-concat", &string_concat_nt);
}
