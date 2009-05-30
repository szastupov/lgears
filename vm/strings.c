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
#include <pthread.h>
#include "native.h"
#include "strings.h"

int t_string, t_symbol;
hash_table_t sym_table;			/* Symbols table */

static void string_repr(void *ptr)
{
	string_t *string = ptr;
	printf("%s", string->str);
}

void string_visit(visitor_t *vs, void *data)
{
	string_t *str = data;
	if (str->allocated)
		str->str = data+sizeof(string_t);
}

static void symbol_repr(void *ptr)
{
	printf("%s", (char*)ptr);
}

static pthread_mutex_t symbol_mutex = PTHREAD_MUTEX_INITIALIZER;

obj_t make_symbol(const char *str)
{
	pthread_mutex_lock(&symbol_mutex);
	void *res = hash_table_lookup(&sym_table, str);
	if (!res) {
		size_t sz = strlen(str)+1;
		res = alloc_global_const(sz, t_symbol);
		memcpy(res, str, sz);
		hash_table_insert(&sym_table, res, res);
	}
	pthread_mutex_unlock(&symbol_mutex);
	return MAKE_CONST_PTR(res);
}

obj_t _string(allocator_t *al, char *str, int copy)
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
		string->allocated = 1;
	} else {
		string->allocated = 0;
		string->str = str;
	}

	return MAKE_TAGGED_PTR(string, al->id);
}

static int make_string(vm_thread_t *thread, obj_t *olen, obj_t *ofill)
{
	SAFE_ASSERT(IS_FIXNUM(*olen));
	SAFE_ASSERT(IS_CHAR(*ofill));

	size_t len = FIXNUM(*olen);
	void *mem = heap_alloc(&thread->heap, sizeof(string_t)+len+1, t_string);
	string_t *str = mem;
	str->str = mem+sizeof(string_t);
	str->size = len+1;
	str->allocated = 1;
	char fill = CHAR(*ofill);
	int i;
	for (i = 0; i < len; i++)
		str->str[i] = fill;
	str->str[len] = '\0';

	RETURN_OBJ(MAKE_HEAP_PTR(str));
}
MAKE_NATIVE_BINARY(make_string);

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

	RETURN_OBJ(MAKE_HEAP_PTR(new_str));
}
MAKE_NATIVE_BINARY(string_concat);

static int string_copy_to(vm_thread_t *thread, obj_t *odest, obj_t *opos, obj_t *osrc)
{
	SAFE_ASSERT(IS_STRING(*odest));
	SAFE_ASSERT(IS_HEAP_PTR(*odest));
	SAFE_ASSERT(IS_STRING(*osrc));
	SAFE_ASSERT(IS_FIXNUM(*opos));

	string_t *src = PTR(*osrc);
	string_t *dst = PTR(*odest);
	SAFE_ASSERT(dst->allocated);
	int pos = FIXNUM(*opos);

	SAFE_ASSERT((pos >= 0)
				&& (pos+src->size <= dst->size));
	memcpy(&dst->str[pos], src->str, src->size-1);

	RETURN_OBJ(cvoid);
}
MAKE_NATIVE_TERNARY(string_copy_to);

static int string_copy(vm_thread_t *thread, obj_t *osrc)
{
#define STRING_SIZE(o) ((string_t*)PTR(o))->size
	SAFE_ASSERT(IS_STRING(*osrc));
	int size = STRING_SIZE(*osrc);

	void *mem = heap_alloc(&thread->heap, size+sizeof(string_t), t_string);
	string_t *str = mem;
	str->str = mem+sizeof(string_t);
	str->size = size;
	str->allocated = 1;
	memcpy(str->str, CSTRING(*osrc), size);

	RETURN_OBJ(MAKE_HEAP_PTR(str));
}
MAKE_NATIVE_UNARY(string_copy);

static int symbol_to_string(vm_thread_t *thread, obj_t *sym)
{
	SAFE_ASSERT(IS_SYMBOL(*sym));
	char *str = SYMBOL(*sym);

	RETURN_OBJ(_string(&thread->heap.allocator, str, 0));
}
MAKE_NATIVE_UNARY(symbol_to_string);

static int string_ref(vm_thread_t *thread, obj_t *ostr, obj_t *opos)
{
	SAFE_ASSERT(IS_STRING(*ostr));
	SAFE_ASSERT(IS_FIXNUM(*opos));
	string_t *str = PTR(*ostr);
	int pos = FIXNUM(*opos);
	SAFE_ASSERT(pos < str->size-1);

	RETURN_CHAR(str->str[pos]);
}
MAKE_NATIVE_BINARY(string_ref);

static int string_set(vm_thread_t *thread, obj_t *ostr,
					  obj_t *opos, obj_t *oval)
{
	SAFE_ASSERT(IS_STRING(*ostr));
	SAFE_ASSERT(IS_FIXNUM(*opos));
	SAFE_ASSERT(IS_CHAR(*oval));

	string_t *str = PTR(*ostr);
	int pos = FIXNUM(*opos);
	SAFE_ASSERT((pos >= 0) && (pos < str->size-1));
	str->str[pos] = CHAR(*oval);

	RETURN_OBJ(cvoid);
}
MAKE_NATIVE_TERNARY(string_set);

static int string_length(vm_thread_t *thread, obj_t *ostr)
{
	SAFE_ASSERT(IS_STRING(*ostr));
	string_t *str = PTR(*ostr);

	RETURN_FIXNUM(str->size-1);
}
MAKE_NATIVE_UNARY(string_length);

static int string_eq(vm_thread_t *thread, obj_t *a, obj_t *b)
{
	SAFE_ASSERT(IS_STRING(*a));
	SAFE_ASSERT(IS_STRING(*b));
	string_t *sa = PTR(*a);
	string_t *sb = PTR(*b);

	RETURN_BOOL(strcmp(sa->str, sb->str) == 0);
}
MAKE_NATIVE_BINARY(string_eq);

static int substring(vm_thread_t *thread, obj_t *ostr, obj_t *ostart, obj_t *oend)
{
	SAFE_ASSERT(IS_STRING(*ostr));
	SAFE_ASSERT(IS_FIXNUM(*ostart));
	SAFE_ASSERT(IS_FIXNUM(*oend));

	int start = FIXNUM(*ostart);
	int end = FIXNUM(*oend);
	string_t *str = PTR(*ostr);

	SAFE_ASSERT((start >= 0)
				&& (start <= end)
				&& (end < str->size));

	size_t nsize = end-start;
	void *mem = heap_alloc(&thread->heap, nsize+sizeof(string_t), t_string);
	string_t *dstr = mem;
	dstr->str = mem+sizeof(string_t);
	dstr->allocated = 1;
	dstr->size = nsize+1;
	memcpy(dstr->str, &CSTRING(*ostr)[start], nsize);

	RETURN_OBJ(MAKE_HEAP_PTR(dstr));
}
MAKE_NATIVE_TERNARY(substring);

void strings_init()
{
	t_string = register_type("string", string_repr, string_visit);
	t_symbol = register_type("symbol", symbol_repr, NULL);

	hash_table_init(&sym_table, string_hash, string_equal);

	ns_install_global("symbol->string", &symbol_to_string_nt);
	ns_install_global("string-ref", &string_ref_nt);
	ns_install_global("string-set!", &string_set_nt);
	ns_install_global("string-length", &string_length_nt);
	ns_install_global("string=?", &string_eq_nt);
	ns_install_global("string-concat", &string_concat_nt);
	ns_install_global("substring", &substring_nt);
	ns_install_global("string-copy", &string_copy_nt);
	ns_install_global("string-copy!", &string_copy_to_nt);
	ns_install_global("make-string", &make_string_nt);
}

void strings_cleanup()
{
	hash_table_destroy(&sym_table);
}
