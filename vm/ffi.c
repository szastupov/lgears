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
#include <dlfcn.h>
#include <ffi.h>
#include "native.h"
#include "string.h"

int t_shared_object, t_foreign_function;

#define IS_SO(o) IS_TYPE(o, t_shared_object)
#define IS_FOREIGN(o) IS_TYPE(o, t_foreign_function)

hash_table_t so_cache;			/* Shared objects cache */

typedef struct {
	void *object;
	char *name;
} shared_object_t;

static void so_repr(void *ptr)
{
	shared_object_t *so = ptr;
	printf("<so: %s, %p>", so->name, so->object);
}

typedef struct {
	ffi_cif cif;
	int argc;
	void (*fn)(void);
	ffi_type **arg_types;
	void **arg_vals;
	void *rval;
	char *name;
} foreign_func_t;

static void foreign_repr(void *ptr)
{
	foreign_func_t *ff = ptr;
	printf("<foreign: %s/%d>", ff->name, ff->argc);
}

static void foreign_visit(visitor_t *vs, void *data)
{
	FATAL("implement me");
}

ffi_type* parse_type(char c)
{
#define VAR(var, val) case var: return &ffi_type_##val
	switch (c) {
		VAR('c', sint8);
		VAR('b', sint8);
		VAR('B', uint8);
		VAR('h', sint16);
		VAR('H', uint16);
		VAR('i', sint32);
		VAR('I', uint32);
#if __WORDSIZE == 64
		VAR('l', sint64);
		VAR('L', uint64);
#else
		VAR('l', sint32);
		VAR('L', uint32);
#endif
		VAR('q', sint64);
		VAR('Q', uint64);
		VAR('f', float);
		VAR('d', double);
		VAR('s', pointer);
		VAR('p', pointer);
		VAR('v', void);
	default:
		FATAL("unknown type %c\n", c);
	}
}

static int make_foreign(vm_thread_t *thread, obj_t *oso, obj_t *oname, obj_t *oformat)
{
	SAFE_ASSERT(IS_STRING(*oname));
	SAFE_ASSERT(IS_SO(*oso));
	SAFE_ASSERT(IS_STRING(*oformat));

	shared_object_t *so = PTR(*oso);
	string_t *name = PTR(*oname);
	void (*fn)(void) = dlsym(so->object, name->str);
	SAFE_ASSERT(fn != NULL);
	string_t *format = PTR(*oformat);
	int argc = format->size-2;
	char *args = format->str+1;

	size_t args_size = sizeof(ffi_type*)*argc;
	size_t vals_size = sizeof(void*)*argc;
	size_t total_size = sizeof(foreign_func_t)+args_size+vals_size+name->size;
	void *mem = heap_alloc(&thread->heap, total_size, t_foreign_function);
	foreign_func_t *ff = mem;
	ff->argc = argc;
	ff->fn = fn;

	mem += sizeof(foreign_func_t);
	ff->arg_types = mem;
	mem += args_size;
	ff->arg_vals = mem;
	mem += vals_size;
	ff->name = mem;

	memcpy(ff->name, name->str, name->size);
	int i;
	for (i = 0; i < argc; i++)
		ff->arg_types[i] = parse_type(args[i]);

	ffi_status rc = ffi_prep_cif(&ff->cif,
								 FFI_DEFAULT_ABI,
								 argc,
								 parse_type(*format->str),
								 ff->arg_types);
	if (rc != FFI_OK)
		FATAL("ffi_prep_cif failed");

	RETURN_OBJ(make_ptr(ff, id_ptr));
}
MAKE_NATIVE_TERNARY(make_foreign);

static int load_so(vm_thread_t *thread, obj_t *oname)
{
	SAFE_ASSERT(IS_STRING(*oname));
	string_t *name = PTR(*oname);

	shared_object_t *so = hash_table_lookup(&so_cache, name->str);
	if (!so) {
		void *so_ptr = dlopen(name->str, RTLD_LAZY);
		SAFE_ASSERT(so_ptr != NULL);

		size_t ss = name->size + sizeof(shared_object_t);
		void *mem = allocator_alloc(&global_const_pool.al, ss, t_shared_object);
		so = mem;
		so->name = mem+sizeof(shared_object_t);
		memcpy(so->name, name->str, name->size);
		so->object = so_ptr;

		hash_table_insert(&so_cache, so->name, so);
	}

	RETURN_OBJ(make_ptr(so, id_const_ptr));
}
MAKE_NATIVE_UNARY(load_so);

void ns_install_ffi(hash_table_t *tbl)
{
	t_shared_object = register_type("so", so_repr, NULL);
	t_foreign_function = register_type("foreign",
									   foreign_repr,
									   foreign_visit);

	hash_table_init(&so_cache, string_hash, string_equal);

	ns_install_native(tbl, "load-so", &load_so_nt);
	ns_install_native(tbl, "make-foreign", &make_foreign_nt);
}
