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
#include "posix.h"

int t_pair, t_cont;

static void pair_visit(visitor_t *vs, void *data)
{
	pair_t *pair = data;
	vs->visit(vs, &pair->car);
	vs->visit(vs, &pair->cdr);
}

static void disp_pair(pair_t *pair)
{
	if (IS_TYPE(pair->cdr, t_pair)) {
		printf(" ");
		pair_t *np = PTR(pair->cdr);
		print_obj(np->car);
		disp_pair(np);
	} else {
		if (!IS_NULL(pair->cdr)) {
			printf(" . ");
			print_obj(pair->cdr);
		}
	}
}

static void pair_repr(void *ptr)
{
	pair_t *pair = ptr;
	printf("(");
	print_obj(pair->car);
	disp_pair(pair);
	printf(")");
}

obj_t _cons(allocator_t *al, obj_t *car, obj_t *cdr)
{
	pair_t *pair = allocator_alloc(al, sizeof(pair_t), t_pair);
	pair->car = *car;
	pair->cdr = *cdr;

	if (IS_NULL(*cdr)) {
		pair->list = 1;
		pair->length = 1;
	} else if (IS_PAIR(*cdr)) {
		pair_t *prev = PTR(*cdr);
		if (prev->list) {
			pair->list = 1;
			pair->length = prev->length+1;
		}
	} else
		pair->list = 0;

	return MAKE_TAGGED_PTR(pair, al->id);
}

obj_t _list(vm_thread_t *thread, obj_t *argv, int argc)
{
	DEFINE_LOCAL1(res);
	res = cnull;

	int i;
	for (i = argc-1; i >= 0; i--)
		res = _cons(&thread->heap.allocator, &argv[i], &res);

	thread->local_roots = NULL;
	return res;
}

static int list(vm_thread_t *thread, obj_t *argv, int argc)
{
	RETURN_OBJ(_list(thread, &argv[1], argc-1));
}
MAKE_NATIVE_VARIADIC(list, 0);

static int list_length(vm_thread_t *thread, obj_t *obj)
{
	if (IS_NULL(*obj)) {
		RETURN_FIXNUM(0);
	} else if (IS_PAIR(*obj)) {
		pair_t *pair = PTR(*obj);
		SAFE_ASSERT(pair->list);
		RETURN_FIXNUM(pair->length);
	} else {
		fprintf(stderr, "not pair or null");
		return RC_ERROR;
	}
}
MAKE_NATIVE_UNARY(list_length);

static int make_list(vm_thread_t *thread, obj_t *count, obj_t *fill)
{
	SAFE_ASSERT(IS_FIXNUM(*count));
	int size = FIXNUM(*count);
	DEFINE_LOCAL1(res);
	res = cnull;

	int i;
	for (i = 0; i < size; i++)
		res = _cons(&thread->heap.allocator, fill, &res);

	thread->local_roots = NULL;
	RETURN_OBJ(res);
}
MAKE_NATIVE_BINARY(make_list);

static int display(vm_thread_t *thread, obj_t *obj)
{
	print_obj(*obj);
	fflush(stdout);
	RETURN_OBJ(cvoid);
}
MAKE_NATIVE_UNARY(display);

static int vm_exit(vm_thread_t *thread)
{
	printf("exiting\n");
	return RC_EXIT;
}
MAKE_NATIVE_NULLARY(vm_exit);

static void continuation_visit(visitor_t *vs, void *data)
{
	continuation_t *cont = data;
	vs->visit(vs, &cont->func);
}

static obj_t continuation_new(heap_t *heap, obj_t *func)
{
	continuation_t *cont = heap_alloc(heap, sizeof(continuation_t), t_cont);
	cont->func = *func;

	return MAKE_HEAP_PTR(cont);
}

static int call_cc(vm_thread_t *thread, obj_t *argv, int argc)
{
	SAFE_ASSERT(IS_FUNC(argv[1]));

	// Push continuation
	STACK_PUSH(argv[0]);
	STACK_PUSH(continuation_new(&thread->heap, &argv[0]));

	thread->tramp.argc = 2;
	thread->tramp.func = argv[1];

	return RC_OK;
}
MAKE_NATIVE(call_cc, native_call_variadic, 1, 0);

static int exception_handlers(vm_thread_t *thread, obj_t *argv, int argc)
{
	if (argc == 2) {
		SAFE_ASSERT(IS_PAIR(argv[1]) || IS_NULL(argv[1]));
		thread->exception_handlers = argv[1];
		RETURN_OBJ(cvoid);
	} else {
		RETURN_OBJ(thread->exception_handlers);
	}
}
MAKE_NATIVE_VARIADIC(exception_handlers, 0);

static int apply(vm_thread_t *thread, obj_t *argv, int argc)
{
	SAFE_ASSERT(IS_FUNC(argv[1]));
	SAFE_ASSERT(IS_TYPE(argv[2], t_pair));

	// Push continuation
	STACK_PUSH(argv[0]);

	// Push arguments from list
	pair_t *pair = PTR(argv[2]);
	int cargc = 1;
	while (1) {
		STACK_PUSH(pair->car);
		cargc++;
		if (IS_TYPE(pair->cdr, t_pair))
			pair = PTR(pair->cdr);
		else
			break;
	}

	thread->tramp.func = argv[1];
	thread->tramp.argc = cargc;

	return RC_OK;
}
MAKE_NATIVE(apply, native_call_variadic, 2, 0);

static int get_void(vm_thread_t *thread)
{
	RETURN_OBJ(cvoid);
}
MAKE_NATIVE_NULLARY(get_void);

static int get_eof(vm_thread_t *thread)
{
	RETURN_OBJ(ceof);
}
MAKE_NATIVE_NULLARY(get_eof);

void ns_install_native(hash_table_t *tbl,
		const char *name, const native_func_t *nt)
{
	obj_t ptr = MAKE_FUNC(nt);
	hash_table_insert(tbl, (char*)name, (void*)ptr);
}

static int char_to_integer(vm_thread_t *thread, obj_t *chr)
{
	SAFE_ASSERT(IS_CHAR(*chr));
	RETURN_FIXNUM(CHAR(*chr));
}
MAKE_NATIVE_UNARY(char_to_integer);

static int integer_to_char(vm_thread_t *thread, obj_t *i)
{
	SAFE_ASSERT(IS_FIXNUM(*i));
	RETURN_CHAR(FIXNUM(*i));
}
MAKE_NATIVE_UNARY(integer_to_char);

static native_module_t modules[] = {
	{ strings_init, strings_cleanup },
	{ struct_init },
	{ bytevector_init },
	{ posix_init },
#ifdef HAVE_LIBFFI
	{ ffi_init, ffi_cleanup }
#endif
};

void primitives_init()
{
	t_pair = register_type("pair", pair_repr, pair_visit);
	t_cont = register_type("cont", NULL, continuation_visit);

	ns_install_global("display", &display_nt);
	ns_install_global("__exit", &vm_exit_nt);
	ns_install_global("call/cc", &call_cc_nt);
	ns_install_global("apply", &apply_nt);
	ns_install_global("exception-handlers", &exception_handlers_nt);
	ns_install_global("list", &list_nt);
	ns_install_global("$make-list", &make_list_nt);
	ns_install_global("void", &get_void_nt);
	ns_install_global("eof-object", &get_eof_nt);
	ns_install_global("char->integer", &char_to_integer_nt);
	ns_install_global("integer->char", &integer_to_char_nt);
	ns_install_global("length", &list_length_nt);

	int i;
	for (i = 0; i < sizeof(modules)/sizeof(native_module_t); i++)
		modules[i].init();
}

void primitives_cleanup()
{
	int i;
	for (i = 0; i < sizeof(modules)/sizeof(native_module_t); i++)
		if (modules[i].cleanup)
			modules[i].cleanup();
}
