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
#include "fixnum.h"
#include "fd.h"

static int raise(vm_thread_t *thread, obj_t obj)
{
	STACK_PUSH(cvoid.ptr);
	STACK_PUSH(ctrue.ptr);
	STACK_PUSH(obj.ptr);
	// TODO check if exception_handler is valid procedure
	pair_t *pair = PTR(thread->exception_handler);
	thread->tramp.func = pair->car;
	thread->tramp.argc = 3;

	return RC_OK;
}

static int raise_msg(vm_thread_t *thread, const char *msg)
{
	obj_t obj;
	obj.ptr = _string(&thread->heap.allocator, (char*)msg, 1) ;
	return raise(thread, obj);
}

void pair_visit(visitor_t *vs, void *data)
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

void pair_repr(void *ptr)
{
	pair_t *pair = ptr;
	printf("(");
	print_obj(pair->car);
	disp_pair(pair);
	printf(")");
}

static void* _cons(heap_t *heap, obj_t *car, obj_t *cdr)
{
	pair_t *pair = heap_alloc(heap, sizeof(pair_t), t_pair);
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

	return make_ptr(pair, id_ptr);
}

static int default_handler(vm_thread_t *thread, obj_t *die, obj_t *obj)
{
	printf("Unhandled exception: ");
	print_obj(*obj);
	printf("\nShutting down the thread\n");
	return RC_EXIT;
}
MAKE_NATIVE_BINARY(default_handler);

void exception_handler_init(vm_thread_t *thread)
{
	ptr_t dflt;
	FUNC_INIT(dflt, &default_handler_nt);
	thread->exception_handler.ptr = _cons(&thread->heap,
										  &dflt.obj,
										  (obj_t*)&cnull.obj);
}

static int exception_handler(vm_thread_t *thread)
{
	RESULT_OBJ(thread->exception_handler);
}
MAKE_NATIVE_NULLARY(exception_handler);

static int exception_handler_set(vm_thread_t *thread, obj_t *obj)
{
	thread->exception_handler = *obj;
	RESULT_OBJ(cvoid.obj);
}
MAKE_NATIVE_UNARY(exception_handler_set);

static int cons(vm_thread_t *thread, obj_t *car, obj_t *cdr)
{
	RESULT_PTR(_cons(&thread->heap, car, cdr));
}
MAKE_NATIVE_BINARY(cons);

void* _list(heap_t *heap, obj_t *argv, int argc)
{
	obj_t res = cnull.obj;

	heap_require_blocks(heap, sizeof(pair_t), argc);
	int i;
	for (i = argc-1; i >= 0; i--)
		res.ptr = _cons(heap, &argv[i], &res);

	return res.ptr;
}

static int list(vm_thread_t *thread, obj_t *argv, int argc)
{
	void *res = _list(&thread->heap, &argv[1], argc-1);
	RESULT_PTR(res);
}
MAKE_NATIVE_VARIADIC(list, 0);

static int list_length(vm_thread_t *thread, obj_t *obj)
{
	if (!IS_PAIR(*obj))
		return raise_msg(thread, "expected propper list");

	pair_t *pair = PTR(*obj);
	if (!pair->list)
		return raise_msg(thread, "expected propper list");

	RESULT_FIXNUM(pair->length);
}
MAKE_NATIVE_UNARY(list_length);

static int make_list(vm_thread_t *thread, obj_t *count, obj_t *fill)
{
	SAFE_ASSERT(count->tag == id_fixnum);
	int size = FIXNUM(*count);
	obj_t res = cnull.obj;

	heap_require_blocks(&thread->heap, sizeof(pair_t), size);
	int i;
	for (i = 0; i < size; i++)
		res.ptr = _cons(&thread->heap, fill, &res);

	RESULT_OBJ(res);
}
MAKE_NATIVE_BINARY(make_list);

static int car(vm_thread_t *thread, obj_t *obj)
{
	SAFE_ASSERT(IS_TYPE(*obj, t_pair));
	pair_t *pair = PTR(*obj);
	RESULT_OBJ(pair->car);
}
MAKE_NATIVE_UNARY(car);

static int cdr(vm_thread_t *thread, obj_t *obj)
{
	SAFE_ASSERT(IS_TYPE(*obj, t_pair));
	pair_t *pair = PTR(*obj);
	RESULT_OBJ(pair->cdr);
}
MAKE_NATIVE_UNARY(cdr);

static int display(vm_thread_t *thread, obj_t *obj)
{
	print_obj(*obj);
	RESULT_OBJ(cvoid.obj);
}
MAKE_NATIVE_UNARY(display);

static int vm_exit(vm_thread_t *thread)
{
	return RC_EXIT;
}
MAKE_NATIVE_NULLARY(vm_exit);

void continuation_visit(visitor_t *vs, void *data)
{
	continuation_t *cont = data;
	vs->visit(vs, &cont->func);
}

static void *continuation_new(heap_t *heap, obj_t *func)
{
	continuation_t *cont = heap_alloc(heap, sizeof(continuation_t), t_cont);
	cont->func = *func;

	return make_ptr(cont, id_ptr);
}

static int call_cc(vm_thread_t *thread, obj_t *argv, int argc)
{
	SAFE_ASSERT(IS_FUNC(argv[1]));

	// Push continuation
	STACK_PUSH(argv[0].ptr);
	STACK_PUSH(continuation_new(&thread->heap, &argv[0]));

	thread->tramp.argc = 2;
	thread->tramp.func = argv[1];

	return RC_OK;
}
MAKE_NATIVE(call_cc, -1, 1, 0);

static int apply(vm_thread_t *thread, obj_t *argv, int argc)
{
	SAFE_ASSERT(IS_FUNC(argv[1]));
	SAFE_ASSERT(IS_TYPE(argv[2], t_pair));

	// Push continuation
	STACK_PUSH(argv[0].ptr);

	// Push arguments from list
	pair_t *pair = PTR(argv[2]);
	int cargc = 1;
	while (1) {
		STACK_PUSH(pair->car.ptr);
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
MAKE_NATIVE(apply, -1, 2, 0);

static int get_void(vm_thread_t *thread)
{
	RESULT_OBJ(cvoid.obj);
}
MAKE_NATIVE_NULLARY(get_void);

void ns_install_native(hash_table_t *tbl,
		char *name, const native_t *nt)
{
	ptr_t ptr;
	FUNC_INIT(ptr, nt);
	hash_table_insert(tbl, name, ptr.ptr); 
}

static int char_to_integer(vm_thread_t *thread, obj_t *chr)
{
	SAFE_ASSERT(IS_CHAR(*chr));
	RESULT_FIXNUM(CHAR(*chr));
}
MAKE_NATIVE_UNARY(char_to_integer);

static int integer_to_char(vm_thread_t *thread, obj_t *i)
{
	SAFE_ASSERT(IS_FIXNUM(*i));
	RESULT_CHAR(FIXNUM(*i));
}
MAKE_NATIVE_UNARY(integer_to_char);

/*
 * Predicates
 */

static int eq(vm_thread_t *thread, obj_t *a, obj_t *b)
{
	RESULT_BOOL(a->ptr == b->ptr);
}
MAKE_NATIVE_BINARY(eq);

static int is_procedure(vm_thread_t *thread, obj_t *obj)
{
	RESULT_BOOL(IS_FUNC(*obj));
}
MAKE_NATIVE_UNARY(is_procedure);

static int is_boolean(vm_thread_t *thread, obj_t *obj)
{
	RESULT_BOOL(IS_BOOL(*obj));
}
MAKE_NATIVE_UNARY(is_boolean);

static int is_null(vm_thread_t *thread, obj_t *obj)
{
	RESULT_BOOL(IS_NULL(*obj));
}
MAKE_NATIVE_UNARY(is_null);

static int is_pair(vm_thread_t *thread, obj_t *obj)
{
	RESULT_BOOL(IS_PAIR(*obj));
}
MAKE_NATIVE_UNARY(is_pair);

static int is_symbol(vm_thread_t *thread, obj_t *obj)
{
	RESULT_BOOL(IS_SYMBOL(*obj));
}
MAKE_NATIVE_UNARY(is_symbol);

static int is_char(vm_thread_t *thread, obj_t *obj)
{
	RESULT_BOOL(IS_CHAR(*obj));
}
MAKE_NATIVE_UNARY(is_char);

static int is_number(vm_thread_t *thread, obj_t *obj)
{
	RESULT_BOOL(IS_FIXNUM(*obj));
}
MAKE_NATIVE_UNARY(is_number);

static int is_list(vm_thread_t *thread, obj_t *obj)
{
	if (IS_PAIR(*obj)) {
		pair_t *pair = PTR(*obj);
		RESULT_BOOL(pair->list);
	}
	RESULT_BOOL(IS_NULL(*obj));
}
MAKE_NATIVE_UNARY(is_list);

void ns_install_primitives(hash_table_t *tbl)
{
	ns_install_native(tbl, "exception-handler", &exception_handler_nt);
	ns_install_native(tbl, "exception-handler-set!", &exception_handler_set_nt);
	
	ns_install_native(tbl, "display", &display_nt);
	ns_install_native(tbl, "__exit", &vm_exit_nt);
	ns_install_native(tbl, "call/cc", &call_cc_nt);
	ns_install_native(tbl, "apply", &apply_nt);
	ns_install_native(tbl, "cons", &cons_nt);
	ns_install_native(tbl, "list", &list_nt);
	ns_install_native(tbl, "$make-list", &make_list_nt);
	ns_install_native(tbl, "car", &car_nt);
	ns_install_native(tbl, "cdr", &cdr_nt);
	ns_install_native(tbl, "void", &get_void_nt);
	ns_install_native(tbl, "char->integer", &char_to_integer_nt);
	ns_install_native(tbl, "integer->char", &integer_to_char_nt);

	ns_install_native(tbl, "eq?", &eq_nt);
	ns_install_native(tbl, "procedure?", &is_procedure_nt);
	ns_install_native(tbl, "boolean?", &is_boolean_nt);
	ns_install_native(tbl, "null?", &is_null_nt);
	ns_install_native(tbl, "char?", &is_char_nt);
	ns_install_native(tbl, "number?", &is_number_nt);
	ns_install_native(tbl, "pair?", &is_pair_nt);
	ns_install_native(tbl, "symbol?", &is_symbol_nt);
	ns_install_native(tbl, "list?", &is_list_nt);
	ns_install_native(tbl, "length", &list_length_nt);

	ns_install_fixnum(tbl);
	ns_install_vector(tbl);
	ns_install_string(tbl);
	ns_install_bytevector(tbl);
	ns_install_fd(tbl);
}
