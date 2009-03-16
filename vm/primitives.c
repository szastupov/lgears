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
#include <string.h>
#include <fcntl.h>
#include <unistd.h>
#include "primitives.h"
#include "fixnum.h"

int native_call(vm_thread_t *thread, native_t *native, obj_t *argv, int argc)
{
	switch (native->arity) {
		case -1:
			return ((native_variadic)native->fp)(thread, argv, argc);
		case 0:
			return ((native_nullary)native->fp)(thread);
		case 1:
			return ((native_unary)native->fp)(thread, &argv[1]);
		case 2:
			return ((native_binary)native->fp)(thread, &argv[1], &argv[2]);
		case 3:
			return ((native_ternary)native->fp)(thread, &argv[1], &argv[2], &argv[3]);
		default:
			FATAL("wrong arity %d of %s\n", native->arity, native->name);
	}
}

void print_obj(obj_t obj);

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

void string_repr(void *ptr)
{
	string_t *string = ptr;
	printf("%s", string->str);
}

void* _string(heap_t *heap, char *str, int copy)
{
	int hsize = sizeof(string_t);
	int ssize = strlen(str)+1;
	if (copy)
		hsize += ssize;

	void *mem = heap_alloc(heap, hsize, t_string);
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

	return make_ptr(string, id_ptr);
}

static void* _cons(heap_t *heap, obj_t *car, obj_t *cdr)
{
	pair_t *pair = heap_alloc(heap, sizeof(pair_t), t_pair);
	pair->car = *car;
	pair->cdr = *cdr;

	return make_ptr(pair, id_ptr);
}

static int cons(vm_thread_t *thread, obj_t *car, obj_t *cdr)
{
	RESULT_PTR(_cons(&thread->heap, car, cdr));
}
MAKE_NATIVE_BINARY(cons);

static int list(vm_thread_t *thread, obj_t *argv, int argc)
{
	obj_t res = cnull.obj;

	heap_require(&thread->heap, sizeof(pair_t)*(argc-1));
	int i;
	for (i = argc-1; i > 0; i--)
		res.ptr = _cons(&thread->heap, &argv[i], &res);

	RESULT_OBJ(res);
}
MAKE_NATIVE_VARIADIC(list, 0);

static int car(vm_thread_t *thread, obj_t *obj)
{
	SAFE_ASSERT(IS_TYPE(*obj, t_pair));
	pair_t *pair = get_typed(*obj, t_pair);
	RESULT_OBJ(pair->car);
}
MAKE_NATIVE_UNARY(car);

static int cdr(vm_thread_t *thread, obj_t *obj)
{
	SAFE_ASSERT(IS_TYPE(*obj, t_pair));
	pair_t *pair = get_typed(*obj, t_pair);
	RESULT_OBJ(pair->cdr);
}
MAKE_NATIVE_UNARY(cdr);

static void print_const(obj_t obj)
{
	const_t c = { .obj = obj };
	static const char* descr[] = {
		"()",
		"#t",
		"#f",
		"<void>"
	};
	static int max_id = sizeof(descr)/sizeof(char*)-1;
	if (c.st.id < 0 || c.st.id > max_id)
		FATAL("wrong const id %d\n", c.st.id);

	printf("%s", descr[c.st.id]);
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
	native_t *native;
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
	switch (obj.tag) {
		case id_ptr:
			print_ptr(obj);
			break;
		case id_fixnum:
			printf("%ld", FIXNUM(obj));
			break;
		case id_char:
			printf("%c", CHAR(obj));
			break;
		case id_func:
			print_func(obj);
			break;
		case id_symbol:
			printf("%s", (const char*)PTR(obj));
			break;
		case id_const:
			print_const(obj);
			break;
		default:
			printf("unknown obj");
	}
}

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
	pair_t *pair = get_typed(argv[2], t_pair);
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

void ns_install_native(hash_table_t *tbl,
		char *name, const native_t *nt)
{
	ptr_t ptr;
	FUNC_INIT(ptr, nt);
	hash_table_insert(tbl, name, ptr.ptr); 
}

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

static int is_pair(vm_thread_t *thread, obj_t *obj)
{
	RESULT_BOOL(obj->tag == id_ptr && IS_TYPE(*obj, t_pair));
}
MAKE_NATIVE_UNARY(is_pair);

static int is_symbol(vm_thread_t *thread, obj_t *obj)
{
	RESULT_BOOL(obj->tag == id_symbol);
}
MAKE_NATIVE_UNARY(is_symbol);

void ns_install_primitives(hash_table_t *tbl)
{
	ns_install_native(tbl, "display", &display_nt);
	ns_install_native(tbl, "__exit", &vm_exit_nt);
	ns_install_native(tbl, "call/cc", &call_cc_nt);
	ns_install_native(tbl, "apply", &apply_nt);
	ns_install_native(tbl, "cons", &cons_nt);
	ns_install_native(tbl, "list", &list_nt);
	ns_install_native(tbl, "car", &car_nt);
	ns_install_native(tbl, "cdr", &cdr_nt);

	ns_install_native(tbl, "eq?", &eq_nt);
	ns_install_native(tbl, "procedure?", &is_procedure_nt);
	ns_install_native(tbl, "boolean?", &is_boolean_nt);
	ns_install_native(tbl, "pair?", &is_pair_nt);
	ns_install_native(tbl, "symbol?", &is_symbol_nt);

	ns_install_fixnum(tbl);
}
