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
#include "primitives.h"
#include <string.h>

typedef struct {
	hobj_hdr_t hdr;
	obj_t car, cdr;
} pair_t;

static void pair_visit(visitor_t *vs, void *data)
{
	pair_t *pair = data;
	vs->visit(vs, &pair->car);
	vs->visit(vs, &pair->cdr);
}

const type_t pair_type = {
	.name = "pair",
	.visit = pair_visit
};

static int cons(heap_t *heap, trampoline_t *tramp, obj_t *argv, int argc)
{
	pair_t *pair = heap_alloc(heap, sizeof(pair_t));
	pair->car = argv[1];
	pair->cdr = argv[2];
	pair->hdr.type = &pair_type;

	tramp->arg[0].ptr = make_ptr(pair, id_ptr);
	tramp->func.obj = argv[0];
	tramp->argc = 1;

	return RC_OK;
}
MAKE_NATIVE(cons, 2, 0);

static int car(heap_t *heap, trampoline_t *tramp, obj_t *argv, int argc)
{
	pair_t *pair = get_typed(argv[1], &pair_type);
	if (pair)
		tramp->arg[0] = pair->car;
	else
		tramp->arg[0].ptr = NULL;
	tramp->func.obj = argv[0];
	tramp->argc = 1;

	return RC_OK;
}
MAKE_NATIVE(car, 1, 0);

static int cdr(heap_t *heap, trampoline_t *tramp, obj_t *argv, int argc)
{
	pair_t *pair = get_typed(argv[1], &pair_type);
	if (pair)
		tramp->arg[0] = pair->cdr;
	else
		tramp->arg[0].ptr = NULL;
	tramp->func.obj = argv[0];
	tramp->argc = 1;

	return RC_OK;
}
MAKE_NATIVE(cdr, 1, 0);

static int eq(heap_t *heap, trampoline_t *tramp, obj_t *argv, int argc)
{
	bool_t res;
	BOOL_INIT(res, (argv[1].ptr == argv[2].ptr));

	tramp->func.obj = argv[0];
	tramp->arg[0] = res.obj;
	tramp->argc = 1;

	return RC_OK;
}
MAKE_NATIVE(eq, 2, 0);

//FIXME
#define DEFINE_ARITH(name, init, op, min) \
	static int arith_##name(heap_t *heap, trampoline_t *tramp, obj_t *argv, int argc) \
{ \
	fixnum_t res; \
	FIXNUM_INIT(res, init); \
	int i; \
	for (i = 1; i < argc; i++) \
		res.val op##= FIXNUM(argv[i]); \
	tramp->func.obj = argv[0]; \
	tramp->arg[0] = res.obj; \
	tramp->argc = 1; \
	return RC_OK; \
}\
MAKE_NATIVE(arith_##name, min, 1);

DEFINE_ARITH(add, 0, +, 0);
DEFINE_ARITH(sub, 0, -, 1);
DEFINE_ARITH(mul, 1, *, 0);
DEFINE_ARITH(div, 1, /, 1);

void print_obj(obj_t obj)
{
	switch (obj.tag) {
		case id_ptr:
			printf("ptr: %p\n", PTR(obj));
			break;
		case id_fixnum:
			printf("fixnum: %d\n", FIXNUM(obj));
			break;
		case id_bool:
			printf("bool: #%c\n", BOOL(obj) ? 't' : 'f');
			break;
		case id_char:
			printf("char: %c\n", CHAR(obj));
			break;
		case id_func:
			printf("func: %p\n", PTR(obj));
			break;
		case id_symbol:
			printf("symbol: %s\n", (const char*)PTR(obj));
			break;
		default:
			printf("unknown obj\n");
	}
}

static int display(heap_t *heap, trampoline_t *tramp,
		obj_t *argv, int argc)
{
	print_obj(argv[1]);
	tramp->func.ptr = argv[0].ptr;
	tramp->arg[0].ptr = NULL;
	tramp->argc = 1;

	return RC_OK;
}
MAKE_NATIVE(display, 1, 0);

void ns_install_native(hash_table_t *tbl,
		char *name, const native_t *nt)
{
	ptr_t ptr;
	FUNC_INIT(ptr, nt);
	hash_table_insert(tbl, name, ptr.ptr); 
}

static int vm_exit(heap_t *heap, trampoline_t *tramp, obj_t *argv, int argc)
{
	return RC_EXIT;
}
MAKE_NATIVE(vm_exit, 0, 0);

static int call_cc(heap_t *heap, trampoline_t *tramp, obj_t *argv, int argc)
{
	tramp->func.ptr = argv[1].ptr;
	// Pass current continuation to both arguments
	tramp->arg[0] = argv[0];
	tramp->arg[1] = argv[0];

	// But for second, change tag
	tramp->arg[1].tag = id_cont;
	//TODO check for valid function

	tramp->argc = 2;
	
	return RC_OK;
}
MAKE_NATIVE(call_cc, 1, 0);

void ns_install_primitives(hash_table_t *tbl)
{
	ns_install_native(tbl, "display", &display_nt);
	ns_install_native(tbl, "__exit", &vm_exit_nt);
	ns_install_native(tbl, "call/cc", &call_cc_nt);
	ns_install_native(tbl, "cons", &cons_nt);
	ns_install_native(tbl, "car", &car_nt);
	ns_install_native(tbl, "cdr", &cdr_nt);
	ns_install_native(tbl, "eq?", &eq_nt);

	ns_install_native(tbl, "+", &arith_add_nt);
	ns_install_native(tbl, "-", &arith_sub_nt);
	ns_install_native(tbl, "*", &arith_mul_nt);
	ns_install_native(tbl, "/", &arith_div_nt);
}
