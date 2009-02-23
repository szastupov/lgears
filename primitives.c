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

	tramp->arg.ptr = make_ptr(pair, id_ptr);
	tramp->func.obj = argv[0];

	return RC_OK;
}
MAKE_NATIVE(cons, 2, 0);

static int car(heap_t *heap, trampoline_t *tramp, obj_t *argv, int argc)
{
	pair_t *pair = get_typed(argv[1], &pair_type);
	if (pair)
		tramp->arg = pair->car;
	else
		tramp->arg.ptr = NULL;
	tramp->func.obj = argv[0];

	return RC_OK;
}
MAKE_NATIVE(car, 1, 0);

static int cdr(heap_t *heap, trampoline_t *tramp, obj_t *argv, int argc)
{
	pair_t *pair = get_typed(argv[1], &pair_type);
	if (pair)
		tramp->arg = pair->cdr;
	else
		tramp->arg.ptr = NULL;
	tramp->func.obj = argv[0];

	return RC_OK;
}
MAKE_NATIVE(cdr, 1, 0);

#if 0
static void* eq(heap_t *heap, obj_t *argv, int argc)
{
	bool_t res;
	bool_init(res, (argv[0].ptr == argv[1].ptr));
	return res.ptr;
}
MAKE_NATIVE(eq, 2, 0);

#define DEFINE_ARITH(name, init, op, min) \
	static void* arith_##name(heap_t *heap, obj_t *argv, int argc) \
{ \
	fixnum_t res; \
	fixnum_init(res, init); \
	int i; \
	for (i = 0; i < argc; i++) \
		res.val op##= fixnum_from_obj(argv[i]); \
	return res.ptr; \
}\
MAKE_NATIVE(arith_##name, min, 1);

DEFINE_ARITH(add, 0, +, 0);
DEFINE_ARITH(sub, 0, -, 1);
DEFINE_ARITH(mul, 1, *, 0);
DEFINE_ARITH(div, 1, /, 1);

#endif

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
	tramp->arg.ptr = NULL;

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

void ns_install_primitives(hash_table_t *tbl)
{
	ns_install_native(tbl, "display", &display_nt);
	ns_install_native(tbl, "__exit", &vm_exit_nt);
	ns_install_native(tbl, "cons", &cons_nt);
	ns_install_native(tbl, "car", &car_nt);
	ns_install_native(tbl, "cdr", &cdr_nt);
#if 0
	ns_install_native(tbl, "eq?", &eq_nt);
	ns_install_native(tbl, "+", &arith_add_nt);
	ns_install_native(tbl, "-", &arith_sub_nt);
	ns_install_native(tbl, "*", &arith_mul_nt);
	ns_install_native(tbl, "/", &arith_div_nt);
#endif
}
