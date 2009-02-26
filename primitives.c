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

static void* _cons(heap_t *heap, obj_t car, obj_t cdr)
{
	pair_t *pair = heap_alloc(heap, sizeof(pair_t));
	pair->car = car;
	pair->cdr = cdr;
	pair->hdr.type = &pair_type;

	return make_ptr(pair, id_ptr);
}

static int cons(heap_t *heap, trampoline_t *tramp, obj_t *argv, int argc)
{
	tramp->arg[0].ptr = _cons(heap, argv[1], argv[2]);

	return RC_OK;
}
MAKE_NATIVE(cons, 2, 0);

static int list(heap_t *heap, trampoline_t *tramp, obj_t *argv, int argc)
{
	obj_t res = const_null;

	int i;
	for (i = argc; i > 0; i--)
		res.ptr = _cons(heap, argv[i], res);

	tramp->arg[0] = res;

	return RC_OK;
}
MAKE_NATIVE(list, 0, 1);

static int is_null(heap_t *heap, trampoline_t *tramp, obj_t *argv, int argc)
{
	bool_t t;
	BOOL_INIT(t, argv[1].tag == id_const);

	tramp->arg[0] = t.obj;

	return RC_OK;
}
MAKE_NATIVE(is_null, 1, 0);

static int car(heap_t *heap, trampoline_t *tramp, obj_t *argv, int argc)
{
	pair_t *pair = get_typed(argv[1], &pair_type);
	if (pair)
		tramp->arg[0] = pair->car;
	else
		tramp->arg[0].ptr = NULL;

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

	return RC_OK;
}
MAKE_NATIVE(cdr, 1, 0);

static int eq(heap_t *heap, trampoline_t *tramp, obj_t *argv, int argc)
{
	bool_t res;
	BOOL_INIT(res, (argv[1].ptr == argv[2].ptr));

	tramp->arg[0] = res.obj;

	return RC_OK;
}
MAKE_NATIVE(eq, 2, 0);

static int fxsum(heap_t *heap, trampoline_t *tramp, obj_t *argv, int argc)
{
	fixnum_t res;
	FIXNUM_INIT(res, 0);
	int i;
	for (i = 1; i < argc; i++)
		res.val += FIXNUM(argv[i]);

	tramp->arg[0] = res.obj;

	return RC_OK;
}
MAKE_NATIVE(fxsum, 0, 1);

static int fxsub(heap_t *heap, trampoline_t *tramp, obj_t *argv, int argc)
{
	fixnum_t res;
	FIXNUM_INIT(res, FIXNUM(argv[1]));
	int i;
	for (i = 2; i < argc; i++)
		res.val -= FIXNUM(argv[i]);

	tramp->arg[0] = res.obj;

	return RC_OK;
}
MAKE_NATIVE(fxsub, 2, 1);

static int fxmul(heap_t *heap, trampoline_t *tramp, obj_t *argv, int argc)
{
	fixnum_t res;
	FIXNUM_INIT(res, 1);
	int i;
	for (i = 1; i < argc; i++)
		res.val *= FIXNUM(argv[i]);

	tramp->arg[0] = res.obj;

	return RC_OK;
}
MAKE_NATIVE(fxmul, 0, 1);

static int fxdiv(heap_t *heap, trampoline_t *tramp, obj_t *argv, int argc)
{
	fixnum_t res;
	FIXNUM_INIT(res, FIXNUM(argv[1]));
	int i;
	for (i = 2; i < argc; i++)
		res.val /= FIXNUM(argv[i]);

	tramp->arg[0] = res.obj;

	return RC_OK;
}
MAKE_NATIVE(fxdiv, 2, 1);

static int fxeq(heap_t *heap, trampoline_t *tramp, obj_t *argv, int argc)
{
	bool_t t;
	BOOL_INIT(t, 1);

	int i;
	for (i = 2; i < argc; i++)
		if (FIXNUM(argv[i-1]) != FIXNUM(argv[i])) {
			t.val = 0;
			break;
		}

	tramp->arg[0] = t.obj;

	return RC_OK;
}
MAKE_NATIVE(fxeq, 2, 1);

void print_obj(obj_t obj)
{
	switch (obj.tag) {
		case id_ptr:
			printf("ptr: %s\n", TYPE_NAME(PTR(obj)));
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
		case id_const:
			printf("const\n");
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
	tramp->argc = 2;

	// But for second, change tag
	tramp->arg[1].tag = id_cont;
	//TODO check for valid function

	return RC_OK;
}
MAKE_NATIVE(call_cc, 1, 0);

void ns_install_primitives(hash_table_t *tbl)
{
	ns_install_native(tbl, "display", &display_nt);
	ns_install_native(tbl, "__exit", &vm_exit_nt);
	ns_install_native(tbl, "call/cc", &call_cc_nt);
	ns_install_native(tbl, "cons", &cons_nt);
	ns_install_native(tbl, "list", &list_nt);
	ns_install_native(tbl, "null?", &is_null_nt);
	ns_install_native(tbl, "car", &car_nt);
	ns_install_native(tbl, "cdr", &cdr_nt);
	ns_install_native(tbl, "eq?", &eq_nt);

	ns_install_native(tbl, "+", &fxsum_nt);
	ns_install_native(tbl, "-", &fxsub_nt);
	ns_install_native(tbl, "*", &fxmul_nt);
	ns_install_native(tbl, "/", &fxdiv_nt);
	ns_install_native(tbl, "=", &fxeq_nt);
}
