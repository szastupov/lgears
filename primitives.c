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
#include "primitives.h"
#include "memory.h"
#include "vm_private.h"

static void print_obj(obj_t obj);

typedef struct {
	hobj_hdr_t hdr;
	obj_t car, cdr;
} pair_t;

void pair_visit(visitor_t *vs, void *data)
{
	pair_t *pair = data;
	vs->visit(vs, &pair->car);
	vs->visit(vs, &pair->cdr);
}

static void disp_pair(pair_t *pair)
{
	printf(" ");
	if (IS_TYPE(pair->cdr, t_pair)) {
		pair_t *np = PTR(pair->cdr);
		print_obj(np->car);
		disp_pair(np);
	} else
		print_obj(pair->cdr);
}

void pair_repr(void *ptr)
{
	pair_t *pair = ptr;
	printf("(");
	print_obj(pair->car);
	disp_pair(pair);
	printf(")");
}


static void* _cons(heap_t *heap, obj_t car, obj_t cdr)
{
	pair_t *pair = heap_alloc(heap, sizeof(pair_t));
	pair->car = car;
	pair->cdr = cdr;
	pair->hdr.type_id = t_pair;

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
	obj_t res = cnull.obj;

	int i;
	for (i = argc-1; i > 0; i--)
		res.ptr = _cons(heap, argv[i], res);

	tramp->arg[0] = res;

	return RC_OK;
}
MAKE_NATIVE(list, 0, 1);

static int car(heap_t *heap, trampoline_t *tramp, obj_t *argv, int argc)
{
	pair_t *pair = get_typed(argv[1], t_pair);
	if (pair)
		tramp->arg[0] = pair->car;
	else
		tramp->arg[0].ptr = NULL;

	return RC_OK;
}
MAKE_NATIVE(car, 1, 0);

static int cdr(heap_t *heap, trampoline_t *tramp, obj_t *argv, int argc)
{
	pair_t *pair = get_typed(argv[1], t_pair);
	if (pair)
		tramp->arg[0] = pair->cdr;
	else
		tramp->arg[0].ptr = NULL;

	return RC_OK;
}
MAKE_NATIVE(cdr, 1, 0);

static int eq(heap_t *heap, trampoline_t *tramp, obj_t *argv, int argc)
{
	const_t res = CIF(argv[1].ptr == argv[2].ptr);
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
	const_t res = ctrue;

	int i;
	for (i = 2; i < argc; i++)
		if (FIXNUM(argv[i-1]) != FIXNUM(argv[i])) {
			res = cfalse;
			break;
		}

	tramp->arg[0] = res.obj;

	return RC_OK;
}
MAKE_NATIVE(fxeq, 2, 1);

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
	hobj_hdr_t *ohdr = ptr;
	type_t *type = &type_table[ohdr->type_id];

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
	switch (FUNC_TYPE(ptr)) {
		case func_inter:
			interp = ptr;
			printf("<lambda/%d>", interp->argc);
			break;
		case func_native:
			native = ptr;
			printf("<native %s/%d>", native->name, native->argc);
			break;
		default:
			printf("<unknown func>");
	}
}

static void print_obj(obj_t obj)
{
	switch (obj.tag) {
		case id_ptr:
			print_ptr(obj);
			break;
		case id_fixnum:
			printf("%d", FIXNUM(obj));
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

static int display(heap_t *heap, trampoline_t *tramp,
		obj_t *argv, int argc)
{
	print_obj(argv[1]);
	printf("\n");
	tramp->arg[0] = cvoid.obj;

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
	ns_install_native(tbl, "car", &car_nt);
	ns_install_native(tbl, "cdr", &cdr_nt);
	ns_install_native(tbl, "eq?", &eq_nt);

	ns_install_native(tbl, "+", &fxsum_nt);
	ns_install_native(tbl, "-", &fxsub_nt);
	ns_install_native(tbl, "*", &fxmul_nt);
	ns_install_native(tbl, "/", &fxdiv_nt);
	ns_install_native(tbl, "=", &fxeq_nt);
}
