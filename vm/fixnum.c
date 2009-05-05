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
#include "native.h"

static int add(vm_thread_t *thread, obj_t *a, obj_t *b)
{
	SAFE_ASSERT(a->tag == id_fixnum);
	SAFE_ASSERT(b->tag == id_fixnum);

	RESULT_FIXNUM(FIXNUM(*a) + FIXNUM(*b));
}
MAKE_NATIVE_BINARY(add);

static int sub(vm_thread_t *thread, obj_t *a, obj_t *b)
{
	SAFE_ASSERT(a->tag == id_fixnum);
	SAFE_ASSERT(b->tag == id_fixnum);

	RESULT_FIXNUM(FIXNUM(*a) - FIXNUM(*b));
}
MAKE_NATIVE_BINARY(sub);

static int mul(vm_thread_t *thread, obj_t *a, obj_t *b)
{
	SAFE_ASSERT(a->tag == id_fixnum);
	SAFE_ASSERT(b->tag == id_fixnum);

	RESULT_FIXNUM(FIXNUM(*a) * FIXNUM(*b));
}
MAKE_NATIVE_BINARY(mul);

static int divide(vm_thread_t *thread, obj_t *a, obj_t *b)
{
	SAFE_ASSERT(a->tag == id_fixnum);
	SAFE_ASSERT(b->tag == id_fixnum);
	SAFE_ASSERT(FIXNUM(*b) != 0);

	RESULT_FIXNUM(FIXNUM(*a) / FIXNUM(*b));
}
MAKE_NATIVE_BINARY(divide);

static int mod(vm_thread_t *thread, obj_t *a, obj_t *b)
{
	SAFE_ASSERT(a->tag == id_fixnum);
	SAFE_ASSERT(b->tag == id_fixnum);

	RESULT_FIXNUM(FIXNUM(*a) % FIXNUM(*b));
}
MAKE_NATIVE_BINARY(mod);

static int ieq(vm_thread_t *thread, obj_t *a, obj_t *b)
{
	SAFE_ASSERT(a->tag == id_fixnum);
	SAFE_ASSERT(b->tag == id_fixnum);

	RESULT_BOOL(FIXNUM(*a) == FIXNUM(*b));
}
MAKE_NATIVE_BINARY(ieq);

static int igreat(vm_thread_t *thread, obj_t *a, obj_t *b)
{
	SAFE_ASSERT(a->tag == id_fixnum);
	SAFE_ASSERT(b->tag == id_fixnum);

	RESULT_BOOL(FIXNUM(*a) > FIXNUM(*b));
}
MAKE_NATIVE_BINARY(igreat);

static int iless(vm_thread_t *thread, obj_t *a, obj_t *b)
{
	SAFE_ASSERT(a->tag == id_fixnum);
	SAFE_ASSERT(b->tag == id_fixnum);

	RESULT_BOOL(FIXNUM(*a) < FIXNUM(*b));
}
MAKE_NATIVE_BINARY(iless);

static int fxior(vm_thread_t *thread, obj_t *argv, int argc)
{
	fixnum_t res;
	FIXNUM_INIT(res, 0);
	int i;
	for (i = 1; i < argc; i++)
		res.val |= FIXNUM(argv[i]);

	RESULT_OBJ(res.obj);
}
MAKE_NATIVE_VARIADIC(fxior, 0);

void ns_install_fixnum(hash_table_t *tbl)
{
	ns_install_native(tbl, "$+", &add_nt);
	ns_install_native(tbl, "$-", &sub_nt);
	ns_install_native(tbl, "$*", &mul_nt);
	ns_install_native(tbl, "$/", &divide_nt);
	ns_install_native(tbl, "mod", &mod_nt);
	ns_install_native(tbl, "$=", &ieq_nt);
	ns_install_native(tbl, "$<", &iless_nt);
	ns_install_native(tbl, "$>", &igreat_nt);
	ns_install_native(tbl, "fxior", &fxior_nt);
}
