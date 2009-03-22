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
#include "native.h"

static int fxsum(vm_thread_t *thread, obj_t *argv, int argc)
{
	fixnum_t res;
	FIXNUM_INIT(res, 0);
	int i;
	for (i = 1; i < argc; i++) {
		ASSERT(argv[i].tag == id_fixnum);
		res.val += FIXNUM(argv[i]);
	}

	RESULT_OBJ(res.obj);
}
MAKE_NATIVE_VARIADIC(fxsum, 0);

static int fxsub(vm_thread_t *thread, obj_t *argv, int argc)
{
	fixnum_t res;
	FIXNUM_INIT(res, FIXNUM(argv[1]));
	int i;
	for (i = 2; i < argc; i++)
		res.val -= FIXNUM(argv[i]);


	RESULT_OBJ(res.obj);
}
MAKE_NATIVE_VARIADIC(fxsub, 2);

static int fxmul(vm_thread_t *thread, obj_t *argv, int argc)
{
	fixnum_t res;
	FIXNUM_INIT(res, 1);
	int i;
	for (i = 1; i < argc; i++)
		res.val *= FIXNUM(argv[i]);

	RESULT_OBJ(res.obj);
}
MAKE_NATIVE_VARIADIC(fxmul, 0);

static int fxdiv(vm_thread_t *thread, obj_t *argv, int argc)
{
	fixnum_t res;
	FIXNUM_INIT(res, FIXNUM(argv[1]));
	int i;
	for (i = 2; i < argc; i++)
		res.val /= FIXNUM(argv[i]);

	RESULT_OBJ(res.obj);
}
MAKE_NATIVE_VARIADIC(fxdiv, 2);

static int fxmod(vm_thread_t *thread, obj_t *argv, int argc)
{
	fixnum_t res;
	FIXNUM_INIT(res, FIXNUM(argv[1]));
	int i;
	for (i = 2; i < argc; i++)
		res.val %= FIXNUM(argv[i]);

	RESULT_OBJ(res.obj);
}
MAKE_NATIVE_VARIADIC(fxmod, 2);

static int fxeq(vm_thread_t *thread, obj_t *argv, int argc)
{
	const_t res = ctrue;

	int i;
	for (i = 2; i < argc; i++) {
		SAFE_ASSERT(argv[i-1].tag == id_fixnum);
		SAFE_ASSERT(argv[i].tag == id_fixnum);
		if (FIXNUM(argv[i-1]) != FIXNUM(argv[i])) {
			res = cfalse;
			break;
		}
	}

	RESULT_OBJ(res.obj);
}
MAKE_NATIVE_VARIADIC(fxeq, 2);

static int fxless(vm_thread_t *thread, obj_t *argv, int argc)
{
	const_t res = ctrue;

	int i;
	for (i = 2; i < argc; i++) {
		if (!(FIXNUM(argv[i-1]) < FIXNUM(argv[i]))) {
			res = cfalse;
			break;
		}
	}

	RESULT_OBJ(res.obj);
}
MAKE_NATIVE_VARIADIC(fxless, 2);

static int fxgreat(vm_thread_t *thread, obj_t *argv, int argc)
{
	const_t res = ctrue;

	int i;
	for (i = 2; i < argc; i++) {
		if (!(FIXNUM(argv[i-1]) > FIXNUM(argv[i]))) {
			res = cfalse;
			break;
		}
	}

	RESULT_OBJ(res.obj);
}
MAKE_NATIVE_VARIADIC(fxgreat, 2);

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
	ns_install_native(tbl, "+", &fxsum_nt);
	ns_install_native(tbl, "-", &fxsub_nt);
	ns_install_native(tbl, "*", &fxmul_nt);
	ns_install_native(tbl, "/", &fxdiv_nt);
	ns_install_native(tbl, "mod", &fxmod_nt);
	ns_install_native(tbl, "=", &fxeq_nt);
	ns_install_native(tbl, "<", &fxless_nt);
	ns_install_native(tbl, ">", &fxgreat_nt);
	ns_install_native(tbl, "fxior", &fxior_nt);
}
