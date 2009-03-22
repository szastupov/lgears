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

