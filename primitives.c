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

void print_obj(obj_t obj);

typedef struct {
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

static void* _cons(heap_t *heap, obj_t car, obj_t cdr)
{
	pair_t *pair = heap_alloc(heap, sizeof(pair_t), t_pair);
	pair->car = car;
	pair->cdr = cdr;

	return make_ptr(pair, id_ptr);
}

static int cons(vm_thread_t *thread, obj_t *argv, int argc)
{
	thread->tramp.arg[0].ptr = _cons(&thread->heap, argv[1], argv[2]);

	return RC_OK;
}
MAKE_NATIVE(cons, 2, 0);

static int list(vm_thread_t *thread, obj_t *argv, int argc)
{
	obj_t res = cnull.obj;

	int i;
	for (i = argc-1; i > 0; i--)
		res.ptr = _cons(&thread->heap, argv[i], res);

	thread->tramp.arg[0] = res;

	return RC_OK;
}
MAKE_NATIVE(list, 0, 1);

static int car(vm_thread_t *thread, obj_t *argv, int argc)
{
	pair_t *pair = get_typed(argv[1], t_pair);
	if (pair)
		thread->tramp.arg[0] = pair->car;
	else
		thread->tramp.arg[0].ptr = NULL;

	return RC_OK;
}
MAKE_NATIVE(car, 1, 0);

static int cdr(vm_thread_t *thread, obj_t *argv, int argc)
{
	pair_t *pair = get_typed(argv[1], t_pair);
	if (pair)
		thread->tramp.arg[0] = pair->cdr;
	else
		thread->tramp.arg[0].ptr = NULL;

	return RC_OK;
}
MAKE_NATIVE(cdr, 1, 0);

static int eq(vm_thread_t *thread, obj_t *argv, int argc)
{
	const_t res = CIF(argv[1].ptr == argv[2].ptr);
	thread->tramp.arg[0] = res.obj;

	return RC_OK;
}
MAKE_NATIVE(eq, 2, 0);

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
#if 0
	void *ptr = PTR(obj);
	hobj_hdr_t *ohdr = ptr;
	const type_t *type = &type_table[ohdr->type_id];

	if (type->repr)
		type->repr(ptr);
	else
		printf("<ptr:%s>", type->name);
#endif
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
			printf("<lambda/%d>", interp->hdr.argc);
			break;
		case func_native:
			native = ptr;
			printf("<native %s/%d>", native->name, native->hdr.argc);
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

static int display(vm_thread_t *thread,
		obj_t *argv, int argc)
{
	print_obj(argv[1]);
	printf("\n");
	thread->tramp.arg[0] = cvoid.obj;

	return RC_OK;
}
MAKE_NATIVE(display, 1, 0);

static int vm_exit(vm_thread_t *thread, obj_t *argv, int argc)
{
	return RC_EXIT;
}
MAKE_NATIVE(vm_exit, 0, 0);

static int call_cc(vm_thread_t *thread, obj_t *argv, int argc)
{
	thread->tramp.func.ptr = argv[1].ptr;
	// Pass current continuation to both arguments
	thread->tramp.arg[0] = argv[0];
	thread->tramp.arg[1] = argv[0];
	thread->tramp.argc = 2;

	// But for second, change tag
	thread->tramp.arg[1].tag = id_cont;
	//TODO check for valid function

	return RC_OK;
}
MAKE_NATIVE(call_cc, 1, 0);

/* 
 * File descriptors
 * Export only low level descriptros,
 * r6rs port system will be build on it
 */

typedef struct {
	char str[3];
	int mode;
} fd_mode_t;

static const fd_mode_t fd_modes[] = {
	{ "r", O_RDONLY },
	{ "r+", O_RDWR },
	{ "w", O_WRONLY|O_TRUNC|O_CREAT },
	{ "w+", O_RDWR|O_TRUNC|O_CREAT },
	{ "a", O_WRONLY|O_APPEND|O_CREAT },
	{ "a+", O_RDWR|O_APPEND|O_CREAT },
};

int fd_parse_mode(const char *str)
{
	int i;
	for (i = 0; i < sizeof(fd_modes)/sizeof(fd_mode_t); i++)
		if (strncmp(str, fd_modes[i].str, 2) == 0)
			return fd_modes[i].mode;
	return fd_modes[0].mode;
}

static int fd_open(vm_thread_t *thread, obj_t *argv, int argc)
{
	string_t *path_str = get_typed(argv[1], t_string);
	ASSERT(path_str != NULL);
	string_t *mode_str = get_typed(argv[2], t_string);
	ASSERT(mode_str != NULL);

	int mode = fd_parse_mode(mode_str->str);
	int fd = open(path_str->str, mode);

	RESULT_FIXNUM(fd);
}
MAKE_NATIVE(fd_open, 2, 0);

static int fd_close(vm_thread_t *thread, obj_t *argv, int argc)
{
	RESULT_FIXNUM(close(FIXNUM(argv[1])));
}
MAKE_NATIVE(fd_close, 1, 0);

static int fd_seek(vm_thread_t *thread, obj_t *argv, int argc)
{
	ASSERT(argv[3].tag == id_fixnum);

	int mode = 0;
	switch (FIXNUM(argv[3])) {
		case 0:
			mode = SEEK_SET;
			break;
		case 1:
			mode = SEEK_CUR;
			break;
		case 2:
			mode = SEEK_END;
			break;
		default:
			mode = SEEK_SET;
	}

	off_t offset = lseek(FIXNUM(argv[1]), FIXNUM(argv[2]), mode);
	RESULT_FIXNUM(offset);
}
MAKE_NATIVE(fd_seek, 3, 0);

static int fd_write(vm_thread_t *thread, obj_t *argv, int argc)
{
	ASSERT(argv[1].tag == id_fixnum);
	ASSERT(argv[2].tag == id_ptr);

	string_t *str = get_typed(argv[2], t_string);
	if (!str)
		FATAL("argument is not a string\n");

	int wrote = write(FIXNUM(argv[1]), str->str, str->size-1);

	RESULT_FIXNUM(wrote);
}
MAKE_NATIVE(fd_write, 2, 0);


void ns_install_native(hash_table_t *tbl,
		char *name, const native_t *nt)
{
	ptr_t ptr;
	FUNC_INIT(ptr, nt);
	hash_table_insert(tbl, name, ptr.ptr); 
}

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

	ns_install_native(tbl, "fd-open", &fd_open_nt);
	ns_install_native(tbl, "fd-close", &fd_close_nt);
	ns_install_native(tbl, "fd-seek", &fd_seek_nt);
	ns_install_native(tbl, "fd-write", &fd_write_nt);

	ns_install_fixnum(tbl);
}
