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
#ifndef PRIMITIVES_H
#define PRIMITIVES_H 

#include "vm.h"

typedef struct native_s native_t;
typedef int (*native_func)(vm_thread_t *thread,
		obj_t *argv, int argc);

enum { RC_OK, RC_ERROR, RC_EXIT };

struct native_s {
	func_hdr_t hdr;
	native_func call;
	const char *name;
};

#define MAKE_NATIVE(func, fargc, fswallow) \
	const native_t func##_nt = { \
		.hdr.type = func_native, \
		.hdr.argc = fargc+1, \
		.hdr.swallow = fswallow, \
		.call = func, \
		.name = #func \
	}

#define RESULT_FIXNUM(num) \
	FIXNUM_INIT(*(fixnum_t*)&thread->tramp.arg[0], num); \
	return RC_OK;

void ns_install_native(hash_table_t *tbl,
		char *name, const native_t *nt);

void ns_install_primitives(hash_table_t *tbl);
void pair_repr(void *ptr);
void pair_visit(visitor_t *vs, void *data);

typedef struct {
	char *str;
	int size;
	unsigned copy:1;
} string_t;

void* _string(heap_t *heap, char *str, int copy);

#endif /* PRIMITIVES_H */
