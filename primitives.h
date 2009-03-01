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

#include "heap.h"
#include "hash.h"

typedef struct {
	ptr_t func;
	obj_t arg[2];
	int argc;
} trampoline_t;

typedef struct native_s native_t;
typedef int (*native_func)(heap_t *heap, trampoline_t *tramp,
		obj_t *argv, int argc);

enum { RC_OK, RC_ERROR, RC_EXIT };

struct native_s {
	func_type_t type;
	short argc;
	unsigned swallow:1;
	native_func call;
	const char *name;
};

#define MAKE_NATIVE(func, fargc, fswallow) \
	const native_t func##_nt = { \
		.type = func_native, \
		.argc = fargc+1, \
		.call = func, \
		.swallow = fswallow, \
		.name = #func \
	}

void ns_install_native(hash_table_t *tbl,
		char *name, const native_t *nt);

void ns_install_primitives(hash_table_t *tbl);
void pair_repr(void *ptr);
void pair_visit(visitor_t *vs, void *data);

#endif /* PRIMITIVES_H */
