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

typedef struct native_s native_t;
typedef void* (*native_func)(heap_t *heap, obj_t *argv);

struct native_s {
	short argc;
	native_func call;
};

#define MAKE_NATIVE(func, fargc) \
	const native_t func##_nt = { \
		.argc = fargc, \
		.call = func \
	}

void* cons(heap_t *heap, obj_t *argv);

void* string(heap_t *heap, const char *src);

#endif /* PRIMITIVES_H */
