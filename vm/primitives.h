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
#include "native.h"
#include "vector.h"

void ns_install_primitives(hash_table_t *tbl);

typedef struct {
	obj_t car, cdr;
} pair_t;

void pair_repr(void *ptr);
void pair_visit(visitor_t *vs, void *data);
void* _list(heap_t *heap, obj_t *argv, int argc);

typedef struct {
	char *str;
	int size;
	unsigned copy:1;
} string_t;

void* _string(heap_t *heap, char *str, int copy);
void string_repr(void *ptr);

typedef struct {
	obj_t func;
} continuation_t;

void continuation_visit(visitor_t *vs, void *data);

#endif /* PRIMITIVES_H */
