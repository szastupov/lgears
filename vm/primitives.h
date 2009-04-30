/*
 * This file is part of lGears scheme system
 * Copyright (C) 2009 Stepan Zastupov
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
#ifndef PRIMITIVES_H
#define PRIMITIVES_H 
#include "native.h"
#include "vector.h"
#include "string.h"
#include "bytevector.h"

void ns_install_primitives(hash_table_t *tbl);

typedef struct {
	obj_t car, cdr;
} pair_t;

void pair_repr(void *ptr);
void pair_visit(visitor_t *vs, void *data);
void* _list(heap_t *heap, obj_t *argv, int argc);

#define IS_PAIR(obj) IS_TYPE(obj, t_pair)

typedef struct {
	obj_t func;
} continuation_t;

void continuation_visit(visitor_t *vs, void *data);

#endif /* PRIMITIVES_H */
