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
#ifndef VECTOR_H
#define VECTOR_H 

typedef struct {
	obj_t *objects;
	int size;
} vector_t;

#define IS_VECTOR(obj) IS_TYPE(obj, t_vector)

void vector_repr(void *ptr);
void vector_visit(visitor_t *vs, void *data);
void ns_install_vector(hash_table_t *tbl);

#endif /* VECTOR_H */
