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
#ifndef STRUCT_H
#define STRUCT_H

typedef struct {
	obj_t type_name;			/* Symbol for type id and name */
	int size;					/* Fields count */
	obj_t *fields;				/* Fields */
} struct_t;

extern int t_struct;

#define IS_STRUCT(obj) IS_TYPE(obj, t_struct)

struct_t* struct_new(allocator_t *al, obj_t *type_name, int size);
void struct_init();

#endif
