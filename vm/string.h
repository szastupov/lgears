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
#ifndef STRING_H
#define STRING_H 

typedef struct {
	char *str;
	int size;
	unsigned copy:1;
} string_t;

#define IS_STRING(obj) IS_TYPE(obj, t_string)

void* _string(allocator_t *alloc, char *str, int copy);
void string_repr(void *ptr);
void string_visit(visitor_t *vs, void *data);
void ns_install_string(hash_table_t *tbl);

#endif /* STRING_H */
