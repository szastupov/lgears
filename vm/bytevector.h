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
#ifndef BYTEVECTOR_H
#define BYTEVECTOR_H

typedef struct {
	unsigned char *data;
	int size;
} bytevector_t;

#define IS_BYTEVECTOR(obj) IS_TYPE(obj, t_bytevector)

void bv_repr(void *ptr);
void bv_visit(visitor_t *vs, void *data);
void ns_install_bytevector(hash_table_t *tbl);

#endif
