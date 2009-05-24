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
#ifndef ALLOCATOR_H
#define ALLOCATOR_H

/* Abstract allocator interface */
typedef struct allocator_s {
	void* (*alloc)(struct allocator_s*, size_t, int);
	int id;
} allocator_t;

static inline void* allocator_alloc(allocator_t *al, size_t size, int type_id)
{
	return al->alloc(al, size, type_id);
}

#endif
