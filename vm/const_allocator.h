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
#ifndef CONST_ALLOCATOR_H
#define CONST_ALLOCATOR_H

#include "types.h"
#include "allocator.h"

typedef struct linked_mem_s {
	struct linked_mem_s *next;
#if __WORDSIZE == 32
	char pad[4];
#endif
	block_hdr_t hdr;
} linked_mem_t;

/* Simple allocator for constants */
typedef struct {
	allocator_t al;
	linked_mem_t *mem;
} const_allocator_t;

void const_allocator_clean(const_allocator_t *al);
void const_allocator_init(const_allocator_t *allocator);
#endif
