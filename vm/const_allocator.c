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
#include "cutil.h"
#include "const_allocator.h"

static void* const_allocator_alloc(allocator_t *al,
								   size_t size,
								   int type_id)
{
	const_allocator_t *cal = container_of(al, const_allocator_t, al);
	void *ptr = mem_alloc(sizeof(linked_mem_t)+size);
	linked_mem_t *new = ptr;
	memset(&new->hdr, 0, sizeof(new->hdr));
	new->hdr.size = size;
	new->hdr.type_id = type_id;
	new->next = cal->mem;
	cal->mem = new;

	return ptr+sizeof(linked_mem_t);
}

void const_allocator_clean(const_allocator_t *al)
{
	linked_mem_t *cur, *next;
	cur = al->mem;
	while (cur) {
		next = cur->next;
		mem_free(cur);
		cur = next;
	}
}

void const_allocator_init(const_allocator_t *allocator)
{
	allocator->al.alloc = const_allocator_alloc;
	allocator->al.id = TAG_CONST_PTR;
	allocator->mem = NULL;
}
