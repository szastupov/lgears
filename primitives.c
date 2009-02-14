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
#include "primitives.h"
#include <string.h>

typedef struct {
	hobj_hdr_t hdr;
	obj_t car, cdr;
} pair_t;

static void pair_visit(visitor_t *vs, void *data)
{
	pair_t *pair = data;
	vs->visit(vs, &pair->car);
	vs->visit(vs, &pair->cdr);
}

const type_t pair_type = {
	.name = "pair",
	.visit = pair_visit
};

void* cons(heap_t *heap, obj_t *argv)
{
	pair_t *pair = heap_alloc(heap, sizeof(pair_t));
	pair->car = argv[0];
	pair->cdr = argv[1];
	pair->hdr.type = &pair_type;

	return_ptr(pair);
}

void* car(heap_t *heap, obj_t *argv)
{
	pair_t *pair = get_typed(argv[0], &pair_type);
	if (pair)
		return pair->car.ptr;
	return NULL;
}

void* cdr(heap_t *heap, obj_t *argv)
{
	pair_t *pair = get_typed(argv[0], &pair_type);
	if (pair)
		return pair->cdr.ptr;
	return NULL;
}

const type_t str_type = {
	.name = "string"
};

typedef struct {
	hobj_hdr_t hdr;
	char *str;
	size_t size;
} str_t;

void* string(heap_t *heap, const char *src)
{
	size_t size = strlen(src)+1;
	void *str_mem = heap_alloc(heap, sizeof(str_t)+size);
	str_t *str = str_mem;
	str->size = size;
	str->str = str_mem+sizeof(str_t);
	str->hdr.type = &str_type;

	return_ptr(str);
}
