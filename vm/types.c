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
#include "types.h"
#include "cutil.h"

static int types_count = 0;
type_t type_table[VM_MAX_TYPES];

int register_type(const char *name, void (*repr)(void*), visitor_fun visit)
{
	if (types_count == VM_MAX_TYPES-1)
		FATAL("failed to register type %s, increase VM_MAX_TYPES please\n", name);
	type_t *type = &type_table[types_count];
	type->name = name;
	type->repr = repr;
	type->visit = visit;

	return types_count++;
}
