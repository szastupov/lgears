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
#ifndef CUTIL_H
#define CUTIL_H

#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <stddef.h>
#include <alloca.h>
#include "config.h"

/* Simplified container_of */
#ifndef container_of
#define container_of(ptr, type, member)			\
	((void*)ptr - offsetof(type, member))
#endif

#define likely(x)	__builtin_expect(!!(x), 1)
#define unlikely(x)	__builtin_expect(!!(x), 0)

#define ASSERT(e) if (!(e)) FATAL("Assertion failed %s\n", #e);

#define LOG_DBG(msg...) fprintf(stderr, msg);
#define LOG_ERR(msg...) {							\
		fprintf(stderr, "%s:%d `%s': ",				\
				__FILE__, __LINE__, __FUNCTION__);	\
		fprintf(stderr, msg);						\
	}

#define FATAL(msg...) {							\
		LOG_ERR(msg);							\
		abort();								\
	}

#define mem_alloc(size) malloc(size)

#define mem_realloc(p, size) realloc(p, size)

#define mem_free(p) free(p)

#define mem_calloc(nmemb, size) calloc(nmemb, size)

#define new0(type) mem_calloc(1, sizeof(type))

#endif
