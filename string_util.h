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
#ifndef STRING_UTIL_H
#define STRING_UTIL_H 

#include <string.h>
#include "streams.h"

typedef struct {
	char *buf;
	size_t allocated;
	size_t len;
} string_t;

string_t* string_alloc(size_t len);
string_t* string_new(const char *src);
void string_free(string_t *str);
void string_clear(string_t *str);
void string_append(string_t *string, char *str);
void string_append_char(string_t *string, char c);
#define string_empty(str) (str->len == 0)
#define string_char(str) str->buf

DEFINE_STREAM(string, string_t *str; int pos; char c);
string_stream_t* str_stream(string_t *string);


#endif /* STRING_UTIL_H */
