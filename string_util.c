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
#include <stdlib.h>
#include "string_util.h"
#include "memory.h"

string_t* string_alloc(size_t len)
{
	string_t *str = type_alloc(string_t);
	str->buf = mem_alloc(len+1);
	str->allocated = len+1;
	str->len = 0;
	return str;
}

string_t* string_new(const char *src)
{
	size_t len = strlen(src);
	string_t *str = string_alloc(len);
	str->len = len;
	memcpy(str->buf, src, len+1);
	return str;
}

void string_free(string_t *str)
{
	free(str->buf);
	free(str);
}

void string_clear(string_t *str)
{
	str->buf[0] = '\0';
	str->len = 0;
}

static inline void string_check_size(string_t *string, size_t add_size)
{
	size_t req_size  = add_size+string->len+1;
	if (req_size > string->allocated) {
		size_t new_size = req_size*1.5;
		string->buf = realloc(string->buf, new_size);
		string->allocated = new_size;
	}
}

void string_append(string_t *string, char *str)
{
	size_t len = strlen(str);
	string_check_size(string, len);
	memcpy(string->buf+string->len, str, len+1);
	string->len = string->len+len;
}

void string_append_char(string_t *string, char c)
{
	string_check_size(string, 1);
	string->buf[string->len] = c;
	string->buf[string->len+1] = '\0';
	string->len++;
}

static void string_next(string_stream_t *ss)
{
	if (ss->pos >= ss->str->len)
		ss->state = STREAM_STOPED;
	else {
		ss->state = STREAM_RUN;
		ss->c = ss->str->buf[ss->pos];
		ss->pos++;
	}
}

string_stream_t* str_stream(string_t *string)
{
	string_stream_t *str = type_alloc(string_stream_t);
	str->str = string;
	str->pos = 0;
	str->c = 0;
	str->state = STREAM_STOPED;
	str->next = string_next;
	return str;
}
