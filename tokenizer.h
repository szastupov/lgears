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
#ifndef TOKENIZER_H
#define TOKENIZER_H 

#include "btree.h"
#include "string_util.h"

#define TOKEN_COPY		1
#define TOKEN_INCLUSIVE	(1 << 1)
#define TOKEN_GROUP		(1 << 2)

typedef struct {
	tree_node_t *short_patterns;
	tree_node_t *long_patterns;
} tokenizer_t;

DEFINE_STREAM(token,
		tokenizer_t *tz;
		int tag;
		char *res;
		const char *p;
		string_t *buf);

typedef struct {
	char *pattern;
	int tag;
	unsigned flags;
} pattern_descr_t;

void tokenizer_add_pat(tokenizer_t *tz, 
		const char *pat, int tag, unsigned flags);

token_stream_t* tokenize(tokenizer_t *tz, const char *str);

void tokenizer_clear(tokenizer_t *tz);

void tokenizer_load_table(tokenizer_t *tz, const pattern_descr_t *table);

#endif /* TOKENIZER_H */
