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
#ifndef HASH_H
#define HASH_H 

typedef unsigned (*hash_func_t)(const void*);
typedef int (*equal_func_t)(const void*, const void*);

typedef struct hash_node_s {
	void *key;
	void *val;
	unsigned key_hash;
	struct hash_node_s *next;
} hash_node_t;

typedef struct {
	int size;
	int nnodes;
	hash_node_t **nodes;
	hash_func_t hash;
	equal_func_t equal;
	void (*destroy_key)(void*);
	void (*destroy_val)(void*);
} hash_table_t;

void hash_table_init(hash_table_t *tbl, hash_func_t hash,
		equal_func_t equal);
void hash_table_destroy(hash_table_t *tbl);
void* hash_table_lookup(hash_table_t *tbl, const void *key);
void hash_table_insert(hash_table_t *tbl, void *key, void *val);

unsigned string_hash(const void *src);
int string_equal(const void *v1, const void *v2);

#endif /* HASH_H */
