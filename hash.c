/* Copyright (C) 2009 Stepan Zastupov
 * Based on GLib implementation
 * Copyright (C) 1995-1997  Peter Mattis, Spencer Kimball and Josh MacDonald
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the
 * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 *
 * Modified by the GLib Team and others 1997-2000.  See the AUTHORS
 * file for a list of people on the GLib Team.  See the ChangeLog
 * files for a list of changes.  These files are distributed with
 * GLib at ftp://ftp.gtk.org/pub/gtk/.
 */

#include "memory.h"
#include "hash.h"

#define HASH_TABLE_MIN_SIZE 11
#define HASH_TABLE_MAX_SIZE 13845163
#define CLAMP(x, low, high)  (((x) > (high)) ? (high) : (((x) < (low)) ? (low) : (x)))

static const unsigned primes[] =
{
	11,
	19,
	37,
	73,
	109,
	163,
	251,
	367,
	557,
	823,
	1237,
	1861,
	2777,
	4177,
	6247,
	9371,
	14057,
	21089,
	31627,
	47431,
	71143,
	106721,
	160073,
	240101,
	360163,
	540217,
	810343,
	1215497,
	1823231,
	2734867,
	4102283,
	6153409,
	9230113,
	13845163,
};

static const unsigned nprimes = sizeof (primes) / sizeof (primes[0]);

unsigned
spaced_primes_closest (unsigned num)
{
	int i;

	for (i = 0; i < nprimes; i++)
		if (primes[i] > num)
			return primes[i];

	return primes[nprimes - 1];
}

void hash_table_init(hash_table_t *tbl, hash_func_t hash,
		equal_func_t equal)
{
	tbl->size = HASH_TABLE_MIN_SIZE;
	tbl->hash = hash;
	tbl->equal = equal;
	tbl->nodes = mem_calloc(tbl->size, sizeof(hash_node_t*));
}

void hash_table_resize(hash_table_t *tbl)
{
	hash_node_t *node, *next, **new_nodes;
	unsigned hash_val;
	int i, new_size;

	new_size = spaced_primes_closest(tbl->nnodes);
	new_size = CLAMP(new_size, HASH_TABLE_MIN_SIZE, HASH_TABLE_MAX_SIZE);

	new_nodes = mem_calloc(new_size, sizeof(hash_node_t*));

	for (i = 0; i < tbl->size; i++)
		for (node = tbl->nodes[i]; node; node = next) {
			next = node->next;

			hash_val = node->key_hash % new_size;

			node->next = new_nodes[hash_val];
			new_nodes[hash_val] = node;
		}

	mem_free(tbl->nodes);
	tbl->nodes = new_nodes;
	tbl->size = new_size;
}

static inline void hash_table_maybe_resize(hash_table_t *tbl)
{
  int nnodes = tbl->nnodes;
  int size = tbl->size;

  if ((size >= 3 * nnodes && size > HASH_TABLE_MIN_SIZE) ||
      (3 * size <= nnodes && size < HASH_TABLE_MAX_SIZE))
    hash_table_resize(tbl);
}

static hash_node_t** hash_table_lookup_node(hash_table_t *tbl,
		const void *key, unsigned *hash_return)
{
	unsigned key_hash = tbl->hash(key);
	hash_node_t *node, **node_ptr;
	node_ptr = &tbl->nodes[key_hash % tbl->size];

	if (hash_return)
		*hash_return = key_hash;

	if (tbl->equal) {
		while ((node = *node_ptr)) {
			if (node->key_hash == key_hash &&
					tbl->equal(node->key, key))
				break;
			node_ptr = &(*node_ptr)->next;
		}
	} else {
		while ((node = *node_ptr)) {
			if (node->key == key)
				break;
			node_ptr = &(*node_ptr)->next;
		}
	}

	return node_ptr;
}

void* hash_table_lookup(hash_table_t *tbl, const void *key)
{
	hash_node_t **node_ptr = hash_table_lookup_node(tbl, key, NULL);
	if (*node_ptr)
		return (*node_ptr)->val;
	return NULL;
}

void hash_table_insert(hash_table_t *tbl, void *key, void *val)
{
	unsigned key_hash;
	hash_node_t *node;
	hash_node_t **node_ptr = hash_table_lookup_node(tbl, key, &key_hash);

	if ((node = *node_ptr)) {
		node->val = val;
		//FIXME add old val destructor
	} else {
		node = mem_alloc(sizeof(hash_node_t));
		node->key = key;
		node->val = val;
		node->key_hash = key_hash;
		node->next = NULL;
		*node_ptr = node;
		tbl->nnodes++;
		hash_table_maybe_resize(tbl);
	}
}

unsigned string_hash(const void *src)
{
	const char *str = (const char*)src;
	int n = strlen(str);
	unsigned h = 0;

	while (n--) {
		h = (h << 5) - h + *str;
		str++;
	}

	return h;
}

int string_equal(const void *v1, const void *v2)
{
	return strcmp((const char*)v1, (const char*)v2) == 0;
}
