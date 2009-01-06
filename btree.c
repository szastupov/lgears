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
#include "btree.h"
#include "memory.h"

void tree_node_init(tree_node_t *bt, const char *name)
{
	bt->name = strdup(name);
	bt->key = *name;
}

void tree_node_insert(tree_node_t **parent, tree_node_t *new)
{
	tree_node_t *p = *parent;
	if (p == NULL) {
		*parent = new;
		return;
	}
	if (new->key < p->key)
		tree_node_insert(&p->l, new);
	else
		tree_node_insert(&p->r, new);
}

tree_node_t* tree_node_search(tree_node_t *parent, const char *name)
{
	if (parent == NULL)
		return NULL;
	if (*name == parent->key && strcmp(name, parent->name) == 0)
		return parent;
	if (*name < parent->key)
		return tree_node_search(parent->l, name);
	return tree_node_search(parent->r, name);
}

void tree_free(tree_node_t *head)
{
	if (!head)
		return;
	mem_free(head->name);
	tree_free(head->l);
	tree_free(head->r);
	mem_free(head);
}
