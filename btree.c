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
