#ifndef BTREE_H
#define BTREE_H 

typedef struct tree_node_s {
	struct tree_node_s *l, *r;
	int key;
	char *name;
} tree_node_t;

void tree_node_init(tree_node_t *bt, const char *name);
tree_node_t* tree_node_search(tree_node_t *parent, const char *name);
void tree_node_insert(tree_node_t **parent, tree_node_t *new);
void tree_free(tree_node_t *head);

#endif /* BTREE_H */
