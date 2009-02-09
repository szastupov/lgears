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
} hash_table_t;

void hash_table_init(hash_table_t *tbl, hash_func_t hash,
		equal_func_t equal);
void hash_table_destroy(hash_table_t *tbl);
void* hash_table_lookup(hash_table_t *tbl, const void *key);
void hash_table_insert(hash_table_t *tbl, void *key, void *val);

unsigned string_hash(const void *src);
int string_equal(const void *v1, const void *v2);

#endif /* HASH_H */
