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
