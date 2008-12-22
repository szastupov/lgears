#include <stdlib.h>
#include <ctype.h>
#include "memory.h"
#include "tokenizer.h"

typedef struct {
	tree_node_t node;
	int tag;
	unsigned flags;
} token_pattern_t;

void tokenizer_add_pat(tokenizer_t *tz, 
		const char *pat, int tag, unsigned flags)
{
	token_pattern_t *new = type_alloc(token_pattern_t);
	new->tag = tag;
	new->flags = flags;
	tree_node_init(&new->node, pat);

	if (strlen(pat) > 1)
		tree_node_insert(&tz->long_patterns, &new->node);
	else
		tree_node_insert(&tz->short_patterns, &new->node);
}

void tokenizer_clear(tokenizer_t *tz)
{
	tree_free(tz->short_patterns);
	tz->short_patterns = NULL;
	tree_free(tz->long_patterns);
	tz->long_patterns = NULL;
}

static token_pattern_t* pattern_search(const char *pat, tree_node_t *tree)
{
	tree_node_t *node = tree_node_search(tree, pat);
	if (node)
		return container_of(node, token_pattern_t, node);
	return NULL;
}

static void token_stream_clear(token_stream_t *ts)
{
	ts->state = STREAM_STOPED;
	if (ts->res) {
		mem_free(ts->res);
		ts->res = NULL;
	}
	if (ts->buf) {
		string_free(ts->buf);
		ts->buf = NULL;
	}
}

static void token_add(token_stream_t *ts, token_pattern_t *pat)
{
	if (pat) {
		if (pat->flags & TOKEN_COPY)
			ts->res = strdup(string_char(ts->buf));
		ts->tag = pat->tag;
	} else {
		ts->tag = -1;
		ts->res = strdup(string_char(ts->buf));
	}
	string_clear(ts->buf);
}

static int long_token(token_stream_t *ts)
{
	if (string_empty(ts->buf))
		return 0;

	token_add(ts,
			pattern_search(string_char(ts->buf),
				ts->tz->long_patterns));
	return 1;
}

static void token_stream_next(token_stream_t *ts)
{
	if (!*ts->p)
		token_stream_clear(ts);
	else {
		ts->state = STREAM_RUN;
		if (ts->res) {
			mem_free(ts->res);
			ts->res = NULL;
		}

		char include = 0;
		token_pattern_t *pat = NULL;

		while (1) {
			const char *p = ts->p;
			ts->p++;
			if (include) {
				if (*p == include) {
					token_add(ts, pat);
					ts->tag = pat->tag;
					include = 0;
					return;
				} else
					string_append_char(ts->buf, *p);
			} else if (isspace(*p) || !*p) {
				if (long_token(ts))
					return;
			} else {
				char str[2] = {*p, '\0'};
				if ((pat = pattern_search(str,
								ts->tz->short_patterns)))
				{
					if (long_token(ts)) {
						p--;
						ts->p--;
						return;
					}
					if (pat->flags & TOKEN_INCLUSIVE)
						include = *p;
					else {
						ts->tag = pat->tag;
						return;
					}
				} else
					string_append_char(ts->buf, *p);
			}
			if (!*p) {
				token_stream_clear(ts);
				break;
			}
		}
	}
}

token_stream_t* tokenize(tokenizer_t *tz, const char *str)
{
	token_stream_t* stream = type_alloc(token_stream_t);
	stream->p = str;
	stream->state = STREAM_STOPED;
	stream->next = token_stream_next;
	stream->destroy = token_stream_clear;
	stream->buf = string_alloc(200);
	stream->tz = tz;

	return stream;
}

void tokenizer_load_table(tokenizer_t *tz, const pattern_descr_t *table)
{
	const pattern_descr_t *cur;
	for (cur = table; cur->pattern != NULL; cur++) 
		tokenizer_add_pat(tz, cur->pattern, cur->tag, cur->flags);
}
