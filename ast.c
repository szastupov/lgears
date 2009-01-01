#include "ast.h"
#include "tokenizer.h"

const pattern_descr_t pat_table[] = {
	{ "(",			SCOPE_OPEN },
	{ ")",			SCOPE_CLOSE },
	{ "define",		DEFINATION },
	{ "lambda",		LAMBDA },
	{ "library",	LIBRARY },
	{ "export",		EXPORT },
	{ "\"",			STRING, TOKEN_INCLUSIVE|TOKEN_COPY },
	{ "if",			IF_STMT },
	{ "and",		AND_STMT },
	{ "or",			OR_STMT },
	{ NULL }
};

static list_t* build_tree(token_stream_t *ts)
{
	stack_t stack;
	list_init(&stack);
	list_t *head = type_alloc(list_t);

	void add_child(ast_node_t *node)
	{
		ast_node_t *parent = stack_head_itm(&stack, ast_node_t);
		list_append(&parent->childs, &node->node);
	}

	stream_iter(ts) {
		switch (ts->tag) {
		case SCOPE_OPEN:
			{
				ast_node_t *node = type_alloc(ast_node_t);
				node->tag = SCOPE_OPEN;
				if (stack_head(&stack))
					add_child(node);
				else
					list_append(head, &node->node);
				stack_push(&stack, node);
			}
			break;
		case SCOPE_CLOSE:
			if (stack_head(&stack))
				stack_pop(&stack);
			else
				FATAL("Unexpected scope close\n");
			break;
		default:
			if (stack_head(&stack))
			{
				ast_node_t *node = type_alloc(ast_node_t);
				node->tag = ts->tag;
				if (ts->res)
					node->data = strdup(ts->res);
				add_child(node);
			} else
				FATAL("Symbols not in scope\n");
		}
	}
	if (stack_head(&stack))
		FATAL("Unclosed scope\n");

	return head;
}

list_t* parse_buf(const char *buf)
{
	tokenizer_t tz;
	memset(&tz, 0, sizeof(tz));
	tokenizer_load_table(&tz, pat_table);

	token_stream_t *ts = tokenize(&tz, buf);
	list_t *res = build_tree(ts);
	stream_free(ts);

	tokenizer_clear(&tz);

	return res;
}

void ast_node_free(ast_node_t *node)
{
	if (node->data)
		mem_free(node->data);

	list_node_t *cur, *save;
	list_for_each_safe(&node->childs, cur, save) {
		ast_node_free(AST_NODE(cur));
	}
	mem_free(node);
}
