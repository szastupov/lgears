#ifndef AST_H
#define AST_H 
#include "list.h"

enum { SCOPE_OPEN, SCOPE_CLOSE, DEFINATION,
	LAMBDA, STRING, NUMBER, LIBRARY, EXPORT, IF_STMT };

typedef struct {
	list_node_t node;
	list_t childs;
	char *data;
	int tag;
} ast_node_t;

#define AST_NODE(l)		container_of(l, ast_node_t, node)
#define AST_CHILD(n)	AST_NODE(list_head(&(n)->childs))
#define AST_NEXT(n)		AST_NODE((n)->node.next)
#define AST_SECOND(n)	AST_NODE((n)->node.next->next)

#define ast_iter_forward(n) \
	for (; n != NULL; n = AST_NEXT(n))

list_t* parse_buf(const char *buf);

#endif /* AST_H */
