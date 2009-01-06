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
#ifndef AST_H
#define AST_H 
#include "list.h"

enum {
	SCOPE_OPEN, SCOPE_CLOSE, DEFINATION,
	LAMBDA, STRING, NUMBER, LIBRARY, EXPORT, IF_STMT,
	AND_STMT, OR_STMT, NOT_OP
};

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

#define ast_iter_forward(n) for (; n != NULL; n = AST_NEXT(n))

list_t* parse_buf(const char *buf);
void ast_node_free(ast_node_t *node);

#endif /* AST_H */
