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
#include "compiler_internal.h"

#define MAX(a,b) (a > b) ? a : b

sc_opcode_t* add_opcode(compiler_t *sc,
		int opcode, int arg, int stack_use)
{
	sc_func_t *func = sc->sc_env_stack->func;
	sc_opcode_t *code = type_alloc(sc_opcode_t);
	code->code = opcode;
	code->arg = arg;
	code->idx = func->op_count;

	list_append(&func->opcodes, &code->next);
	func->stack_use += stack_use;
	func->stack_size = MAX(func->stack_size, func->stack_use);
	func->op_count++;
	return code;
}

int next_opcode_idx(compiler_t *sc)
{
	return sc->sc_env_stack->func->op_count;
}

sc_func_t* function_new(compiler_t *sc)
{
	sc_func_t *func = type_alloc(sc_func_t);
	AREA_APPEND(sc->functions, func);

	return func;
}

sc_const_t* const_new(compiler_t *sc)
{
	sc_const_t *cst = type_alloc(sc_const_t);
	AREA_APPEND(sc->consts, cst);

	return cst;
}

sc_func_t* get_func_by_id(compiler_t *sc, int id)
{
	if (id >= sc->functions.count)
		FATAL("Function index out of range\n");
	int i = 0;
	list_node_t *cur;
	list_for_each(&sc->functions.list, cur) {
		if (i == id)
			break;
		i++;
	}

	return container_of(cur, sc_func_t, next);
}

sc_env_t* sc_env_new(sc_env_t *parent)
{
	sc_env_t *env	= type_alloc(sc_env_t);
	env->tbl	= NULL;
	env->parent	= parent;

	return env;
}

void sc_env_free(sc_env_t *env)
{
	tree_free(env->tbl);
	mem_free(env);
}

void sc_env_stack_push(compiler_t *sc)
{
	sc->sc_env_stack = sc_env_new(sc->sc_env_stack);
}

void sc_env_stack_pop(compiler_t *sc)
{
	sc_env_t *tmp = sc->sc_env_stack;
	sc->sc_env_stack = tmp->parent;
	sc_env_free(tmp);
}

void sc_env_define(sc_env_t *env, const char *name,
		int type, int idx)
{
	load_t *load = type_alloc(load_t);
	load->type	= type;
	load->idx	= idx;

	tree_node_init(&load->node, name);
	tree_node_insert(&env->tbl, &load->node);
}

load_t *sc_env_lookup(sc_env_t *env, const char *arg)
{
	if (!env)
		return NULL;

	tree_node_t *node = tree_node_search(env->tbl, arg);
	if (node) {
		return container_of(node, load_t, node);
	}

	return sc_env_lookup(env->parent, arg);
}

