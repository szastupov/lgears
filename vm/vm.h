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
#ifndef VM_H
#define VM_H 
#include <stdint.h>
#include "memory.h"
#include "types.h"
#include "heap.h"
#include "hash.h"

extern hash_table_t ns_global;
extern hash_table_t sym_table;

typedef struct {
	short up, idx;
} bind_t;

typedef struct module_s module_t;
typedef struct {
	func_hdr_t hdr;
	int stack_size;
	short env_size;
	int op_count;
	short heap_env;
	int depth;
	char *opcode;
	bind_t *bindings;
	int bcount;
	int *bindmap;
	int bmcount;
	module_t *module;

	char *dbg_symbols;
	const char **dbg_table;
} func_t;

struct module_s {
	char *code;
	func_t *functions;
	int entry_point;
	int fun_count;
	obj_t *symbols;
	obj_t *imports;
	char **strings;
};

typedef struct {
	int size;
	obj_t *objects;
} env_t;
#define ENV(o) ((env_t*)PTR(o))

typedef struct display_s {
	struct display_s *prev;
	unsigned depth;
	unsigned has_env:1;
} display_t;

typedef struct {
	display_t *display;
	func_t *func;
} closure_t;

typedef struct {
	obj_t func;
	int argc;
} trampoline_t;

typedef struct {
	heap_t heap;

	/* Variables representing execution state */
	int ssize;			/**< Size of stack */
	obj_t *opstack;		/**< Operands stack */
	int op_stack_idx;	/**< Stack index */
	obj_t *objects;	
	env_t *env;
	display_t *display;
	obj_t *bindmap;
	func_t *func;

	trampoline_t tramp;
} vm_thread_t;

#define STACK_PUSH(n) thread->opstack[thread->op_stack_idx++].ptr = n
#define STACK_POP() thread->opstack[--thread->op_stack_idx]

void* make_symbol(hash_table_t *tbl, const char *str);

#endif /* VM_H */
