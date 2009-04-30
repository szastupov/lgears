/*
 * This file is part of lGears scheme system
 * Copyright (C) 2009 Stepan Zastupov
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 3 of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General
 * Public Licens along with this program, if not; see
 * <http://www.gnu.org/licenses>.
 */
#ifndef VM_H
#define VM_H
#include <stdint.h>
#include "hash.h"
#include "memory.h"
#include "types.h"
#include "heap.h"

extern hash_table_t ns_global;

typedef struct {
	short up, idx;
} bind_t;

typedef struct module_s module_t;
typedef struct {
	func_hdr_t hdr;
	short env_size;
	int op_count;
	short heap_env;
	int depth;
	int16_t *opcode;
	bind_t *bindings;
	int bcount;
	int *bindmap;
	int bmcount;
	module_t *module;

	char *dbg_symbols;
	const char **dbg_table;
} func_t;

typedef struct linked_mem_s {
	struct linked_mem_s *next;
	block_hdr_t hdr;
} linked_mem_t;

/* Simple allocator for constants */
typedef struct {
	allocator_t al;
	linked_mem_t *mem;
} const_allocator_t;

struct module_s {
	func_t *functions;
	int entry_point;
	int fun_count;
	obj_t *imports;
	obj_t *consts;
	const_allocator_t allocator;
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

void* make_symbol(const char *str);
void thread_after_gc(visitor_t *visitor, vm_thread_t *thread);
void thread_get_roots(visitor_t *visitor, vm_thread_t *thread);

#endif /* VM_H */
