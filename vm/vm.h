/*
 * This file is part of lGears scheme system
 * Copyright (C) 2009 Stepan Zastupov <redchrom@gmail.com>
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
#include "cutil.h"
#include "types.h"
#include "heap.h"

extern hash_table_t builtin;
extern char *cache_path;

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
#if __WORDSIZE == 32
	char pad[4];
#endif
	block_hdr_t hdr;
} linked_mem_t;

/* Simple allocator for constants */
typedef struct {
	allocator_t al;
	linked_mem_t *mem;
} const_allocator_t;

/* Module representation */
struct module_s {
	func_t *functions;
	int entry_point;
	int fun_count;
	obj_t *consts;
	const_allocator_t allocator;
};

typedef struct env_s {
	struct env_s *prev;
	int size;
	int depth;
	obj_t *objects;
} env_t;
#define ENV(o) ((env_t*)PTR(o))

typedef struct {
	env_t *env;
	obj_t *bindmap;
	func_t *func;
} closure_t;

typedef struct {
	obj_t func;
	int argc;
} trampoline_t;

typedef struct {
	heap_t heap;

	obj_t *opstack;
	int op_stack_idx;
	int op_stack_size;
	obj_t *objects;
	env_t *env;
	obj_t *bindmap;
	func_t *func;
	closure_t *closure;
	int heap_env;

	obj_t lib_cache;
	obj_t exception_handlers;

	trampoline_t tramp;
} vm_thread_t;

#if CHECK_OPSTACK
static inline void __stack_push(vm_thread_t *thread, obj_t obj)
{
	if (thread->op_stack_idx == thread->op_stack_size-1)
		FATAL("stack overflow (push)\n");

	thread->opstack[thread->op_stack_idx++] = obj;
}

static inline obj_t __stack_pop(vm_thread_t *thread)
{
	if (thread->op_stack_idx == 0)
		FATAL("stack overflow (pop)\n");

	return thread->opstack[--thread->op_stack_idx];
}
#define STACK_PUSH(n) __stack_push(thread, n)
#define STACK_POP() __stack_pop(thread)
#else
#define STACK_PUSH(n) thread->opstack[thread->op_stack_idx++] = n
#define STACK_POP() thread->opstack[--thread->op_stack_idx]
#endif

obj_t make_symbol(const char *str);
void thread_after_gc(visitor_t *visitor, vm_thread_t *thread);
void thread_get_roots(visitor_t *visitor, vm_thread_t *thread);

#endif /* VM_H */
