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

typedef struct vm_thread_s vm_thread_t;

#include "heap.h"
#include "const_allocator.h"

extern hash_table_t builtin;
extern const_allocator_t global_const_pool;
extern char *cache_path;

typedef struct {
	short up, idx;
} bind_t;

typedef struct module_s module_t;
typedef struct {
	func_hdr_t hdr;

	/* Fields copied from func_hdr_s */
	short env_size;
	int op_count;
	short heap_env;
	int depth;
	int bcount;
	int bmcount;

	int16_t *opcode;			/* Opcode */
	bind_t *bindings;			/* Bindings */
	int *bindmap;				/* Bindmap */
	module_t *module;			/* Module */

	char *dbg_symbols;
	const char **dbg_table;
} func_t;

/* Module representation */
struct module_s {
	func_t *functions;			/* Functions */
	int entry_point;			/* Entry point */
	int fun_count;				/* Functions count */
	obj_t *consts;				/* Consant pool */
	const_allocator_t allocator; /* Constant allocator */
};

typedef struct env_s {
	struct env_s *prev;			/* Parent environment */
	int size;					/* Objects count */
	int depth;					/* Depth marker */
	obj_t *objects;				/* Objects */
} env_t;
#define ENV(o) ((env_t*)PTR(o))

typedef struct {
	env_t *env;					/* Parent invironment */
	obj_t *bindmap;				/* Precreated bindmap */
	func_t *func;				/* Function */
} closure_t;

typedef struct {
	obj_t func;					/* Function to pass control */
	int argc;					/* Arguments count */
} trampoline_t;

struct vm_thread_s {
	heap_t heap;				/* Heap */
	obj_t *opstack;				/* Operands stack */
	int op_stack_idx;			/* Stack index */
	int op_stack_size;			/* Stack size (max objects count) */
	obj_t *objects;				/* Locals pointer (may be stack or env) */
	env_t *env;					/* Last environment */
	obj_t *bindmap;				/* Current bindmap (if exists) */
	func_t *func;				/* Current function */
	closure_t *closure;			/* Current closure (if exists) */
	int heap_env;				/* Is env allocated on heap? */
	obj_t lib_cache;			/* Library cache register */
	obj_t exception_handlers;	/* Exception handlers register */
	trampoline_t tramp;			/* Trampoline for native/built-in functions */
};

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
