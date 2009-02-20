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
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>

#include "memory.h"
#include "opcodes.h"
#include "types.h"
#include "heap.h"
#include "primitives.h"
#include "hash.h"
#include "vm_private.h"

static void env_visit(visitor_t *vs, void *data)
{
	env_t *env = data;
	int i;
	for (i = 0; i < env->size; i++)
		vs->visit(vs, &env->objects[i]);
}

const type_t env_type = {
	.name = "env",
	.visit = env_visit
};

env_t* env_new(heap_t *heap, int size)
{
	void *mem = heap_alloc(heap, sizeof(env_t)+sizeof(obj_t)*size);
	env_t *env = mem;
	env->objects = mem+sizeof(env_t);
	env->size = size;
	env->hdr.type = &env_type;

	return env;
}

void mark_env(env_t **env, visitor_t *visitor)
{
	ptr_t ptr;
	ptr_init(&ptr, *env);
	obj_t tmp = { .ptr = ptr.ptr };
	printf("env mem %p, %p, %p\n", *env, ptr_get(&ptr), ptr_from_obj(tmp));
	visitor->visit(visitor, &tmp);
	ptr.ptr = tmp.ptr;
	*env = ptr_get(&ptr);
	printf("new mem %p\n", *env);
}

func_t* load_func(module_t *module, int index)
{
	if (index > module->fun_count)
		FATAL("index %d out of range\n", index);
	return &module->functions[index];
}

#if 0
frame_t* frame_create(func_t *func, vm_thread_t *thread)
{
	size_t size = sizeof(obj_t)*func->stack_size + sizeof(frame_t);

	void *mem = lalloc_get(&thread->lalloc, size);
	frame_t *frame = mem;
	frame->prev = thread->fp;
	frame->opstack = mem+sizeof(frame_t);
	frame->func = func;
	frame->opcode = func->opcode;
	frame->op_stack_idx = 0;
	frame->env = env_new(&thread->heap, func->env_size);

	int depth = 0;
	if (thread->fp) {
		if (thread->fp->func == func)
			frame->display = thread->fp->display;
		else {
			depth = thread->fp->depth + 1;
			frame->display = lalloc_get(&thread->lalloc, depth * sizeof(env_t*));
			frame->display[0] = thread->fp->env;
			if (depth > 1)
				memcpy(&frame->display[1], thread->fp->display, thread->fp->depth * sizeof(env_t*));
		}
	}
	frame->depth = depth;

	return frame;
}
#endif

void eval_thread(vm_thread_t *thread, module_t *module)
{
	char *opcode;
	func_t *func;

	func = load_func(module, module->entry_point);
	opcode = func->opcode;
	thread->env = env_new(&thread->heap, func->env_size);

#define STACK_PUSH(n) thread->opstack[thread->op_stack_idx++].ptr = n
#define STACK_POP() thread->opstack[--thread->op_stack_idx]
#define STACK_HEAD() thread->opstack[thread->op_stack_idx-1]

	/*
	 * On dispatching speed-up:
	 * http://gcc.gnu.org/onlinedocs/gcc/Labels-as-Values.html
	 * Inspired by http://bugs.python.org/issue4753, thanks Antoine Pitrou!
	 */

#ifdef COMPUTED_GOTO
#include "opcode_targets.h"
#define TARGET(op) \
	TARGET_##op: \
	op_code = *(opcode++); \
	op_arg = *(opcode++); \
	printf("\t%s : %d\n", opcode_name(op_code), op_arg);
#define NEXT() goto *opcode_targets[(int)*opcode]
#define DISPATCH() NEXT();
#else
#define TARGET(op) case op:\
	printf("\t%s : %d\n", opcode_name(op_code), op_arg);
#define NEXT() continue
#define DISPATCH() \
	op_code = *(opcode++); \
	op_arg = *(opcode++); \
	switch (op_code)
#endif

	int op_code, op_arg;
	for (;;) {
		DISPATCH() {
			TARGET(LOAD_LOCAL)
				STACK_PUSH(thread->env->objects[op_arg].ptr);
			NEXT();

			TARGET(LOAD_SYM) 
				STACK_PUSH(func->module->symbols[op_arg].ptr);
			NEXT();

			TARGET(LOAD_IMPORT)
				STACK_PUSH(func->module->imports[op_arg].ptr);
			NEXT();

			TARGET(JUMP_IF_FALSE)
				if (is_false(STACK_HEAD()))
					opcode += op_arg*2;
			NEXT();

			TARGET(JUMP_IF_TRUE)
				if (!is_false(STACK_HEAD()))
					opcode += op_arg*2;
			NEXT();

			TARGET(JUMP_FORWARD)
				opcode += op_arg*2;
			NEXT();

			TARGET(LOAD_FUNC) 
				STACK_PUSH(make_ptr(load_func(func->module, op_arg), id_func));
			NEXT();

			TARGET(FUNC_CALL) {
				static ptr_t fp;
				static void *ptr;
				static void *args;
				fp.ptr = STACK_POP().ptr;
dispatch_func:
				if (fp.tag != id_func)
					FATAL("expected function but got tag %d\n", fp.tag);
				ptr = ptr_get(&fp);

				switch (*((func_type_t*)ptr)) {
					case func_inter: 
						{
							if (func != ptr) {
								thread->depth++;
								void *dptr = thread->display;
								dptr -= sizeof(env_t);
								thread->display = dptr;
								thread->display[0] = thread->env;
							}

							func = ptr;
							opcode = func->opcode;
							thread->env = env_new(&thread->heap, func->env_size);
							args = &thread->opstack[thread->op_stack_idx - op_arg];
							memcpy(thread->env->objects, args, op_arg*sizeof(obj_t));
							thread->op_stack_idx = 0;
						}
						NEXT();
					case func_native: 
						{
							static trampoline_t tramp;
							static native_t *func;
							func = ptr;

							if (func->swallow) {
								if (op_arg < func->argc)
									FATAL("%s need minimum %d arguments, but got %d",
											func->name, func->argc, op_arg);
							} else {
								if (op_arg != func->argc)
									FATAL("try to pass %d args when %d requred by %s\n", op_arg, func->argc, func->name);
							}

							thread->op_stack_idx -= op_arg;
							args = &thread->opstack[thread->op_stack_idx];
							func->call(&thread->heap, &tramp, args, op_arg);

							STACK_PUSH(tramp.arg.ptr);
							fp = tramp.func;
							op_arg = 1;
							goto dispatch_func;
						}
						break;
					default:
						FATAL("BUG! Expected function but got type tag: %d\n", fp.tag);
				}
			}
			NEXT();

			TARGET(SET_LOCAL)
				thread->env->objects[op_arg] = STACK_POP();
			NEXT();

			TARGET(LOAD_ENV)
				STACK_PUSH(thread->display[op_arg-1]);
			NEXT();

			TARGET(LOAD_FROM_ENV) {
				env_t *env = STACK_POP().ptr;
				STACK_PUSH(env->objects[op_arg].ptr);
			}
			NEXT();
		}
	}
}

void* make_symbol(hash_table_t *tbl, const char *str)
{
	void *res = hash_table_lookup(tbl, str);
	if (!res) {
		res = strdup(str);
		hash_table_insert(tbl, res, res);
	}
	ptr_t sp;
	symbol_init(sp, res);
	return sp.ptr;
}

module_t* module_load(vm_thread_t *thread, const char *path)
{
	module_t *mod;

	void populate_sym_table(const char *str)
	{
		int i;
		int count = *(str++);
		mod->symbols = mem_calloc(count, sizeof(obj_t));
		for (i = 0; i < count; i++) {
			int len = *(str++);
			void *sym = make_symbol(&thread->sym_table, str);
			mod->symbols[i].ptr = sym;
			printf("Created symbol for '%s' = %p\n", str, sym);
			str += len+1;
		}
	}

	void load_imports(const char *str)
	{
		int i;
		int count = *(str++);
		mod->imports = mem_calloc(count, sizeof(obj_t));
		for (i = 0; i < count; i++) {
			int len = *(str++);
			void *ptr = hash_table_lookup(&thread->ns_global, str);
			if (ptr)
				mod->imports[i].ptr = ptr;
			else
				FATAL("variable %s not found\n", str);
			str += len+1;
		}
	}

	int fd = open(path, O_RDONLY);
	if (fd == -1)
		FATAL("nothing to load\n");

	mod = type_alloc(module_t);

	/* Read module header */
	struct module_hdr_s mhdr;
	if (read(fd, &mhdr, MODULE_HDR_OFFSET) != MODULE_HDR_OFFSET)
		FATAL("Failed to read module header\n");

	/* Allocate functions storage */
	mod->functions = mem_calloc(mhdr.fun_count, sizeof(func_t));
	mod->fun_count = mhdr.fun_count;
	mod->entry_point = mhdr.entry_point;


	char *import = mem_alloc(mhdr.import_size);
	if (read(fd, import, mhdr.import_size) != mhdr.import_size)
		FATAL("Failed to read imports");
	load_imports(import);
	mem_free(import);

	char *symbols = mem_alloc(mhdr.symbols_size);
	if (read(fd, symbols, mhdr.symbols_size) != mhdr.symbols_size)
		FATAL("Failed to read symbols");
	populate_sym_table(symbols);
	mem_free(symbols);

	int count;
	struct func_hdr_s hdr;
	for (count = 0; count < mhdr.fun_count; count++) {
		if (read(fd, &hdr, FUN_HDR_SIZE) != FUN_HDR_SIZE)
			FATAL("Failed to read func header\n");
		func_t *func = &mod->functions[count];
		func->type = func_inter;
		func->env_size = hdr.env_size;
		func->argc = hdr.argc;
		func->stack_size = hdr.stack_size;
		func->op_count = hdr.op_count;

		int opcode_size = hdr.op_count * 2;
		func->opcode = mem_alloc(opcode_size);
		if (read(fd, func->opcode, opcode_size) != opcode_size)
			FATAL("Failed to read opcode");
		func->module = mod;
	}

	close(fd);
	return mod;
}

void module_free(module_t *module)
{
	int i;
	for (i = 0; i < module->fun_count; i++)
		mem_free(module->functions[i].opcode);
	mem_free(module->functions);
	mem_free(module->symbols);
	mem_free(module->imports);
	mem_free(module);
}

static void vm_inspect(visitor_t *visitor, void *self)
{
	vm_thread_t *thread = self;

	int i;

	mark_env(&thread->env, visitor);

	for (i = 0; i < thread->depth; i++)
		mark_env(&thread->display[i], visitor);

	for (i = 0; i < thread->op_stack_idx; i++)
		visitor->visit(visitor, &thread->opstack[i]);
}

void vm_thread_init(vm_thread_t *thread)
{
	memset(thread, 0, sizeof(*thread));

	heap_init(&thread->heap, vm_inspect, thread);

	thread->ssize = sysconf(_SC_PAGE_SIZE);
	void *smem = mmap(NULL, thread->ssize,
			PROT_READ|PROT_WRITE, MAP_PRIVATE|MAP_ANONYMOUS, 0, 0);
	thread->opstack = smem;
	thread->display = smem+thread->ssize;


	hash_table_init(&thread->sym_table, string_hash, string_equal);
	thread->sym_table.destroy_key = free;
	hash_table_init(&thread->ns_global, string_hash, string_equal);

	ns_install_primitives(&thread->ns_global);
}

void vm_thread_destroy(vm_thread_t *thread)
{
	hash_table_destroy(&thread->sym_table);
	hash_table_destroy(&thread->ns_global);
	heap_destroy(&thread->heap);
	munmap(thread->opstack, thread->ssize);
}

int main()
{
	vm_thread_t thread;
	vm_thread_init(&thread);

	module_t *mod = module_load(&thread, "/tmp/assembly");

	eval_thread(&thread, mod);

	vm_thread_destroy(&thread);
	module_free(mod);
	return 0;
}
