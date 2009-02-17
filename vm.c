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

func_t* load_func(module_t *module, int index)
{
	if (index > module->fun_count)
		FATAL("index %d out of range\n", index);
	return &module->functions[index];
}

static lpage_t* lalloc_new_page(lpage_t *prev)
{
	size_t ps = sysconf(_SC_PAGE_SIZE);
	void *page = mmap(NULL, ps,
			PROT_READ|PROT_WRITE, MAP_PRIVATE|MAP_ANONYMOUS, 0, 0);
	lpage_t *lp = page;
	lp->start = page+sizeof(lpage_t);
	lp->pos = lp->start;
	lp->end = page+ps;

	return lp;
}

void lalloc_init(lalloc_t *al)
{
	al->page = lalloc_new_page(NULL);
}

void lalloc_destroy(lalloc_t *al)
{
	lpage_t *cur_page = al->page;
	lpage_t *next;
	while (cur_page) {
		next = cur_page->prev;
		munmap(cur_page, sysconf(_SC_PAGE_SIZE));
		cur_page = next;
	}
}

void* lalloc_get(lalloc_t *al, size_t size)
{
	if (al->page->pos + size > al->page->end) {
		al->page = lalloc_new_page(al->page);
	}
	void *res = al->page->pos;
	al->page->pos += size;
	return res;
}

void lalloc_put(lalloc_t *al, void *ptr)
{
	if (ptr < al->page->start || ptr > al->page->end)
		FATAL("Try to put %p which does not belong to linear allocator %p\n",
				ptr, al);
	al->page->pos = ptr;
	if (al->page->prev && al->page->pos == al->page->start) {
		lpage_t *tmp = al->page;
		al->page = tmp->prev;
		munmap(tmp, sysconf(_SC_PAGE_SIZE));
	}
}

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

void frame_destroy(vm_thread_t *thread, frame_t *frame)
{
	lalloc_put(&thread->lalloc, frame);
}

void eval_thread(vm_thread_t *thread, module_t *module)
{
	thread->fp = frame_create(
			load_func(module, module->entry_point), thread);
	frame_t *frame = thread->fp;

#define STACK_PUSH_ON(frame, n) frame->opstack[frame->op_stack_idx++].ptr = n
#define STACK_PUSH(n) STACK_PUSH_ON(frame, n)
#define STACK_POP() frame->opstack[--frame->op_stack_idx]
#define STACK_HEAD() frame->opstack[frame->op_stack_idx-1]

	/*
	 * On dispatching speed-up:
	 * http://gcc.gnu.org/onlinedocs/gcc/Labels-as-Values.html
	 * Inspired by http://bugs.python.org/issue4753, thanks Antoine Pitrou!
	 */

#ifdef COMPUTED_GOTO
#include "opcode_targets.h"
#define TARGET(op) \
	TARGET_##op: \
	op_code = *(frame->opcode++); \
	op_arg = *(frame->opcode++); \
	printf("\t%s : %d\n", opcode_name(op_code), op_arg);
#define NEXT() goto *opcode_targets[(int)*frame->opcode]
#define DISPATCH() NEXT();
#else
#define TARGET(op) case op:\
	printf("\t%s : %d\n", opcode_name(op_code), op_arg);
#define NEXT() continue
#define DISPATCH() \
	op_code = *(frame->opcode++); \
	op_arg = *(frame->opcode++); \
	switch (op_code)
#endif

	int op_code, op_arg;
	for (;;) {
		DISPATCH() {
			TARGET(LOAD_LOCAL)
				STACK_PUSH(frame->env->objects[op_arg].ptr);
			NEXT();

			TARGET(LOAD_SYM) 
				STACK_PUSH(frame->func->module->symbols[op_arg].ptr);
			NEXT();

			TARGET(LOAD_IMPORT)
				STACK_PUSH(frame->func->module->imports[op_arg].ptr);
			NEXT();

			TARGET(JUMP_IF_FALSE)
				if (is_false(STACK_HEAD()))
					frame->opcode += op_arg*2;
			NEXT();

			TARGET(JUMP_IF_TRUE)
				if (!is_false(STACK_HEAD()))
					frame->opcode += op_arg*2;
			NEXT();

			TARGET(JUMP_FORWARD)
				frame->opcode += op_arg*2;
			NEXT();

			TARGET(LOAD_FUNC) 
				STACK_PUSH(make_ptr(load_func(frame->func->module, op_arg), id_func));
			NEXT();

			TARGET(FUNC_CALL) {
				ptr_t fp = { .ptr = STACK_POP().ptr };
				if (fp.tag != id_func)
					FATAL("expected function but got tag %d\n", fp.tag);
				void *ptr = ptr_get(&fp);
				switch (*((func_type_t*)ptr)) {
					case func_inter: 
						{
							func_t *func = ptr;
							if (func->argc != op_arg)
								FATAL("try to pass %d args when %d requred\n", op_arg, func->argc);
							frame_t *new_frame = frame_create(func, thread);
							thread->fp = new_frame;

							frame->op_stack_idx -= op_arg;
							memcpy(new_frame->env->objects,
									&frame->opstack[frame->op_stack_idx], op_arg*sizeof(obj_t));
							frame = new_frame;

							NEXT();
						}
					case func_native: 
						{
							native_t *func = ptr;
							if (func->swallow) {
								if (op_arg < func->argc)
									FATAL("%s need minimum %d arguments, but got %d",
											func->name, func->argc, op_arg);
							} else {
								if (op_arg != func->argc)
									FATAL("try to pass %d args when %d requred\n", op_arg, func->argc);
							}

							frame->op_stack_idx -= op_arg;
							STACK_PUSH(func->call(&thread->heap,
										&frame->opstack[frame->op_stack_idx], op_arg));
						}
						break;
					default:
						FATAL("Expected function but got type tag: %d\n", fp.tag);
				}
			}
			NEXT();

			TARGET(RETURN) {
				obj_t ret = STACK_POP();
				frame_t *parent = frame->prev;
				thread->fp = parent;
				frame_destroy(thread, frame);
				if (parent) {
					STACK_PUSH_ON(parent, ret.ptr);
					frame = parent;
				} else {
					print_obj(ret);
					return;
				}
			}
			NEXT();

			TARGET(SET_LOCAL)
				frame->env->objects[op_arg] = STACK_POP();
			NEXT();

			TARGET(LOAD_ENV)
				STACK_PUSH(frame->display[op_arg-1]);
			NEXT();

			TARGET(LOAD_FROM_ENV)
			{
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
	read(fd, import, mhdr.import_size);
	load_imports(import);
	mem_free(import);

	char *symbols = mem_alloc(mhdr.symbols_size);
	read(fd, symbols, mhdr.symbols_size);
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

static void mark_env(env_t **env, visitor_t *visitor)
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

static void vm_inspect(visitor_t *visitor, void *self)
{
	vm_thread_t *thread = self;

	frame_t *cur_frame = thread->fp;
	while (cur_frame) {
		int i;

		mark_env(&cur_frame->env, visitor);

		for (i = 0; i < cur_frame->depth; i++)
			mark_env(&cur_frame->display[i], visitor);

		for (i = 0; i < cur_frame->op_stack_idx; i++)
			visitor->visit(visitor, &cur_frame->opstack[i]);

		cur_frame = cur_frame->prev;
	}
}

void vm_thread_init(vm_thread_t *thread)
{
	memset(thread, 0, sizeof(*thread));
	thread->fp = NULL;

	heap_init(&thread->heap, vm_inspect, thread);
	lalloc_init(&thread->lalloc);

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
	lalloc_destroy(&thread->lalloc);
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
