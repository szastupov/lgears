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

#include "memory.h"
#include "bytecode.h"
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

void* symbol_get(hash_table_t *tbl, const char *str)
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

func_t* load_func(module_t *module, int index)
{
	if (index > module->fun_count)
		FATAL("index %d out of range\n", index);
	return &module->functions[index];
}

frame_t* frame_create(func_t *func, frame_t *parent, heap_t *heap)
{
	frame_t *frame = type_alloc(frame_t);
	frame->prev = parent;
	frame->opstack = mem_calloc(func->stack_size, sizeof(obj_t));
	frame->func = func;
	frame->env = env_new(heap, func->env_size);
	return frame;
}

void frame_destroy(frame_t *frame)
{
	mem_free(frame->opstack);
	mem_free(frame);
}

void print_obj(obj_t obj)
{
	switch (obj.tag) {
	case id_ptr:
		printf("ptr: %p\n", ptr_from_obj(obj));
		break;
	case id_fixnum:
		printf("fixnum: %d\n", fixnum_from_obj(obj));
		break;
	case id_bool:
		printf("bool: #%c\n", bool_from_obj(obj) ? 't' : 'f');
		break;
	case id_char:
		printf("char: %c\n", char_from_obj(obj));
		break;
	case id_func:
		printf("func: %p\n", ptr_from_obj(obj));
		break;
	case id_symbol:
		printf("symbol: %s\n", (const char*)ptr_from_obj(obj));
		break;
	default:
		printf("unknown obj\n");
	}
}

static void* display(obj_t *argv)
{
	print_obj(argv[0]);
	return NULL;
}
MAKE_NATIVE(display, 1);

void eval_thread(vm_thread_t *thread, module_t *module)
{
	thread->frame_stack = frame_create(
			load_func(module, module->entry_point),
			thread->frame_stack,
			&thread->heap);
	frame_t *frame = thread->frame_stack;


#define STACK_PUSH_ON(frame, n) frame->opstack[frame->op_stack_idx++].ptr = n
#define STACK_PUSH(n) STACK_PUSH_ON(frame, n)
#define STACK_POP() frame->opstack[--frame->op_stack_idx]
#define STACK_HEAD() frame->opstack[frame->op_stack_idx-1]

	int op_code, op_arg;
	char *code;
	while (frame->step < frame->func->op_count) {
next_cmd:
		code = frame->func->opcode+(frame->step*2);
		op_code = code[0];
		op_arg = code[1];
		printf("%d\t%s : %d\n",
				frame->step, opcode_name(op_code), op_arg);

		switch (op_code) {
		case LOAD_LOCAL:
			STACK_PUSH(frame->env->objects[op_arg].ptr);
			break;

		case LOAD_SYM:
			{
				void *sym_ptr = frame->func->module->symbols[op_arg].ptr;
				STACK_PUSH(sym_ptr);
			}
			break;

		case LOAD_IMPORT:
			{
				ptr_t ptr;
				native_init(ptr, &display_nt); // FIXME
				STACK_PUSH(ptr.ptr);
			}
			break;

		case JUMP_IF_FALSE:
			if (is_false(STACK_HEAD())) {
				frame->step = op_arg;
				printf("jumping to %d\n", op_arg);
				goto next_cmd;
			}
			break;

		case JUMP_IF_TRUE:
			if (!is_false(STACK_HEAD())) {
				frame->step = op_arg;
				printf("jumping to %d\n", op_arg);
				goto next_cmd;
			}
			break;

		case JUMP_FORWARD:
			frame->step += op_arg;
			printf("jumping to %d\n", frame->step);
			goto next_cmd;

		case LOAD_FUNC:
			{
				func_t *func = load_func(frame->func->module, op_arg);
				printf("loaded func %p\n", func);
				ptr_t fp;
				func_init(fp, func);
				STACK_PUSH(fp.ptr);
			}
			break;

		case FUNC_CALL:
			{
				obj_t obj = STACK_POP();
				ptr_t fp = { .ptr = obj.ptr };
					int i;
				switch (obj.tag) {
				case id_func:
					{
						func_t *func = ptr_get(&fp);
						if (func->argc != op_arg)
							FATAL("try to pass %d args when %d requred\n", op_arg, func->argc);
						frame_t *new_frame = frame_create(func, frame, &thread->heap);
						thread->frame_stack = new_frame;

						for (i = 0; i < func->argc; i++)
							new_frame->env->objects[i] = STACK_POP();
						frame = new_frame;

						goto next_cmd;
					}
					break;
				case id_native:
					{
						native_t *func = ptr_get(&fp);
						if (func->argc != op_arg)
							FATAL("try to pass %d args when %d requred\n", op_arg, func->argc);
						obj_t *argv = mem_calloc(func->argc, sizeof(obj_t));
						for (i = 0; i < func->argc; i++)
							argv[i] = STACK_POP();
						func->call(argv);
						mem_free(argv);
					}
					break;
				default:
					FATAL("Expected function but got type tag: %d\n", obj.tag);
				}
			}
			break;

		case RETURN:
			{
				obj_t ret = STACK_POP();
				frame_t *parent = frame->prev;
				thread->frame_stack = parent;
				frame_destroy(frame);
				if (parent) {
					STACK_PUSH_ON(parent, ret.ptr);
					frame = parent;
				} else {
					print_obj(ret);
					return;
				}
			}
			break;

		default:
			FATAL("Unhandled opcode %s\n", opcode_name(op_code));
		}

		frame->step++;
	}
}

module_t* module_load(vm_thread_t *thread, const char *path)
{
	module_t *mod;

	void populate_sym_table(const char *str, int size)
	{
		int i;
		const char *end = str+size;
		mod->symbols = mem_calloc(*(str++), sizeof(obj_t));
		while (str < end) {
			int len = *(str++);
			void *sym = symbol_get(&thread->sym_table, str);
			mod->symbols[i++].ptr = sym;
			printf("Created symbol for '%s' = %p\n", str, sym);
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
	//mod->entry_point = mhdr.entry_point;
	mod->entry_point = 1; //FIXME


	char *import = mem_alloc(mhdr.import_size);
	read(fd, import, mhdr.import_size);
	//	print_strings(import, mhdr.import_size);
	mem_free(import);

	char *symbols = mem_alloc(mhdr.symbols_size);
	read(fd, symbols, mhdr.symbols_size);
	populate_sym_table(symbols, mhdr.symbols_size);
	mem_free(symbols);

	int count;
	struct func_hdr_s hdr;
	for (count = 0; count < mhdr.fun_count; count++) {
		if (read(fd, &hdr, FUN_HDR_SIZE) != FUN_HDR_SIZE)
			FATAL("Failed to read func header\n");
		func_t *func = &mod->functions[count];
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
	mem_free(module);
}

static void vm_inspect(visitor_t *visitor, void *self)
{
	vm_thread_t *thread = self;

	frame_t *cur_frame = thread->frame_stack;
	while (cur_frame) {
		int i;

		/*
		 * We need to update env pointer if needed
		 * FIXME cleanup this ugly code
		 */
		ptr_t ptr;
		ptr_init(&ptr, &cur_frame->env);
		obj_t tmp = { .ptr = ptr.ptr };
		visitor->visit(visitor, &tmp);
		ptr.ptr = tmp.ptr;
		cur_frame->env = ptr_get(&ptr);

		for (i = 0; i < cur_frame->op_stack_idx; i++)
			visitor->visit(visitor, &cur_frame->opstack[i]);
		cur_frame = cur_frame->prev;
	}
}

void vm_thread_init(vm_thread_t *thread)
{
	thread->frame_stack = NULL;
	heap_init(&thread->heap, vm_inspect, thread);
	hash_table_init(&thread->sym_table, string_hash, string_equal);
}

void vm_thread_destroy(vm_thread_t *thread)
{
	hash_table_destroy(&thread->sym_table);
	heap_destroy(&thread->heap);
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
