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
#include <stdlib.h>
#include <stdio.h>

#include "opcodes.h"
#include "primitives.h"
#include "vm_private.h"
#include "module.h"

static void env_visit(visitor_t *vs, void *data);
static void display_visit(visitor_t *vs, void *data);
static void closure_visit(visitor_t *vs, void *data);

type_t type_table[] = {
	{ .name = "env", .visit = env_visit },
	{ .name = "closure", .visit = closure_visit },
	{ .name = "display", .visit = display_visit },
	{ .name = "pair", .visit = pair_visit, .repr = pair_repr }
};


static void env_visit(visitor_t *vs, void *data)
{
	env_t *env = data;
	int i;
	for (i = 0; i < env->size; i++)
		vs->visit(vs, &env->objects[i]);
}

env_t* env_new(heap_t *heap, int size)
{
	void *mem = heap_alloc0(heap, sizeof(env_t)+sizeof(obj_t)*size);
	env_t *env = mem;
	env->objects = mem+sizeof(env_t);
	env->size = size;
	env->hdr.type_id = t_env;

	return env;
}

static void mark_env(env_t **env, visitor_t *visitor)
{
	if (!*env)
		return;
	ptr_t ptr;
	PTR_INIT(ptr, *env);
	visitor->visit(visitor, &ptr.obj);
	*env = PTR_GET(ptr);
}

static void mark_display(display_t **display, visitor_t *visitor)
{
	ptr_t ptr;
	PTR_INIT(ptr, *display);
	visitor->visit(visitor, &ptr.obj);
	*display = PTR_GET(ptr);
}

static void display_visit(visitor_t *vs, void *data)
{
	display_t *display = data;
	if (display->has_env) {
		void *emem = display;
		emem += sizeof(display_t);
		env_t **env = emem;
		mark_env(env, vs);
	}
	mark_display(&display->prev, vs);
}

static display_t* display_new(heap_t *heap, display_t *prev, env_t **env)
{
	int dsize = sizeof(display_t);
	if (env)
		dsize += sizeof(env_t*);

	void *mem = heap_alloc(heap, dsize);
	display_t *display = mem;
	display->prev = prev;
	display->hdr.type_id = t_display;
	display->depth = prev ? prev->depth+1 : 0;

	if (*env) {
		env_t **ep = mem+sizeof(display_t);
		*ep = *env;
		display->has_env = 1;
	} else
		display->has_env = 0;

	return display;
}

static env_t* display_env(display_t *display, int idx)
{
	while (idx--) 
		display = display->prev;
	if (!display->has_env)
		FATAL("display %p with depth %d doesn't has an env\n", display, display->depth);
	
	void *emem = display;
	emem += sizeof(display_t);
	env_t **env = emem;
	return *env;
}

static void closure_visit(visitor_t *vs, void *data)
{
	closure_t *closure = data;
	mark_display(&closure->display, vs);
}

void* closure_new(heap_t *heap, func_t *func, display_t **display)
{
	closure_t *closure = heap_alloc(heap, sizeof(closure_t));
	closure->hdr.type_id = t_closure;
	closure->func = func;
	closure->display = *display;

	return make_ptr(closure, id_ptr);
}

void eval_thread(vm_thread_t *thread, module_t *module)
{
	char *opcode;
	func_t *func;
	int i;

#define MODULE_FUNC(module, idx) &(module)->functions[idx]

	func = MODULE_FUNC(module, module->entry_point);
	thread->func = func;
	opcode = func->opcode;
	if (func->heap_env) {
		thread->env = env_new(&thread->heap, func->env_size);
		thread->objects = thread->env->objects;
		thread->display = display_new(&thread->heap, NULL, &thread->env);
	} else {
		thread->env = NULL;
		thread->objects = thread->opstack;
		thread->op_stack_idx = func->env_size;
	}

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
	DBG("\t%s : %d\n", opcode_name(op_code), op_arg);
#define NEXT() goto *opcode_targets[(int)*opcode]
#define DISPATCH() NEXT();
#else
#define TARGET(op) case op:\
	DBG("\t%s : %d\n", opcode_name(op_code), op_arg);
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
				STACK_PUSH(thread->objects[op_arg].ptr);
			NEXT();

			TARGET(LOAD_SYM) 
				STACK_PUSH(func->module->symbols[op_arg].ptr);
			NEXT();

			TARGET(LOAD_IMPORT)
				STACK_PUSH(func->module->imports[op_arg].ptr);
			NEXT();

			TARGET(JUMP_IF_FALSE)
				if (IS_FALSE(STACK_POP()))
					opcode += op_arg*2;
			NEXT();

			TARGET(JUMP_IF_TRUE)
				if (!IS_FALSE(STACK_POP()))
					opcode += op_arg*2;
			NEXT();

			TARGET(JUMP_FORWARD)
				opcode += op_arg*2;
			NEXT();

			TARGET(LOAD_FUNC) 
				STACK_PUSH(make_ptr(MODULE_FUNC(func->module, op_arg), id_func));
			NEXT();

			TARGET(FUNC_CALL) {
				ptr_t fp;
				void *ptr;
				void *args;
				fp.obj = STACK_POP();
dispatch_func:
				if (fp.tag != id_func && fp.tag != id_ptr && fp.tag != id_cont)
					FATAL("expected function or closure but got tag %d\n", fp.tag);

				if (fp.tag == id_ptr) {
					closure_t *closure = get_typed(fp.obj, t_closure);
					if (!closure)
						FATAL("got pointer but it isn't a closure\n");

					ptr = closure->func;
					thread->display = closure->display;
					goto call_inter;
				} else if (fp.tag == id_cont) {
					op_arg--;
					ptr = PTR_GET(fp);
				} else {
					ptr = PTR_GET(fp);
				}

				switch (FUNC_TYPE(ptr)) {
					case func_inter: 
						{
call_inter:
							func = ptr;
							thread->func = func;
							if (op_arg != func->argc)
								FATAL("try to pass %d args when %d requred\n", op_arg, func->argc);

							if (fp.tag != id_ptr && thread->display) {
								while (func->depth-1 < thread->display->depth)
									thread->display = thread->display->prev;
							}

							opcode = func->opcode;
							if (func->heap_env) {
								thread->env = env_new(&thread->heap, func->env_size);
								thread->objects = thread->env->objects;
								args = &thread->opstack[thread->op_stack_idx - op_arg];
								thread->op_stack_idx = 0;
								memcpy(thread->objects, args, op_arg*sizeof(obj_t));
							} else {
								thread->env = NULL;
								thread->objects = &thread->opstack[thread->op_stack_idx - op_arg];
							}

							if (func->bcount) {
								thread->bindmap = (void*)&thread->opstack[thread->op_stack_idx];
								for (i = 0; i < func->bmcount; i++) {
									env_t *env = display_env(thread->display, (func->bindmap[i]-1));
									if (!env)
										FATAL("null env\n");
									STACK_PUSH(make_ptr(env, id_ptr));
								}
							}

							thread->display = display_new(&thread->heap, thread->display, &thread->env);
						}
						NEXT();
					case func_native: 
						{
							trampoline_t tramp;
							native_t *func;
							func = ptr;

							if (func->swallow) {
								if (op_arg < func->argc)
									FATAL("%s need minimum %d arguments, but got %d",
											func->name, func->argc, op_arg);
							} else {
								if (op_arg != func->argc)
									FATAL("try to pass %d args when %d requred by %s\n", op_arg, func->argc, func->name);
							}

							DBG("calling native %s\n", func->name);
							obj_t *argv = &thread->opstack[thread->op_stack_idx - op_arg];
							tramp.argc = 1;
							tramp.func.obj = argv[0];
							switch (func->call(&thread->heap, &tramp, argv, op_arg)) {
								case RC_EXIT:
									/* Terminate thread */
									heap_stat(&thread->heap);
									return;
								case RC_ERROR:
								case RC_OK:
								default:
									break;
							}

							thread->op_stack_idx = 0;
							op_arg = 0;
							for (i = 0; i < tramp.argc; i++) {
								STACK_PUSH(tramp.arg[i].ptr);
								op_arg++;
							}
							fp = tramp.func;
							goto dispatch_func;
						}
						break;
					default:
						FATAL("BUG! Expected function but got type tag: %d\n", fp.tag);
				}
			}
			NEXT();

			TARGET(SET_LOCAL)
				thread->objects[op_arg] = STACK_POP();
			NEXT();

			TARGET(SET_BIND)
			{
				bind_t *bind = &func->bindings[op_arg];
				env_t *env = ENV(thread->bindmap[bind->up]);
				env->objects[bind->idx] = STACK_POP();
			}
			NEXT();

			TARGET(LOAD_BIND)
			{
				bind_t *bind = &func->bindings[op_arg];
				env_t *env = ENV(thread->bindmap[bind->up]);
				STACK_PUSH(env->objects[bind->idx].ptr);
			}
			NEXT();

			TARGET(LOAD_CLOSURE) {
				func_t *nf = MODULE_FUNC(func->module, op_arg);
				STACK_PUSH(closure_new(&thread->heap,
							nf, &thread->display));
			}
			NEXT();

			TARGET(PUSH_BOOL)
				STACK_PUSH(CIF(op_arg).ptr);
			NEXT();

			TARGET(PUSH_FIXNUM) {
				fixnum_t n;
				FIXNUM_INIT(n, op_arg);
				STACK_PUSH(n.ptr);
			}
			NEXT();

			TARGET(LOAD_CONST)
				STACK_PUSH(cnull.ptr);
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
	return make_ptr(res, id_symbol);
}


static void vm_inspect(visitor_t *visitor, void *self)
{
	vm_thread_t *thread = self;

	int i;

	if (thread->env)
		mark_env(&thread->env, visitor);

	for (i = 0; i < thread->op_stack_idx; i++)
		visitor->visit(visitor, &thread->opstack[i]);

	mark_display(&thread->display, visitor);
}

void vm_thread_init(vm_thread_t *thread)
{
	memset(thread, 0, sizeof(*thread));

	heap_init(&thread->heap, vm_inspect, thread);

	//thread->ssize = sysconf(_SC_PAGE_SIZE);
	thread->ssize = 1024;
	void *smem = mmap(NULL, thread->ssize,
			PROT_READ|PROT_WRITE, MAP_PRIVATE|MAP_ANONYMOUS, 0, 0);
	thread->opstack = smem;

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
