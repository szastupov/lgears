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
#include <pthread.h>
#include <sys/stat.h>
#include <string.h>
#include <errno.h>

#include "opcodes.h"
#include "primitives.h"
#include "vm.h"
#include "module.h"

static void env_visit(visitor_t *vs, void *data);
static void display_visit(visitor_t *vs, void *data);
static void closure_visit(visitor_t *vs, void *data);

const type_t type_table[] = {
	{ .name = "env", .visit = env_visit },
	{ .name = "closure", .visit = closure_visit },
	{ .name = "display", .visit = display_visit },
	{ .name = "pair", .visit = pair_visit, .repr = pair_repr },
	{ .name = "string", .repr = string_repr }
};

hash_table_t ns_global;
hash_table_t sym_table;

static void env_visit(visitor_t *vs, void *data)
{
	env_t *env = data;
	env->objects = data+sizeof(env_t);
	int i;
	for (i = 0; i < env->size; i++)
		vs->visit(vs, &env->objects[i]);
}

env_t* env_new(heap_t *heap, int size)
{
	void *mem = heap_alloc0(heap, sizeof(env_t)+sizeof(obj_t)*size, t_env);
	env_t *env = mem;
	env->objects = mem+sizeof(env_t);
	env->size = size;
	LOG_DBG("New env %p\n", env);

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

static display_t* display_new(heap_t *heap, display_t **prev, env_t **env)
{
	int dsize = sizeof(display_t);
	if (env)
		dsize += sizeof(env_t*);

	void *mem = heap_alloc(heap, dsize, t_display);
	display_t *display = mem;
	if (prev) {
		display->prev = *prev;
		display->depth = *prev ? (*prev)->depth+1 : 0;
	} else {
		display->prev = NULL;
		display->depth = 0;
	}

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
	if (!display->has_env) {
		LOG_ERR("display %p with depth %d doesn't has an env\n",
				display, display->depth);
		return NULL;
	}
	
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

static void* closure_new(heap_t *heap, func_t *func, display_t **display)
{
	closure_t *closure = heap_alloc(heap, sizeof(closure_t), t_closure);
	closure->func = func;
	closure->display = *display;

	return make_ptr(closure, id_ptr);
}

static void enter_interp(vm_thread_t *thread, func_t *func, int op_arg, int tag)
{
	thread->func = func;
	int i;

	if (tag != id_ptr && thread->display) {
		while (func->depth-1 < thread->display->depth)
			thread->display = thread->display->prev;
	}

	if (func->heap_env) {
		thread->env = env_new(&thread->heap, func->env_size);
		thread->objects = thread->env->objects;
		if (op_arg) {
			void *args = &thread->opstack[thread->op_stack_idx - op_arg];
			thread->op_stack_idx = 0;
			memcpy(thread->objects, args, op_arg*sizeof(obj_t));
		}
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

	thread->display = display_new(&thread->heap, &thread->display, &thread->env);
}

static void eval_thread(vm_thread_t *thread, module_t *module)
{
	char *opcode;
	int op_code, op_arg;
	func_t *func;
	int i;
	void (*trace_func)();

	void trace_opcode()
	{
		LOG_DBG("\t%s : %d\n", opcode_name(op_code), op_arg);
	}

	void trace_opcode_sym()
	{
		LOG_DBG("\t%s : %d (%s)\n", opcode_name(op_code), op_arg,
				func->dbg_table[(opcode-func->opcode-2)/2]);
	}

	void set_trace_func()
	{
		if (func->dbg_symbols)
			trace_func = trace_opcode_sym;
		else
			trace_func = trace_opcode;
	}

#define MODULE_FUNC(module, idx) &(module)->functions[idx]
#define THREAD_ERROR(msg...) { \
	LOG_ERR(msg); \
	fprintf(stderr, "\tshutting down the thread...\n"); \
	return; \
}

	LOG_DBG("entering func %d\n", module->entry_point);
	func = MODULE_FUNC(module, module->entry_point);
	thread->func = func;
	opcode = func->opcode;
	thread->env = env_new(&thread->heap, func->env_size);
	thread->objects = thread->env->objects;
	thread->display = display_new(&thread->heap, NULL, &thread->env);

	set_trace_func();

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
	trace_func();
#define NEXT() goto *opcode_targets[(int)*opcode]
#define DISPATCH() NEXT();
#else
#define TARGET(op) case op:\
	trace_func();
#define NEXT() continue
#define DISPATCH() \
	op_code = *(opcode++); \
	op_arg = *(opcode++); \
	switch (op_code)
#endif

	for (;;) {
		DISPATCH() {
			TARGET(LOAD_LOCAL)
				STACK_PUSH(thread->objects[op_arg].ptr);
			NEXT();

			TARGET(LOAD_SYM) 
				STACK_PUSH(func->module->symbols[op_arg].ptr);
			NEXT();

			TARGET(LOAD_STRING) {
				string_t *str = _string(&thread->heap,
						func->module->strings[op_arg], 0);
				STACK_PUSH(str);
			}
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
				fp.obj = STACK_POP();
dispatch_func:
				switch (fp.tag) {
					case id_ptr:
						{
							closure_t *closure = get_typed(fp.obj, t_closure);
							if (!closure)
								THREAD_ERROR("got pointer but it isn't a closure\n");

							ptr = closure->func;
							thread->display = closure->display;
						}
						break;
					case id_cont:
						op_arg--;
						ptr = PTR_GET(fp);
						break;
					case id_func:
						ptr = PTR_GET(fp);
						break;
					default:
						THREAD_ERROR("expected function or closure but got tag %d\n", fp.tag);
				}

				func_hdr_t *fhdr = (func_hdr_t*)ptr;
				if (fhdr->swallow) {
					if (op_arg < fhdr->argc)
						THREAD_ERROR("try to pass %d args when at least %d requred\n", op_arg, fhdr->argc);
				} else {
					if (op_arg != fhdr->argc)
						THREAD_ERROR("try to pass %d args when %d requred\n", op_arg, fhdr->argc);
				}

				switch (fhdr->type) {
					case func_inter: 
						{
							func = ptr;
							opcode = func->opcode;
							enter_interp(thread, func, op_arg, fp.tag);
							set_trace_func();
						}
						NEXT();
					case func_native: 
						{
							native_t *func = ptr;

							obj_t *argv = &thread->opstack[thread->op_stack_idx - op_arg];
							thread->tramp.argc = 1;
							thread->tramp.func.ptr = NULL;
							switch (native_call(thread, func, argv, op_arg)) {
								case RC_EXIT:
									/* Terminate thread */
									heap_stat(&thread->heap);
									return;
								case RC_ERROR:
									/* C-api is only for low-level things,
									 * so no exceptions mechanism inside VM,
									 * check whatever you wand and throw
									 * exceptions from scheme, otherwise -
									 * thread will be terminated.
									 */
									fprintf(stderr, "%s failed, shutting down the thread...\n", func->name);
									return;
								case RC_OK:
								default:
									break;
							}

							if (thread->tramp.func.ptr)
								fp.obj = thread->tramp.func;
							else
								fp.obj = argv[0];

							thread->op_stack_idx = 0;
							for (i = 0; i < thread->tramp.argc; i++)
								STACK_PUSH(thread->tramp.arg[i].ptr);
							op_arg = thread->tramp.argc;

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
				LOG_DBG("BIND_SET %p\n", env);
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

static pthread_mutex_t symbol_mutex = PTHREAD_MUTEX_INITIALIZER;

void* make_symbol(hash_table_t *tbl, const char *str)
{
	pthread_mutex_lock(&symbol_mutex);
	void *res = hash_table_lookup(tbl, str);
	if (!res) {
		res = strdup(str);
		hash_table_insert(tbl, res, res);
	}
	pthread_mutex_unlock(&symbol_mutex);
	return make_ptr(res, id_symbol);
}

static void vm_get_roots(visitor_t *visitor, void *self)
{
	vm_thread_t *thread = self;
	int i;

	if (thread->env) {
		mark_env(&thread->env, visitor);
		thread->objects = thread->env->objects;
	}

	for (i = 0; i < thread->op_stack_idx; i++)
		visitor->visit(visitor, &thread->opstack[i]);

	if (!thread->env)
		for (i = 0; i < thread->func->env_size; i++)
			visitor->visit(visitor, &thread->objects[i]);

	mark_display(&thread->display, visitor);
}

static void vm_thread_init(vm_thread_t *thread)
{
	memset(thread, 0, sizeof(*thread));

	heap_init(&thread->heap, vm_get_roots, thread);

	thread->ssize = 1024;
	thread->opstack = mem_alloc(thread->ssize);

}

static void vm_thread_destroy(vm_thread_t *thread)
{
	heap_destroy(&thread->heap);
	mem_free(thread->opstack);
}

static void *thread_start(void *mod_arg)
{
	module_t *mod = mod_arg;
	vm_thread_t thread;
	vm_thread_init(&thread);

	eval_thread(&thread, mod);

	vm_thread_destroy(&thread);

	return NULL;
}

void vm_eval_module(module_t *mod)
{
	pthread_t thread;
	if (pthread_create(&thread, NULL, thread_start, mod) != 0)
		FATAL("pthread_create: %s\n", strerror(errno));
	pthread_join(thread, NULL);
}

void vm_init()
{
	hash_table_init(&ns_global, string_hash, string_equal);
	ns_install_primitives(&ns_global);
	hash_table_init(&sym_table, string_hash, string_equal);
	sym_table.destroy_key = free;
}

void vm_cleanup()
{
	hash_table_destroy(&ns_global);
	hash_table_destroy(&sym_table);
}

#define SIZE_INFO(t) printf("sizeof(%s) = %zd\n", #t, sizeof(t))

static void info()
{
	SIZE_INFO(vm_thread_t);
	SIZE_INFO(fixnum_t);
	SIZE_INFO(ptr_t);
	SIZE_INFO(char_t);
	SIZE_INFO(type_t);
	SIZE_INFO(block_hdr_t);
	SIZE_INFO(display_t);
	SIZE_INFO(env_t);
	SIZE_INFO(closure_t);
	SIZE_INFO(string_t);
	SIZE_INFO(pair_t);
}

int main()
{
	info();
//	exit(0);

	vm_init();

	module_t *mod = module_load("/tmp/assembly");
	vm_eval_module(mod);
	module_free(mod);

	vm_cleanup();
	return 0;
}
