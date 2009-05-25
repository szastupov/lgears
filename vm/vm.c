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
#include <stdlib.h>
#include <stdio.h>
#include <pthread.h>
#include <sys/stat.h>
#include <string.h>
#include <errno.h>
#include <locale.h>
#include <stdarg.h>

#include "opcodes.h"
#include "vm.h"
#include "primitives.h"
#include "module.h"

#define MODULE_FUNC(module, idx) &(module)->functions[idx]

hash_table_t builtin;			/* Builtin functions */
hash_table_t libraries;			/* Libraries */
const_allocator_t global_const_pool;
int t_env, t_closure;
char *cache_path;

static void mark_env(env_t **env, visitor_t *visitor)
{
	if (!*env)
		return;
	ptr_t ptr;
	PTR_INIT(ptr, *env);
	visitor->visit(visitor, &ptr.obj);
	*env = PTR_GET(ptr);
}

static void env_visit(visitor_t *vs, void *data)
{
	env_t *env = data;
	env->objects = data+sizeof(env_t);
	if (env->prev)
		mark_env(&env->prev, vs);
	int i;
	for (i = 0; i < env->size; i++)
		vs->visit(vs, &env->objects[i]);
}

static env_t* env_new(heap_t *heap, env_t **prev, int size, int depth)
{
	void *mem = heap_alloc0(heap, sizeof(env_t)+sizeof(obj_t)*size, t_env);
	env_t *env = mem;
	env->objects = mem+sizeof(env_t);
	env->depth = depth;
	if (prev)
		env->prev = *prev;
	env->size = size;

	return env;
}

static void closure_visit(visitor_t *vs, void *data)
{
	closure_t *closure = data;
	mark_env(&closure->env, vs);
	if (closure->func->bmcount) {
		closure->bindmap = data+sizeof(closure_t);
		int i;
		for (i = 0; i < closure->func->bmcount; i++)
			vs->visit(vs, &closure->bindmap[i]);
	}
}

/* Search environment by depth mark */
env_t *env_display(env_t *env, int idx)
{
	while (idx != env->depth) {
		env = env->prev;
		if (!env)
			FATAL("null env");
	}

	return env;
}

static void bindmap_init(obj_t *bindmap, env_t *senv, func_t *func)
{
	int i;
	for (i = 0; i < func->bmcount; i++) {
		env_t *env = env_display(senv, func->bindmap[i]);
		bindmap[i] = make_ptr(env, id_ptr);
	}
}

static obj_t closure_new(heap_t *heap, func_t *func, env_t **env)
{
	size_t size = sizeof(closure_t);
	if (func->bmcount)
		size += func->bmcount*sizeof(obj_t);

	void *mem = heap_alloc(heap, size, t_closure);
	closure_t *closure = mem;
	closure->func = func;
	closure->env = *env;

	if (func->bmcount) {
		closure->bindmap = mem+sizeof(closure_t);
		bindmap_init(closure->bindmap, *env, func);
	} else
		closure->bindmap = NULL;

	return make_ptr(closure, id_ptr);
}

static int load_library(vm_thread_t *thread, obj_t *argv, int argc)
{
	SAFE_ASSERT(IS_SYMBOL(argv[1]));
	char *libname = SYMBOL(argv[1]);
	module_t *mod = hash_table_lookup(&libraries, libname);

	if (!mod) {
		char *path = alloca(256);
		snprintf(path, 256, "%s/%s.scm.o", cache_path, libname);
		LOG_DBG("Loading lib %s ...\n", path);
		mod = module_load(path);
		hash_table_insert(&libraries, libname, mod);
	}

	func_t *func = MODULE_FUNC(mod, mod->entry_point);
	obj_t enter = make_ptr(func, id_func);

	STACK_PUSH(argv[0]);	/* Push continuation */
	thread->tramp.argc = 1;
	thread->tramp.func = enter;

	return RC_OK;
}
MAKE_NATIVE(load_library, -1, 1, 0);

static int library_cache(vm_thread_t *thread, obj_t *argv, int argc)
{
	if (argc == 2) {
		SAFE_ASSERT(IS_PAIR(argv[1]));
		thread->lib_cache = argv[1];
		RETURN_OBJ(cvoid.obj);
	} else
		RETURN_OBJ(thread->lib_cache);
}
MAKE_NATIVE_VARIADIC(library_cache, 0);

/* Initialize interpreted function  */
static void enter_interp(vm_thread_t *thread, func_t *func, int op_arg, int tag)
{
	thread->func = func;
	int i;
	void *args;

	if (func->hdr.swallow) {
		i = op_arg - func->hdr.argc;
		if (i) {
			heap_require_blocks(&thread->heap, sizeof(pair_t), i);
			void *args = &thread->opstack[thread->op_stack_idx - i];
			thread->op_stack_idx -= i;
			STACK_PUSH(_list(&thread->heap, args, i));
			op_arg -= (i-1);
		} else {
			STACK_PUSH(cnull.obj);
			op_arg++;
		}
	}

	args = &thread->opstack[thread->op_stack_idx - op_arg];
	if (func->heap_env) {
		/* Store objects on the env */
		thread->env = env_new(&thread->heap, &thread->env, func->env_size, func->depth);
		thread->objects = thread->env->objects;
		if (op_arg) {
			thread->op_stack_idx = 0;
			memcpy(thread->objects, args, op_arg*sizeof(obj_t));
		}
	} else {
		/* Store objects on the stack */
		if (thread->op_stack_idx > op_arg*2) {
			thread->objects = memcpy(thread->opstack,
									 args, op_arg*sizeof(obj_t));
			thread->op_stack_idx = op_arg;
		} else
			thread->objects = args;
	}
	thread->heap_env = func->heap_env;

	/* Initialize bindmap */
	if (func->bcount && !thread->bindmap) {
		thread->bindmap = &thread->opstack[thread->op_stack_idx];
		thread->op_stack_idx += func->bmcount;
		bindmap_init(thread->bindmap, thread->env, func);
	}
}

/* Initialize current exception handler and build error message */
int push_exception_handler(vm_thread_t *thread, const char *msg, ...)
{
	char buf[256];
	va_list ap;
	va_start(ap, msg);
	vsnprintf(buf, sizeof(buf), msg, ap);
	va_end(ap);

	if (!IS_PAIR(thread->exception_handlers))
		goto bad_exception;
	pair_t *pair = PTR(thread->exception_handlers);

	if (!IS_FUNC(pair->car))
		goto bad_exception;

	obj_t obj = _string(&thread->heap.allocator, buf, 1);

	obj_t cont = make_ptr((native_func_t*)&vm_exit_nt, id_func);
	STACK_PUSH(cont);
	STACK_PUSH(obj);
	STACK_PUSH(pair->car);

	thread->exception_handlers = pair->cdr;

	return 2;

bad_exception:
	fprintf(stderr, "Unhandler VM error: `%s'\n", buf);
	return 0;
}

/*
 * The heart of lGears Virtual Machine
 *
 * lGears VM is designed to evaluate Continuation Passing Style code,
 * so it doesn't has frame stack. A continuation passed as first
 * argument on each function call. Compiler should decide itself how to
 * allocate function. I.e. if we pass continuation to native function
 * we don't have to allocate full closure, only function pointer is
 * needed. All thread-local data is stored inside thread structure, it
 * slow-down access but allows to get rid of some GC problems.
 * Although the VM doesn't has a frame stack, it has the operand stack
 * and stack nature.
 */
static void eval_thread(vm_thread_t *thread, module_t *module)
{
	/* This variables updated every time a new function takes control */
	int16_t *opcode;
	int op_code, op_arg;
	func_t *func;

	fixnum_t fxa, fxb, fxc;
	fxc.tag = id_fixnum;
#define FETCH_AB() fxb.obj = STACK_POP(); fxa.obj = STACK_POP();

	/*
	 * Try to raise an exception. If no suitable exception found -
	 * just kill the thread
	 */
#define RAISE(who, msg...)  {									\
		op_arg = push_exception_handler(thread, who": "msg);	\
		if (!op_arg)											\
			return;												\
		goto funcall;											\
	}

	/*
	 * Tracing is optional and suitable only for VM hacking
	 */
#if DEBUG_TRACE_OPCODE
	void (*trace_func)();
#define TRACE() trace_func()
#define SET_TRACE() set_trace_func()

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
#else
#define TRACE()
#define SET_TRACE()
#endif

	/* First time initialization */
	func = MODULE_FUNC(module, module->entry_point);
	thread->func = func;
	opcode = func->opcode;
	/* FIXME, entry point may not requite heap env */
	thread->env = env_new(&thread->heap, NULL, func->env_size, func->depth);
	thread->objects = thread->env->objects;
	thread->heap_env = 1;

	thread->objects[0].ptr = hash_table_lookup(&builtin, "__exit");
	thread->lib_cache = cnull.obj;
	thread->exception_handlers = cnull.obj;

	SET_TRACE();

	/* Computed goto (a.k.a. Threaded code) may speed-up performance
	 * but is's not standard. But 90% of us use gcc so, it should work.
	 */
#if COMPUTED_GOTO
#include "opcode_targets.h"		/* Targets generated bu gen-headers */
#define TARGET(op)								\
	TARGET_##op:								\
		op_code = *(opcode++);					\
	op_arg = *(opcode++); TRACE();
#define NEXT() goto *opcode_targets[(int)*opcode]
#define DISPATCH() NEXT();
#else
#define TARGET(op) case op: TRACE();
#define NEXT() continue
#define DISPATCH()								\
	op_code = *(opcode++);						\
	op_arg = *(opcode++);						\
	switch (op_code)
#endif

	/* Opcode description available in opcodes.h */
	for (;;) {
		DISPATCH() {
			TARGET(LOAD_LOCAL)
				STACK_PUSH(thread->objects[op_arg]);
			NEXT();

			TARGET(LOAD_CONST)
				STACK_PUSH(func->module->consts[op_arg]);
			NEXT();

			TARGET(JUMP_IF_FALSE)
				if (IS_FALSE(STACK_POP()))
					opcode += op_arg*2;
			NEXT();

			TARGET(JUMP_FORWARD)
				opcode += op_arg*2;
			NEXT();

			TARGET(LOAD_FUNC)
				STACK_PUSH(make_ptr(MODULE_FUNC(func->module, op_arg), id_func));
			NEXT();

			TARGET(FUNC_CALL)
				funcall:
			{
				ptr_t fp;
				void *ptr;
				fp.obj = STACK_POP();
dispatch_func:
				switch (fp.tag) {
				case id_ptr: {
					if (IS_TYPE(fp.obj, t_closure)) {
						closure_t *closure = PTR(fp.obj);
						ptr = closure->func;
						thread->env = closure->env;
						thread->bindmap = closure->bindmap;
						thread->closure = closure;
					} else if (IS_TYPE(fp.obj, t_cont)) {
						/* Unpack function and dispatch again */
						continuation_t *cont = PTR(fp.obj);
						fp.obj = cont->func;
						op_arg--;
						goto dispatch_func;
					} else
						RAISE("got pointer but it isn't a closure or a continuation");
					break;
				}
				case id_func:
					ptr = PTR_GET(fp);
					thread->bindmap = NULL;
					thread->closure = NULL;
					break;
				default:
					RAISE("call", "expected function or closure but got tag %d\n", fp.tag);
				}

				func_hdr_t *fhdr = (func_hdr_t*)ptr;
				if (fhdr->swallow) {
					if (op_arg < fhdr->argc)
						RAISE("call", "try to pass %d args when at least %d requred", op_arg-1, fhdr->argc-1);
				} else {
					if (op_arg != fhdr->argc)
						RAISE("call", "try to pass %d args when %d requred", op_arg-1, fhdr->argc-1);
				}

				/* TODO: maybe use threaded code here? */
				switch (fhdr->type) {
				case func_inter: {
					func = ptr;
					opcode = func->opcode;
					enter_interp(thread, func, op_arg, fp.tag);
					SET_TRACE();
					NEXT();
				}
				case func_native: {
					native_func_t *func = ptr;
					obj_t *argv = &thread->opstack[thread->op_stack_idx - op_arg];
					thread->tramp.argc = 1;
					thread->tramp.func.ptr = NULL;

					switch (native_call(thread, func, argv, op_arg)) {
					case RC_EXIT:
						/* Terminate thread */
						return;
					case RC_ERROR:
						RAISE("call", "%s failed", func->name);
						return;
					case RC_OK:
					default:
						break;
					}

					if (thread->tramp.func.ptr)
						fp.obj = thread->tramp.func;
					else
						fp.obj = argv[0];

					op_arg = thread->tramp.argc;

					goto dispatch_func;
				}
				default:
					FATAL("BUG! Expected function but got type tag: %d\n", fp.tag);
				}
			}
			NEXT();

			TARGET(SET_LOCAL) {
				thread->objects[op_arg] = STACK_POP();
				MARK_MODIFIED(&thread->heap, thread->env);
			}
			NEXT();

			TARGET(SET_BIND) {
				bind_t *bind = &func->bindings[op_arg];
				env_t *env = ENV(thread->bindmap[bind->up]);
				env->objects[bind->idx] = STACK_POP();
				MARK_MODIFIED(&thread->heap, env);
			}
			NEXT();

			TARGET(LOAD_BIND) {
				bind_t *bind = &func->bindings[op_arg];
				env_t *env = ENV(thread->bindmap[bind->up]);
				STACK_PUSH(env->objects[bind->idx]);
			}
			NEXT();

			TARGET(LOAD_CLOSURE) {
				func_t *nf = MODULE_FUNC(func->module, op_arg);
				STACK_PUSH(closure_new(&thread->heap,
							nf, &thread->env));
			}
			NEXT();

			TARGET(PUSH_BOOL)
				STACK_PUSH(CIF(op_arg).obj);
			NEXT();

			TARGET(PUSH_NULL)
				STACK_PUSH(cnull.obj);
			NEXT();

			TARGET(OP_NOT)
				STACK_PUSH(CIF(IS_FALSE(STACK_POP())).obj);
			NEXT();

			TARGET(OP_EQ_PTR) {
				FETCH_AB();		/* It's ok to use fixnum */
				STACK_PUSH(CIF(fxa.ptr == fxb.ptr).obj);
			}
			NEXT();

#define COMPARE_TARGET(name, op)							\
			TARGET(OP_##name) {								\
				FETCH_AB();									\
				STACK_PUSH(CIF(fxa.val op fxb.val).obj);	\
			}												\
			NEXT();

			COMPARE_TARGET(EQ, ==);

			COMPARE_TARGET(GT, >);

			COMPARE_TARGET(LT, <);

#define ARITHMETIC_TARGET(name, op)				\
			TARGET(OP_##name) {					\
				FETCH_AB();						\
				fxc.val = fxa.val op fxb.val;	\
				STACK_PUSH(fxc.obj);			\
			}									\
			NEXT();

			ARITHMETIC_TARGET(MOD, %);

			ARITHMETIC_TARGET(DIV, /);

			ARITHMETIC_TARGET(MUL, *);

			ARITHMETIC_TARGET(ADD, +);

			ARITHMETIC_TARGET(SUB, -);

			TARGET(OP_CONS) {
				heap_require(&thread->heap, sizeof(pair_t));
				obj_t b = STACK_POP();
				obj_t a = STACK_POP();
				STACK_PUSH(_cons(&thread->heap.allocator, &a, &b));
			}
			NEXT();

			TARGET(OP_CAR) {
				obj_t p = STACK_POP();
				if (!IS_PAIR(p))
					RAISE("car", "expected pair");
				pair_t *pair = PTR(p);
				STACK_PUSH(pair->car);
			}
			NEXT();

			TARGET(OP_CDR) {
				obj_t p = STACK_POP();
				if (!IS_PAIR(p))
					RAISE("cdr", "expected pair");
				pair_t *pair = PTR(p);
				STACK_PUSH(pair->cdr);
			}
			NEXT();
		}
	}
}

/* Collect root objects */
void thread_get_roots(visitor_t *visitor, vm_thread_t *thread)
{
	int i;

	mark_env(&thread->env, visitor);

	for (i = 0; i < thread->op_stack_idx; i++)
		visitor->visit(visitor, &thread->opstack[i]);

	if (thread->closure) {
		ptr_t ptr;
		PTR_INIT(ptr, thread->closure);
		visitor->visit(visitor, &ptr.obj);
		thread->closure = PTR_GET(ptr);
	}

	visitor->visit(visitor, &thread->lib_cache);
	visitor->visit(visitor, &thread->exception_handlers);
}

/* Update pointers */
void thread_after_gc(visitor_t *visitor, vm_thread_t *thread)
{
	if (thread->heap_env)
		thread->objects = thread->env->objects;
	if (thread->closure)
		thread->bindmap = thread->closure->bindmap;
}

/* Initialize thread */
static void vm_thread_init(vm_thread_t *thread)
{
	memset(thread, 0, sizeof(*thread));

	heap_init(&thread->heap, thread);

	int ssize = 4096;
	thread->opstack = mem_alloc(ssize);
	thread->op_stack_size = ssize/sizeof(obj_t);
}

/* Cleanup thread */
static void vm_thread_destroy(vm_thread_t *thread)
{
	heap_destroy(&thread->heap);
	mem_free(thread->opstack);
}

/* Start thread */
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

void ns_install_global(const char *name, const native_func_t *nt)
{
	ns_install_native(&builtin, name, nt);
}

void* lookup_global(const char *name)
{
	return hash_table_lookup(&builtin, name);
}

void* alloc_global_const(size_t sz, int type_id)
{
	return allocator_alloc(&global_const_pool.al, sz, type_id);
}

/* Initialize global vm structures */
void vm_init()
{
	t_env = register_type("env", NULL, env_visit);
	t_closure = register_type("closure", NULL, closure_visit);

	hash_table_init(&builtin, string_hash, string_equal);
	primitives_init();
	ns_install_global("load-library", &load_library_nt);
	ns_install_global("library-cache", &library_cache_nt);

	hash_table_init(&libraries, direct_hash, direct_equal);
	libraries.destroy_val = (destroy_func)module_free;

	cache_path = getenv("LGEARS_CACHE");
	if (!cache_path) {
		cache_path = mem_alloc(256);
		snprintf(cache_path, 256, "%s/.cache/lgears", getenv("HOME"));
	}

	const_allocator_init(&global_const_pool);
}

/* Cleanup VM (I want clear valgrind output) */
void vm_cleanup()
{
	primitives_cleanup();
	hash_table_destroy(&builtin);
	hash_table_destroy(&libraries);
	const_allocator_clean(&global_const_pool);
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
	SIZE_INFO(env_t);
	SIZE_INFO(closure_t);
	SIZE_INFO(string_t);
	SIZE_INFO(pair_t);
}

int main(int argc, char **argv)
{
#ifndef JARI
	setlocale(LC_ALL, "");
#endif
	if (argc < 2) {
		fprintf(stderr, "Usage: %s /path/to/assembly\n", argv[0]);
		exit(1);
	}

	if (!strcmp(argv[1], "info")) {
		info();
		exit(0);
	}

	vm_init();

	module_t *mod = module_load(argv[1]);
	vm_eval_module(mod);
	module_free(mod);

	vm_cleanup();
	return 0;
}
