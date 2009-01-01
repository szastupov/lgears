#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>

#include <stdio.h>
#include "ast.h"
#include "btree.h"
#include "opcode.h"
#include "compiler_internal.h"

#define MAX(a,b) (a > b) ? a : b

static sc_opcode_t* add_opcode(compiler_t *sc,
		int opcode, int arg, int stack_use)
{
	sc_func_t *func = sc->sc_env_stack->func;
	sc_opcode_t *code = type_alloc(sc_opcode_t);
	code->code = opcode;
	code->arg = arg;
	code->idx = func->op_count;

	list_append(&func->opcodes, &code->next);
	func->stack_use += stack_use;
	func->stack_size = MAX(func->stack_size, func->stack_use);
	func->op_count++;
	return code;
}

static int next_opcode_idx(compiler_t *sc)
{
	return sc->sc_env_stack->func->op_count;
}

static sc_func_t* function_new(compiler_t *sc)
{
	sc_func_t *func = type_alloc(sc_func_t);
	AREA_APPEND(sc->functions, func);

	return func;
}

static sc_const_t* const_new(compiler_t *sc)
{
	sc_const_t *cst = type_alloc(sc_const_t);
	AREA_APPEND(sc->consts, cst);

	return cst;
}

static sc_func_t* get_func_by_id(compiler_t *sc, int id)
{
	if (id >= sc->functions.count)
		FATAL("Function index out of range\n");
	int i = 0;
	list_node_t *cur;
	list_for_each(&sc->functions.list, cur) {
		if (i == id)
			break;
		i++;
	}

	return container_of(cur, sc_func_t, next);
}

static sc_env_t* sc_env_new(sc_env_t *parent)
{
	sc_env_t *env	= type_alloc(sc_env_t);
	env->tbl	= NULL;
	env->parent	= parent;

	return env;
}

static void sc_env_free(sc_env_t *env)
{
	tree_free(env->tbl);
	mem_free(env);
}

static void sc_env_stack_push(compiler_t *sc)
{
	sc->sc_env_stack = sc_env_new(sc->sc_env_stack);
}

static void sc_env_stack_pop(compiler_t *sc)
{
	sc_env_t *tmp = sc->sc_env_stack;
	sc->sc_env_stack = tmp->parent;
	sc_env_free(tmp);
}

static void sc_env_define(sc_env_t *env, const char *name,
		int type, int idx)
{
	load_t *load = type_alloc(load_t);
	load->type	= type;
	load->idx	= idx;

	tree_node_init(&load->node, name);
	tree_node_insert(&env->tbl, &load->node);
}

static load_t *sc_env_lookup(sc_env_t *env, const char *arg)
{
	if (!env)
		return NULL;

	tree_node_t *node = tree_node_search(env->tbl, arg);
	if (node) {
		return container_of(node, load_t, node);
	}

	return sc_env_lookup(env->parent, arg);
}

/*
 * Closures (include lambda) is functions too but use it's
 * top-parent local-storage for data
 * Normal functions has own local-storage
 */
static int compile_func(compiler_t *sc,
		ast_node_t *args, ast_node_t *body)
{
	sc_func_t *func = function_new(sc);
	sc_func_t *top = NULL;
	int closure = 0;

	if (sc->sc_env_stack) {
		closure = 1;	//We are a closure
		top = sc->sc_env_stack->top;
	} else
		top = func; //We are a top function

	sc_env_stack_push(sc);
	sc->sc_env_stack->func = func;
	sc->sc_env_stack->top = top;

	if (closure)
		add_opcode(sc, LOAD_ENV, NO_ARG, 0);

	ast_iter_forward(args) {
		sc_env_define(sc->sc_env_stack, args->data,
				LOAD_FAST, top->locals);
		func->argc++;
		top->locals++;
	}

	ast_iter_forward(body)
		compile(sc, body);

	add_opcode(sc, RETURN, NO_ARG, 0);
	sc_env_stack_pop(sc);

	return func->id;
}

static load_t* compile_named(compiler_t *sc, ast_node_t *node)
{
	if (!node->data)
		FATAL("empty node\n");
	load_t *lc = sc_env_lookup(sc->sc_env_stack, node->data);

	if (!lc)
		FATAL("variable `%s' not found", node->data);
	add_opcode(sc,	lc->type, lc->idx, 1);

	return lc;
}

static void compile_call(compiler_t *sc, ast_node_t *node)
{
	load_t *load = compile_named(sc, node);

	/*
	 * We need to check passed arguments at compile and run time.
	 * Compile time: check only if we know that the function came from a func area
	 * Run time: pass counted argc to a function call, if mismatch - throw exception
	 * run time check may guarantee that function will not cirrupt the stack
	 */
	int argc = 0;
	ast_node_t *arg = AST_NEXT(node);
	//FIXME, compile it backward
	ast_iter_forward(arg) {
		compile(sc, arg);
		argc++;
	}

	/*
	 * If the function is local then we can validate it here
	 */
	if (load->type == LOAD_FUNC) {
		sc_func_t *func = get_func_by_id(sc, load->idx);
		if (func->argc != argc)
			FATAL("function `%s' require %d arguments, but %d are passed\n",
					node->data, func->argc, argc);
	}

	add_opcode(sc,	FUNC_CALL, argc, -argc);
}

static void compile_if(compiler_t *sc, ast_node_t *node)
{
	sc_opcode_t *if_code, *finish_code;

	/* compile predicate */
	compile(sc, node);
	if_code = add_opcode(sc, JUMP_IF_FALSE, NO_ARG, -1);

	/* compile if-clause */
	compile(sc, AST_NEXT(node));
	finish_code = add_opcode(sc, JUMP_TO, NO_ARG, 0);

	/* set jump to the else-clause */
	if_code->arg = next_opcode_idx(sc);

	/* compile else-clause */
	compile(sc, AST_SECOND(node));

	/* set jump to skip else-clause */
	finish_code->arg = next_opcode_idx(sc);
}

static void compile_and_or(compiler_t *sc, ast_node_t *node, int code)
{
	stack_t tmp;
	memset(&tmp, 0, sizeof(stack_t));
	sc_opcode_t *opcode = NULL;

	ast_iter_forward(node) {
		compile(sc, node);
		opcode = add_opcode(sc, code, NO_ARG, -1);
		stack_push(&tmp, opcode);
	}

	int finish = next_opcode_idx(sc);

	list_node_t *cur, *save;
	list_for_each_safe(&tmp, cur, save) {
		stack_itm_t *itm = stack_cast(cur);
		opcode = itm->ptr;
		opcode->arg = finish;
		mem_free(itm);
	}
}

/*
 * Main compile dispatcher
 */
static void compile(compiler_t *sc, ast_node_t *node)
{
#define ASSERT_UNHANDLED(node)	if (node->tag != -1) FATAL("unhandled %d, %s\n", node->tag, node->data);

	if (node->tag == SCOPE_OPEN) {
		node = AST_CHILD(node);
		switch (node->tag) {
		case LAMBDA:
			node = AST_NEXT(node);
			compile_func(sc, AST_CHILD(node), AST_NEXT(node));
			break;

		case DEFINATION:
			node = AST_NEXT(node);
			if (AST_CHILD(node)) {
				int idx = compile_func(sc, AST_NEXT(AST_CHILD(node)), AST_NEXT(node));
				sc_env_define(sc->sc_env_stack,
						AST_CHILD(node)->data, LOAD_FUNC, idx);
			} else
				FATAL("fixme, i can't define non functions\n");
			break;

		case IF_STMT:
			compile_if(sc, AST_NEXT(node));
			break;

		case AND_STMT:
			compile_and_or(sc, AST_NEXT(node), JUMP_IF_FALSE);
			break;

		case OR_STMT:
			compile_and_or(sc, AST_NEXT(node), JUMP_IF_TRUE);
			break;

		default:
			ASSERT_UNHANDLED(node);
			compile_call(sc, node);
		}
	} else {
		switch (node->tag) {
		case STRING:
			{
//				printf("got string %s\n", node->data);
				sc_const_t *cst = const_new(sc);
				add_opcode(sc, LOAD_CONST, cst->id, 1);
				//FIXME populate const
			}
			break;
		default:
			ASSERT_UNHANDLED(node);
			compile_named(sc, node);
		}
	}
}

static void compiler_clear(compiler_t *sc)
{
	list_node_t *cur, *save;
	list_for_each_safe(&sc->functions.list, cur, save) {
		sc_func_t *func = container_of(cur, sc_func_t, next);

		list_node_t *op_save;
		list_for_each_safe(&func->opcodes, cur, op_save) {
			sc_opcode_t *code = container_of(cur, sc_opcode_t, next);
			free(code);
		}

		free(func);
	}

	list_for_each_safe(&sc->consts.list, cur, save) {
		sc_const_t *cst = container_of(cur, sc_const_t, next);
		free(cst);
	}
}

void dump_opcode(sc_func_t *func)
{
	printf("\tOpcode:\n");
	sc_opcode_t *code;
	list_for_each_entry(&func->opcodes, code, next) {
		printf("%d\t%s : %d\n", code->idx, opcode_name(code->code), code->arg);
	}
}

void dump_comp(compiler_t *sc)
{
	printf("\nFunctions count: %d\n", sc->functions.count);
	sc_func_t *func;
	list_for_each_entry(&sc->functions.list, func, next) {
		printf("\nFunction %d {\n", func->id);
		printf("\targc: %d\n", func->argc);
		printf("\tlocals: %d\n", func->locals);
		printf("\tstack_size: %d\n", func->stack_size);
		dump_opcode(func);
		printf("}\n");
	}
}

void dump_const(compiler_t *sc)
{
	printf("\nConsts count: %d\n", sc->consts.count);
	sc_const_t *cst;
	list_for_each_entry(&sc->consts.list, cst, next) {
		printf("Const %d %s\n", cst->id, cst->data);
	}
}

void assemble(compiler_t *sc)
{
	int start_offset	= CODE_START_OFFSET(sc->functions.count);
	int code_offset		= 0;
	struct module_hdr_s mhdr;

	int fd = creat("/tmp/assembly", S_IRWXU);

	//Write main module header
	mhdr.fun_count = sc->functions.count;
	mhdr.entry_point = 0;
	write(fd, &mhdr, MODULE_HDR_OFFSET);

	struct func_hdr_s hdr;
	sc_func_t *func;
	list_for_each_entry(&sc->functions.list, func, next) {

		//Write function header
		hdr.argc		= func->argc;
		hdr.locals		= func->locals;
		hdr.stack_size	= func->stack_size;
		hdr.op_count	= func->op_count;
		hdr.offset		= code_offset;
		FUN_SEEK(fd, func->id);
		write(fd, &hdr, FUN_HDR_SIZE);

		//Write function opcode
		lseek(fd, code_offset+start_offset, SEEK_SET);
		sc_opcode_t *code;
		list_for_each_entry(&func->opcodes, code, next) {
			char bcode[2];
			bcode[0] = code->code;
			bcode[1] = code->arg;
			write(fd, bcode, 2);
		}

		code_offset += hdr.op_count*2;
	}

	close(fd);
}

int main()
{
//	list_t *head = parse_buf("(lambda (x y z c) (if (and x y) (y \"a\") \"sdfsdf\"))");
	list_t *head = parse_buf("(lambda (a b c d) (and a b c d))");
	list_node_t *cur, *save;
	compiler_t sc;
	memset(&sc, 0, sizeof(sc));
	list_for_each_safe(head, cur, save) {
		compile(&sc, AST_NODE(cur));
		ast_node_free(AST_NODE(cur));
	}
	mem_free(head);

	assemble(&sc);
	dump_comp(&sc);
	compiler_clear(&sc);
	return 0;
}
