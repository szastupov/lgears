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

inter_opcode_t* add_opcode(compiler_t *sc,
		int opcode, int arg, int stack_use)
{
	inter_func_t *func = sc->env_stack->func;
	inter_opcode_t *code = type_alloc(inter_opcode_t);
	code->code = opcode;
	code->arg = arg;
	code->idx = func->op_count;

	list_append(&func->opcodes, &code->next);
	func->stack_use += stack_use;
	func->stack_size = MAX(func->stack_size, func->stack_use);
	func->op_count++;
	return code;
}

int next_opcode_idx(compiler_t *sc)
{
	return sc->env_stack->func->op_count;
}

static inter_func_t* function_new(compiler_t *sc)
{
	inter_func_t *func = type_alloc(inter_func_t);
	AREA_APPEND(sc->functions, func);

	return func;
}

static inter_const_t* const_new(compiler_t *sc)
{
	inter_const_t *cst = type_alloc(inter_const_t);
	AREA_APPEND(sc->consts, cst);

	return cst;
}

static inter_func_t* get_func_by_id(compiler_t *sc, int id)
{
	if (id >= sc->functions.count)
		FATAL("Function index out of range\n");
	int i = 0;
	list_node_t *cur;
	list_iter_forward(&sc->functions.list, cur) {
		if (i == id)
			break;
		i++;
	}

	return container_of(cur, inter_func_t, next);
}

static env_t* env_new(env_t *parent)
{
	env_t *env	= type_alloc(env_t);
	env->tbl	= NULL;
	env->parent	= parent;

	return env;
}

static void env_free(env_t *env)
{
	tree_free(env->tbl);
	mem_free(env);
}

static void env_stack_push(compiler_t *sc)
{
	sc->env_stack = env_new(sc->env_stack);
}

static void env_stack_pop(compiler_t *sc)
{
	env_t *tmp = sc->env_stack;
	sc->env_stack = tmp->parent;
	env_free(tmp);
}

static void env_define(env_t *env, const char *name,
		int type, int idx)
{
	load_t *load = type_alloc(load_t);
	load->type	= type;
	load->idx	= idx;

	tree_node_init(&load->node, name);
	tree_node_insert(&env->tbl, &load->node);
}

load_t *env_lookup(env_t *env, const char *arg)
{
	if (!env)
		return NULL;

	tree_node_t *node = tree_node_search(env->tbl, arg);
	if (node) {
		return container_of(node, load_t, node);
	}

	return env_lookup(env->parent, arg);
}

/*
 * Closures (include lambda) is functions too but use it's
 * top-parent local-storage for data
 * Normal functions has own local-storage
 */
static int compile_func(compiler_t *sc,
		ast_node_t *args, ast_node_t *body)
{
	inter_func_t *func = function_new(sc);
	inter_func_t *top = NULL;
	int closure = 0;

	if (sc->env_stack) {
		closure = 1;	//We are a closure
		top = sc->env_stack->top;
	} else
		top = func; //We are a top function

	env_stack_push(sc);
	sc->env_stack->func = func;
	sc->env_stack->top = top;

	if (closure)
		add_opcode(sc, LOAD_ENV, NO_ARG, 0);

	ast_iter_forward(args) {
		env_define(sc->env_stack, args->data,
				LOAD_FAST, top->locals);
		func->argc++;
		top->locals++;
	}

	ast_iter_forward(body)
		compile(sc, body);

	add_opcode(sc, RETURN, NO_ARG, 0);
	env_stack_pop(sc);

	return func->id;
}

static load_t* compile_named(compiler_t *sc, ast_node_t *node)
{
	if (!node->data)
		FATAL("empty node\n");
	load_t *lc = env_lookup(sc->env_stack, node->data);

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
		inter_func_t *func = get_func_by_id(sc, load->idx);
		if (func->argc != argc)
			FATAL("function `%s' require %d arguments, but %d are passed\n",
					node->data, func->argc, argc);
	}

	add_opcode(sc,	FUNC_CALL, argc, -argc);
}

static void compile_if(compiler_t *sc, ast_node_t *node)
{
	inter_opcode_t *if_code, *finish_code;

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
				env_define(sc->env_stack,
						AST_CHILD(node)->data, LOAD_FUNC, idx);
			} else
				FATAL("fixme, i can't define non functions\n");
			break;

		case IF_STMT:
			node = AST_NEXT(node);
			compile_if(sc, node);
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
				inter_const_t *cst = const_new(sc);
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

void dump_opcode(inter_func_t *func)
{
	printf("\tOpcode:\n");
	list_node_t *cur;
	list_iter_forward(&func->opcodes, cur) {
		inter_opcode_t *code = container_of(cur, inter_opcode_t, next);
		printf("%d\t%s : %d\n", code->idx, opcode_name(code->code), code->arg);
	}
}

void dump_comp(compiler_t *sc)
{
	printf("\nFunctions count: %d\n", sc->functions.count);
	list_node_t *cur;
	list_iter_forward(&sc->functions.list, cur) {
		inter_func_t *func = container_of(cur, inter_func_t, next);
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
	list_node_t *cur;
	list_iter_forward(&sc->consts.list, cur) {
		inter_const_t *cst = container_of(cur, inter_const_t, next);
		printf("Const %d %s\n", cst->id, cst->data);
	}
}

void assemble(compiler_t *sc)
{
	struct func_hdr_s hdr;
	int start_offset	= CODE_START_OFFSET(sc->functions.count);
	int code_offset		= 0;

	int fd = creat("/tmp/assembly", S_IRWXU);

	//Write functions count
	uint32_t fun_count = sc->functions.count;
	write(fd, &fun_count, HDR_OFFSET);

	list_node_t *cur, *op_cur;
	list_iter_forward(&sc->functions.list, cur) {
		inter_func_t *func = container_of(cur, inter_func_t, next);

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
		list_iter_forward(&func->opcodes, op_cur) {
			inter_opcode_t *code = container_of(op_cur, inter_opcode_t, next);
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
	list_t *head = parse_buf("(lambda (x y) (if (x) (y \"a\") \"sdfsdf\")");
	list_node_t *cur;
	compiler_t sc;
	memset(&sc, 0, sizeof(sc));
	list_iter_forward(head, cur) {
		compile(&sc, AST_NODE(cur));
	}
	assemble(&sc);
//	dump_comp(&sc);
	return 0;
}
