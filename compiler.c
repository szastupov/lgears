#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>

#include <stdio.h>
#include "opcode.h"
#include "compiler_internal.h"

static void compile(compiler_t *sc, ast_node_t *node);

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
	/*
	 * We need to check passed arguments at compile and run time.
	 * Compile time: check only if we know that the function came from a func area
	 * Run time: pass counted argc to a function call, if mismatch - throw exception
	 * run time check may guarantee that function will not corrupt the stack
	 */
	int argc = 0;
	ast_node_t *arg = AST_NEXT(node);
	//FIXME, compile it backward
	ast_iter_forward(arg) {
		compile(sc, arg);
		argc++;
	}

	load_t *load = compile_named(sc, node);

	/*
	 * If the function is local then we can validate it here
	 */
	if (load->type == LOAD_FUNC) {
		sc_func_t *func = get_func_by_id(sc, load->idx);
		if (func->argc != argc)
			FATAL("function `%s' require %d arguments, but %d are passed\n",
					node->data, func->argc, argc);
	}

	add_opcode(sc,	FUNC_CALL, argc, -argc+1);
}

static void compile_if(compiler_t *sc, ast_node_t *node)
{
	sc_opcode_t *if_code, *finish_code;

	/* compile predicate */
	compile(sc, node);
	if_code = add_opcode(sc, JUMP_IF_FALSE, NO_ARG, 0);

	/* compile if-clause */
	compile(sc, AST_NEXT(node));
	if (AST_SECOND(node))
		finish_code = add_opcode(sc, JUMP_TO, NO_ARG, 0);

	/* set jump to the else-clause */
	if_code->arg = next_opcode_idx(sc);

	/* compile else-clause */
	if (AST_SECOND(node)) {
		compile(sc, AST_SECOND(node));
		/* set jump to skip else-clause */
		finish_code->arg = next_opcode_idx(sc);
	}
}

/*
 * FIXME rewrite it with define-syntax
 */
static void compile_and_or(compiler_t *sc, ast_node_t *node, int code)
{
	stack_t tmp;
	memset(&tmp, 0, sizeof(stack_t));
	sc_opcode_t *opcode = NULL;

	/* Compile body */
	ast_iter_forward(node) {
		compile(sc, node);
		if (AST_NEXT(node)) {
			opcode = add_opcode(sc, code, NO_ARG, 0);
			stack_push(&tmp, opcode);
		}
	}

	int finish = next_opcode_idx(sc);

	/* Set jumps */
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
			{
				node = AST_NEXT(node);
				int idx = compile_func(sc, AST_CHILD(node), AST_NEXT(node));
				if (sc->sc_env_stack)
					add_opcode(sc, LOAD_FUNC, idx, 1);
			}
			break;

		case SCOPE_OPEN:
			compile(sc, node);
			compile(sc, AST_NEXT(node));
//			compile_call(sc, AST_NEXT(node));
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

		case NOT_OP:
			compile(sc, AST_NEXT(node));
			add_opcode(sc, UNARY_NOT, NO_ARG, 0);
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
//	list_t *head = parse_buf("(lambda (a b c d) (if (not (and a b c d)) a))");
	list_t *head = parse_buf("(lambda (a b) (if a a b))");
//	list_t *head = parse_buf("(lambda (a) (lambda () a))");
//	list_t *head = parse_buf("((lambda (a) (a)) (lambda ()))");
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
