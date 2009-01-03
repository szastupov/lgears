#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
#include <stdlib.h>
#include <stdio.h>

#include "memory.h"
#include "opcode.h"
#include "types.h"

typedef struct {
	int stack_size;
	int locals;
	int argc;
	int op_count;
	char *opcode;
} func_t;

typedef struct {
	char *code;
	func_t *functions;
	int entry_point;
	int fun_count;
} module_t;

typedef struct frame_s {
	struct frame_s *prev;
	obj_t	*opstack;
	obj_t	*locals;
	func_t	*func;
	int		step;
	int		op_stack_idx;
} frame_t;

func_t* load_func(module_t *module, int index)
{
	if (index > module->fun_count)
		FATAL("index %d out of range\n", index);
	return &module->functions[index];
}

frame_t* frame_create(func_t *func, frame_t *parent)
{
	frame_t *frame = type_alloc(frame_t);
	frame->prev = parent;
	frame->opstack = mem_calloc(func->stack_size, sizeof(obj_t));
	frame->locals = mem_calloc(func->locals, sizeof(obj_t));
	frame->func = func;

	return frame;
}

void frame_destroy(frame_t *frame)
{
	mem_free(frame->opstack);
	mem_free(frame->locals);
	mem_free(frame);
}

typedef struct {
	frame_t *frame_stack;
} vm_thread_t;

void eval_thread(vm_thread_t *thread, module_t *module)
{
	thread->frame_stack = frame_create(
			load_func(module, module->entry_point),
			thread->frame_stack);
	frame_t *frame = thread->frame_stack;

	int i;
	for (i = 0; i < frame->func->argc; i++) {
		int_t t;
		INIT_INT(&t, 1);
		frame->locals[i] = OBJ_CAST(&t);
	}

#define STACK_PUSH_ON(frame, n) frame->opstack[frame->op_stack_idx++] = n
#define STACK_PUSH(n) STACK_PUSH_ON(frame, n)
#define STACK_POP() frame->opstack[--frame->op_stack_idx]
#define STACK_HEAD() frame->opstack[frame->op_stack_idx-1]

	register int op_code, op_arg;
	char *code;
	while (frame->step < frame->func->op_count) {
next_cmd:
		code = frame->func->opcode+(frame->step*2);
		op_code = code[0];
		op_arg = code[1];
		printf("%d\t%s : %d\n",
				frame->step, opcode_name(op_code), op_arg);

		switch (op_code) {
		case LOAD_FAST:
			STACK_PUSH(frame->locals[op_arg]);
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

		case UNARY_NOT:
			if (is_false(STACK_POP())) {
				int_t t;
				INIT_INT(&t, 0);
				STACK_PUSH(OBJ_CAST(&t));
			}
			break;

		case LOAD_FUNC:
			{
				func_t *func = load_func(module, op_arg);
				func_ptr_t fp;
				INIT_FUNC_PTR(&fp, func);
				STACK_PUSH(OBJ_CAST(&fp));
			}
			break;

		case FUNC_CALL:
			{
				func_t *func = GET_FUNC(STACK_POP());
				if (func->argc != op_arg)
					FATAL("try to pass %d args when %d requred\n", op_arg, func->argc);
				frame = frame_create(func, frame);
				thread->frame_stack = frame;
				goto next_cmd;
			}
			break;

		case RETURN:
			{
				obj_t ret = STACK_POP();
				frame_t *parent = frame->prev;
				frame_destroy(frame);
				if (parent) {
					STACK_PUSH_ON(parent, ret);
					frame = parent;
				} else {
					printf("Result %p\n", ret);
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

module_t* module_load(const char *path)
{
	int fd = open(path, O_RDONLY);
	if (fd == -1)
		FATAL("nothing to load\n");

	module_t *mod = type_alloc(module_t);

	struct stat sb;
	fstat(fd, &sb);

	/* Read 4 bytes with functions count */
	struct module_hdr_s mhdr;
	read(fd, &mhdr, MODULE_HDR_OFFSET);
	/* Allocate functions storage */
	mod->functions = mem_calloc(mhdr.fun_count, sizeof(func_t));
	mod->fun_count = mhdr.fun_count;
	mod->entry_point = mhdr.entry_point;

	int code_offset = CODE_START_OFFSET(mhdr.fun_count);
	int code_size = sb.st_size - code_offset;
	/* Allocate code storage and read opcode */
	mod->code = malloc(code_size);
	lseek(fd, code_offset, SEEK_SET);
	read(fd, mod->code, code_size);

	int count;
	struct func_hdr_s hdr;
	lseek(fd, MODULE_HDR_OFFSET, SEEK_SET);
	for (count = 0; count < mhdr.fun_count; count++) {
		read(fd, &hdr, FUN_HDR_SIZE);
		func_t *func	= &mod->functions[count];
		func->stack_size	= hdr.stack_size;
		func->locals	= hdr.locals;
		func->argc		= hdr.argc;
		func->op_count	= hdr.op_count;
		func->opcode	= mod->code + hdr.offset;
	}

	close(fd);
	return mod;
}

void module_free(module_t *module)
{
	free(module->code);
	free(module->functions);
	free(module);
}

int main()
{
	module_t *mod = module_load("/tmp/assembly");
	vm_thread_t *thread = type_alloc(vm_thread_t);
	eval_thread(thread, mod);
	module_free(mod);
	mem_free(thread);
	return 0;
}
