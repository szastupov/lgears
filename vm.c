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
} module_t;

typedef struct {
	void **opstack;
	obj_t *locals;
	int op_stack_idx;
	int cmd_counter;
	func_t *code;
} frame_t;

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
	module_free(mod);
	return 0;
}
