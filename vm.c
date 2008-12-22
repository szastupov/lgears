#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
#include <stdlib.h>
#include <stdio.h>

#include "memory.h"
#include "opcode.h"

int main()
{
	int fd = open("/tmp/assembly", O_RDONLY);
	if (fd == -1)
		FATAL("nothing to run\n");

	struct func_hdr_s hdr;
	uint32_t fun_count = 0;

	read(fd, &fun_count, sizeof(fun_count));
	printf("Total functions %d\n", fun_count);
	int code_offset = CODE_START_OFFSET(fun_count);

	int count = 0;
	while (count < fun_count) {
		FUN_SEEK(fd, count);
		read(fd, &hdr, sizeof(hdr));
		printf("Function %d, argc %d, locals %d, stack_size %d, offset %d, op_count %d\n",
				count, hdr.argc, hdr.locals, hdr.stack_size, hdr.offset, hdr.op_count);

		lseek(fd, code_offset, SEEK_SET);
		char bcode[2];
		int i;
		for (i = 0; i < hdr.op_count; i++) {
			read(fd, bcode, 2);
			printf("%d\t%s : %d\n", i, opcode_name(bcode[0]), bcode[1]);
		}

		code_offset += hdr.op_count*2;
		count++;
	}

	close(fd);
	return 0;
}
