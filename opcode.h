#ifndef OPCODE_H
#define OPCODE_H 
#include <stdint.h>

enum {
	LOAD_FAST,	/**< Load object local to frame */
	LOAD_CONST,	/**< Load object from constant area */
	LOAD_FUNC,	/**< Load object from functions table */
	FUNC_CALL,	/**< Call func */
	LOAD_ENV,	/**< Load env */
	JUMP_IF_FALSE,	/**< Jump to code on false */
	JUMP_IF_TRUE,	/**< Jump to code on true */
	JUMP_TO,		/**< Non-conditional jump */
	RETURN,			/**< Return from frame */
	UNARY_NOT,		/**< Unary not */
};

#define NO_ARG -1

#define OP_CASE(code) case code: return #code

const char* opcode_name(int code)
{
	switch (code) {
		OP_CASE(LOAD_FAST);
		OP_CASE(LOAD_CONST);
		OP_CASE(LOAD_FUNC);
		OP_CASE(FUNC_CALL);
		OP_CASE(LOAD_ENV);
		OP_CASE(JUMP_IF_FALSE);
		OP_CASE(JUMP_IF_TRUE);
		OP_CASE(JUMP_TO);
		OP_CASE(RETURN);
		OP_CASE(UNARY_NOT);
	}
	return "unknown";
}

struct func_hdr_s {
	uint8_t argc;
	uint32_t locals;
	uint32_t stack_size;
	uint32_t offset;
	uint32_t op_count;
} __attribute__((__packed__));

struct module_hdr_s {
	uint32_t fun_count;
	uint32_t entry_point;
} __attribute__((__packed__));

#define MODULE_HDR_OFFSET	sizeof(struct module_hdr_s)
#define FUN_HDR_SIZE sizeof(struct func_hdr_s)
#define CODE_START_OFFSET(count) count * FUN_HDR_SIZE + MODULE_HDR_OFFSET
#define FUN_SEEK(fd, id) lseek(fd, id * FUN_HDR_SIZE + MODULE_HDR_OFFSET, SEEK_SET)

#endif /* OPCODE_H */
