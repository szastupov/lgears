#ifndef OPCODES_H
#define OPCODES_H

/* Generated by gen-header.scm, do not edit. */

#define LOAD_FUNC	0	/* Load function local to module */
#define LOAD_LOCAL	1	/* Load object from frame-local area */
#define LOAD_SYM	2	/* Load predefined symbol local to module */
#define LOAD_IMPORT	3	/* Load object from module import table */
#define JUMP_IF_FALSE	4	/* Jump if false */
#define JUMP_IF_TRUE	5	/* Jump if true */
#define JUMP_FORWARD	6	/* Jump forward */
#define FUNC_CALL	7	/* Call function */
#define SET_LOCAL	8	/* Assign new value to local binding */
#define SET_IN_ENV	9	/* Assing new value to evn binding */
#define LOAD_ENV	10	/* Load env from display */
#define LOAD_FROM_ENV	11	/* Load object from env */

#define OP_CASE(code) case code: return #code

const char* opcode_name(int code)
{
	switch (code) {
		OP_CASE(LOAD_FUNC);
		OP_CASE(LOAD_LOCAL);
		OP_CASE(LOAD_SYM);
		OP_CASE(LOAD_IMPORT);
		OP_CASE(JUMP_IF_FALSE);
		OP_CASE(JUMP_IF_TRUE);
		OP_CASE(JUMP_FORWARD);
		OP_CASE(FUNC_CALL);
		OP_CASE(SET_LOCAL);
		OP_CASE(SET_IN_ENV);
		OP_CASE(LOAD_ENV);
		OP_CASE(LOAD_FROM_ENV);
	}
	return "unknown";
}

#endif