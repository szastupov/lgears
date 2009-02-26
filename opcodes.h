#ifndef OPCODES_H
#define OPCODES_H

/* Generated by gen-header.scm, do not edit. */

#define LOAD_FUNC	0	/* Load function local to module */
#define LOAD_CLOSURE	1	/* Make closure object */
#define LOAD_LOCAL	2	/* Load object from frame-local area */
#define LOAD_BIND	3	/* Load object form binding */
#define LOAD_SYM	4	/* Load predefined symbol local to module */
#define LOAD_BOOL	5	/* Push boolean value */
#define LOAD_IMPORT	6	/* Load object from module import table */
#define JUMP_IF_FALSE	7	/* Jump if false */
#define JUMP_IF_TRUE	8	/* Jump if true */
#define JUMP_FORWARD	9	/* Jump forward */
#define FUNC_CALL	10	/* Call function */
#define SET_LOCAL	11	/* Assign new value to local binding */
#define SET_BIND	12	/* Asign new value to non-local binding */

#define OP_CASE(code) case code: return #code

const char* opcode_name(int code)
{
	switch (code) {
		OP_CASE(LOAD_FUNC);
		OP_CASE(LOAD_CLOSURE);
		OP_CASE(LOAD_LOCAL);
		OP_CASE(LOAD_BIND);
		OP_CASE(LOAD_SYM);
		OP_CASE(LOAD_BOOL);
		OP_CASE(LOAD_IMPORT);
		OP_CASE(JUMP_IF_FALSE);
		OP_CASE(JUMP_IF_TRUE);
		OP_CASE(JUMP_FORWARD);
		OP_CASE(FUNC_CALL);
		OP_CASE(SET_LOCAL);
		OP_CASE(SET_BIND);
	}
	return "unknown";
}

#endif