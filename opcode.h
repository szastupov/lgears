#ifndef OPCODE_H
#define OPCODE_H 

enum { LOAD_FAST, LOAD_CONST, LOAD_FUNC,
	FUNC_CALL, LOAD_ENV,
	JUMP_IF_FALSE, JUMP_TO, RETURN };

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
		OP_CASE(JUMP_TO);
		OP_CASE(RETURN);
	}
	return "unknown";
}

#endif /* OPCODE_H */
