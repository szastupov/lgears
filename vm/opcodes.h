#ifndef OPCODES_H
#define OPCODES_H

/* Generated by gen-headers.scm, do not edit. */

#define OP_CASE(code) case code: return #code

#define LOAD_FUNC	0x0	/* Load function local to module */
#define LOAD_CLOSURE	0x1	/* Make closure on heap */
#define LOAD_LOCAL	0x2	/* Load object from frame-local area */
#define LOAD_BIND	0x3	/* Load object form binding */
#define LOAD_CONST	0x4	/* Load object from module constant area */
#define PUSH_BOOL	0x5	/* Push boolean value */
#define PUSH_NULL	0x6	/* Load built-in constant */
#define JUMP_IF_FALSE	0x7	/* Jump if false */
#define JUMP_FORWARD	0x8	/* Jump forward */
#define FUNC_CALL	0x9	/* Call function */
#define SET_LOCAL	0xa	/* Assign new value to local binding */
#define SET_BIND	0xb	/* Asign new value to non-local binding */
#define OP_CONS	0xc	/* cons */
#define OP_CAR	0xd	/* car */
#define OP_CDR	0xe	/* cdr */
#define OP_SUB	0xf	/* - */
#define OP_ADD	0x10	/* + */
#define OP_MUL	0x11	/* * */
#define OP_DIV	0x12	/* / */
#define OP_MOD	0x13	/* % */
#define OP_LT	0x14	/* < */
#define OP_GT	0x15	/* > */
#define OP_EQ	0x16	/* = */
#define OP_EQ_PTR	0x17	/* eq? */
#define OP_NOT	0x18	/* ! */

#define OT_FIXNUM	0x0	/* Fixed number */
#define OT_CHARACTER	0x1	/* Character */
#define OT_STRING	0x2	/* String */
#define OT_SYMBOL	0x3	/* Symbol */
#define OT_STATIC	0x4	/* Static variable */
#define OT_PAIR_BEGIN	0x5	/* Pair begin */
#define OT_PAIR_END	0x6	/* Pair end */
#define OT_VECTOR	0x7	/* Vector begin */
#define OT_BOOLEAN	0x8	/* Boolean */
#define OT_NULL	0x9	/* Null object */

static inline const char* opcode_name(int code)
{
	switch (code) {
		OP_CASE(LOAD_FUNC);
		OP_CASE(LOAD_CLOSURE);
		OP_CASE(LOAD_LOCAL);
		OP_CASE(LOAD_BIND);
		OP_CASE(LOAD_CONST);
		OP_CASE(PUSH_BOOL);
		OP_CASE(PUSH_NULL);
		OP_CASE(JUMP_IF_FALSE);
		OP_CASE(JUMP_FORWARD);
		OP_CASE(FUNC_CALL);
		OP_CASE(SET_LOCAL);
		OP_CASE(SET_BIND);
		OP_CASE(OP_CONS);
		OP_CASE(OP_CAR);
		OP_CASE(OP_CDR);
		OP_CASE(OP_SUB);
		OP_CASE(OP_ADD);
		OP_CASE(OP_MUL);
		OP_CASE(OP_DIV);
		OP_CASE(OP_MOD);
		OP_CASE(OP_LT);
		OP_CASE(OP_GT);
		OP_CASE(OP_EQ);
		OP_CASE(OP_EQ_PTR);
		OP_CASE(OP_NOT);
	}
	return "unknown";
}

static inline const char* object_type_name(int code)
{
	switch (code) {
		OP_CASE(OT_FIXNUM);
		OP_CASE(OT_CHARACTER);
		OP_CASE(OT_STRING);
		OP_CASE(OT_SYMBOL);
		OP_CASE(OT_STATIC);
		OP_CASE(OT_PAIR_BEGIN);
		OP_CASE(OT_PAIR_END);
		OP_CASE(OT_VECTOR);
		OP_CASE(OT_BOOLEAN);
		OP_CASE(OT_NULL);
	}
	return "unknown";
}

#endif
