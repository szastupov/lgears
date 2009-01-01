#ifndef COMPILER_INTERNAL_H
#define COMPILER_INTERNAL_H 
#include "btree.h"
#include "ast.h"

typedef struct {
	tree_node_t node;
	int type;	/**< Type of load command */
	int idx;	/**< Index */
} load_t;

typedef struct {
	list_node_t next;	/**< Next function */
	list_t opcodes;		/**< List of opcodes */
	int stack_size;		/**< Maximum used stat size */
	int stack_use;		/**< Stack use during compilation (used to calculate stack_size) */
	int argc;			/**< Arguments count */
	int locals;			/**< Size of local memory */
	int id;				/**< Function id (index in func table)*/
	int op_count;		/**< Count of operations */
} sc_func_t;

typedef struct {
	list_node_t next;	/**< Next opcode */
	int idx;			/**< Index relative to function */
	int code;			/**< id of code */
	int arg;			/**< argument */
} sc_opcode_t;

typedef struct {
	list_node_t next;	/**< Next constant */
	int id;				/**< Constant id in table */
	int type;
	char *data;
} sc_const_t;

typedef struct sc_env_s {
	struct sc_env_s *parent;	/**< Pointer on parent env */
	tree_node_t	*tbl;		/**< symbol table */
	sc_func_t *func;		/**< function of current env */
	sc_func_t *top;		/**< top function */
} sc_env_t;

typedef struct {
	int count;
	list_t list;
} area_table;

#define AREA_APPEND(area, itm) {				\
	list_append(&(area).list, &(itm)->next);	\
	(itm)->id = (area).count;					\
	(area).count++;								\
}

typedef struct {
	sc_env_t *sc_env_stack;
	area_table functions;
	area_table consts;
} compiler_t;

sc_opcode_t* add_opcode(compiler_t *sc,
		int opcode, int arg, int stack_use);

int next_opcode_idx(compiler_t *sc);
sc_func_t* function_new(compiler_t *sc);
sc_const_t* const_new(compiler_t *sc);
sc_func_t* get_func_by_id(compiler_t *sc, int id);

sc_env_t* sc_env_new(sc_env_t *parent);
void sc_env_free(sc_env_t *env);
void sc_env_stack_push(compiler_t *sc);
void sc_env_stack_pop(compiler_t *sc);
void sc_env_define(sc_env_t *env, const char *name,
		int type, int idx);
load_t *sc_env_lookup(sc_env_t *env, const char *arg);

#endif /* COMPILER_INTERNAL_H */
