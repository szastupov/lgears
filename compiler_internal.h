#ifndef COMPILER_INTERNAL_H
#define COMPILER_INTERNAL_H 

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
} inter_func_t;

typedef struct {
	list_node_t next;	/**< Next opcode */
	int idx;			/**< Index relative to function */
	int code;			/**< id of code */
	int arg;			/**< argument */
} inter_opcode_t;

typedef struct {
	list_node_t next;	/**< Next constant */
	int id;				/**< Constant id in table */
	int type;
	char *data;
} inter_const_t;

typedef struct env_s {
	struct env_s *parent;	/**< Pointer on parent env */
	tree_node_t	*tbl;		/**< symbol table */
	inter_func_t *func;		/**< function of current env */
	inter_func_t *top;		/**< top function */
} env_t;

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
	env_t *env_stack;
	area_table functions;
	area_table consts;
} compiler_t;

static void compile(compiler_t *sc, ast_node_t *node);

#endif /* COMPILER_INTERNAL_H */
