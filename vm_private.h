/*
 * Copyright (C) 2009 - Stepan Zastupov
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
 */
#ifndef VM_PRIVATE_H
#define VM_PRIVATE_H 

typedef struct {
	short up, idx;
} bind_t;

typedef struct module_s module_t;
typedef struct {
	func_type_t type;
	int stack_size;
	short env_size;
	short argc;
	int op_count;
	short heap_env;
	int depth;
	char *opcode;
	bind_t *bindings;
	int bcount;
	int *bindmap;
	int bmcount;
	module_t *module;
} func_t;

struct module_s {
	char *code;
	func_t *functions;
	int entry_point;
	int fun_count;
	obj_t *symbols;
	obj_t *imports;
};

typedef struct {
	hobj_hdr_t hdr;
	int size;
	obj_t *objects;
} env_t;

typedef struct {
	hobj_hdr_t hdr;
	env_t **bindings;
	func_t *func;
} closure_t;

typedef struct {
	heap_t heap;
	hash_table_t sym_table;
	hash_table_t ns_global;

	/* Variables representing execution state */
	int ssize;			/**< Size of stack */
	obj_t *opstack;		/**< Operands stack */
	int op_stack_idx;	/**< Stack index */
	obj_t *objects;	
	env_t *env;
	env_t **display;
	env_t **bindmap;
	func_t *func;
} vm_thread_t;

struct func_hdr_s {
	uint8_t env_size;
	uint8_t argc;
	uint8_t stack_size;
	uint8_t op_count;
	uint8_t heap_env;
	uint8_t depth;
	uint8_t bcount;
	uint8_t bmcount;
} __attribute__((__packed__));

struct module_hdr_s {
	uint32_t import_size;
	uint32_t symbols_size;
	uint8_t fun_count;
	uint8_t entry_point;
} __attribute__((__packed__));

#define MODULE_HDR_OFFSET	sizeof(struct module_hdr_s)
#define FUN_HDR_SIZE sizeof(struct func_hdr_s)


#endif /* VM_PRIVATE_H */
