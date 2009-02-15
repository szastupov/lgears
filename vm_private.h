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

typedef struct module_s module_t;

typedef struct {
	int stack_size;
	int env_size;
	int argc;
	int op_count;
	char *opcode;
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

typedef struct frame_s {
	struct frame_s *prev;
	obj_t	*opstack;
	env_t	*env;
	const func_t *func;
	const char	*opcode;
	int		op_stack_idx;
} frame_t;

typedef struct {
	void *page;
	void *end;
	void *pos;
} lalloc_t;

typedef struct {
	frame_t *frame_stack;
	heap_t heap;
	lalloc_t lalloc;
	hash_table_t sym_table;
	hash_table_t ns_global;
} vm_thread_t;

struct func_hdr_s {
	uint32_t env_size;
	uint32_t argc;
	uint32_t stack_size;
	uint32_t op_count;
} __attribute__((__packed__));

struct module_hdr_s {
	uint32_t import_size;
	uint32_t symbols_size;
	uint32_t fun_count;
	uint32_t entry_point;
} __attribute__((__packed__));

#define MODULE_HDR_OFFSET	sizeof(struct module_hdr_s)
#define FUN_HDR_SIZE sizeof(struct func_hdr_s)


#endif /* VM_PRIVATE_H */
