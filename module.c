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
#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>
#include <fcntl.h>
#include "vm_private.h"

module_t* module_load(vm_thread_t *thread, const char *path)
{
	module_t *mod;

	void populate_sym_table(const char *str)
	{
		int i;
		int count = *(str++);
		mod->symbols = mem_calloc(count, sizeof(obj_t));
		for (i = 0; i < count; i++) {
			int len = *(str++);
			void *sym = make_symbol(&thread->sym_table, str);
			mod->symbols[i].ptr = sym;
			DBG("Created symbol for '%s' = %p\n", str, sym);
			str += len+1;
		}
	}

	void load_imports(const char *str)
	{
		int i;
		int count = *(str++);
		mod->imports = mem_calloc(count, sizeof(obj_t));
		for (i = 0; i < count; i++) {
			int len = *(str++);
			void *ptr = hash_table_lookup(&thread->ns_global, str);
			if (ptr)
				mod->imports[i].ptr = ptr;
			else
				FATAL("variable %s not found\n", str);
			str += len+1;
		}
	}

	int fd = open(path, O_RDONLY);
	if (fd == -1)
		FATAL("nothing to load\n");

	mod = type_alloc(module_t);

	/* Read module header */
	struct module_hdr_s mhdr;
	if (read(fd, &mhdr, MODULE_HDR_OFFSET) != MODULE_HDR_OFFSET)
		FATAL("Failed to read module header\n");

	/* Allocate functions storage */
	mod->functions = mem_calloc(mhdr.fun_count, sizeof(func_t));
	mod->fun_count = mhdr.fun_count;
	mod->entry_point = mhdr.entry_point;


	char *import = mem_alloc(mhdr.import_size);
	if (read(fd, import, mhdr.import_size) != mhdr.import_size)
		FATAL("Failed to read imports");
	load_imports(import);
	mem_free(import);

	char *symbols = mem_alloc(mhdr.symbols_size);
	if (read(fd, symbols, mhdr.symbols_size) != mhdr.symbols_size)
		FATAL("Failed to read symbols");
	populate_sym_table(symbols);
	mem_free(symbols);

	int count;
	struct func_hdr_s hdr;
	for (count = 0; count < mhdr.fun_count; count++) {
		if (read(fd, &hdr, FUN_HDR_SIZE) != FUN_HDR_SIZE)
			FATAL("Failed to read func header\n");
		func_t *func = &mod->functions[count];
		func->type = func_inter;
		func->env_size = hdr.env_size;
		func->argc = hdr.argc;
		func->stack_size = hdr.stack_size;
		func->op_count = hdr.op_count;
		func->heap_env = hdr.heap_env;
		func->depth = hdr.depth;
		func->bcount = hdr.bcount;
		func->bmcount = hdr.bmcount;

		if (hdr.bcount) {
			int i;
			char *cur;

			int bind_size = hdr.bcount * 2;
			char *bindings = mem_alloc(bind_size);
			func->bindings = mem_calloc(hdr.bcount, sizeof(bind_t));
			if (read(fd, bindings, bind_size) != bind_size)
				FATAL("Failet to read bindings");

			char *bindmap = mem_alloc(hdr.bmcount);
			if (read(fd, bindmap, hdr.bmcount) != hdr.bmcount)
				FATAL("Failed to read bindmap");
			func->bindmap = mem_calloc(hdr.bmcount, sizeof(int));
			cur = bindmap;
			for (i = 0; i < hdr.bmcount; i++) {
				func->bindmap[i] = *(cur++);
			}
			mem_free(bindmap);

			cur = bindings;
			for (i = 0; i < hdr.bcount; i++) {
				func->bindings[i].up = *(cur++);
				func->bindings[i].idx = *(cur++);
			}

			mem_free(bindings);
		} else
			func->bindings = NULL;

		int opcode_size = hdr.op_count * 2;
		func->opcode = mem_alloc(opcode_size);
		if (read(fd, func->opcode, opcode_size) != opcode_size)
			FATAL("Failed to read opcode");
		func->module = mod;
	}

	close(fd);
	return mod;
}

void module_free(module_t *module)
{
	int i;
	for (i = 0; i < module->fun_count; i++) {
		mem_free(module->functions[i].opcode);
		if (module->functions[i].bindings)
			mem_free(module->functions[i].bindings);
		if (module->functions[i].bindmap)
			mem_free(module->functions[i].bindmap);
	}
	mem_free(module->functions);
	mem_free(module->symbols);
	mem_free(module->imports);
	mem_free(module);
}
