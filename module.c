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
#include <errno.h>
#include "vm.h"

typedef struct {
	void *addr;
	size_t size;
} map_t;

int mapfile(const char *path, map_t *map)
{
	int fd = open(path, O_RDONLY);
	if (fd == -1) {
		fprintf(stderr, "Failed to open file %s : %s\n", path, strerror(errno));
		return -1;
	}
	struct stat st;
	fstat(fd, &st);

	void *res = mmap(NULL, st.st_size, PROT_READ, MAP_SHARED, fd, 0);
	int ret = -1;
	if (res == MAP_FAILED)
		fprintf(stderr, "mmap() failed %s\n", strerror(errno));
	else {
		ret = 0;
		map->addr = res;
		map->size = st.st_size;
	}
	close(fd);

	return ret;
}

static module_t* module_parse(const uint8_t *code, size_t code_size)
{
	module_t *mod = type_alloc(module_t);
	int codecpy(void *dest, size_t size)
	{
		if (size > code_size)
			return -1;
		memcpy(dest, code, size);
		code += size;
		code_size -= size;

		return 0;
	}

	const void* code_assign(size_t size)
	{
		if (size > code_size)
			return NULL;
		const void *res = code;
		code += size;
		code_size -= size;

		return res;
	}

	void populate_sym_table(const char *str)
	{
		int i;
		int count = *(str++);
		mod->symbols = mem_calloc(count, sizeof(obj_t));
		for (i = 0; i < count; i++) {
			int len = *(str++);
			void *sym = make_symbol(&sym_table, str);
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
			void *ptr = hash_table_lookup(&ns_global, str);
			if (ptr)
				mod->imports[i].ptr = ptr;
			else
				FATAL("variable %s not found\n", str);
			str += len+1;
		}
	}

	/* Read module header */
	const struct module_hdr_s *mhdr = code_assign(MODULE_HDR_OFFSET);

	/* Allocate functions storage */
	mod->functions = mem_calloc(mhdr->fun_count, sizeof(func_t));
	mod->fun_count = mhdr->fun_count;
	mod->entry_point = mhdr->entry_point;


	const void *import = code_assign(mhdr->import_size);
	load_imports(import);

	const void *symbols = code_assign(mhdr->symbols_size);
	populate_sym_table(symbols);

	int count;
	const struct func_hdr_s *hdr;
	for (count = 0; count < mhdr->fun_count; count++) {
		hdr = code_assign(FUN_HDR_SIZE);
		func_t *func = &mod->functions[count];
		func->type = func_inter;
		func->env_size = hdr->env_size;
		func->argc = hdr->argc;
		func->stack_size = hdr->stack_size;
		func->op_count = hdr->op_count;
		func->heap_env = hdr->heap_env;
		func->depth = hdr->depth;
		func->bcount = hdr->bcount;
		func->bmcount = hdr->bmcount;

		if (hdr->bcount) {
			int i;
			const char *cur;

			int bind_size = hdr->bcount * 2;
			const char *bindings = code_assign(bind_size);
			func->bindings = mem_calloc(hdr->bcount, sizeof(bind_t));

			const char *bindmap = code_assign(hdr->bmcount);
			func->bindmap = mem_calloc(hdr->bmcount, sizeof(int));
			cur = bindmap;
			for (i = 0; i < hdr->bmcount; i++) {
				func->bindmap[i] = *(cur++);
			}

			cur = bindings;
			for (i = 0; i < hdr->bcount; i++) {
				func->bindings[i].up = *(cur++);
				func->bindings[i].idx = *(cur++);
			}

		} else
			func->bindings = NULL;

		int opcode_size = hdr->op_count * 2;
		func->opcode = mem_alloc(opcode_size);
		if (codecpy(func->opcode, opcode_size) != 0)
			FATAL("Failed to read opcode");
		func->module = mod;
	}

	return mod;
}

module_t* module_load(const char *path)
{
	map_t map;

	mapfile(path, &map);
	module_t *mod = module_parse(map.addr, map.size); //TODO add error check
	munmap(map.addr, map.size);

	return mod;
}

module_t* module_load_static(const uint8_t *mem, int size)
{
	return module_parse(mem, size);
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
