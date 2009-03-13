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
#include <sys/mman.h>
#include <unistd.h>
#include <fcntl.h>
#include <errno.h>
#include "vm.h"

struct func_hdr_s {
	uint16_t env_size;
	uint16_t argc;
	uint16_t stack_size;
	uint16_t op_count;
	uint16_t heap_env;
	uint16_t depth;
	uint16_t bcount;
	uint16_t bmcount;
} __attribute__((__packed__));

struct module_hdr_s {
	uint32_t import_size;
	uint32_t symbols_size;
	uint32_t strings_size;
	uint16_t fun_count;
	uint16_t entry_point;
} __attribute__((__packed__));

#define MODULE_HDR_OFFSET	sizeof(struct module_hdr_s)
#define FUN_HDR_SIZE sizeof(struct func_hdr_s)

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

typedef struct {
	const char *buf;
	const char *cur;
	const char *end;
} string_iter_t;

void string_iter_init(string_iter_t *itr, const char *buf, int size)
{
	itr->end = buf+size;
	itr->cur = buf;
	itr->buf = buf;
}

const char* string_iter_next(string_iter_t *itr)
{
	const char *res = itr->cur;
	while (1) {
		if (itr->cur == itr->end)
			return NULL;
		if (*(itr->cur++))
			continue;
		else
			return res;
	}
}


static module_t* module_parse(const uint8_t *code, size_t code_size)
{
	module_t *mod = new0(module_t);
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

	void populate_sym_table(const char *str, int size)
	{
		int i;
		int count = *(str++);
		mod->symbols = mem_calloc(count, sizeof(obj_t));

		string_iter_t itr;
		string_iter_init(&itr, str, size);
		for (i = 0; i < count; i++) {
			const char *s = string_iter_next(&itr);
			void *sym = make_symbol(&sym_table, s);
			mod->symbols[i].ptr = sym;
			LOG_DBG("Created symbol for '%s' = %p\n", s, sym);
		}
	}

	void load_imports(const char *str, int size)
	{
		int i;
		int count = *(str++);
		mod->imports = mem_calloc(count, sizeof(obj_t));

		string_iter_t itr;
		string_iter_init(&itr, str, size);
		for (i = 0; i < count; i++) {
			const char *import = string_iter_next(&itr);
			void *ptr = hash_table_lookup(&ns_global, import);
			if (ptr)
				mod->imports[i].ptr = ptr;
			else
				FATAL("variable %s not found\n", import);
		}
	}

	void load_strings(const char *str, int size)
	{
		int i;
		int count = *(str++);
		mod->strings = mem_calloc(count, sizeof(char*));

		string_iter_t itr;
		string_iter_init(&itr, str, size);
		for (i = 0; i < count; i++) {
			const char *string = string_iter_next(&itr);
			LOG_DBG("loaded string '%s'\n", string);
			mod->strings[i] = strdup(string);
		}
	}

	/* Read module header */
	const struct module_hdr_s *mhdr = code_assign(MODULE_HDR_OFFSET);

	/* Allocate functions storage */
	mod->functions = mem_calloc(mhdr->fun_count, sizeof(func_t));
	mod->fun_count = mhdr->fun_count;
	mod->entry_point = mhdr->entry_point;


	const char *import = code_assign(mhdr->import_size);
	load_imports(import, mhdr->import_size);

	const char *symbols = code_assign(mhdr->symbols_size);
	populate_sym_table(symbols, mhdr->symbols_size);

	const char *strings = code_assign(mhdr->strings_size);
	load_strings(strings, mhdr->strings_size);

	int count;
	const struct func_hdr_s *hdr;
	for (count = 0; count < mhdr->fun_count; count++) {
		hdr = code_assign(FUN_HDR_SIZE);
		func_t *func = &mod->functions[count];
		func->hdr.swallow = 0;
		func->hdr.type = func_inter;
		func->hdr.argc = hdr->argc;
		func->env_size = hdr->env_size;
		func->stack_size = hdr->stack_size;
		func->op_count = hdr->op_count;
		func->heap_env = hdr->heap_env;
		func->depth = hdr->depth;
		func->bcount = hdr->bcount;
		func->bmcount = hdr->bmcount;
		func->dbg_symbols = NULL;

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

	if (mapfile(path, &map) == -1)
		FATAL("Failed to read %s\n", path);

	module_t *mod = module_parse(map.addr, map.size); //TODO add error check
	munmap(map.addr, map.size);

	char *dbg_path = mem_alloc(strlen(path)+5);
	sprintf(dbg_path, "%s.dbg", path);
	struct stat st;
	if (stat(dbg_path, &st) == 0) {
		int fd = open(dbg_path, O_RDONLY);
		int i;
		uint16_t dsize = 0;
		for (i = 0; i < mod->fun_count; i++) {
			func_t *func = &mod->functions[i];
			read(fd, &dsize, sizeof(dsize));
			char *buf = mem_alloc(dsize);
			func->dbg_symbols = buf;
			func->dbg_table = mem_alloc(func->op_count*sizeof(char*));
			read(fd, buf, dsize);
			string_iter_t itr;
			string_iter_init(&itr, buf, dsize);
			int j;
			for (j = 0; j < func->op_count; j++) {
				func->dbg_table[j] = string_iter_next(&itr);
			}
		}
		close(fd);
	}
	mem_free(dbg_path);

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
		func_t *func = &module->functions[i];
		mem_free(func->opcode);
		if (func->bindings)
			mem_free(func->bindings);
		if (func->bindmap)
			mem_free(func->bindmap);
		if (func->dbg_symbols) {
			mem_free(func->dbg_symbols);
			mem_free(func->dbg_table);
		}
	}
	mem_free(module->functions);
	mem_free(module->symbols);
	mem_free(module->imports);
	mem_free(module->strings);
	mem_free(module);
}
