/*
 * This file is part of lGears scheme system
 * Copyright (C) 2009 Stepan Zastupov
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 3 of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General
 * Public Licens along with this program, if not; see
 * <http://www.gnu.org/licenses>.
 */
#include <sys/stat.h>
#include <sys/types.h>
#include <sys/mman.h>
#include <unistd.h>
#include <fcntl.h>
#include <errno.h>
#include "vm.h"
#include "opcodes.h"
#include "string.h"

struct func_hdr_s {
    uint16_t env_size;
    uint16_t argc;
    uint16_t swallow;
    uint16_t op_count;
    uint16_t heap_env;
    uint16_t depth;
    uint16_t bcount;
    uint16_t bmcount;
} __attribute__((__packed__));

struct module_hdr_s {
    uint32_t imports_size;
    uint32_t undefs_size;
    uint32_t consts_size;
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

static void* const_allocator_alloc(allocator_t *al,
                                   size_t size,
                                   int type_id)
{
    const_allocator_t *cal = container_of(al, const_allocator_t, al);
    void *ptr = mem_alloc(sizeof(linked_mem_t)+size);
    linked_mem_t *new = ptr;
    memset(&new->hdr, 0, sizeof(new->hdr));
    new->hdr.size = size;
    new->hdr.type_id = type_id;
    new->next = cal->mem;
    cal->mem = new;

    return ptr+sizeof(linked_mem_t);
}

static void const_allocator_clean(const_allocator_t *al)
{
    linked_mem_t *cur, *next;
    cur = al->mem;
    while (cur) {
        next = cur->next;
        mem_free(cur);
        cur = next;
    }
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

typedef struct {
    const uint8_t *code;
    size_t code_size;
} code_t;

#define CODE_ASSERT(code, sz) ASSERT((code)->code_size >= sz)

static uint8_t code_read8(code_t *code)
{
    CODE_ASSERT(code, 1);
    code->code_size--;
    return *(code->code++);
}

static int64_t code_read64(code_t *code)
{
    CODE_ASSERT(code, sizeof(int64_t));
    code->code_size -= sizeof(int64_t);
    int64_t res = *(int64_t*)code->code;
    code->code += sizeof(int64_t);

    return res;
}

static uint16_t code_read16(code_t *code)
{
    CODE_ASSERT(code, sizeof(uint16_t));
    code->code_size -= sizeof(uint16_t);
    uint16_t res = *(uint16_t*)code->code;
    code->code += sizeof(uint16_t);

    return res;
}

static const void* code_read_bytes(code_t *code, size_t size)
{
    CODE_ASSERT(code, size);
    code->code_size -= size;
    const uint8_t *res = code->code;
    code->code += size;

    return res;
}

static const char* code_read_string(code_t *code)
{
    int len = code_read8(code);
    CODE_ASSERT(code, len);
    code->code_size -= len;
    const char *res = (const char*)code->code;
    code->code += len;

    return res;
}

static void load_consts(module_t *mod, code_t *code)
{
    int ccount = code_read8(code);
    mod->consts = mem_alloc(ccount*sizeof(obj_t));
    int loaded = 0;
#define PUSH_CONST(p) mod->consts[loaded++].ptr = p

    while (loaded < ccount) {
        int type = code_read8(code);
        switch (type) {
        case OT_FIXNUM: {
            fixnum_t n;
            int64_t val = code_read64(code);
            FIXNUM_INIT(n, val);
            PUSH_CONST(n.ptr);
            break;
        }
        case OT_CHARACTER: {
            char_t c;
            uint16_t val = code_read16(code);
            CHAR_INIT(c, val);
            PUSH_CONST(c.ptr);
            break;
        }
        case OT_SYMBOL: {
            const char *str = code_read_string(code);
            void *sym = make_symbol(str);
            PUSH_CONST(sym);
            break;
        }
        case OT_STRING: {
            const char *str = code_read_string(code);
            PUSH_CONST(_string(&mod->allocator.al,
                               (char*)str, 1));
            break;
        }
        default:
            FATAL("unhandled const type: %s\n", object_type_name(type));
        }
    }
}

static void load_undefs(module_t *mod, const char *str, int size)
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

static module_t* module_parse(const uint8_t *code_src, size_t code_size)
{
    code_t code = {
        .code = code_src,
        .code_size = code_size
    };
    module_t *mod = new0(module_t);
    mod->allocator.al.alloc = const_allocator_alloc;
    mod->allocator.al.id = id_const_ptr;

    /* Read module header */
    const struct module_hdr_s *mhdr = code_read_bytes(&code, MODULE_HDR_OFFSET);

    /* Allocate functions storage */
    mod->functions = mem_calloc(mhdr->fun_count, sizeof(func_t));
    mod->fun_count = mhdr->fun_count;
    mod->entry_point = mhdr->entry_point;

    {
        code_t imports = {
            .code = code_read_bytes(&code, mhdr->imports_size),
            .code_size = mhdr->imports_size
        };
    }

    const char *undefs = code_read_bytes(&code, mhdr->undefs_size);
    load_undefs(mod, undefs, mhdr->undefs_size);

    {
        code_t consts = {
            .code = code_read_bytes(&code, mhdr->consts_size),
            .code_size = mhdr->consts_size
        };
        load_consts(mod, &consts);
    }

    int count;
    const struct func_hdr_s *hdr;
    for (count = 0; count < mhdr->fun_count; count++) {
        hdr = code_read_bytes(&code, FUN_HDR_SIZE);
        func_t *func = &mod->functions[count];
        func->hdr.swallow = hdr->swallow;
        func->hdr.type = func_inter;
        func->hdr.argc = hdr->argc;
        func->env_size = hdr->env_size;
        func->op_count = hdr->op_count;
        func->heap_env = hdr->heap_env;
        func->depth = hdr->depth;
        func->bcount = hdr->bcount;
        func->bmcount = hdr->bmcount;
        func->dbg_symbols = NULL;

        if (hdr->bcount) {
            int i;
            const uint16_t *cur;

            int bind_size = hdr->bcount * 4;
            const uint16_t *bindings = code_read_bytes(&code, bind_size);
            func->bindings = mem_calloc(hdr->bcount, sizeof(bind_t));

            int bindmap_size = hdr->bmcount * 2;
            const uint16_t *bindmap = code_read_bytes(&code, bindmap_size);
            func->bindmap = mem_calloc(bindmap_size, sizeof(int));
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

        int opcode_size = hdr->op_count * 4;
        func->opcode = mem_alloc(opcode_size);
        memcpy(func->opcode,
               code_read_bytes(&code, opcode_size),
               opcode_size);
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
    mem_free(module->imports);
    mem_free(module->consts);
    const_allocator_clean(&module->allocator);
    mem_free(module);
}
