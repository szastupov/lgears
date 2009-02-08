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
#ifndef BYTECODE_H
#define BYTECODE_H 
#include "opcodes.h"
#include <stdint.h>

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
} __attribute__((__packed__));

#define NO_ARG -1

#define MODULE_HDR_OFFSET	sizeof(struct module_hdr_s)
#define FUN_HDR_SIZE sizeof(struct func_hdr_s)
#define CODE_START_OFFSET(count) count * FUN_HDR_SIZE + MODULE_HDR_OFFSET
#define FUN_SEEK(fd, id) lseek(fd, id * FUN_HDR_SIZE + MODULE_HDR_OFFSET, SEEK_SET)

#endif /* BYTECODE_H */
