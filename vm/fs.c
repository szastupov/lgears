/*
 * This file is part of lGears scheme system
 * Copyright (C) 2009 Stepan Zastupov <redchrom@gmail.com>
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
#include <unistd.h>
#include <limits.h>

#include "native.h"
#include "struct.h"
#include "string.h"

static int fs_stat(vm_thread_t *thread, obj_t *opath)
{
	SAFE_ASSERT(IS_STRING(*opath));

	string_t *path = PTR(*opath);
	struct stat st = {0};

	if (stat(path->str, &st) == 0) {
		obj_t type_name = make_symbol("stat");
		struct_t *pstat = struct_new(&thread->heap.allocator,
									 &type_name,
									 10);
		fixnum_t fx;
		fx.tag = id_fixnum;
#define ST_SET(idx,v)							\
		fx.val = v;								\
		pstat->fields[idx] = fx.obj;

		ST_SET(0, st.st_dev);
		ST_SET(1, st.st_ino);
		ST_SET(2, st.st_mode);
		ST_SET(3, st.st_nlink);
		ST_SET(4, st.st_uid);
		ST_SET(5, st.st_gid);
		ST_SET(6, st.st_size);
		ST_SET(7, st.st_atime);
		ST_SET(8, st.st_mtime);
		ST_SET(9, st.st_ctime);

		RETURN_OBJ(make_ptr(pstat, id_ptr));
	} else {
		RETURN_OBJ(cfalse.obj);
	}
}
MAKE_NATIVE_UNARY(fs_stat);

static int fs_remove(vm_thread_t *thread, obj_t *opath)
{
	SAFE_ASSERT(IS_STRING(*opath));

	string_t *path = PTR(*opath);
	RETURN_BOOL(remove(path->str) == 0);
}
MAKE_NATIVE_UNARY(fs_remove);

static int fs_getcwd(vm_thread_t *thread)
{
	char path[PATH_MAX];		/* FIXME */
	if (getcwd(path, sizeof(path))) {
		RETURN_OBJ(_string(&thread->heap.allocator,
						   path,
						   1));
	} else {
		RETURN_OBJ(cfalse.obj);
	}
}
MAKE_NATIVE_NULLARY(fs_getcwd);

void ns_install_fs(hash_table_t *tbl)
{
	ns_install_native(tbl, "fs-stat", &fs_stat_nt);
	ns_install_native(tbl, "fs-remove", &fs_remove_nt);
	ns_install_native(tbl, "fs-getcwd", &fs_getcwd_nt);
}
