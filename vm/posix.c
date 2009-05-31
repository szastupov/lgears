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
#include <fcntl.h>
#include <unistd.h>
#include <limits.h>
#include <dirent.h>
#include <errno.h>

#include "native.h"
#include "struct.h"
#include "strings.h"

/*
 * This module pretend to be a typesafe interface to posix operations.
 * Only basic amount of operations is supported, but you always may
 * use ffi at you own risk :)
 */

/* Remember? we are safe, so we have to make some types */
int t_dir;

typedef struct {
	DIR *dir;
	obj_t opath;
} dir_t;

static void dir_repr(void *ptr)
{
	dir_t *dir = ptr;
	string_t *path = PTR(dir->opath);
	printf("<dir: %s>", path->str);
}

static void dir_visit(visitor_t *vs, void *data)
{
	dir_t *dir = data;
	vs->visit(vs, &dir->opath);
}

/*
 * Stat call, scheme code should provide syntactic sugar
 * TODO: export inode type helpers
 */
static int fs_stat(vm_thread_t *thread, obj_t *opath)
{
	SAFE_ASSERT(IS_STRING(*opath));

	struct stat st = {0};

	if (stat(CSTRING(*opath), &st) == 0) {
		obj_t type_name = make_symbol("posix-stat");
		struct_t *pstat = struct_new(&thread->heap.allocator,
									 &type_name,
									 10);
#define ST_SET(idx,v) pstat->fields[idx] = MAKE_FIXNUM(v);

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

		RETURN_OBJ(MAKE_HEAP_PTR(pstat));
	} else {
		RETURN_OBJ(cfalse);
	}
}
MAKE_NATIVE_UNARY(fs_stat);

static int fs_remove(vm_thread_t *thread, obj_t *opath)
{
	SAFE_ASSERT(IS_STRING(*opath));
	RETURN_BOOL(remove(CSTRING(*opath)) == 0);
}
MAKE_NATIVE_UNARY(fs_remove);

static int fs_getcwd(vm_thread_t *thread)
{
	char path[PATH_MAX];
	if (getcwd(path, sizeof(path))) {
		RETURN_OBJ(_string(&thread->heap.allocator,
						   path,
						   1));
	} else {
		RETURN_OBJ(cfalse);
	}
}
MAKE_NATIVE_NULLARY(fs_getcwd);

static int fs_opendir(vm_thread_t *thread, obj_t *opath)
{
	SAFE_ASSERT(IS_STRING(*opath));

	DIR *dir = opendir(CSTRING(*opath));
	if (dir) {
		dir_t *dt = heap_alloc(&thread->heap, sizeof(dir_t), t_dir);
		dt->opath = *opath;
		dt->dir = dir;
		RETURN_OBJ(MAKE_HEAP_PTR(dt));
	} else {
		RETURN_OBJ(cfalse);
	}
}
MAKE_NATIVE_UNARY(fs_opendir);

static int fs_closedir(vm_thread_t *thread, obj_t *odir)
{
	SAFE_ASSERT(IS_TYPE(*odir, t_dir));
	dir_t *dt = PTR(*odir);

	RETURN_BOOL(closedir(dt->dir) == 0);
}
MAKE_NATIVE_UNARY(fs_closedir);

static int fs_readdir(vm_thread_t *thread, obj_t *odir)
{
	SAFE_ASSERT(IS_TYPE(*odir, t_dir));
	dir_t *dt = PTR(*odir);
	struct dirent *de = readdir(dt->dir);
	if (de) {
		RETURN_OBJ(_string(&thread->heap.allocator,
					de->d_name, 1));
	} else {
		RETURN_OBJ(ceof);
	}
}
MAKE_NATIVE_UNARY(fs_readdir);

static int fs_chdir(vm_thread_t *thread, obj_t *opath)
{
	SAFE_ASSERT(IS_STRING(*opath));
	RETURN_BOOL(chdir(CSTRING(*opath)) == 0);
}
MAKE_NATIVE_UNARY(fs_chdir);

static int fs_mkdir(vm_thread_t *thread, obj_t *opath, obj_t *omode)
{
	SAFE_ASSERT(IS_STRING(*opath));
	SAFE_ASSERT(IS_FIXNUM(*omode));
	const char *path = CSTRING(*opath);
	mode_t mode = FIXNUM(*omode);

	RETURN_BOOL(mkdir(path, mode) == 0);
}
MAKE_NATIVE_BINARY(fs_mkdir);

static int os_getenv(vm_thread_t *thread, obj_t *okey)
{
	SAFE_ASSERT(IS_STRING(*okey));
	char *res = getenv(CSTRING(*okey));
	if (res) {
		RETURN_OBJ(_string(&thread->heap.allocator, res, 1));
	} else {
		RETURN_OBJ(cfalse);
	}
}
MAKE_NATIVE_UNARY(os_getenv);

static int os_unsetenv(vm_thread_t *thread, obj_t *okey)
{
	SAFE_ASSERT(IS_STRING(*okey));
	RETURN_BOOL(unsetenv(CSTRING(*okey)) == 0);
}
MAKE_NATIVE_UNARY(os_unsetenv);

static int os_setenv(vm_thread_t *thread, obj_t *okey,
					 obj_t *oval, obj_t *overwrite)
{
	SAFE_ASSERT(IS_STRING(*okey));
	SAFE_ASSERT(IS_STRING(*oval));
	const char *key = CSTRING(*okey);
	const char *val = CSTRING(*oval);

	RETURN_BOOL(setenv(key, val, !IS_FALSE(*overwrite)) == 0);
}
MAKE_NATIVE_TERNARY(os_setenv);

static int os_system(vm_thread_t *thread, obj_t *ocommand)
{
	SAFE_ASSERT(IS_STRING(*ocommand));
	string_t *command = PTR(*ocommand);

	RETURN_FIXNUM(system(command->str));
}
MAKE_NATIVE_UNARY(os_system);

static int os_errno(vm_thread_t *thread)
{
	RETURN_FIXNUM(errno);
}
MAKE_NATIVE_NULLARY(os_errno);

static int os_strerror(vm_thread_t *thread, obj_t *oerrnum)
{
	SAFE_ASSERT(IS_FIXNUM(*oerrnum));
	char *descr = strerror(FIXNUM(*oerrnum));

	RETURN_OBJ(_string(&thread->heap.allocator, descr, 0));
}
MAKE_NATIVE_UNARY(os_strerror);

/*
 * Unix file descriptors interface
 */

static int fd_modes[] = {
	O_RDONLY,					/* r */
	O_RDWR,						/* r+ */
	O_WRONLY|O_TRUNC|O_CREAT,	/* w */
	O_RDWR|O_TRUNC|O_CREAT,		/* w+ */
	O_WRONLY|O_APPEND|O_CREAT,	/* a */
	O_RDWR|O_APPEND|O_CREAT,	/* a+ */
};

static int fd_open(vm_thread_t *thread, obj_t *opath, obj_t *omode)
{
	SAFE_ASSERT(IS_STRING(*opath));
	SAFE_ASSERT(IS_FIXNUM(*omode));

	string_t *path = PTR(*opath);
	int mode = FIXNUM(*omode);
	SAFE_ASSERT(mode >= 0 && mode < sizeof(fd_modes)/sizeof(int));
	int fd = open(path->str, fd_modes[mode], S_IRWXU);

	RETURN_FIXNUM(fd);
}
MAKE_NATIVE_BINARY(fd_open);

static int fd_close(vm_thread_t *thread, obj_t *fd)
{
	SAFE_ASSERT(IS_FIXNUM(*fd));
	RETURN_FIXNUM(close(FIXNUM(*fd)));
}
MAKE_NATIVE_UNARY(fd_close);

static int fd_seek(vm_thread_t *thread, obj_t *fd, obj_t *offt, obj_t *omode)
{
	SAFE_ASSERT(IS_FIXNUM(*fd));
	SAFE_ASSERT(IS_FIXNUM(*omode));
	SAFE_ASSERT(IS_FIXNUM(*offt));

	int mode = 0;
	switch (FIXNUM(*omode)) {
		case 0:
			mode = SEEK_SET;
			break;
		case 1:
			mode = SEEK_CUR;
			break;
		case 2:
			mode = SEEK_END;
			break;
		default:
			mode = SEEK_SET;
	}

	off_t offset = lseek(FIXNUM(*fd), FIXNUM(*offt), mode);
	RETURN_FIXNUM(offset);
}
MAKE_NATIVE_TERNARY(fd_seek);

static int fd_write(vm_thread_t *thread, obj_t *fd, obj_t *data)
{
	SAFE_ASSERT(IS_FIXNUM(*fd));
	SAFE_ASSERT(IS_STRING(*data)); /* TODO: accept bytevectors too */

	string_t *str = PTR(*data);
	int wrote = write(FIXNUM(*fd), str->str, str->size-1);

	RETURN_FIXNUM(wrote);
}
MAKE_NATIVE_BINARY(fd_write);

static int fd_read_string(vm_thread_t *thread, obj_t *ofd, obj_t *odest)
{
	SAFE_ASSERT(IS_FIXNUM(*ofd));
	SAFE_ASSERT(IS_STRING(*odest));
	SAFE_ASSERT(IS_HEAP_PTR(*odest));

	int fd = FIXNUM(*ofd);
	string_t *str = PTR(*odest);
	SAFE_ASSERT(str->allocated);

	int rec = read(fd, str->str, str->size-1);
	if (rec > 0) {
		RETURN_FIXNUM(rec);
	} else {
		RETURN_OBJ(ceof);
	}
}
MAKE_NATIVE_BINARY(fd_read_string);

void posix_init()
{
	t_dir = register_type("directory", dir_repr, dir_visit);

	ns_install_global("fs-stat", &fs_stat_nt);
	ns_install_global("fs-remove", &fs_remove_nt);
	ns_install_global("fs-getcwd", &fs_getcwd_nt);
	ns_install_global("fs-opendir", &fs_opendir_nt);
	ns_install_global("fs-closedir", &fs_closedir_nt);
	ns_install_global("fs-readdir", &fs_readdir_nt);
	ns_install_global("fs-chdir", &fs_chdir_nt);
	ns_install_global("fs-mkdir", &fs_mkdir_nt);

	ns_install_global("os-getenv", &os_getenv_nt);
	ns_install_global("os-unsetenv", &os_unsetenv_nt);
	ns_install_global("os-setenv", &os_setenv_nt);
	ns_install_global("os-system", &os_system_nt);
	ns_install_global("os-errno", &os_errno_nt);
	ns_install_global("os-strerror", &os_strerror_nt);

	ns_install_global("fd-write", &fd_write_nt);
	ns_install_global("fd-seek", &fd_seek_nt);
	ns_install_global("fd-open", &fd_open_nt);
	ns_install_global("fd-close", &fd_close_nt);
	ns_install_global("fd-read-string!", &fd_read_string_nt);
}