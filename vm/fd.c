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
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
#include "native.h"
#include "string.h"

/*
 * File descriptors
 * Export only low level descriptros,
 * r6rs port system will be build on it
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
	SAFE_ASSERT(mode > 0 && mode < sizeof(fd_modes)/sizeof(int));
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
	SAFE_ASSERT(IS_STRING(*data));

	string_t *str = PTR(*data);
	int wrote = write(FIXNUM(*fd), str->str, str->size-1);

	RETURN_FIXNUM(wrote);
}
MAKE_NATIVE_BINARY(fd_write);

void fd_init()
{
	ns_install_global("fd-open", &fd_open_nt);
	ns_install_global("fd-close", &fd_close_nt);
	ns_install_global("fd-seek", &fd_seek_nt);
	ns_install_global("fd-write", &fd_write_nt);
}
