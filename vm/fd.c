
/* 
 * File descriptors
 * Export only low level descriptros,
 * r6rs port system will be build on it
 */

typedef struct {
	char str[3];
	int mode;
} fd_mode_t;

static const fd_mode_t fd_modes[] = {
	{ "r", O_RDONLY },
	{ "r+", O_RDWR },
	{ "w", O_WRONLY|O_TRUNC|O_CREAT },
	{ "w+", O_RDWR|O_TRUNC|O_CREAT },
	{ "a", O_WRONLY|O_APPEND|O_CREAT },
	{ "a+", O_RDWR|O_APPEND|O_CREAT },
};

int fd_parse_mode(const char *str)
{
	int i;
	for (i = 0; i < sizeof(fd_modes)/sizeof(fd_mode_t); i++)
		if (strncmp(str, fd_modes[i].str, 2) == 0)
			return fd_modes[i].mode;
	return fd_modes[0].mode;
}

static int fd_open(vm_thread_t *thread, obj_t *ostring, obj_t *omode)
{
	string_t *path_str = get_typed(*ostring, t_string);
	SAFE_ASSERT(path_str != NULL);
	string_t *mode_str = get_typed(*omode, t_string);
	SAFE_ASSERT(mode_str != NULL);

	int mode = fd_parse_mode(mode_str->str);
	int fd = open(path_str->str, mode);

	RESULT_FIXNUM(fd);
}
MAKE_NATIVE_BINARY(fd_open);

static int fd_close(vm_thread_t *thread, obj_t *fd)
{
	RESULT_FIXNUM(close(FIXNUM(*fd)));
}
MAKE_NATIVE_UNARY(fd_close);

static int fd_seek(vm_thread_t *thread, obj_t *fd, obj_t *offt, obj_t *omode)
{
	SAFE_ASSERT(omode->tag == id_fixnum);
	SAFE_ASSERT(offt->tag == id_fixnum);

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
	RESULT_FIXNUM(offset);
}
MAKE_NATIVE_TERNARY(fd_seek);

static int fd_write(vm_thread_t *thread, obj_t *fd, obj_t *data)
{
	SAFE_ASSERT(fd->tag == id_fixnum);
	SAFE_ASSERT(data->tag == id_ptr);

	string_t *str = get_typed(*data, t_string);
	if (!str)
		RESULT_ERROR("argument is not a string\n");

	int wrote = write(FIXNUM(*fd), str->str, str->size-1);

	RESULT_FIXNUM(wrote);
}
MAKE_NATIVE_BINARY(fd_write);


