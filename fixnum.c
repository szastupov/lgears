#include "primitives.h"

static int fxsum(heap_t *heap, trampoline_t *tramp, obj_t *argv, int argc)
{
	fixnum_t res;
	FIXNUM_INIT(res, 0);
	int i;
	for (i = 1; i < argc; i++)
		res.val += FIXNUM(argv[i]);

	tramp->arg[0] = res.obj;

	return RC_OK;
}
MAKE_NATIVE(fxsum, 0, 1);

static int fxsub(heap_t *heap, trampoline_t *tramp, obj_t *argv, int argc)
{
	fixnum_t res;
	FIXNUM_INIT(res, FIXNUM(argv[1]));
	int i;
	for (i = 2; i < argc; i++)
		res.val -= FIXNUM(argv[i]);

	tramp->arg[0] = res.obj;

	return RC_OK;
}
MAKE_NATIVE(fxsub, 2, 1);

static int fxmul(heap_t *heap, trampoline_t *tramp, obj_t *argv, int argc)
{
	fixnum_t res;
	FIXNUM_INIT(res, 1);
	int i;
	for (i = 1; i < argc; i++)
		res.val *= FIXNUM(argv[i]);

	tramp->arg[0] = res.obj;

	return RC_OK;
}
MAKE_NATIVE(fxmul, 0, 1);

static int fxdiv(heap_t *heap, trampoline_t *tramp, obj_t *argv, int argc)
{
	fixnum_t res;
	FIXNUM_INIT(res, FIXNUM(argv[1]));
	int i;
	for (i = 2; i < argc; i++)
		res.val /= FIXNUM(argv[i]);

	tramp->arg[0] = res.obj;

	return RC_OK;
}
MAKE_NATIVE(fxdiv, 2, 1);

static int fxeq(heap_t *heap, trampoline_t *tramp, obj_t *argv, int argc)
{
	const_t res = ctrue;

	int i;
	for (i = 2; i < argc; i++)
		if (FIXNUM(argv[i-1]) != FIXNUM(argv[i])) {
			res = cfalse;
			break;
		}

	tramp->arg[0] = res.obj;

	return RC_OK;
}
MAKE_NATIVE(fxeq, 2, 1);


static int fxior(heap_t *heap, trampoline_t *tramp, obj_t *argv, int argc)
{
	fixnum_t res;
	FIXNUM_INIT(res, 0);
	int i;
	for (i = 1; i < argc; i++)
		res.val |= FIXNUM(argv[i]);

	tramp->arg[0] = res.obj;

	return RC_OK;
}
MAKE_NATIVE(fxior, 0, 1);

void ns_install_fixnum(hash_table_t *tbl)
{
	ns_install_native(tbl, "+", &fxsum_nt);
	ns_install_native(tbl, "-", &fxsub_nt);
	ns_install_native(tbl, "*", &fxmul_nt);
	ns_install_native(tbl, "/", &fxdiv_nt);
	ns_install_native(tbl, "=", &fxeq_nt);
	ns_install_native(tbl, "fxior", &fxior_nt);
}
