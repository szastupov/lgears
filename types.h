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
#ifndef TYPES_H
#define TYPES_H
#include <stdlib.h>

/*
 * Primitive types that fit in word size
 *
 * All primitive types has a 3-bit type tag.
 * In order to fit a pointer in __WORDSIZE-3, we have to shift pointer value, so
 * it means that pointers should be at least 8 bytes aligned. VM heaps do that,
 * glibc malloc too. Need to check it on other systems...
 */

#define TYPE_TAG unsigned tag:3;

/**
 * @brief Base object
 */
typedef union {
	TYPE_TAG;
	void *ptr;			/**< Pointer for casting */
} obj_t;

/**
 * @brief Basic type ids
 */
enum {
	id_ptr,		/**< Pointer on a heap-allocated object */
	id_fixnum,	/**< Integer */
	id_char,	/**< Character */
	id_bool,	/**< Boolean */
	id_func,	/**< Function pointer */
	id_native,	/**< Native function */
	id_symbol	/**< Symbol pointer */
};

/**
 * @brief Tagged poiner repesintation
 *
 * Assuming that pointer is 8-byte aligned,
 * we can use 3 bits for type tag.
 * This type used for both heap and func pointers but
 * use different tags.
 * Use helper macros to init, get and set pointer value.
 */
typedef union {
	struct {
		TYPE_TAG;
		unsigned long addr:__WORDSIZE-3;
	};
	void *ptr;
} ptr_t;

#define ptr_set(p,a) (p)->addr = (unsigned long)a >> 2
#define ptr_get(p) (void*)(unsigned long)((p)->addr << 2)
#define ptr_init(p, a) { (p)->tag = id_ptr; ptr_set(p, a); }
#define return_ptr(a) { ptr_t res; ptr_init(&res, a); return res.ptr; }
#define func_init(i, v) { (i).tag = id_func; ptr_set(&i, v); }
#define native_init(i, v) { (i).tag = id_native; ptr_set(&i, v); }
#define symbol_init(i, v) { (i).tag = id_symbol; ptr_set(&i, v); }

typedef union {
	struct {
		TYPE_TAG;
#if __WORDSIZE == 64
		int val;
#else
		short val;
#endif
	};
	void *ptr;
} fixnum_t;

#define fixnum_init(n,v) { (n).tag = id_fixnum; (n).val = v; }

typedef union {
	struct {
		TYPE_TAG;
		char c;
	};
	void *ptr;
} char_t;

#define char_init(c,v) { (c).tag = id_char; (c).val = v; }

typedef union {
	struct {
		TYPE_TAG;
		short val;
	};
	void *ptr;
} bool_t;

#define bool_init(b,v) { (b).tag = id_bool; (b).val = v; }

/*
 * Utilites
 */

static inline int is_false(obj_t obj)
{
	bool_t b = { .ptr = obj.ptr };
	return obj.tag == id_bool && b.val == 0;
}

static inline void* ptr_from_obj(obj_t obj)
{
	ptr_t p = { .ptr = obj.ptr };
	return ptr_get(&p);
}

static inline int fixnum_from_obj(obj_t obj)
{
	fixnum_t f = { .ptr = obj.ptr };
	return f.val;
}

static inline char char_from_obj(obj_t obj)
{
	char_t c = { .ptr = obj.ptr };
	return c.c;
}

static inline int bool_from_obj(obj_t obj)
{
	bool_t b = { .ptr = obj.ptr };
	return b.val;
}

typedef struct visitor_s {
	void (*visit)(struct visitor_s*, obj_t*);
	void *user_data;
} visitor_t;

typedef void (*visitor_fun)(visitor_t*, void*);

/**
 * @brief Type description for heap-allocated types
 */
typedef struct {
	const char *name;
	void (*destructor)(void*);
	visitor_fun visit;
} type_t;

/*
 * Heap allocated objects always has a hobj_hdr_t header
 * with pointer on type_t type info
 */

typedef struct {
	const type_t *type;
} hobj_hdr_t;

typedef struct native_s native_t;
typedef void* (*native_func)(obj_t *argv);

struct native_s {
	short argc;
	native_func call;
};

#define MAKE_NATIVE(func, fargc) \
	const native_t func##_nt = { \
		.argc = fargc, \
		.call = func \
	}

#endif /* TYPES_H */
