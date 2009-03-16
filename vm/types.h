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
#include <stdio.h>
#include <stdint.h>

/*
 * Primitive types that fit in word size
 *
 * All primitive types has a 3-bit type tag.
 * In order to fit a pointer in __WORDSIZE-3, we have to shift pointer value, so
 * it means that pointers should be at least 8 bytes aligned. VM heaps do that,
 * glibc malloc too. Need to check it on other systems...
 */

#define TYPE_TAG unsigned tag:3
#define TYPE_CAST(o, type) ((type)(o))

/**
 * @brief Base object
 */
typedef union {
	TYPE_TAG;
	void *ptr;	/**< Pointer for casting */
} obj_t;

/**
 * @brief Basic type ids
 */
enum {
	id_ptr,		/**< Pointer on a heap-allocated object */
	id_fixnum,	/**< Integer */
	id_char,	/**< Character */
	id_func,	/**< Function pointer */
	id_symbol,	/**< Symbol pointer */
	id_const	/**< Constant */
};

#define DEFINE_TYPE(name, members...) \
	typedef union { \
		struct { \
			TYPE_TAG; \
			members; \
		}; \
		void *ptr; \
		obj_t obj; \
	} name;

/** 
 * @brief constant
 */
typedef union {
	struct {
		TYPE_TAG;
		short id;
	} st;
	void *ptr;
	obj_t obj;
} const_t;

#define DEFINE_CONST(name, nid) \
	static const const_t name = { \
		.st.tag = id_const, \
		.st.id = nid, \
	};

DEFINE_CONST(cnull, 0);
DEFINE_CONST(ctrue, 1);
DEFINE_CONST(cfalse, 2);
DEFINE_CONST(cvoid, 3);

#define CIF(a) ((a) ? ctrue : cfalse)
#define IS_FALSE(obj) ((obj).ptr == cfalse.ptr)
#define IS_TRUE(obj) ((obj).ptr == ctrue.ptr)
#define IS_BOOL(obj) ((obj).tag == id_const && (IS_TRUE(obj) || IS_FALSE(obj)))
#define IS_NULL(obj) ((obj).ptr == cnull.ptr)
#define IS_FUNC(obj) ((obj).tag == id_func || \
		((obj).tag == id_ptr && \
		 (IS_TYPE((obj), t_closure) || IS_TYPE((obj), t_cont))))

/**
 * @brief Tagged poiner repesintation
 *
 * Assuming that pointer is 8-byte aligned,
 * we can use 3 bits for type tag.
 * This type used for both heap and func pointers but
 * use different tags.
 * Use helper macros to init, get and set pointer value.
 */
DEFINE_TYPE(ptr_t, unsigned long addr:__WORDSIZE-3);
#define PTR(o) PTR_GET(TYPE_CAST(o, ptr_t))
#define PTR_SET(p,a) (p).addr = (unsigned long)a >> 3
#define PTR_GET(p) (void*)(unsigned long)((p).addr << 3)
#define PTR_INIT(p, a) { (p).tag = id_ptr; PTR_SET(p, a); }

#define FUNC_INIT(i, v) { (i).tag = id_func; PTR_SET(i, v); }
#define SYMBOL_INIT(i, v) { (i).tag = id_symbol; PTR_SET(i, v); }
#define SYMBOL_TO_CHARP(o) (const char*)PTR(o)

/** 
 * @brief function types
 */
typedef enum {
	func_inter,	/**< Normal interpreted function */
	func_native	/**< Native C-function */
} func_type_t;

/** 
 * @brief Common header for functions
 */
typedef struct {
	func_type_t type;
	uint16_t argc;
	uint16_t swallow:1;
} func_hdr_t;

DEFINE_TYPE(fixnum_t, long val:__WORDSIZE-3);
#define FIXNUM_INIT(n,v) { (n).tag = id_fixnum; (n).val = v; }
#define FIXNUM(o) (long)TYPE_CAST(o, fixnum_t).val

DEFINE_TYPE(char_t, short val);
#define CHAR_INIT(c,v) { (c).tag = id_char; (c).val = v; }
#define CHAR(o) TYPE_CAST(o, char_t).val

/*
 * Utilites
 */

static inline void* make_ptr(void *ptr, int tag)
{
	ptr_t p;
	PTR_SET(p, ptr);
	p.tag = tag;
	return p.ptr;
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
	void (*repr)(void*);
	visitor_fun visit;
} type_t;

/** 
 * @brief Type table
 */
extern const type_t type_table[];
enum { t_env, t_closure, t_cont, t_display, t_pair, t_string };

#endif /* TYPES_H */
