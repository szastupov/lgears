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
 * glibc malloc too.
 * Porting note: if system's malloc does not allign poiters by 8
 * bytes, we may use posix_memalign. Also I haven't tested it on big endian.
 */

#define TYPE_TAG unsigned tag:3
#define TYPE_CAST(o, type) ((type)(o))

/* Fundamental VM object, provide tag and pointer for casting */
typedef union {
	TYPE_TAG;					/* Type tag */
	void *ptr;					/* Pointer for casting */
} obj_t;

/* Basic type identifiers */
enum {
	id_ptr,		/* Pointer on a heap-allocated object */
	id_const_ptr,				/* Pinter on constant object */
	id_fixnum,	/* Integer */
	id_char,	/* Character */
	id_func,	/* Function pointer */
	id_symbol,	/* Symbol pointer */
	id_const	/* Constant */
};

#define DEFINE_TYPE(name, members...)			\
	typedef union {								\
		struct {								\
			TYPE_TAG;							\
			members;							\
		};										\
		void *ptr;								\
		obj_t obj;								\
	} name;

/* Constant values */
typedef union {
	struct {
		TYPE_TAG;
		short id;
	} st;
	void *ptr;
	obj_t obj;
} const_t;

#define DEFINE_CONST(name, nid)					\
	static const const_t name = {				\
		.st.tag = id_const,						\
		.st.id = nid,							\
	};

DEFINE_CONST(cnull, 0);			/* null '() */
DEFINE_CONST(ctrue, 1);			/* true #t */
DEFINE_CONST(cfalse, 2);		/* false #f */
DEFINE_CONST(cvoid, 3);			/* void unspecified */

/* Predicates */
#define CIF(a) ((a) ? ctrue : cfalse)
#define IS_PTR(obj) ((obj).tag == id_ptr)
#define IS_FIXNUM(obj) ((obj).tag == id_fixnum)
#define IS_CHAR(obj) ((obj).tag == id_char)
#define IS_SYMBOL(obj) ((obj).tag == id_symbol)
#define IS_CONST(obj) ((obj).tag == id_const)
#define IS_FALSE(obj) ((obj).ptr == cfalse.ptr)
#define IS_TRUE(obj) ((obj).ptr == ctrue.ptr)
#define IS_BOOL(obj) ((obj).tag == id_const && (IS_TRUE(obj) || IS_FALSE(obj)))
#define IS_NULL(obj) ((obj).ptr == cnull.ptr)
#define IS_FUNC(obj) ((obj).tag == id_func ||							\
					  ((obj).tag == id_ptr &&							\
					   (IS_TYPE((obj), t_closure) || IS_TYPE((obj), t_cont))))

/*
 * Tagged poiner repesintation
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

/* Function types */
typedef enum {
	func_inter,	/* Normal interpreted function */
	func_native	/* Native C-function */
} func_type_t;

/* Common function header */
typedef struct {
	func_type_t type;			/* type */
	uint16_t argc;				/* arguments count */
	uint16_t swallow:1;			/* swallow? */
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

static inline obj_t make_ptr(void *ptr, int tag)
{
	ptr_t p;
	PTR_SET(p, ptr);
	p.tag = tag;
	return p.obj;
}

typedef struct visitor_s {
	void (*visit)(struct visitor_s*, obj_t*);
	void *user_data;
} visitor_t;

typedef void (*visitor_fun)(visitor_t*, void*);

/* Type description for heap-allocated types */
typedef struct {
	const char *name;
	void (*repr)(void*);
	visitor_fun visit;
} type_t;

/* Type table */
extern const type_t type_table[];

enum {
	t_env,
	t_closure,
	t_cont,
	t_pair,
	t_string,
	t_struct,
	t_bytevector
};

/* Header attached to each object on heap */
typedef struct {
	unsigned size;			/* Size of block (with padding if need) */
	unsigned type_id:4;		/* Type id @see type_table */
	unsigned forward:1;	/* Indicate that pointer should be forwarded */
	unsigned generation:2;	  /* Generation */
	unsigned remembered:1;	  /* Indicate that object is remembered */
} block_hdr_t;

#define BHDR_SIZE sizeof(block_hdr_t)
#define HTYPE(ptr) ((block_hdr_t*)((void*)ptr-BHDR_SIZE))
#define HTYPE_TAG(ptr) HTYPE(ptr)->type_id
#define IS_TYPE(obj, tid)									\
	(((obj).tag == id_const_ptr || ((obj).tag == id_ptr))	\
	 && (HTYPE_TAG(PTR(obj)) == tid))

#endif /* TYPES_H */
