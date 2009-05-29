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

typedef uintptr_t obj_t;

#define TAG_SIZE 3				/* Use 3 bits for tag */
#define TAG_MASK 7				/* 111 */
#define TAG_UNMASK ((unsigned long)-1-TAG_MASK)

enum {
	TAG_FIXNUM,					/* Fixed number */
	TAG_PTR,					/* Heap pointer */
	TAG_CONST_PTR,				/* Constant area pointer */
	TAG_CHAR,					/* Character */
	TAG_FUNC,					/* Function */
	TAG_CONST					/* Constant */
};

/*
 * VM use two ways to tag an object:
 * 1) Shift and or - used for fixnums and immediate objects, where we
 * can limit size of a value
 * 2) Clear and or - used for pointers which must be properly aligned
 */

#define ADD_TAG(o, tag) (((o) << TAG_SIZE) | tag)
#define SET_TAG(o, tag) ((o) | tag)
#define UNSET_TAG(o) ((o) & TAG_UNMASK)
#define GET_TAG(o) ((o) & TAG_MASK)
#define TEST_TAG(o, tag) (GET_TAG(o) == tag)
#define MAKE_OBJ(o, tag) (obj_t)ADD_TAG(o, tag)

#define FIXNUM(o) ((o) >> TAG_SIZE)
#define MAKE_FIXNUM(v) MAKE_OBJ(v, TAG_FIXNUM)
#define IS_FIXNUM(v) TEST_TAG(v, TAG_FIXNUM)

#define PTR(o) (void*)UNSET_TAG(o)
#define MAKE_TAGGED_PTR(v, tag) SET_TAG(UNSET_TAG((uintptr_t)v), tag)
#define MAKE_HEAP_PTR(v) MAKE_TAGGED_PTR(v, TAG_PTR)
#define MAKE_CONST_PTR(v) MAKE_TAGGED_PTR(v, TAG_CONST_PTR)
#define IS_HEAP_PTR(v) TEST_TAG(v, TAG_PTR)
#define IS_CONST_PTR(v) TEST_TAG(v, TAG_CONST_PTR)

#define CHAR(o) (short)FIXNUM(o)
#define MAKE_CHAR(v) MAKE_OBJ(v, TAG_CHAR)
#define IS_CHAR(v) TEST_TAG(v, TAG_CHAR)

#define MAKE_FUNC(v) MAKE_TAGGED_PTR(v, TAG_FUNC)
#define IS_FUNC(o) (TEST_TAG(o, TAG_FUNC)								\
					|| (IS_HEAP_PTR(o)									\
						&& (IS_TYPE(o, t_closure) || IS_TYPE(o, t_cont))))

#define DEFINE_CONST(name, val) \
	static const obj_t name = MAKE_OBJ(val, TAG_CONST)
#define IS_CONST(v) TEST_TAG(v, TAG_CONST)

DEFINE_CONST(cnull, 0);
DEFINE_CONST(ctrue, 1);
DEFINE_CONST(cfalse, 2);
DEFINE_CONST(cvoid, 3);
DEFINE_CONST(ceof, 4);
#define IS_NULL(o) (o == cnull)
#define IS_TRUE(o) (o == ctrue)
#define IS_FALSE(o) (o == cfalse)
#define IS_BOOL(o) (IS_TRUE(o) || IS_FALSE(o))
#define IS_VOID(o) (o == cvoid)
#define IS_EOF(o) (o == ceof)
#define CIF(a) ((a) ? ctrue : cfalse)

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

/* Don't forget about block_hdr_t::type_id when changing it */
#define VM_MAX_TYPES 16
extern type_t type_table[VM_MAX_TYPES];
int register_type(const char *name, void (*repr)(void*), visitor_fun visit);
extern int t_closure, t_cont;

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

#define IS_TYPE(obj, tid)						\
	((IS_HEAP_PTR(obj) || IS_CONST_PTR(obj))	\
	 && (HTYPE_TAG(PTR(obj)) == tid))

#endif /* TYPES_H */
