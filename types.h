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
	id_fixnum,		/**< Integer */
	id_char,	/**< Character */
	id_func_ptr	/**< Function pointer */
};

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
#define init_func_ptr(i, v) { (i).tag = id_func_ptr; ptr_set(&i, v); }

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

static inline int is_false(obj_t obj)
{
	fixnum_t n;
	n.ptr = obj.ptr;
	return obj.tag == id_fixnum && n.val == 0;
}

typedef struct visitor_s {
	void (*visit)(struct visitor_s*, obj_t*);
	void *user_data;
} visitor_t;

typedef void (*visitor_fun)(visitor_t*, void*);

typedef struct {
	const char *name;
	void (*destructor)(void*);
	visitor_fun visit;
} type_t;

typedef struct {
	type_t *type;
} heap_obj_hdr_t;

#endif /* TYPES_H */
