#ifndef TYPES_H
#define TYPES_H

/**
 * @file types.h
 * @brief Primitive types defination
 */

typedef void* obj_t;
#define OBJ_CAST(o) (*(obj_t*)o)

#define TAG_SIZE	3
#define MAX_LONG	__WORDSIZE-TAG_SIZE
#define TYPE_HDR	int tag:TAG_SIZE

#define DEFINE_TYPE(name, members...)	\
	typedef struct {					\
		TYPE_HDR;						\
		members;						\
	} name

enum { id_ptr, id_int, id_char, id_func_ptr };

typedef union {
	TYPE_HDR;
	obj_t obj;
} empty_t;

/*
 * Primitives that fit in wordsize
 */
DEFINE_TYPE(ptr_t, long val:MAX_LONG);
DEFINE_TYPE(int_t, long val:MAX_LONG);
DEFINE_TYPE(char_t, char val);
DEFINE_TYPE(func_ptr_t, long val:MAX_LONG);

#define GET_TAG(obj) ((empty_t*)obj)->tag
#define INIT_PTR(i, v) { (i)->tag = id_ptr; (i)->val = v; }
#define INIT_INT(i, v) { (i)->tag = id_int; (i)->val = v; }
#define INIT_CHAR(i, v) { (i)->tag = id_char; (i)->val = v; }
#define INIT_FUNC_PTR(i, v) { (i)->tag = id_func_ptr; (i)->val = (long)v; }

#define GET_FUNC(obj) (func_t*)(long)((func_ptr_t*)&obj)->val
#define GET_INT(obj) ((int_t*)&obj)->val
#define INT_CAST(obj) ((int_t*)obj)
#define IS_FALSE(obj) (INT_CAST(obj)->tag == id_int && INT_CAST(obj)->val == 0)

static inline int is_false(obj_t obj)
{
	return INT_CAST(&obj)->tag == id_int && GET_INT(obj) == 0;
}

#endif /* TYPES_H */
