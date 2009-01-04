#ifndef TYPES_H
#define TYPES_H

/**
 * @file types.h
 * @brief Primitive types defination
 */

typedef union {
	unsigned int tag:2;
	void *ptr;
} obj_t;

#define OBJ_CAST(o) (*(obj_t*)o)

#define NUM_VAL_SIZE	__WORDSIZE-5
typedef struct {
	unsigned int tag:2;
	unsigned int shift:3;
	unsigned long val:NUM_VAL_SIZE;
} num_t;

static long max_num = ((long)2 << (NUM_VAL_SIZE - 1)) - 1;

inline static void num_set(num_t *num, unsigned int val)
{
	num->shift = 0;
	if (val >= max_num) {
		int i;
		for (i = 1; i <= 6; i++) {
			val >>= 1;
			if (val <= max_num) {
				num->shift = i;
				break;
			}
		}
		printf("set with shift %d\n", num->shift);
		printf("reduced to %lu\n", (unsigned long)num->val);
	}
	num->val = val;
}

#define num_get(num) ((num)->val << (num)->shift)

enum { id_ptr, id_int, id_char, id_func_ptr };

#define INIT_INT(i, v) { (i)->tag = id_int; num_set(i, v); }
#define INIT_CHAR(i, v) { (i)->tag = id_char; (i)->val = v; }
#define INIT_FUNC_PTR(i, v) { (i)->tag = id_func_ptr; num_set(i, (long)v); }

#define GET_FUNC(obj) (func_t*)(long)GET_INT(obj)
#define GET_INT(obj) num_get((num_t*)&obj)

static inline int is_false(obj_t obj)
{
	return obj.tag == id_int && GET_INT(obj) == 0;
}

#endif /* TYPES_H */
