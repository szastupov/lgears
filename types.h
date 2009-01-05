#ifndef TYPES_H
#define TYPES_H

/*
 * Primitive types that fit in word size
 */

/**
 * @brief Base object
 */
typedef union {
	unsigned int tag:2;	/**< Type tag */
	void *ptr;			/**< Pointer for casting */
} obj_t;

#define NUM_VAL_SIZE	__WORDSIZE-4
/**
 * @brief Basic numeric type
 *
 * Because we loose 2 bits for type tag,
 * num_t contain shift bits
 */
typedef union {
	struct {
		unsigned int tag:2;				/**< Type tag */
		unsigned int shift:2;			/**< Shift bits */
		unsigned long val:NUM_VAL_SIZE;	/**< Value */
	};
	void *ptr;		/*< Pointer for casting */
} num_t;

static const long max_num = ((long)2 << (NUM_VAL_SIZE - 1)) - 1;

/**
 * @rief set value for num_t
 *
 * if val does not more that max_num - we shift it
 */
inline static void num_set(num_t *num, unsigned long val)
{
	num->shift = 0;
	if (val >= max_num) {
		for (num->shift = 1; num->shift <= 3; 
				num->shift++)
		{
			val >>= 2;
			if (val <= max_num)
				break;
		}
		printf("set with shift %d\n", num->shift);
	}
	num->val = val;
	if (num->shift)
		printf("reduced to %lu\n", (unsigned long)num->val);
}

/**
 * @brief Get numeric value
 */
#define num_get(num) (unsigned long)(num.shift ? num.val << ((num).shift*2) : num.val)

/**
 * @brief Basic type ids
 */
enum {
	id_ptr,		/**< Pointer on a heap-allocated object */
	id_int,		/**< Integer */
	id_char,	/**< Character */
	id_func_ptr	/**< Function pointer */
};

#define INIT_INT(i, v) { i.tag = id_int; num_set(&i, v); }
#define INIT_CHAR(i, v) { (i)->tag = id_char; (i)->val = v; }
#define INIT_FUNC_PTR(i, v) { (i)->tag = id_func_ptr; num_set(i, (unsigned long)v); }

static inline int is_false(obj_t obj)
{
	num_t n;
	n.ptr = obj.ptr;
	return obj.tag == id_int && num_get(n) == 0;
}

/*
 * For heap types see heap.h
 */

#endif /* TYPES_H */
