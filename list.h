#ifndef LIST_H
#define LIST_H 

#include "memory.h"

typedef struct _list_node {
	struct _list_node *next;
} list_node_t;

typedef struct {
	list_node_t *head;
	list_node_t *tail;
} list_t;

#define list_head(lst) (lst)->head
#define list_tail(lst) (lst)->tail
#define list_init(lst) memset((lst), 0, sizeof(list_t))

static inline void list_append(list_t *lst,
		list_node_t *new)
{
	new->next = NULL;
	if (list_tail(lst))
		list_tail(lst)->next = new;
	list_tail(lst) = new;

	if (!list_head(lst))
		list_head(lst) = new;
}

static inline void list_prepend(list_t *lst,
		list_node_t *new)
{
	new->next = list_head(lst);
	list_head(lst) = new;
	if (!list_tail(lst))
		list_tail(lst) = new;
}

#define list_for_each(lst, cur)	\
	for (cur = list_head(lst); cur != NULL; cur = (cur)->next)

#define list_for_each_entry(lst, iter, member)							\
	for (iter = container_of(list_head(lst), typeof(*iter), member);	\
			&iter->member != NULL; iter = container_of((iter)->member.next, typeof(*iter), member))

#define node_next(n) (n ? (n)->next : NULL)

#define list_for_each_safe(lst, cur, save)				\
	for (cur = list_head(lst), save = node_next(cur);	\
			cur != NULL; cur = save, save = node_next(cur))

typedef list_t stack_t;

typedef struct {
	list_node_t lst;
	void *ptr;
} stack_itm_t;

static inline void stack_push(stack_t *stack, void *ptr)
{
	stack_itm_t *itm = type_alloc(stack_itm_t);
	itm->ptr = ptr;
	list_prepend(stack, &itm->lst);
}

#define stack_head(stack) list_head(stack)

static inline void stack_pop(stack_t *stack)
{
	list_node_t *next = list_head(stack)->next;
	stack_itm_t *itm = container_of(stack_head(stack), stack_itm_t, lst);
	free(itm);
	stack_head(stack) = next;
}

#define stack_head_ptr(stack)	\
	container_of(stack_head(stack), stack_itm_t, lst)->ptr;

#define stack_head_itm(stack, type)	\
	(type*)stack_head_ptr(stack)

#endif /* LIST_H */
