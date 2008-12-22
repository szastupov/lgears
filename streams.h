#ifndef STREAMS_H
#define STREAMS_H 
#include "memory.h"

enum {
	STREAM_RUN,
	STREAM_STOPED
};

#define DEFINE_STREAM(name, members...)				\
	typedef struct __##name##_stream {				\
		members;									\
		void (*next)(struct __##name##_stream*);	\
		void (*destroy)(struct __##name##_stream*);	\
		int	state;									\
	} name##_stream_t

#define stream_next(stream) ((stream)->next(stream))

#define stream_run(stream) ((stream)->state == STREAM_RUN)

#define stream_iter(stream)	\
	for (stream_next(stream); stream_run(stream); stream_next(stream))

#define stream_free(stream) {		\
	if ((stream)->destroy)			\
		(stream)->destroy(stream);	\
	mem_free(stream);				\
}

#endif /* STREAMS_H */
