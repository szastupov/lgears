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
