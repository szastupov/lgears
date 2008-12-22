#ifndef STRING_UTIL_H
#define STRING_UTIL_H 

#include <string.h>
#include "streams.h"

typedef struct {
	char *buf;
	size_t allocated;
	size_t len;
} string_t;

string_t* string_alloc(size_t len);
string_t* string_new(const char *src);
void string_free(string_t *str);
void string_clear(string_t *str);
void string_append(string_t *string, char *str);
void string_append_char(string_t *string, char c);
#define string_empty(str) (str->len == 0)
#define string_char(str) str->buf

DEFINE_STREAM(string, string_t *str; int pos; char c);
string_stream_t* str_stream(string_t *string);


#endif /* STRING_UTIL_H */
