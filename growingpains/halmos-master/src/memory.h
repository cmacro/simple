#ifndef _HALMOSMEMORY_H_
#define _HALMOSMEMORY_H_
#include <stddef.h>
void* xmalloc(size_t size);
void* xrealloc(void* p, size_t size);
#endif
