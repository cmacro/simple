#ifndef _HALMOSARRAY_H_
#define _HALMOSARRAY_H_

#include "dbg.h"
#include "memory.h"

#define DECLARE_ARRAY(type) \
struct type ## Array { \
  type * vals; \
  size_t size; \
  size_t max; \
}; \
void type ## ArrayInit(struct type ## Array*, size_t); \
void type ## ArrayClean(struct type ## Array*); \
void type ## ArrayResize(struct type ## Array*, size_t); \
void type ## ArrayAdd(struct type ## Array*, type); \
void type ## ArrayAppend(struct type ## Array*, const type*, size_t); \
void type ## ArrayEmpty(struct type ## Array*);

#define DEFINE_ARRAY(type) \
void \
type ## ArrayInit(struct type ## Array* a, size_t max) { \
  a->vals = xmalloc(sizeof(type) * max); \
  a->size = 0; \
  a->max = max; \
} \
void \
type ## ArrayClean(struct type ## Array* a) { \
  free(a->vals); \
  a->vals = NULL; \
  a->size = 0; \
  a->max = 0; \
} \
void \
type ## ArrayResize(struct type ## Array* a, size_t max) { \
  a->vals = xrealloc(a->vals, sizeof(type) * max); \
  a->max = max; \
} \
void \
type ## ArrayAdd(struct type ## Array* a, type v) { \
  if (a->size >= a->max) { \
    type ## ArrayResize(a, a->max * 2); \
  } \
  a->vals[a->size++] = v; \
} \
void \
type ## ArrayAppend(struct type ## Array* a, const type* v, size_t size) { \
  if (a->size + size > a->max) { \
    type ## ArrayResize(a, (a->size + size) * 2); \
  } \
  size_t i; \
  for (i = 0; i < size; i++) { \
    a->vals[a->size++] = v[i]; \
  } \
} \
void \
type ## ArrayEmpty(struct type ## Array* a) { \
  a->size = 0; \
}

DECLARE_ARRAY(char)
DECLARE_ARRAY(int)
DECLARE_ARRAY(size_t)

#endif
