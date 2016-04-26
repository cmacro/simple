#include "dbg.h"
#include "memory.h"
#include <stdlib.h>
void*
xmalloc(size_t size) {
  void* p = malloc(size);
  if (!p) {
    LOG_FAT("malloc failed");
    abort();
  }
  return p;
}

void*
xrealloc(void* p, size_t size) {
  void* q = realloc(p, size);
  if (!q) {
    LOG_FAT("realloc failed");
    abort();
  }
  return q;
}
