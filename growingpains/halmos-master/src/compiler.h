#ifndef _HALMOSCOMPILER_H_
#define _HALMOSCOMPILER_H_
#include "frame.h"
#include "symtab.h"
struct reader;
struct compiler {
  struct symtree tab;
  struct symbolArray symbols;
  struct frameArray frames;
  struct reader* r;
};
void
compilerInit(struct compiler* cmp);
void 
compilerClean(struct compiler* cmp);
void
compilerCompile(struct compiler* cmp, char* in, char* out);
#endif
