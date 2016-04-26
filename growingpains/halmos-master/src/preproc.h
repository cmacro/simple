#ifndef _HALMOSPREPROC_H_
#define _HALMOSPREPROC_H_
#include "error.h"
struct reader;
struct readerArray;
struct preproc {
  struct readerArray* rs;
  struct reader* r;
  enum error err;
  size_t errCount;
};

void
preprocInit(struct preproc* p);

void
preprocClean(struct preproc* p);

void
preprocParseComment(struct preproc* p, FILE* fOut);

void
preprocParseInclude(struct preproc* p, FILE* fOut);

void
preprocParseFile(struct preproc* p, const char* in, FILE* fOut);

void
preprocCompile(struct preproc* p, const char* in, const char* out);

#endif
