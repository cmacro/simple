#include "compiler.h"
#include "reader.h"
void
compilerInit(struct compiler* cmp)
{
  symtreeInit(&cmp->tab);
  symbolArrayInit(&cmp->symbols, 1);
  frameArrayInit(&cmp->frames, 1);
}
void
compilerClean(struct compiler* cmp)
{
  size_t i; 
  for (i = 0; i < cmp->frames.size; i++) {
    frameClean(&cmp->frames.vals[i]);
  }
  frameArrayClean(&cmp->frames);
  for (i = 0; i < cmp->symbols.size; i++) {
    symbolClean(&cmp->symbols.vals[i]);
  }
  symbolArrayClean(&cmp->symbols);
  symtreeClean(&cmp->tab);
}
void
compilerParseFile(struct compiler* cmp)
{
  (void) cmp;
}
void
compilerCompile(struct compiler* cmp, char* in, char* out)
{
  FILE* fin = fopen(in, "r");
  if (fin == NULL) {
    fprintf(stderr, "failed to open %s", in);
  }
  struct reader r;
  readerInitFile(&r, fin, in);
  cmp->r = &r;
  FILE* fout = fopen(out, "w");
  compilerParseFile(cmp);
  readerClean(&r);
  fclose(fin);
  fclose(fout);
}
