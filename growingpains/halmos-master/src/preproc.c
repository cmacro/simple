#include "array.h"
#include "dbg.h"
#include "logger.h"
#include "preproc.h"
#include "reader.h"

static const char* whitespace = " \f\n\r\t";

void
preprocInit(struct preproc* p)
{
  p->rs = xmalloc(sizeof(struct readerArray));
  readerArrayInit(p->rs, 1);
  /* add an empty reader, required for P_LOG */
  struct reader r;
  readerInitString(&r, "");
  readerArrayAdd(p->rs, r);
  p->r = &p->rs->vals[0];
  p->err = error_none;
  p->errCount = 0;
}

void
preprocClean(struct preproc* p)
{
  size_t i;
  for (i = 0; i < p->rs->size; i++) {
    readerClean(&p->rs->vals[i]);
  }
  readerArrayClean(p->rs);
  free(p->rs);
}

void
preprocSetError(struct preproc* p, enum error err)
{
  p->err = err;
  p->errCount++;
}

int
preprocIsFresh(const struct preproc* p, const char* filename)
{
  size_t i;
  for (i = 0; i < p->rs->size; i++) {
    if (strcmp(readerGetFilename(&p->rs->vals[i]), filename) == 0) {
      return 0;
    }
  }
  return 1;
}

char*
preprocParseSymbol(struct preproc* p, int* isEnd, int end)
{
  readerSkip(p->r, whitespace);
  char* tok = readerGetToken(p->r, whitespace);
  size_t len = strlen(tok);
  if (len == 2) {
    if ((tok[0] == '$') && (tok[1] == end)) {
      *isEnd = 1;
    }
  }
  if (p->r->err && !isEnd) {
    P_LOG_ERR(p, error_unterminatedFileInclusion, 
      "reached end of file before $]");
  }
  return tok;
}

void
preprocParseComment(struct preproc* p, FILE* fOut)
{
  while (!p->r->err) {
    readerFind(p->r, "$\n");
    int c = readerGet(p->r);
    if (p->r->err) { break; }
    if (c == '\n') {
/* emit a newline to keep line number in sync */
      fprintf(fOut, "\n");
      continue;
    }
/* look at the char after $ */
    c = readerGet(p->r);
    if (p->r->err) { break; }
    if (c == ')') { break; }
    
    if (c == '(') {
      P_LOG_ERR(p, error_nestedComment, "comments cannot be nested");
      p->err = error_none;
      preprocParseComment(p, fOut);
      break;
    }
  }
  if (p->r->err) {
    P_LOG_ERR(p, error_unterminatedComment, "reached end of file before $)");
  }
}

void
preprocParseInclude(struct preproc* p, FILE* fOut)
{
  int isEnd = 0;
  char* tok = preprocParseSymbol(p, &isEnd, ']');
  if (p->r->err) {
    return;
  }
/* if it is a new file, parse it */
  if (preprocIsFresh(p, tok)) {
    struct reader* r = p->r;
    preprocParseFile(p, tok, fOut);
    p->r = r;
/* leave a special comment to indicate we are going back to the original */
/* file */
    fprintf(fOut, "$( %s %lu $)\n", p->r->filename.vals, p->r->line);
  }
/* find $] */
  tok = preprocParseSymbol(p, &isEnd, ']');
  if (p->r->err) {
    return;
  }
  if (!isEnd) {
    P_LOG_ERR(p, error_expectedClosingBracket, "%s found instead of $]", tok);
    while (!p->r->err) {
      readerFind(p->r, "$");
      tok = preprocParseSymbol(p, &isEnd, ']');
      if (p->r->err) { break; }
      if (isEnd) { break; }
    }
    if (p->r->err) {
      P_LOG_ERR(p, error_unterminatedFileInclusion, 
        "reached end of file before $]");
    }
  }
}

void
preprocParseFile(struct preproc* p, const char* in, FILE* fOut)
{
  FILE* fIn = fopen(in, "r");
  if (fIn == NULL) {
    P_LOG_ERR(p, error_failedOpenFile, "failed to open input file %s", in);
    return;
  }
/* add the file to the list and begin reading it */
  struct reader r;
  readerInitFile(&r, fIn, in);
  readerArrayAdd(p->rs, r);
  p->r = &r;
/* leave a special comment for indicating file name and line. This is used */
/* by the verifier when reporting errors */
  fprintf(fOut, "$( %s 0 $)\n", in);
  while (!p->r->err) {
    int c = readerGet(p->r);
    if (p->r->err) { break; }
    if (c == '$') {
      c = readerGet(p->r);
      if (p->r->err) { break; }
      if (c == '(') {
        preprocParseComment(p, fOut);
      } else if (c == '[') {
        preprocParseInclude(p, fOut);
      } else {
        fprintf(fOut, "$%c", c);
      }
    } else {
      fprintf(fOut, "%c", c);
    }
  }
  fclose(fIn);
}

void
preprocCompile(struct preproc* p, const char* in, const char* out)
{
  FILE* fOut = fopen(out, "w");
  if (!fOut) {
    P_LOG_ERR(p, error_failedOpenFile, "failed to open output file %s", out);
    return;
  }
  preprocParseFile(p, in, fOut);
  fclose(fOut);
}
