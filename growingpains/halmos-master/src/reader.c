#include "dbg.h"
#include "reader.h"
#include <string.h>

static const int mode_none = 0;
static const int mode_string = 1;
static const int mode_file = 2;

//int
//readerGetString(struct reader* r)
//{
//  if (*r->stream.s == '\0') {
//    r->err = error_endOfString;
//    return '\0';
//  }
//  return *(r->stream.s++);
//}
int
readerGetFile(struct reader* r)
{
  if (r->bufferPos >= r->bufferSize) {
    r->buffer[0] = EOF;
    r->bufferSize = fread(r->buffer, sizeof(char), reader_bufferSize, 
        r->f);
    //if (r->bufferSize == 0) {
      //r->err = error_endOfFile;
      //return EOF;
    //}
    r->bufferPos = 0;
  }
  // int c = getc(r->stream.f);
  return r->buffer[r->bufferPos++];
}

void
readerInit(struct reader* r)
{
  charArrayInit(&r->tok, 256);
  charArrayInit(&r->filename, 256);
  memset(r->buffer, 0, reader_bufferSize);
  r->f = NULL;
  r->bufferSize = 0;
  r->bufferPos = 0;
  r->line = 1;
  r->offset = 0;
  r->skipped = 0;
  r->didSkip = 0;
  r->last = 0;
  r->mode = mode_none;
  //r->get = NULL;
  r->err = error_none;
}

void
readerInitString(struct reader* r, const char* s)
{
  readerInit(r);
  charArrayAdd(&r->filename, '\0');
/* a hack. We buffer the string to pretend it is a file */
  size_t len = strlen(s) + 1;
  if (len > reader_bufferSize) { len = reader_bufferSize; }
  strncpy((char*)r->buffer, s, len);
  r->bufferSize = len;
/* the size includes the \0 character */
  DEBUG_ASSERT(r->bufferSize > 0, "bad buffersize");
  r->buffer[r->bufferSize - 1] = EOF;
  r->bufferPos = 0;
  //r->stream.s = s;
  r->mode = mode_string;
  //r->get = &readerGetString;
}

void
readerInitFile(struct reader* r, FILE* f, const char* filename)
{
  readerInit(r);
  charArrayAppend(&r->filename , filename, strlen(filename) + 1);
  r->f = f;
  r->mode = mode_file;
  //r->get = &readerGetFile;
}

void
readerClean(struct reader* r)
{
  charArrayClean(&r->tok);
  charArrayClean(&r->filename);
}

char*
readerGetFilename(struct reader* r)
{
  return r->filename.vals;
}

int
readerGet(struct reader* r)
{
  if (r->didSkip) {
    r->didSkip = 0;
    return r->skipped;
  }
/* read a character */
  if (r->bufferPos >= r->bufferSize) {
    if (r->err) { return EOF; }
    r->buffer[0] = EOF;
    r->bufferSize = fread((char*)r->buffer, sizeof(char), reader_bufferSize, 
        r->f);
    //if (r->bufferSize == 0) {
      //r->err = error_endOfFile;
      //return EOF;
    //}
    r->bufferPos = 0;
  }
  int c = r->buffer[r->bufferPos++];
  // int c = getc(r->stream.f);
  //int c = (*r->get)(r);
  if (c == EOF) {
    r->err = error_endOfFile;
  } else if (c == '\n') {
    r->line++;
    r->offset = 0;
  } else {
    r->offset++;
  }
  return c;
}

int
readerPeek(struct reader* r)
{
  if (r->didSkip) {
    return r->skipped;
  } else {
    int c = readerGet(r);
    r->didSkip = 1;
    r->skipped = c;
    return c;
  }
}

char*
readerGetToken(struct reader* r, const char* delimiters)
{
  charArrayEmpty(&r->tok);
  while (1) {
    int c = readerGet(r);
    if (r->err != error_none) {
      charArrayAdd(&r->tok, '\0');
      break;
    }
    if (strchr(delimiters, c)) {
      charArrayAdd(&r->tok, '\0');
      r->last = c;
      break;
    }
    charArrayAdd(&r->tok, c);
    r->last = c;
  }
  return r->tok.vals;
}

void
readerSkipExplicit(struct reader* r, const char* s, int skipOnMatch)
{
  while (1) {
    int c = readerGet(r);
    if (r->err != error_none) {
      break;
    }
    char* isMatch = strchr(s, c);
    if ((isMatch && !skipOnMatch) || (!isMatch && skipOnMatch)) {
      r->skipped = c;
      r->didSkip = 1;
      break;
    }
    r->last = c;
  }
}

void
readerSkip(struct reader* r, const char* skip)
{
  readerSkipExplicit(r, skip, 1);
}

void
readerFind(struct reader* r, const char* find)
{
  readerSkipExplicit(r, find, 0);
}

DEFINE_ARRAY(reader)
