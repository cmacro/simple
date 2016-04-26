#include "unittest.h"
#include "reader.h"
#include <string.h>

static int
Test_readerGet(void)
{
  int c;
  const char s[60] =
  "In Xanadu did Kubla Khan\n"
  "A stately pleasure-dome decree :";
  struct reader r;
  readerInitString(&r, s);
  ut_assert(r.err == error_none, "err == %s, expected None",
   errorString(r.err));
  c = readerGet(&r);
  ut_assert(c == 'I', "get() == %c, expected 'I'", c);
  ut_assert(r.err == error_none, ".err == %s, expected None",
   errorString(r.err));
  c = readerGet(&r);
  ut_assert(c == 'n', "get() == %c, expected 'n'", c);
  ut_assert(r.err == error_none, ".err == %s, expected None",
   errorString(r.err));
  int i;
  for (i = 0; i < 55; i++) {
    readerGet(&r);
    ut_assert(r.err == error_none, ".err == %s, expected None",
     errorString(r.err));
  }
  readerGet(&r);
  ut_assert(r.err == error_endOfFile, ".err == %s, expected %s",
    errorString(r.err), errorString(error_endOfFile));
  readerClean(&r);
  return 0;
}

static int
Test_readerGetToken(void)
{
  const char s[] = 
  "Where Alph, the sacred river, ran\n"
  "Through caverns measureless to man\n"
  "Down to a sunless sea.";
  struct reader r;
  readerInitString(&r, s);
  readerGetToken(&r, " ");
  ut_assert(r.err == error_none, "err == %s, expected None",
    errorString(r.err));
  ut_assert(strcmp(r.tok.vals, "Where") == 0,
    "tok == %s, expected Where", r.tok.vals);
  // ut_assert(r.last == 'e', "last == %c, expected e", r.last);
  readerGetToken(&r, " ,");
  ut_assert(r.err == error_none, "err == %s, expected None",
    errorString(r.err));
  ut_assert(strcmp(r.tok.vals, "Alph") == 0,
    "tok == %s, expected Alph", r.tok.vals);
  // ut_assert(r.last == 'h', "last == %c, expected h", r.last);
  readerGetToken(&r, "$");
  // ut_assert(r.last == '.', "last == %c, expected '.'", r.last);
  readerClean(&r);
  size_t size = 1000;
  char t[size + 1];
  memset(t, 'a', size);
  t[size] = '\0';
  readerInitString(&r, t);
  readerGetToken(&r, "");
  ut_assert(r.err == error_endOfFile, ".err == %s, expected %s",
    errorString(r.err), errorString(error_endOfFile));
  t[size - 1] = ' ';
  readerClean(&r);
  readerInitString(&r, t);
  readerGetToken(&r, " ");
  ut_assert(r.err == error_none, ".err == %s, expected %s",
    errorString(r.err), errorString(error_none));
  readerClean(&r);
  return 0;
}

static int
Test_readerSkip(void)
{
  int c;
  const char f[] =
  "So twice five miles of fertile ground\n"
  "With walls and towers were girdled round;";
  struct reader r;
  readerInitString(&r, f);
  readerSkip(&r, "Sotwicefivemiles ");
  ut_assert(r.err == error_none, "err == %s, expected None",
    errorString(r.err));
  ut_assert(r.didSkip != 0, ".didSkip == 0, expected 1");
  ut_assert(r.skipped == 'r', ".skipped == %c, expected r", r.skipped);
  // ut_assert(r.last == 'e', "last == %c, expected e", r.last);
  c = readerGet(&r);
  ut_assert(c == 'r', "get() == %c, expected r", c);
  readerSkip(&r, "tileground\nWithwallsandtowersweregirdledround; ");
  ut_assert(r.err == error_endOfFile, ".err == %s, expected %s",
    errorString(r.err), errorString(error_endOfFile));
  ut_assert(r.last == ';', "last == %c, expected ;", r.last);
  readerClean(&r);
  return 0;
}

static int
Test_readerFind(void)
{
  int c;
  const char f[] =
  "And there were gardens bright with sinuous rills,\n"
  "Where blossomed many many an incense-bearing tree";
  struct reader r;
  readerInitString(&r, f);
  readerFind(&r, "g");
  ut_assert(r.err == error_none, "err == %s, expected None",
    errorString(r.err));
  c = readerGet(&r);
  ut_assert(c == 'g', "Get() == %c, expected g", c);
  readerClean(&r);
  return 0;
}

static int
Test_readerPeek(void)
{
  const char* f = "The yellow fog that rubs its back upon the window-panes,";
  size_t i;
  struct reader r;
  readerInitString(&r, f);
  for (i = 0; i < strlen(f); i++) {
    int c = readerPeek(&r);
    int d = readerGet(&r);
    ut_assert(d == c, "got %c, expected %c", d, c);
  }
  return 0;
}

// static int
// Test_readerOpen(void)
// {
//   const char* filename = "To lead you to an overwhelming question ...";
//   struct reader r;
//   readerOpen(&r, filename, "s");
//   ut_assert(r.err == error_none, "err = %s, expected none",
//     errorString(r.err));
//   return 0;
// }

// static int
// Test_readerClose(void)
// {
//   struct reader r;
//   readerOpen(&r, "Oh, do not ask, \"What is it?\"", "s");
//   readerClose(&r);
//   ut_assert(r.err == error_none, "err = %s, expected none",
//     errorString(r.err));
//   return 0;
// }

static int
all()
{
  ut_run(Test_readerGet);
  ut_run(Test_readerGetToken);
  ut_run(Test_readerSkip);
  ut_run(Test_readerFind);
  ut_run(Test_readerPeek);
  return 0;
}

RUN(all)
