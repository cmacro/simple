/* verifier_tests.c */
/* 15 Oct 2015 */
#include "unittest.h"
#include "reader.h"
#include "verifier.h"
#include "frame.h"

#define check_err(actual, expected) \
do { \
  ut_assert(actual == expected, "err = %s, expected %s", errorString(actual), \
   errorString(expected)); \
} while (0)

static int
Test_frameInit(void)
{
  struct frame frm;
  frameInit(&frm);
  ut_assert(frm.stmts.size == 0, "size == %lu, expected 0", frm.stmts.size);
  frameClean(&frm);
  return 0;
}

static int
Test_verifierInit(void)
{
  struct verifier vrf;
  verifierInit(&vrf);
/* check we don't leak with valgrind */
  verifierClean(&vrf);
  return 0;
}

static int
Test_verifierAddSymbol(void)
{
  enum { test_size = 12 };
  const char* names[test_size] = {
    "c1", "c1", "c1",
    "fad$sfgs", "$v",
    "abcdefghijklmnopqrstuvwxyz!@#%^&*()_+\\][{}|,./<>\?",
    "hleraiugh5&*", "@*#(Nfad", "*#V#HFU(",
    "=-)F", "_-.fhakl", "_-.fhakl"
  };
  enum symType types[test_size] = {
    symType_constant, symType_constant, symType_variable,
    symType_constant, symType_variable,
    symType_variable,
    symType_floating, symType_essential, symType_assertion,
    symType_provable, symType_floating, symType_essential
  };
  enum error errs[test_size] = {
    error_none, error_duplicateSymbol, error_duplicateSymbol,
    error_invalidSymbol, error_invalidSymbol,
    error_none,
    error_invalidLabel, error_invalidLabel, error_invalidLabel,
    error_invalidLabel, error_none, error_duplicateSymbol
  };
  size_t i;
  struct verifier vrf;
  verifierInit(&vrf);
  struct reader r;
  readerInitString(&r, "");
  verifierBeginReadingFile(&vrf, &r);
  for (i = 0; i < test_size; i++) {
    LOG_DEBUG("test %lu", i);
    vrf.err = error_none;
    verifierAddSymbol(&vrf, names[i], types[i]);
    check_err(vrf.err, errs[i]);
  }
  readerClean(&r);
  verifierClean(&vrf);
  return 0;
}

static int
Test_verifierGetSymId(void)
{
  enum { test_size = 4, symbol_size = 4 };
  const char* syms[symbol_size] = {
    "c1", "c2", "c3", "c3"
  };
  const size_t isActive[symbol_size] = {
    1, 1, 0, 1
  };
  const char* query[test_size] = {
    "c1", "c2", "c3", "undef"
  };
  const size_t expId[test_size] = {
    1, 2, 4, 0
  };
  struct reader r;
  readerInitString(&r, "");
  struct verifier vrf;
  verifierInit(&vrf);
  verifierBeginReadingFile(&vrf, &r);
  size_t i;
  for (i = 0; i < symbol_size; i++) {
    size_t symId = verifierAddSymbol(&vrf, syms[i], symType_constant);
    vrf.symbols.vals[symId].isActive = isActive[i];
  }
  for (i = 0; i < test_size; i++) {
    size_t symId = verifierGetSymId(&vrf, query[i]);
    ut_assert(symId == expId[i], "got %lu, expected %lu", symId, expId[i]);
  }
  verifierClean(&vrf);
  readerClean(&r);
  return 0;
} 

static int
Test_verifierDeactivateSymbols(void)
{
  struct verifier vrf;
  struct reader r;
  verifierInit(&vrf);
  readerInitString(&r, "");
  verifierBeginReadingFile(&vrf, &r);
  verifierDeactivateSymbols(&vrf);
  check_err(vrf.err, error_none);
  readerClean(&r);
  verifierClean(&vrf);
  return 0;
}

static int
Test_verifierAddDisjoint(void)
{
  enum {
    disjoints_size = 5
  };
  const size_t disjoints[disjoints_size] = {0, 1, 2, 3, 4};
  struct symstring d;
  symstringInit(&d);
  size_tArrayAppend(&d, disjoints, 5);
  struct verifier vrf;
  verifierInit(&vrf);
  struct reader r;
  readerInitString(&r, "");
  verifierBeginReadingFile(&vrf, &r);
/* add scope to bypass assertion */
  size_tArrayAdd(&vrf.disjointScope, 0);
  verifierAddDisjoint(&vrf, &d);
  check_err(vrf.err, error_none);
  size_t size = vrf.symCount[symType_disjoint];
  ut_assert(size == 10, 
    "added %lu disjoint pairs, expected 10", size);
  readerClean(&r);
  verifierClean(&vrf);
  return 0;
}

static int
Test_verifierGetVariables(void)
{
  struct verifier vrf;
  verifierInit(&vrf);
  struct reader r;
  readerInitString(&r, "");
  verifierBeginReadingFile(&vrf, &r);
  size_t zero = verifierAddConstant(&vrf, "0");
  size_t x = verifierAddVariable(&vrf, "x");
  size_t y = verifierAddVariable(&vrf, "y");
  size_t z = verifierAddVariable(&vrf, "z");
  struct symstring stmt;
  symstringInit(&stmt);
  symstringAdd(&stmt, zero);
  symstringAdd(&stmt, x);
  symstringAdd(&stmt, y);
  struct symstring stmt2;
  symstringInit(&stmt2);
  symstringAdd(&stmt2, zero);
  symstringAdd(&stmt2, z);
  struct symstring set;
  symstringInit(&set);
  verifierGetVariables(&vrf, &set, &stmt);
  check_err(vrf.err, error_none);
  ut_assert(set.size == 2, "set size = %lu, expected 2", set.size);
/* check we don't add duplicate symbols */
  verifierGetVariables(&vrf, &set, &stmt2);
  check_err(vrf.err, error_none);
  ut_assert(set.size == 3, "set size = %lu, expected 3", set.size);
  symstringClean(&stmt);
  symstringClean(&stmt2);
  symstringClean(&set);
  readerClean(&r);
  verifierClean(&vrf);
  return 0;
}

static int 
Test_verifierMakeFrame(void)
{
  size_t i;
  enum {
    cst_size = 5,
    var_size = 4,
    flt_size = 4,
    ess_size = 1
  };
  const char* cstSyms[cst_size] = {"|-", "wff", "0", "1", "+"};
  size_t cstIds[cst_size];
  const char* varSyms[var_size] = {"x", "y", "z", "w"};
  size_t varIds[var_size];
  const char* fltSyms[flt_size] = {"wff_x", "wff_y", "wff_z", "wff_w"};
  size_t fltIds[flt_size];
  size_t fltSS[flt_size][2];
  const char* essSyms[ess_size] = {"plus"};
  // size_t essIds[ess_size];
  struct verifier vrf;
  verifierInit(&vrf);
/* make a mock file because verifierAdd... requires vrf->rId to be valid */
  LOG_DEBUG("making mock file because verifierAdd...() requires valid "
    "vrf->rId");
  struct reader file;
  readerInitString(&file, "");
  verifierBeginReadingFile(&vrf, &file);
  size_tArrayAdd(&vrf.disjointScope, 0);
  LOG_DEBUG("adding constants");
  for (i = 0; i < cst_size; i++) {
    cstIds[i] = verifierAddConstant(&vrf, cstSyms[i]);
  }
  LOG_DEBUG("adding variables");
  for (i = 0; i < var_size; i++) {
    varIds[i] = verifierAddVariable(&vrf, varSyms[i]);
  }
  ut_assert(vrf.symCount[symType_variable] == var_size,
    "added %lu variables, expected %d", vrf.symCount[symType_variable],
    var_size);
  LOG_DEBUG("preparing floats");
  struct symstring wff[flt_size];
  for (i = 0; i < flt_size; i++) {
    fltSS[i][0] = cstIds[1];
    fltSS[i][1] = varIds[i];
    symstringInit(&wff[i]);
    size_tArrayAppend(&wff[i], fltSS[i], 2);
  }
  LOG_DEBUG("preparing essentials");
  struct symstring plus;
  size_t ssplus[4] = {cstIds[0], varIds[0], cstIds[4], varIds[1]};
  symstringInit(&plus);
  size_tArrayAppend(&plus, ssplus, 4);
  LOG_DEBUG("adding floats and essentials");
  fltIds[0] = verifierAddFloating(&vrf, fltSyms[0], &wff[0]);
  check_err(vrf.err, error_none);
  fltIds[1] = verifierAddFloating(&vrf, fltSyms[1], &wff[1]);
  check_err(vrf.err, error_none);
  verifierAddEssential(&vrf, essSyms[0], &plus);
  check_err(vrf.err, error_none);
  fltIds[2] = verifierAddFloating(&vrf, fltSyms[2], &wff[2]);
  check_err(vrf.err, error_none);
  fltIds[3] = verifierAddFloating(&vrf, fltSyms[3], &wff[3]);
  check_err(vrf.err, error_none);
  ut_assert(vrf.symCount[symType_floating] == 4, "added %lu floating, "
    "expected 4", vrf.symCount[symType_floating]);
  ut_assert(vrf.symCount[symType_essential] == 1, "added %lu essential, "
    "expected 1", vrf.symCount[symType_essential]);
  ut_assert(vrf.hypotheses.size == 5, "added %lu "
    "hypotheses, expected 5", vrf.hypotheses.size);
  LOG_DEBUG("preparing disjoint");
  struct symstring disjoint;
  symstringInit(&disjoint);
  symstringAdd(&disjoint, varIds[0]);
  symstringAdd(&disjoint, varIds[1]);
  LOG_DEBUG("adding disjoint");
  verifierAddDisjoint(&vrf, &disjoint);
  ut_assert(vrf.disjoint1.size == 1, "added %lu disjoints, "
    "expected 1", vrf.disjoint1.size);
  LOG_DEBUG("preparing assertion");
  struct symstring asr;
  symstringInit(&asr);
  size_t ssasr[6] =
  {cstIds[0], varIds[0], cstIds[4], varIds[1], cstIds[4], varIds[2]};
  size_tArrayAppend(&asr, ssasr, 6);
  LOG_DEBUG("making the frame");
  struct frame frm;
  frameInit(&frm);
  LOG_DEBUG("calling verifierMakeFrame()");
  verifierMakeFrame(&vrf, &frm, &asr);
  check_err(vrf.err, error_none);
  ut_assert(frm.disjoint1.size == 1, "%lu disjoints in frame, expected 1",
    frm.disjoint1.size);
  LOG_DEBUG("make sure frame is the right size");
  ut_assert(frm.stmts.size == 4, "frame has size %lu, expected 4",
    frm.stmts.size);
  LOG_DEBUG("make sure wff_w has not been added to the frame");
  for (i = 0; i < frm.stmts.size; i++) {
    ut_assert(frm.stmts.vals[i] != fltIds[3],
      "wff_w should not be in the frame");
  }
  LOG_DEBUG("cleaning up");
  frameClean(&frm);
  symstringClean(&asr);
  readerClean(&file);
  verifierClean(&vrf);
  return 0;
}

static int
Test_verifierIsValidDisjointPairSubstitution(void)
{
  struct verifier vrf;
  struct frame frm;
  struct substitution sub;
  verifierInit(&vrf);
  struct reader r;
  readerInitString(&r, "");
  verifierBeginReadingFile(&vrf, &r);
  size_tArrayAdd(&vrf.disjointScope, 0);
  LOG_DEBUG("adding symbols");
  size_t v1 = verifierAddSymbol(&vrf, "v1", symType_variable);
  size_t v2 = verifierAddSymbol(&vrf, "v2", symType_variable);
  size_t v3 = verifierAddSymbol(&vrf, "v3", symType_variable);
  LOG_DEBUG("adding disjoints");
  frameInit(&frm);
  frameAddDisjoint(&frm, v1, v2);
  LOG_DEBUG("building the substitution manually");
  struct symstring str1, str2;
  symstringInit(&str1);
  symstringInit(&str2);
  symstringAdd(&str1, v3);
  symstringAdd(&str2, v3);
  substitutionInit(&sub);
  substitutionAdd(&sub, v1, &str1);
  substitutionAdd(&sub, v2, &str2);
  struct frame ctx;
  frameInit(&ctx);
  frameAddDisjoint(&ctx, v1, v2);
  ut_assert(!verifierIsValidDisjointPairSubstitution(&vrf, &ctx, &frm, &sub,
   0, 1), "sub should be invalid");
  frameClean(&ctx);
/* str1 and str2 cleaned here */
  substitutionClean(&sub);
  frameClean(&frm);
  frameInit(&frm);
  frameAddDisjoint(&frm, v1, v2);
  symstringInit(&str1);
  symstringInit(&str2);
  symstringAdd(&str1, v1);
  symstringAdd(&str1, v2);
  symstringAdd(&str2, v3);
  substitutionInit(&sub);
  substitutionAdd(&sub, v1, &str1);
  substitutionAdd(&sub, v2, &str2);
  frameInit(&ctx);
  frameAddDisjoint(&ctx, v1, v2);
  ut_assert(!verifierIsValidDisjointPairSubstitution(&vrf, &ctx, &frm, &sub,
    0, 1), "sub should be invalid");
  frameAddDisjoint(&frm, v1, v3);
  frameAddDisjoint(&ctx, v1, v3);
  ut_assert(!verifierIsValidDisjointPairSubstitution(&vrf, &ctx, &frm, &sub,
   0, 1), "sub should be invalid");
  frameAddDisjoint(&frm, v2, v3);
  ut_assert(!verifierIsValidDisjointPairSubstitution(&vrf, &ctx, &frm, &sub,
   0, 1), "sub should be invalid");
  frameAddDisjoint(&ctx, v2, v3);
  ut_assert(verifierIsValidDisjointPairSubstitution(&vrf, &ctx, &frm, &sub, 
    0, 1), "sub should be valid");
  substitutionClean(&sub);
  frameClean(&ctx);
  frameClean(&frm);
  readerClean(&r);
  verifierClean(&vrf);
  return 0;
}

static int
Test_verifierIsValidSubstitution(void)
{
  struct verifier vrf;
  struct substitution sub;
  struct frame frm;
  struct frame ctx;
  verifierInit(&vrf);
  struct reader r;
  readerInitString(&r, "");
  verifierBeginReadingFile(&vrf, &r);
  size_tArrayAdd(&vrf.disjointScope, 0);
  size_t v1 = verifierAddSymbol(&vrf, "v1", symType_variable);
  size_t v2 = verifierAddSymbol(&vrf, "v2", symType_variable);
  size_t v3 = verifierAddSymbol(&vrf, "v3", symType_variable);
  frameInit(&frm);
  frameInit(&ctx);
/* build the substitution */
  struct symstring str1, str2, str3;
  symstringInit(&str1);
  symstringInit(&str2);
  symstringInit(&str3);
  symstringAdd(&str1, v1);
  symstringAdd(&str2, v1);
  symstringAdd(&str3, v3);
  substitutionInit(&sub);
  substitutionAdd(&sub, v1, &str1);
  substitutionAdd(&sub, v2, &str2);
  substitutionAdd(&sub, v3, &str3);
  ut_assert(verifierIsValidSubstitution(&vrf, &ctx, &frm, &sub),
   "invalid sub");
  frameAddDisjoint(&frm, v2, v3);
  ut_assert(!verifierIsValidSubstitution(&vrf, &ctx, &frm, &sub),
    "sub should be invalid");
  frameAddDisjoint(&ctx, v2, v3);
  ut_assert(!verifierIsValidSubstitution(&vrf, &ctx, &frm, &sub),
    "sub should be invalid");
  frameAddDisjoint(&frm, v1, v3);
  ut_assert(!verifierIsValidSubstitution(&vrf, &ctx, &frm, &sub),
    "sub should be invalid");
  frameAddDisjoint(&ctx, v1, v3);
  ut_assert(verifierIsValidSubstitution(&vrf, &ctx, &frm, &sub),
   "invalid sub");
  substitutionClean(&sub);
  frameClean(&ctx);
  frameClean(&frm);
  readerClean(&r);
  verifierClean(&vrf);
  return 0;
}

static int
Test_verifierUnify(void)
{
  struct verifier vrf;
  struct reader r;
  struct substitution sub;
  struct symstring str;
  struct symstring floating;
  verifierInit(&vrf);
  readerInitString(&r, "");
  verifierBeginReadingFile(&vrf, &r);
  size_t type = verifierAddSymbol(&vrf, "type", symType_constant);
  vrf.symbols.vals[type].isActive = 0;
  size_t var = verifierAddSymbol(&vrf, "var", symType_variable);
  vrf.symbols.vals[var].isActive = 0;
  size_t type2 = verifierAddSymbol(&vrf, "type2", symType_constant);
  vrf.symbols.vals[type2].isActive = 0;
  size_t strVals[4] = {type, var, var, var};
  size_t floatingVals[2] = {type, var};
  size_t floatingVals2[2] = {type2, var};
  symstringInit(&str);
  symstringInit(&floating);
  size_tArrayAppend(&str, strVals, 4);
  size_tArrayAppend(&floating, floatingVals, 2);
  substitutionInit(&sub);
  verifierUnify(&vrf, &sub, &str, &floating);
  ut_assert(!vrf.err, "unification failed");
  symstringClean(&floating);
  symstringInit(&floating);
  symstringClean(&str);
  symstringInit(&str);
  size_tArrayAppend(&str, strVals, 4);
  size_tArrayAppend(&floating, floatingVals2, 2);
  verifierUnify(&vrf, &sub, &str, &floating);
  ut_assert(vrf.err == error_mismatchedType, "unifciation should have failed");
  symstringClean(&str);
  symstringClean(&floating);
  substitutionClean(&sub);
  readerClean(&r);
  verifierClean(&vrf);
  return 0;
}

static int
Test_verifierApplyAssertion(void)
{
  const size_t t = 1;
  const size_t x = 2;
  const size_t y = 3;
  const size_t a = 4;
  const size_t b = 5;
  // const size_t tx = 6;
  // const size_t ty = 7;
  // const size_t txy = 8;
  const size_t tyx = 9;
  const size_t tx_f[2] = {t, x};
  const size_t ty_f[2] = {t, y};
  const size_t txy_e[3] = {t, x, y};
  const size_t tyx_a[3] = {t, y, x};
  // const size_t frame_a[3] = {tx, ty, txy};
  const size_t stack1[2] = {t, a};
  const size_t stack2[2] = {t, b};
  const size_t stack3[3] = {t, a, b};
  const size_t res[3] = {t, b, a};
  struct verifier vrf;
  verifierInit(&vrf);
  struct reader r;
  readerInitString(&r, "");
  verifierBeginReadingFile(&vrf, &r);
  struct symstring stmt;
  LOG_DEBUG("add constant");
  verifierAddConstant(&vrf, "t");
  LOG_DEBUG("add variables");
  verifierAddVariable(&vrf, "x");
  verifierAddVariable(&vrf, "y");
  verifierAddVariable(&vrf, "a");
  verifierAddVariable(&vrf, "b");
  LOG_DEBUG("add floatings");
  symstringInit(&stmt);
  size_tArrayAppend(&stmt, tx_f, 2);
  verifierAddFloating(&vrf, "tx", &stmt);
  symstringInit(&stmt);
  size_tArrayAppend(&stmt, ty_f, 2);
  verifierAddFloating(&vrf, "ty", &stmt);
  LOG_DEBUG("add essential");
  symstringInit(&stmt);
  size_tArrayAppend(&stmt, txy_e, 3);
  verifierAddEssential(&vrf, "txy", &stmt);
  LOG_DEBUG("add assertion");
  symstringInit(&stmt);
  size_tArrayAppend(&stmt, tyx_a, 3);
  verifierAddAssertion(&vrf, "tyx", &stmt);
  LOG_DEBUG("prepare stack");
  symstringInit(&stmt);
  size_tArrayAppend(&stmt, stack1, 2);
  symstringArrayAdd(&vrf.stack, stmt);
  symstringInit(&stmt);
  size_tArrayAppend(&stmt, stack2, 2);
  symstringArrayAdd(&vrf.stack, stmt);
  symstringInit(&stmt);
  size_tArrayAppend(&stmt, stack3, 3);
  symstringArrayAdd(&vrf.stack, stmt);
  LOG_DEBUG("prepare context frame");
  struct frame ctx;
  frameInit(&ctx);
  LOG_DEBUG("apply assertion");
  verifierApplyAssertion(&vrf, &ctx, tyx);
  ut_assert(!vrf.err, "assertion application failed");
  symstringInit(&stmt);
  size_tArrayAppend(&stmt, res, 3);
  ut_assert(symstringIsEqual(&vrf.stack.vals[0], &stmt), "result of assertion "
    "application is wrong");
  LOG_DEBUG("clean up");
  frameClean(&ctx);
  symstringClean(&stmt);
  readerClean(&r);
/* frm is cleaned here */
  verifierClean(&vrf);
  return 0;
}

static int
Test_verifierParseSymbol(void)
{
  char* file;
  struct reader r;
  struct verifier vrf;
  int isEndOfStatement;
#define testfile(f, e) \
  do { \
    file = f; \
    readerInitString(&r, file); \
    verifierInit(&vrf); \
    verifierBeginReadingFile(&vrf, &r); \
    verifierParseSymbol(&vrf, &isEndOfStatement, '.'); \
    check_err(vrf.err, e); \
    readerClean(&r); \
    verifierClean(&vrf); \
  } while (0)
  testfile("forced:", error_unterminatedStatement);
  ut_assert(isEndOfStatement == 0, "isEndOfStatement is %d, expected 0",
   isEndOfStatement);
  // testfile("bursts$. ", error_invalidSymbol);
  // ut_assert(isEndOfStatement == 0, "isEndOfStatement is %d, expected 0",
   // isEndOfStatement);
  testfile("$. ", error_none);
  ut_assert(isEndOfStatement == 1, "isEndOfStatement is %d, expected 1",
   isEndOfStatement);
  return 0;
#undef testfile
}

static int
Test_verifierParseStatementContent(void)
{
  enum { file_size = 2 };
  const char* file[file_size] = {
    "He clasps the crag with crooked hands $* ",
    "close to the sun in lonely lands, $* "
  };
  const size_t lens[file_size] = {
    7, 7
  };
  size_t i;
  struct verifier vrf;
  verifierInit(&vrf);
  for (i = 0; i < file_size; i++) {
    struct symstring stmt;
    symstringInit(&stmt);
    struct reader r;
    readerInitString(&r, file[i]);
    verifierBeginReadingFile(&vrf, &r);
    verifierParseStatementContent(&vrf, &stmt, '*');
    ut_assert(stmt.size == lens[i], "parsed %lu items, expected %lu",
      stmt.size, lens[i]);
    readerClean(&r);
    symstringClean(&stmt);
  }
  verifierClean(&vrf);
  return 0;
}

static int
Test_verifierParseConstants(void)
{
  char* file;
  struct reader r;
  struct verifier vrf;
#define testparse(f, e) \
do { \
  file = f; \
  readerInitString(&r, file); \
  verifierInit(&vrf); \
  verifierBeginReadingFile(&vrf, &r); \
  verifierParseConstants(&vrf); \
  check_err(vrf.err, e); \
  readerClean(&r); \
  verifierClean(&vrf); \
} while (0)
  testparse("", error_unterminatedStatement);
  testparse("A savage place! c as holy and enchanted $.\n", error_none);
  readerInitString(&r, "savage $.\n");
  charArrayAppend(&r.filename, "file1", 5 + 1);
  verifierInit(&vrf);
  verifierBeginReadingFile(&vrf, &r);
  verifierParseConstants(&vrf);
  readerClean(&r);
  readerInitString(&r, "savage $.\n");
  charArrayAppend(&r.filename, "file2", 5 + 1);
  verifierBeginReadingFile(&vrf, &r);
  verifierParseConstants(&vrf);
  readerClean(&r);
  check_err(vrf.err, error_duplicateSymbol);
  verifierClean(&vrf);
  return 0;
#undef testparse
}

static int
Test_verifierParseVariables(void)
{
  char* file = "And here were forests ancient as the hills $.\n";
  struct reader r;
  struct verifier vrf;
  readerInitString(&r, file);
  charArrayAppend(&r.filename, "file", 4 + 1);
  verifierInit(&vrf);
  verifierBeginReadingFile(&vrf, &r);
  verifierParseVariables(&vrf);
  check_err(vrf.err, error_none);
  size_t numvar = vrf.symCount[symType_variable];
  ut_assert(numvar == 8, "parsed %lu variables, should be 8", numvar);
  readerClean(&r);
  verifierClean(&vrf);
  return 0;
}

static int
Test_verifierParseDisjoints(void)
{
  enum {
    file_size = 2
  };
  const char* file[file_size] = {
    "v c $. ",
    "v v $. "
  };
  const enum error errs[file_size] = {
    error_expectedVariableSymbol,
    error_none
  };
  size_t i;
  struct verifier vrf;
  verifierInit(&vrf);
  struct reader r[file_size];
  struct symstring stmts[file_size];
  for (i = 0; i < file_size; i++) {
    readerInitString(&r[i], file[i]);
    symstringInit(&stmts[i]);
  }
  LOG_DEBUG("call BeginReadingFile to set valid vrf.rId");
  verifierBeginReadingFile(&vrf, &r[0]);
  size_t c = verifierAddConstant(&vrf, "c");
  size_t v = verifierAddVariable(&vrf, "v");
  struct symstring flt;
  symstringInit(&flt);
  symstringAdd(&flt, c);
  symstringAdd(&flt, v);
  verifierAddFloating(&vrf, "f", &flt);
  for (i = 0; i < file_size; i++) {
    LOG_DEBUG("testing file %lu", i);
    verifierBeginReadingFile(&vrf, &r[i]);
    verifierParseDisjoints(&vrf, &stmts[i]);
    check_err(vrf.err, errs[i]);
    readerClean(&r[i]);
  }
  LOG_DEBUG("clean up");
  for (i = 0; i < file_size; i++) {
    symstringClean(&stmts[i]);
  }
  verifierClean(&vrf);
  return 0;
}

static int
Test_verifierParseFloating(void)
{
  size_t i;
  enum {
    file_size = 5
  };
  const char* file[file_size] = {
    "$. ",
    "And mid these dancing rocks at once and ever $.\n",
    "Chopin piano $. ",
    "wff x $. ",
    "wff y $. "
  };
  struct verifier vrf;
  verifierInit(&vrf);
  struct reader r[file_size];
  struct symstring stmts[file_size];
  for (i = 0; i < file_size; i++) {
    symstringInit(&stmts[i]);
    readerInitString(&r[i], file[i]);
    check_err(vrf.err, error_none);
  }
#define test_file(rId, error) \
do { \
  LOG_DEBUG("testing file %d", rId); \
  verifierBeginReadingFile(&vrf, &r[rId]); \
  verifierParseFloating(&vrf, &stmts[rId]); \
  check_err(vrf.err, error); \
  readerClean(&r[rId]); \
} while (0)
  test_file(0, error_invalidFloatingStatement);
  test_file(1, error_invalidFloatingStatement);
  test_file(2, error_expectedVariableSymbol);
  verifierAddConstant(&vrf, "wff");
  test_file(3, error_expectedVariableSymbol);
  verifierAddVariable(&vrf, "y");
  test_file(4, error_none);
  LOG_DEBUG("cleaning stmts");
  for (i = 0; i < file_size; i++) {
    symstringClean(&stmts[i]);
  }
  LOG_DEBUG("cleaning vrf");
  verifierClean(&vrf);
  return 0;
#undef test_file
}

static int
Test_verifierParseEssential(void)
{
  enum {
    file_size = 3
  };
  const char* file[file_size] = {
    "$. ",
    "v |- $. ",
    "|- v $. "
  };
  const enum error errs[file_size] = {
    error_invalidEssentialStatement,
/* we expect this error to be emitted, but it won't be the most recent */
/* for now, use error_none */
    // error_expectedConstantSymbol,
    error_none,
    error_none
  };
  const size_t errcs[file_size] = {
    1,
    1,
    0
  };
  size_t i;
  struct verifier vrf;
  verifierInit(&vrf);
  struct reader r[file_size];
  struct symstring stmts[file_size];
  for (i = 0; i < file_size; i++) {
    readerInitString(&r[i], file[i]);
    symstringInit(&stmts[i]);
  }
  verifierBeginReadingFile(&vrf, &r[0]);
  size_t c = verifierAddConstant(&vrf, "|-");
  size_t v = verifierAddVariable(&vrf, "v");
  struct symstring flt;
  symstringInit(&flt);
  symstringAdd(&flt, c);
  symstringAdd(&flt, v);
  verifierAddFloating(&vrf, "f", &flt);
  for (i = 0; i < file_size; i++) {
    LOG_DEBUG("testing file %lu", i);
    vrf.errc = 0;
    verifierBeginReadingFile(&vrf, &r[i]);
    LOG_DEBUG("parsing essential");
    verifierParseEssential(&vrf, &stmts[i]);
    LOG_DEBUG("done parsing, checking for errors");
    check_err(vrf.err, errs[i]);
    ut_assert(vrf.errc == errcs[i], "error count vrf.errc = %lu, expected %lu",
      vrf.errc, errcs[i]);
  }
  LOG_DEBUG("clean up");
  for (i = 0; i < file_size; i++) {
    symstringClean(&stmts[i]);
    readerClean(&r[i]);
  }
  verifierClean(&vrf);
  return 0;
}

static int
Test_verifierParseAssertion(void)
{
  enum {
    file_size = 3
  };
  const char* file[file_size] = {
    "$. ",
    "v |- $. ",
    "|- v $. "
  };
  const enum error errs[file_size] = {
    error_invalidAssertionStatement,
/* we expect this error to be emitted, but it won't be the most recent */
/* for now, use error_none */
    // error_expectedConstantSymbol,
    error_none,
    error_none
  };
  const size_t errcs[file_size] = {
    1,
    1,
    0
  };
  size_t i;
  struct verifier vrf;
  verifierInit(&vrf);
  struct reader r[file_size];
  struct symstring stmts[file_size];
  for (i = 0; i < file_size; i++) {
    readerInitString(&r[i], file[i]);
    symstringInit(&stmts[i]);
  }
  verifierBeginReadingFile(&vrf, &r[0]);
  size_t c = verifierAddConstant(&vrf, "|-");
  size_t v = verifierAddVariable(&vrf, "v");
  struct symstring flt;
  symstringInit(&flt);
  symstringAdd(&flt, c);
  symstringAdd(&flt, v);
  verifierAddFloating(&vrf, "f", &flt);
  for (i = 0; i < file_size; i++) {
    LOG_DEBUG("testing file %lu", i);
    vrf.errc = 0;
    verifierBeginReadingFile(&vrf, &r[i]);
    LOG_DEBUG("parsing assertion");
    verifierParseAssertion(&vrf, &stmts[i]);
    LOG_DEBUG("done parsing, checking for errors");
    check_err(vrf.err, errs[i]);
    ut_assert(vrf.errc == errcs[i], "error count vrf.errc = %lu, expected %lu",
      vrf.errc, errcs[i]);
  }
  LOG_DEBUG("clean up");
  for (i = 0; i < file_size; i++) {
    symstringClean(&stmts[i]);
    readerClean(&r[i]);
  }
  verifierClean(&vrf);
  return 0;
}

static int
Test_verifierParseProofSymbol(void)
{
  enum {
    file_size = 4
  };
  const char* file[file_size] = {
    "$. ",
    "undefined ",
    "defined_float ",
    "defined_assert ",
  };
  size_t i;
  int isEndOfProof;
  struct verifier vrf;
  verifierInit(&vrf);
  struct reader r[file_size];
  for (i = 0; i < file_size; i++) {
    readerInitString(&r[i], file[i]);
  }
  struct frame ctx;
  frameInit(&ctx);
#define test_file(f, error) \
do { \
  verifierBeginReadingFile(&vrf, &r[f]); \
  verifierParseProofSymbol(&vrf, &ctx, &isEndOfProof); \
  check_err(vrf.err, error); \
  readerClean(&r[f]); \
} while (0)
  test_file(0, error_none);
  test_file(1, error_undefinedSymbol);
  size_t c = verifierAddConstant(&vrf, "c");
  size_t v = verifierAddVariable(&vrf, "v");
  struct symstring stmt1;
  symstringInit(&stmt1);
  symstringAdd(&stmt1, c);
  symstringAdd(&stmt1, v);
  verifierAddFloating(&vrf, "defined_float", &stmt1);
  test_file(2, error_none);
  struct symstring stmt2;
  symstringInit(&stmt2);
  verifierAddAssertion(&vrf, "defined_assert", &stmt2);
  test_file(3, error_none);
  ut_assert(vrf.stack.size == 2, "stack size == %lu, should be 2",
    vrf.stack.size);
  frameClean(&ctx);
  verifierClean(&vrf);
  return 0;
#undef test_file
}

static int
Test_verifierParseProof(void)
{
  enum {
    file_size = 2
  };
  const char* file[file_size] = {
    "$. ",
    "p $. "
  };
  const size_t thms_len[file_size] = {
    1, 2
  };
  const size_t thms_0[1] = {0};
  const size_t thms_1[2] = {0, 1};
  const size_t* thms_s[file_size] = {
    thms_0,
    thms_1
  };
  size_t i;
  struct verifier vrf;
  verifierInit(&vrf);
  struct reader r[file_size];
  struct symstring thms[file_size];
  for (i = 0; i < file_size; i++) {
    readerInitString(&r[i], file[i]);
    symstringInit(&thms[i]);
    size_tArrayAppend(&thms[i], thms_s[i], thms_len[i]);
  }
  struct frame ctx;
  frameInit(&ctx);
#define test_file(f, error) \
do { \
  verifierBeginReadingFile(&vrf, &r[f]); \
  verifierParseProof(&vrf, &ctx); \
  verifierCheckProof(&vrf, &thms[f]); \
  check_err(vrf.err, error); \
  readerClean(&r[f]); \
} while (0)
  test_file(0, error_incorrectProof);
  verifierAddConstant(&vrf, "type");
  verifierAddVariable(&vrf, "var");
  struct symstring stmt;
  symstringInit(&stmt);
  symstringAdd(&stmt, 0);
  symstringAdd(&stmt, 1);
  verifierAddFloating(&vrf, "p", &stmt);
  test_file(1, error_none);
  for (i = 0; i < file_size; i++) {
    symstringClean(&thms[i]);
  }
  frameClean(&ctx);
  verifierClean(&vrf);
  return 0;
#undef test_file
}

static int
Test_verifierParseProvable(void)
{
  enum {
    file_size = 2
  };
  const char* file[file_size] = {
    "type var $= $. ",
    "type var $= flt $. "
  };
  const enum error errs[file_size] = {
    error_incorrectProof,
    error_none
  };
  size_t i;
  struct verifier vrf;
  verifierInit(&vrf);
  struct reader r[file_size];
  for (i = 0; i < file_size; i++) {
    readerInitString(&r[i], file[i]);
    check_err(vrf.err, error_none);
  }
  verifierBeginReadingFile(&vrf, &r[0]);
  size_t type, var, flt;
  type = verifierAddConstant(&vrf, "type");
  var = verifierAddVariable(&vrf, "var");
  struct symstring floating;
  symstringInit(&floating);
  symstringAdd(&floating, type);
  symstringAdd(&floating, var);
  flt = verifierAddFloating(&vrf, "flt", &floating);
  check_err(vrf.err, error_none);
  for (i = 0; i < file_size; i++) {
    LOG_DEBUG("testing file %lu", i);
    struct symstring stmt;
    symstringInit(&stmt);
    verifierBeginReadingFile(&vrf, &r[i]);
    struct frame frm;
    frameInit(&frm);
    verifierParseProvable(&vrf, &stmt, &frm);
    check_err(vrf.err, errs[i]);
    frameClean(&frm);
    symstringClean(&stmt);
    readerClean(&r[i]);
  }
  verifierClean(&vrf);
  return 0;
}

static int
Test_verifierParseUnlabelledStatement(void)
{
  enum { file_size = 3 };
  const char* tok[file_size] = {
    "$c",
    "$v",
    "$the",
  };
  const char* file[file_size] = {
    "the muttering retreats $. ",
    "Of restless nights in one-night cheap hotels $. ",
    "And sawdust restaurants with oyster-shells: ",
  };
  const enum error errs[file_size] = {
    error_none,
    error_none,
    error_invalidKeyword,
  };
  size_t i;
  int isEndOfScope;
  struct verifier vrf;
  verifierInit(&vrf);
  for (i = 0; i < file_size; i++) {
    LOG_DEBUG("testing file %lu", i);
    struct reader r;
    readerInitString(&r, file[i]);
    verifierBeginReadingFile(&vrf, &r);
    verifierParseUnlabelledStatement(&vrf, &isEndOfScope, tok[i]);
    check_err(vrf.err, errs[i]);
    readerClean(&r);
  }
  verifierClean(&vrf);
  return 0;
}

static int
Test_verifierParseLabelledStatement(void)
{
  enum { file_size = 3 };
  const char* file[file_size] = {
    "Let us go then, you and I ",
    "$( Like a patient etherized upon a table $) $c ",
    "$f Let us $. "
  };
  const enum error errs[file_size] = {
    error_expectedKeyword,
    error_unexpectedKeyword,
    error_expectedVariableSymbol
  };
  size_t i;
  struct verifier vrf;
  verifierInit(&vrf);
  for (i = 0; i < file_size; i++) {
    LOG_DEBUG("testing file %lu", i);
    struct reader r;
    readerInitString(&r, file[i]);
    verifierBeginReadingFile(&vrf, &r);
    verifierParseLabelledStatement(&vrf, "label");
    check_err(vrf.err, errs[i]);
    readerClean(&r);
  }
  verifierClean(&vrf);
  return 0;
}

static int
Test_verifierParseStatement(void)
{
  enum { file_size = 11 };
  const char* file[file_size] = {
    "$c |- number 0 1 + $. ",
    "$v x y z w $. ",
    "num_x $f number x $. ",
    "num_y $f number y $. ",
    "ess $e number x + y $. ",
    "asr $a number x + 0 + y $. ",
    "thm $p number x + y + 0 + x $= ess num_x asr $. ",
    "$d x y $. ",
    " ",
    "$Streets |- that follow $( like $v a tedious argument $. $. ",
    "$O f insidious intent $. ",
  };
  const enum error errs[file_size] = {
    error_none,
    error_none,
    error_none,
    error_none,
    error_none,
    error_none,
    error_none,
    error_none,
    error_expectedNewLine,
    error_invalidKeyword,
    error_unexpectedKeyword,
  };
  size_t i;
  int isEndOfScope;
  struct verifier vrf;
  verifierInit(&vrf);
  size_tArrayAdd(&vrf.disjointScope, 0);
  for (i = 0; i < file_size; i++) {
    LOG_DEBUG("testing file %lu", i);
    struct reader r;
    readerInitString(&r, file[i]);
    verifierBeginReadingFile(&vrf, &r);
    verifierParseStatement(&vrf, &isEndOfScope);
    check_err(vrf.err, errs[i]);
    readerClean(&r);
  }
  verifierClean(&vrf);
  return 0;
}

static int
Test_verifierParseBlock(void)
{
  enum { file_size = 4 };
  const char* file[file_size] = {
/* file 0 */
    "$c |- wff S 0 $. "
    "${ "
    "$v x y z w $. "
    "${ "
    "$v a b c $. "
    "$} "
    "$v a b c $. "
    "$} "
    "$v x $. \n",
/* file 1 - parse two proofs */
    "$c |- num 0 S $. "
    "$v x $. "
    "a.num.0 $a num 0 $. "
    "num.x $f num x $. "
    "a.num.succ $a num S x $. "
    "thm.one $p num S 0 $= a.num.0 a.num.succ $. "
    "thm.succ2 $p num S S x $= num.x a.num.succ a.num.succ $.\n",
/* file 2 - test compressed proof */
    "$c |- num S 0 $. $v x y $. "
    "numt.0 $a num 0 $. "
    "num.0 $a |- num 0 $. "
    "num.x $f num x $. "
    "num.succ $a |- num S x $. "
    "thm $p |- num S 0 $= ( numt.0 num.succ ) "
    "AB $. \n",
/* file 3 - test disjoint variable restriction */
    "$c | $. \n"
    "$v x y B R $. \n"
    "tx $f | x $. ty $f | y $. tB $f | B $. tR $f | R $. \n"
    "${ $d x y $.  $d y B $.  $d y R $. $} \n",
/* to do: write test for compressed proof with Z tag */

  };
  const enum error errs[file_size] = {
    error_none,
    error_none,
    error_none,
    error_none,
  };
  const size_t errc[file_size] = {
    0,
    0,
    0,
    0,
  };
  const size_t dsymnum[file_size] = {
    0,
    0,
    0,
    3
  };
  size_t i;
  for (i = 0; i < file_size; i++) {
    LOG_DEBUG("testing file %lu", i);
    struct verifier vrf;
    verifierInit(&vrf);
    verifierSetVerbosity(&vrf, 5);
    struct reader r;
    readerInitString(&r, file[i]);
    verifierBeginReadingFile(&vrf, &r);
    // verifierSetVerbosity(&vrf, 5);
    verifierParseBlock(&vrf);
    check_err(vrf.err, errs[i]);
    ut_assert(vrf.symCount[symType_disjoint] == dsymnum[i],
      "found %lu disjoint pairs, expected %lu", vrf.symCount[symType_disjoint],
      dsymnum[i]);
    ut_assert(vrf.errc == errc[i], "found %lu errors, expected %lu",
      vrf.errc, errc[i]);
    readerClean(&r);
    verifierClean(&vrf);
  }
  return 0;
}

static int
all(void)
{
  ut_run(Test_frameInit);
  ut_run(Test_verifierInit);
  ut_run(Test_verifierAddSymbol);
  ut_run(Test_verifierGetSymId);
  ut_run(Test_verifierDeactivateSymbols);
  ut_run(Test_verifierAddDisjoint);
  ut_run(Test_verifierGetVariables);
  ut_run(Test_verifierMakeFrame);
  ut_run(Test_verifierIsValidDisjointPairSubstitution);
  ut_run(Test_verifierIsValidSubstitution);
  ut_run(Test_verifierUnify);
  ut_run(Test_verifierApplyAssertion);
  ut_run(Test_verifierParseSymbol);
  ut_run(Test_verifierParseStatementContent);
  ut_run(Test_verifierParseConstants);
  ut_run(Test_verifierParseVariables);
  ut_run(Test_verifierParseDisjoints);
  ut_run(Test_verifierParseFloating);
  ut_run(Test_verifierParseEssential);
  ut_run(Test_verifierParseAssertion);
  ut_run(Test_verifierParseProofSymbol);
  ut_run(Test_verifierParseProof);
  ut_run(Test_verifierParseProvable);
  ut_run(Test_verifierParseUnlabelledStatement);
  ut_run(Test_verifierParseLabelledStatement);
  ut_run(Test_verifierParseStatement);
  ut_run(Test_verifierParseBlock);
  return 0;
}

RUN(all)
