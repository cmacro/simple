#include "unittest.h"
#include "symstring.h"

static int
test_symstringInsert(void)
{
  const size_t a[4] = {0, 1, 2, 3};
  const size_t b[3] = {4, 5, 6};
  const size_t c[7] = {0, 1, 4, 5, 6, 2, 3};
  struct symstring sa;
  struct symstring sb;
  symstringInit(&sa);
  symstringInit(&sb);
  size_tArrayAppend(&sa, a, 4);
  size_tArrayAppend(&sb, b, 3);
  symstringInsert(&sa, 2, &sb);
  ut_assert(sa.size == 7, "size is %lu, expected 7", sa.size);
  size_t i;
  for (i = 0; i < 7; i++) {
    ut_assert(sa.vals[i] == c[i], "sa[%lu] == %lu, expected %lu", i,
      sa.vals[i], c[i]);
  }
  symstringClean(&sa);
  symstringClean(&sb);
  return 0;
}

static int
test_symstringDelete(void)
{
  const size_t a[4] = {6, 7, 8, 9};
  const size_t b[3] = {6, 8, 9};
  struct symstring s;
  symstringInit(&s);
  size_tArrayAppend(&s, a, 4);
  symstringDelete(&s, 1);
  ut_assert(s.size == 3, "size is %lu, expected 3", s.size);
  size_t i;
  for (i = 0; i < 3; i++) {
    ut_assert(s.vals[i] == b[i], "s[%lu] == %lu, expected %lu", i,
      s.vals[i], b[i]);
  }
  symstringClean(&s);
  return 0;
}

static int
test_symstringSubstitute(void)
{
  const size_t a[4] = {1, 4, 1, 3};
  const size_t b[2] = {1, 2};
  const size_t c[6] = {1, 2, 4, 1, 2, 3};
  struct symstring sa;
  struct symstring sb;
  symstringInit(&sa);
  symstringInit(&sb);
  size_tArrayAppend(&sa, a, 4);
  size_tArrayAppend(&sb, b, 2);
  symstringSubstitute(&sa, 1, &sb);
  ut_assert(sa.size == 6, "size is %lu, expected 6", sa.size);
  size_t i;
  for (i = 0; i < 6; i++) {
    ut_assert(sa.vals[i] == c[i], "s[%lu] == %lu, expected %lu", i,
      sa.vals[i], c[i]);
  }
  symstringClean(&sa);
  symstringClean(&sb);
  return 0;
}

static int
test_substitutionSubstitute(void)
{
  const size_t a[4] = {1, 4, 1, 3};
  const size_t b[2] = {1, 2};
  const size_t c[6] = {1, 2, 4, 1, 2, 3};
  struct symstring sa;
  struct symstring sb;
  symstringInit(&sa);
  symstringInit(&sb);
  size_tArrayAppend(&sa, a, 4);
  size_tArrayAppend(&sb, b, 2);

  struct substitution sub;
  substitutionInit(&sub);
  substitutionAdd(&sub, 1, &sb);
  symstringInit(&sub.isMarked);
  substitutionUnmark(&sub, sa.size);
  substitutionSubstitute(&sub, 0, &sa);
  ut_assert(sa.size == 6, "size is %lu, expected 6", sa.size);
  size_t i;
  for (i = 0; i < 6; i++) {
    ut_assert(sa.vals[i] == c[i], "s[%lu] == %lu, expected %lu", i,
      sa.vals[i], c[i]);
  }
  symstringClean(&sa);
  symstringClean(&sub.isMarked);
/* sb is cleaned here */
  substitutionClean(&sub);
  return 0;
}

static int
test_substitutionApply(void)
{
  const size_t a[6] = {1, 2, 3, 4, 2, 1};
  const size_t b1[2] = {2, 1};
  const size_t b2[3] = {1, 5, 1};
  const size_t c[12] = {2, 1, 1, 5, 1, 3, 4, 1, 5, 1, 2, 1};
  struct symstring sa, sb1, sb2;
  symstringInit(&sa);
  symstringInit(&sb1);
  symstringInit(&sb2);
  size_tArrayAppend(&sa, a, 6);
  size_tArrayAppend(&sb1, b1, 2);
  size_tArrayAppend(&sb2, b2, 3);
  struct substitution sub;
  substitutionInit(&sub);
  substitutionAdd(&sub, 1, &sb1);
  substitutionAdd(&sub, 2, &sb2);
  substitutionApply(&sub, &sa);
  ut_assert(sa.size == 12, "size == %lu, expected 12", sa.size);
  size_t i;
  for (i = 0; i < 12; i++) {
    ut_assert(sa.vals[i] == c[i], "sa[%lu] == %lu, expected %lu", i,
      sa.vals[i], c[i]);
  }
  symstringClean(&sa);
  substitutionClean(&sub);
  return 0;
}

static int
test_all(void)
{
  ut_run(test_symstringInsert);
  ut_run(test_symstringDelete);
  ut_run(test_symstringSubstitute);
  ut_run(test_substitutionSubstitute);
  ut_run(test_substitutionApply);
  return 0;
}

RUN(test_all)
