#include "unittest.h"
#include "symtab.h"

static int
test_symnodeInit(void)
{
  struct symnode n;
  symnodeInit(&n);
  ut_assert(n.symId == 0, "n.symId not NULL");
  ut_assert(n.next == NULL, "n.next not NULL");
  symnodeClean(&n);
  return 0;
}

static int
test_symnodeInsert(void)
{
  struct symnode n;
  symnodeInit(&n);
  size_t s = 7269;
  size_t s2 = 137645;
  size_t s3 = 54436;
  symnodeInsert(&n, 0, s);
  ut_assert(n.symId == s, " n.symId is wrong");
  symnodeInsert(&n, 0, s2);
  ut_assert(n.symId == s, "n.symId changed");
  ut_assert(n.next != NULL, "n.next is NULL");
  ut_assert(n.next->symId == s2, "n.next->symId is wrong");
  symnodeInsert(&n, 0, s3);
  ut_assert(n.symId == s, "n.symId changed");
  ut_assert(n.next != NULL, "n.next is NULL");
  ut_assert(n.next->symId == s2, "n.next->symId is wrong");
  ut_assert(n.next->next != NULL, "n.next->next is NULL");
  ut_assert(n.next->next->symId == s3, "n->next->next->symId is wrong");
  symnodeClean(&n);
  return 0;
}

static int
test_symtreeInsert(void)
{
  struct symtree t;
  symtreeInit(&t);
  size_t s1=3486, s2=137486, s3=1798265, s4=734856;
  symtreeInsert(&t, 50, s1);
  ut_assert(t.node.symId == s1, "t.node.symId wrong");
  symtreeInsert(&t, 25, s2);
  ut_assert(t.less != NULL, "t.less is NULL");
  ut_assert(t.less->node.symId == s2, "t.less->node.symId is wrong");
  symtreeInsert(&t, 75, s3);
  ut_assert(t.more != NULL, "t.more is NULL");
  ut_assert(t.more->node.symId == s3, "t.more->node.symId is wrong");
  symtreeInsert(&t, 50, s4);
  ut_assert(t.node.next != NULL, "t.node.next is NULL");
  symtreeClean(&t);
  return 0;
}

static int
test_symtreeFind(void)
{
  struct symtree t;
  symtreeInit(&t);
  size_t s1 = 3456, s2=275, s3=9814;
  size_t s4=54763;
  symtreeInsert(&t, 50, s1);
  symtreeInsert(&t, 100, s2);
  symtreeInsert(&t, 25, s3);
  symtreeInsert(&t, 25, s4);
  struct symtree* f = symtreeFind(&t, 100);
  ut_assert(f->node.symId == s2, "failed to find 100");
  f = symtreeFind(&t, 50);
  ut_assert(f->node.symId == s1, "failed to find 50");
  f = symtreeFind(&t, 25);
  ut_assert(f->node.symId == s3, "failed to find 25");
  ut_assert(f->node.next != NULL, "failed to find collision at 25");
  ut_assert(f->node.next->symId == s4, "wrong next");
  symtreeClean(&t);
  return 0;
}

static int
all(void)
{
  ut_run(test_symnodeInit);
  ut_run(test_symnodeInsert);
  ut_run(test_symtreeInsert);
  ut_run(test_symtreeFind);
  return 0;
}

RUN(all)
