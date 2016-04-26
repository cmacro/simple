#include "unittest.h"
#include "array.h"

static int Test_arrayAdd(void)
{
  struct intArray a;
  intArrayInit(&a, 1);
  intArrayAdd(&a, 3);
  ut_assert(a.vals[0] == 3, "vals == %d, expected 3", a.vals[0]);
  ut_assert(a.size == 1, "size == %lu, expected 1", a.size);
  ut_assert(a.max == 1, "max == %lu, expected 1", a.max);
  intArrayAdd(&a, 4);
  ut_assert(a.vals[1] == 4, "vals == %d, expected 4", a.vals[1]);
  ut_assert(a.size == 2, "size == %lu, expected 2", a.size);
  ut_assert(a.max == 2, "max == %lu, expected 2", a.max);
  intArrayClean(&a);
  return 0;
}

static int Test_arrayEmpty(void)
{
  struct intArray a;
  intArrayInit(&a, 1);
  intArrayAdd(&a, 3);
  intArrayEmpty(&a);
  intArrayAdd(&a, 4);
  ut_assert(a.vals[0] == 4, "vals == %d, expected 4", a.vals[0]);
  ut_assert(a.size == 1, "size == %lu, expected 1", a.size);
  intArrayClean(&a);
  return 0;
}

static int Test_arrayAppend(void)
{
  int vals[] = {1, 3, 5};
  struct intArray a;
  intArrayInit(&a, 1);
  intArrayAppend(&a, vals, 3);
  ut_assert(a.vals[0] == 1, "vals == %d, expected 1", a.vals[0]);
  ut_assert(a.vals[1] == 3, "vals == %d, expected 1", a.vals[1]);
  ut_assert(a.vals[2] == 5, "vals == %d, expected 1", a.vals[2]);
  ut_assert(a.size == 3, "size == %lu, expected 3", a.size);
  intArrayClean(&a);
  return 0;
}

static int all()
{
  ut_run(Test_arrayAdd);
  ut_run(Test_arrayEmpty);
  ut_run(Test_arrayAppend);
  return 0;
}

RUN(all)
