#include "unittest.h"
#include "halmos.h"

#define test_file(filename, count) \
static int test_ ## filename(void) \
{ struct halmos h; \
  halmosInit(&h); \
  halmosCompile(&h, "tests/mm/" #filename ".mm"); \
  halmosClean(&h); \
  return 0; \
}

test_file(test1, 0)
// test_file(big_unifier, 0)
// test_file(big_unifier_comp, 0)
test_file(recursive_include, 0)
test_file(symbol_import, 0)
test_file(bugged_1, 1)
// test_file(demo0, 0)
// test_file(miu, 0)
// test_file(hol, 0)

static int
all(void)
{
  ut_run(test_test1);
  // ut_run(test_big_unifier);
  // ut_run(test_big_unifier_comp);
  ut_run(test_recursive_include);
  ut_run(test_symbol_import);
  ut_run(test_bugged_1);
  // ut_run(test_demo0);
  // ut_run(test_miu);
  // ut_run(test_hol);
  return 0;
}

RUN(all)
