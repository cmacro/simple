#include "unittest.h"
#include "preproc.h"

static int
test_preprocInit(void)
{
  struct preproc p;
  preprocInit(&p);
  preprocClean(&p);
  return 0;
}

static int
all(void)
{
  ut_run(test_preprocInit);
  return 0;
}

RUN(all)
