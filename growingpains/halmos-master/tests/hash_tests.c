#include "unittest.h"
#include "hash.h"

static int
test_hash_djb2(void)
{
  const char* s = "hello";
  hash_djb2(s, strlen(s), 0);
  return 0;
}

static int
test_hash_murmur3(void)
{
  const char* s[4] = {
    "hello",
    "hello ",
    "hello w",
    "hello wo"
  };
  size_t i;
  for (i = 0; i < 4; i++) {
    hash_murmur3(s[i], strlen(s[i]), 0);
  }
  return 0;
}

static int
all(void)
{
  ut_run(test_hash_djb2);
  ut_run(test_hash_murmur3);
  return 0;
}

RUN(all)
