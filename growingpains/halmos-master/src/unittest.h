#ifndef _HALMOSUNITTEST_H_
#define _HALMOSUNITTEST_H_
#include "dbg.h"
#include <stdio.h>
#include <string.h>

enum {
  ut_message_size = 1024
};

int ut_tests_run;

char ut_message[ut_message_size];

#define ut_assert(test, ...) \
do { \
  if (!(test)) { \
    LOG_ERR(__VA_ARGS__); \
    snprintf(ut_message, ut_message_size, __VA_ARGS__); \
    return 1; \
  } \
} while (0)

#define ut_run(test) \
do { \
  LOG_DEBUG("\n-----%s", " " #test); \
  int result = test(); \
  ut_tests_run++; \
  if (result) { return result; } \
} while (0)

#define RUN(all) \
int main(int argc, char* argv[]) { \
  (void) argc; \
  printf("RUNNING -----%s\n", argv[0]); \
  ut_tests_run = 0; \
  int result = all(); \
  if (result) { \
    printf("%s\n", ut_message); \
  } else { \
    printf("ALL TESTS PASSED\n"); \
  } \
  printf("Tests run: %d\n", ut_tests_run); \
  return (result != 0); \
}

#endif
