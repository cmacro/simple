#ifndef _HALMOSDBG_H_
#define _HALMOSDBG_H_

#include <stdio.h>
#include <errno.h>
#include <stdlib.h>
#include <string.h>

#if defined (__GNUC__)
#define __FUNC__ __PRETTY_FUNCTION__
#else
#define __FUNC__ "unknown"
#endif

#define _CLEARERRNO() (errno == 0 ? "None" : strerror(errno))

#define _LOG(lab, ...) \
do { \
  fprintf(stderr, "%s:%d:%s " lab " [errno: %s] ", __FILE__, __LINE__, \
   __FUNC__, _CLEARERRNO());\
  fprintf(stderr, __VA_ARGS__); \
  fprintf(stderr, "\n"); \
} while (0)

#define LOG_FAT(...) _LOG("FATAL", __VA_ARGS__)

#define LOG_ERR(...) _LOG("ERROR", __VA_ARGS__)

#define LOG_WARN(...) _LOG("WARN", __VA_ARGS__)

#define LOG_INFO(...) _LOG("INFO", __VA_ARGS__)

#ifdef NDEBUG
#define LOG_DEBUG(...)
#define DEBUG_ASSERT(test, ...)
#else
#define LOG_DEBUG(...) _LOG("DEBUG", __VA_ARGS__)

#define DEBUG_ASSERT(test, ...) \
do { \
  if (!(test)) { \
    _LOG("ASSERT FAILED", __VA_ARGS__); \
    abort(); \
  } \
} while (0)

#endif /* NDEBUG */

#endif /* _HALMOSDBG_H_ */
