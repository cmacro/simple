#ifndef _HALMOSLOGGER_H_
#define _HALMOSLOGGER_H_

/* logging with the verifier */
#define H_LOG(vrf, err, verbosity, lab, ...) \
do { \
  if (vrf->verb < (verbosity)) { break; } \
  fprintf(stderr, "%s:%lu:%lu " lab " [%s] ", \
    vrf->files.vals[vrf->rId].vals, \
    vrf->r->line, \
    vrf->r->offset, \
    errorString(err)); \
  fprintf(stderr, __VA_ARGS__); \
  fprintf(stderr, "\n"); \
} while (0)

/* use this for reporting general errors not associated with file content */
#define G_LOG(vrf, err, lab, ...) \
do { \
  verifierSetError(vrf, err); \
  fprintf(stderr, lab " [%s] ", errorString(err)); \
  fprintf(stderr, __VA_ARGS__); \
  fprintf(stderr, "\n"); \
} while (0)

/* logging for the preprocessor */
#define P_LOG(p, err, lab, ...) \
do { \
  preprocSetError(p, err); \
  fprintf(stderr, "%s:%lu:%lu " lab " [%s] ", \
    p->r->filename.vals, p->r->line, p->r->offset, errorString(err)); \
  fprintf(stderr, __VA_ARGS__); \
  fprintf(stderr, "\n"); \
} while (0)

#define H_LOG_ERR(vrf, err, verb, ...) \
do { \
  verifierSetError(vrf, err); \
  H_LOG(vrf, err, verb, "error", __VA_ARGS__); \
} while (0)

#define H_LOG_WARN(vrf, err, verb, ...) \
H_LOG(vrf, err, verb, "warning", __VA_ARGS__)

#define H_LOG_INFO(vrf, verb, ...) \
H_LOG(vrf, vrf->err, verb, "info", __VA_ARGS__)

#define G_LOG_ERR(vrf, err, ...) G_LOG(vrf, err, "error", __VA_ARGS__)

#define G_LOG_WARN(vrf, err, ...) G_LOG(vrf, err, "warning", __VA_ARGS__)

#define P_LOG_ERR(p, err, ...) P_LOG(p, err, "error", __VA_ARGS__)

#define P_LOG_WARN(p, err, ...) P_LOG(p, err, "warning", __VA_ARGS__)

#endif
