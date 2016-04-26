#ifndef _HALMOSFRAME_H_
#define _HALMOSFRAME_H_
#include "array.h"
struct frame;
typedef struct frame frame;
DECLARE_ARRAY(frame)

/* frame, for assertions and provables. This is not an extended frame */
struct frame {
/* indices to verifier->stmts. These are mandatory hypotheses */
  struct size_tArray stmts;
/* indices to verifier->symbols for pairwise disjoint variables */
  struct size_tArray disjoint1;
  struct size_tArray disjoint2;
};

void
frameInit(struct frame* frm);

void
frameClean(struct frame* frm);

void
frameAddDisjoint(struct frame* frm, size_t v1, size_t v2);

int
frameAreDisjoint(const struct frame* frm, size_t v1, size_t v2);
#endif
