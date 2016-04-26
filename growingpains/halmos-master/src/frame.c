#include "array.h"
#include "frame.h"
DEFINE_ARRAY(frame)
void
frameInit(struct frame* frm)
{
  size_tArrayInit(&frm->stmts, 1);
  size_tArrayInit(&frm->disjoint1, 1);
  size_tArrayInit(&frm->disjoint2, 1);
}

void
frameClean(struct frame* frm)
{
  size_tArrayClean(&frm->disjoint2);
  size_tArrayClean(&frm->disjoint1);
  size_tArrayClean(&frm->stmts);
}

int
frameAreDisjoint(const struct frame* frm, size_t v1, size_t v2)
{
  size_t i;
  for (i = 0; i < frm->disjoint1.size; i++) {
    if ((frm->disjoint1.vals[i] == v1) && (frm->disjoint2.vals[i] == v2)) {
      return 1;
    }
    if ((frm->disjoint1.vals[i] == v2) && (frm->disjoint2.vals[i] == v1)) {
      return 1;
    }
  }
  return 0;
}

void
frameAddDisjoint(struct frame* frm, size_t v1, size_t v2)
{
  size_tArrayAdd(&frm->disjoint1, v1);
  size_tArrayAdd(&frm->disjoint2, v2);
}

