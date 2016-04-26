#include "dbg.h"
#include "symstring.h"

/* we want to write 'struct symstring' and 'struct symstringArray'. We */
/* #defined symstring as size_tArray in the header, so undo this temporarily. */
#ifdef symstring
#undef symstring
DEFINE_ARRAY(symstring)
#define symstring size_tArray
#endif

// void
// symstringInit(struct symstring* str)
// {
//   size_tArrayInit(str, default_size);
// }

// void
// symstringClean(struct symstring* str)
// {
//   size_tArrayClean(str);
// }

// void
// symstringAdd(struct symstring* str, size_t symId)
// {
//   size_tArrayAdd(str, symId);
// }

// void
// symstringAppend(struct symstring* a, const struct symstring* b)
// {
//   size_tArrayAppend(a, b->vals, b->size);
// }

void
symstringInsert(struct symstring* a, size_t idx, const struct symstring* b)
{
  DEBUG_ASSERT(a->size >= idx,
    "invalid index: size is %lu but index was %lu", a->size, idx);
  if (b->size == 0) {
    return;
  }
  struct size_tArray tmp;
  size_tArrayInit(&tmp, a->size - idx);
  size_tArrayAppend(&tmp, &a->vals[idx], a->size - idx);
  a->size = idx;
  symstringAppend(a, b);
  size_tArrayAppend(a, tmp.vals, tmp.size);
  size_tArrayClean(&tmp);
}

/* delete the item at idx */
void
symstringDelete(struct symstring* a, size_t idx)
{
  DEBUG_ASSERT(a->size > idx,
    "invalid index: size is %lu but index was %lu", a->size, idx);
  size_t i;
  for (i = idx; i < a->size - 1; i++) {
    a->vals[i] = a->vals[i + 1];
  }
  a->size--;
}

/* replace each occurrence of s by b */
void
symstringSubstitute(struct symstring* a, size_t s, const struct symstring* b)
{
  size_t i = 0;
  const size_t len = b->size;
  while (i < a->size) {
    if (a->vals[i] == s) {
      symstringDelete(a, i);
      symstringInsert(a, i, b);
      i += len;
    } else {
      i++;
    }
  }
}

int
symstringIsEqual(const struct symstring* a, const struct symstring* b)
{
  size_t i;
  if (a->size != b->size) { return 0; }
  for (i = 0; i < a->size; i++) {
    if (a->vals[i] != b->vals[i]) { return 0; }
  }
  return 1;
}

int
symstringIsIn(const struct symstring* a, size_t sym)
{
  size_t i;
  for (i = 0; i < a->size; i++) {
    if (a->vals[i] == sym) { return 1; }
  }
  return 0;
}

int
symstringIsIntersecting(const struct symstring* a, const struct symstring* b)
{
  size_t i;
  for (i = 0; i < a->size; i++) {
    if (symstringIsIn(b, a->vals[i])) { return 1; }
  }
  return 0;
}

void
substitutionInit(struct substitution* sub)
{
  size_tArrayInit(&sub->vars, 1);
  symstringArrayInit(&sub->subs, 1);
/* we don't initialize isMarked */
}

void
substitutionClean(struct substitution* sub)
{
  size_t i;
  for (i = 0; i < sub->subs.size; i++) {
    symstringClean(&sub->subs.vals[i]);
  }
  symstringArrayClean(&sub->subs);
  size_tArrayClean(&sub->vars);
}

void
substitutionUnmark(struct substitution* sub, size_t size)
{
  size_t i;
  for (i = 0; i < size; i++) {
    symstringAdd(&sub->isMarked, 0);
  }
}

void
substitutionAdd(struct substitution* sub, size_t var, struct symstring* str)
{
  size_tArrayAdd(&sub->vars, var);
  symstringArrayAdd(&sub->subs, *str);
}

/* sub->isMarked must have been initialized before calling this */
void
substitutionSubstitute(struct substitution* sub, size_t varId, 
  struct symstring* str)
{
  DEBUG_ASSERT(varId < sub->vars.size,
   "invalid varId: varId is %lu but size is %lu", varId, sub->vars.size);
  size_t i = 0;
  const size_t var = sub->vars.vals[varId];
  const size_t len = sub->subs.vals[varId].size;
  while (i < str->size) {
    if (str->vals[i] == var && !sub->isMarked.vals[i]) {
      symstringDelete(str, i);
      symstringInsert(str, i, &sub->subs.vals[varId]);
/* insert markers */
      symstringDelete(&sub->isMarked, i);
      size_t j;
      struct symstring tmp;
      symstringInit(&tmp);
      for (j = 0; j < len; j++) {
        symstringAdd(&tmp, 1);
      }
      symstringInsert(&sub->isMarked, i, &tmp);
      symstringClean(&tmp);
/* skip over the string we inserted */
      i += len;
    } else {
      i++;
    }
  }
}

/* do a simultaneous substitution */
void
substitutionApply(struct substitution* sub, struct symstring* str)
{
  size_t i;
/* initialize the marker */
  symstringInit(&sub->isMarked);
  substitutionUnmark(sub, str->size);
  for (i = 0; i < sub->vars.size; i++) {
    substitutionSubstitute(sub, i, str);
  }
  symstringClean(&sub->isMarked);
}
