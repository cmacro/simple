#include "charstring.h"

/* we want to write 'struct charstring' and 'struct charstringArray'. We */
/* #defined charstring as charArray in the header, so undo this temporarily. */
#ifdef charstring
#undef charstring
DEFINE_ARRAY(charstring)
#define charstring charArray
#endif

void
charstringInit(struct charstring* s)
{
  charArrayInit(s, 1);
}

void
charstringClean(struct charstring* s)
{
  charArrayClean(s);
}
