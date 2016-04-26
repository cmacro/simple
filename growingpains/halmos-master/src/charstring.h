#ifndef _HALMOSCHARSTRING_H_
#define _HALMOSCHARSTRING_H_

#include "array.h"

typedef struct charArray charstring;
DECLARE_ARRAY(charstring)

/* we do this to be able to write 'struct charstring' */
#define charstring charArray

void
charstringInit(struct charstring* s);

void
charstringClean(struct charstring* s);

#endif
