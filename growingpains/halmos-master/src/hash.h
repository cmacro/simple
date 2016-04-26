#ifndef _HALMOSHASH_H_
#define _HALMOSHASH_H_

#include <stddef.h>
#include <stdint.h>

typedef uint32_t (*hashfunction)(const char*, size_t, uint32_t);

uint32_t hash_djb2(const char*, size_t, uint32_t);

uint32_t hash_murmur3(const char*, size_t, uint32_t);

#endif
