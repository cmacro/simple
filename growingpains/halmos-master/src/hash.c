#include "hash.h"

uint32_t hash_djb2(const char* str, size_t len, uint32_t seed)
{
  (void) len;
  (void) seed;
  uint32_t h = 5381;
  int c;
  while ((c = *str++)) {
    h = ((h << 5) + h) + c;
  }
  return h;
}

uint32_t hash_murmur3(const char* str, size_t len, uint32_t seed)
{
  uint32_t c1 = 0xcc9e2d51;
  uint32_t c2 = 0x1b873593;
  uint32_t r1 = 15;
  uint32_t r2 = 13;
  uint32_t m = 5;
  uint32_t n = 0xe6546b64;
  uint32_t h = seed;
  size_t block_size = len / 4;
  const uint32_t* blocks = (uint32_t*) str;
  size_t i;
  for (i = 0; i < block_size; i++) {
    uint32_t k = blocks[i];
    k *= c1;
/* rotate left by r1 */
    k = (k << r1) | (k >> (32 - r1));
    k *= c2;
    h ^= k;
    h = (h << r2) | (h >> (32 - r2));
    h = h * m + n;
  }
  const uint8_t* tail = (uint8_t*)&blocks[block_size];
  uint32_t k1 = 0;
  switch (len & 3) {
  case 3:
    k1 ^= tail[2] << 16;
  case 2:
    k1 ^= tail[1] << 8;
  case 1:
    k1 ^= tail[0];
    k1 += c1;
    k1 = (k1 << r1) | (k1 >> (32 - r1));
    k1 += c2;
    h ^= k1;
  }
  h ^= len;
  h ^= (h >> 16);
  h *= 0x85ebca6b;
  h ^= (h >> 13);
  h *= 0xc2b2ae35;
  h ^= (h >> 16);
  return h;
}
