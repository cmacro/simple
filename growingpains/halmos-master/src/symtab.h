#ifndef _HALMOSSYMTAB_H_
#define _HALMOSSYMTAB_H_
#include "array.h"
#include <stdint.h>

struct symbol;
typedef struct symbol symbol;
DECLARE_ARRAY(symbol)

enum symType {
  symType_none,
  symType_constant,
  symType_variable,
  symType_disjoint,
  symType_floating,
  symType_essential,
  symType_assertion,
  symType_provable,
  symType_size
};

const char* symTypeString(enum symType type);

struct symbol {
  struct charArray sym;
  enum symType type;
/* 1 if the symbol is currently in scope */
/* used for checking freshness */
  int isActive;
/* 1 if the symbol is a variable and typed through $f */
  int isTyped;
/* the nesting level */
  size_t scope;
/* for $f, $e, $a, and $p statements */
  size_t stmt;
/* for $a and $p assertions */
  size_t frame;
/* index to verifier->files, which is an array of readers */
  size_t file; 
  size_t line; 
  size_t offset; 
};

struct symnode;

struct symnode {
/* hash */
  uint32_t h;
/* index to the symbol in the array. If p is 0, we are in an empty node */
  size_t symId;
/* if there is a collision, point to the next node */
/* If NULL, there are no more nodes */
  struct symnode* next;
};

struct symtree;
struct symtree {
  struct symnode node;
  struct symtree* less;
  struct symtree* more;
};

// struct symtab {
//   struct symtree t;
//  we want the symbol table here, but keep it in verifier for now 
//   size_t seed;
// };

void
symbolInit(struct symbol* sym);

void
symbolClean(struct symbol* sym);

void
symnodeInit(struct symnode* n);

void
symnodeClean(struct symnode* n);

void
symnodeInsert(struct symnode* n, uint32_t h, size_t symId);

void
symtreeInit(struct symtree* t);

void
symtreeClean(struct symtree* t);

/* insert p with key h */
void
symtreeInsert(struct symtree* t, uint32_t h, size_t symId);

/* find the first subtree with node of the given hash, or if none exists, */
/* the last subtree searched */
struct symtree*
symtreeFind(struct symtree* t, uint32_t h);

// void
// symtabInit(struct symtab* tab);

// void
// symtabClean(struct symtab* tab);

// void
// symtabAdd(struct symtab* tab, struct symbol* sym);

/* find the subtree matching the hash of sym, or if none exists, the last */
/* subtree searched */
// struct symtree*
// symtabFind(struct symtab* tab, const char* sym);

#endif
