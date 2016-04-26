#include "dbg.h"
#include "hash.h"
#include "symtab.h"
#include "memory.h"

void
symbolInit(struct symbol* sym)
{
  charArrayInit(&sym->sym, 1);
  sym->type = symType_none;
  sym->isActive = 0;
  sym->isTyped = 0;
  sym->scope = 0;
  sym->stmt = 0;
  sym->frame = 0;
}

void
symbolClean(struct symbol* sym)
{
  charArrayClean(&sym->sym);
}

void
symnodeInit(struct symnode* n)
{
  n->h = 0;
  n->symId = 0;
  n->next = NULL;
}

void
symnodeClean(struct symnode* n)
{
  if (n->next != NULL) {
    symnodeClean(n->next);
    free(n->next);
    n->symId = 0;
    n->next = NULL;
  }
}

void
symnodeInsert(struct symnode* n, uint32_t h, size_t symId)
{
  DEBUG_ASSERT(symId != 0, "tried adding symId 0");
  if (n->symId == 0) {
/* we have an empty node */
    n->h = h;
    n->symId = symId;
  } else {
    if (n->next == NULL) {
      n->next = xmalloc(sizeof(struct symnode));
      symnodeInit(n->next);
    }
    symnodeInsert(n->next, h, symId);
  }
}

void
symtreeInit(struct symtree* t)
{
  symnodeInit(&t->node);
  t->less = NULL;
  t->more = NULL;
}

void
symtreeClean(struct symtree* t)
{
  if (t->less != NULL) {
    symtreeClean(t->less);
    free(t->less);
    t->less = NULL;
  }
  if (t->more != NULL) {
    symtreeClean(t->more);
    free(t->more);
    t->more = NULL;
  }
  symnodeClean(&t->node);
}

void
symtreeInsert(struct symtree* t, uint32_t h, size_t symId)
{
  DEBUG_ASSERT(symId != 0, "tried adding symId 0");
  if (t->node.symId == 0) {
/* we are in an empty leaf */
    t->node.h = h;
    t->node.symId = symId;
    t->node.next = NULL;
  } else {
    if (h < t->node.h) {
      if (t->less == NULL) {
        t->less = xmalloc(sizeof(struct symtree));
        symtreeInit(t->less);
      }
      symtreeInsert(t->less, h, symId);
    } else if (h > t->node.h) {
      if (t->more == NULL) {
        t->more = xmalloc(sizeof(struct symtree));
        symtreeInit(t->more);
      }
      symtreeInsert(t->more, h, symId);
    } else {
/* we have a key collision */
      symnodeInsert(&t->node, h, symId);
    }
  }
}

struct symtree*
symtreeFind(struct symtree* t, uint32_t h)
{
  if (h < t->node.h) {
    if (t->less == NULL) { return t; }
    return symtreeFind(t->less, h);
  } else if (h > t->node.h) {
    if (t->more == NULL) { return t; }
    return symtreeFind(t->more, h);
  } else {
    return t;
  }
}

// void
// symtabInit(struct symtab* tab)
// {
//   symtreeInit(&tab->t);
//   tab->seed = 0;
// }

// void
// symtabClean(struct symtab* tab)
// {
//   symtreeClean(&tab->t);
// }

// void
// symtabAdd(struct symtab* tab, struct symbol* sym)
// {
//   uint32_t h = hash_murmur3(sym->sym.vals, strlen(sym->sym.vals), tab->seed);
//   symtreeInsert(&tab->t, h, sym);
// }

// struct symtree*
// symtabFind(struct symtab* tab, const char* sym)
// {
//   uint32_t h = hash_murmur3(sym, strlen(sym), tab->seed);
//   return symtreeFind(&tab->t, h);
// }
