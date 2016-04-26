#ifndef _HALMOSVERIFIER_H_
#define _HALMOSVERIFIER_H_
#include "array.h"
#include "charstring.h"
#include "error.h"
#include "frame.h"
#include "reader.h"
#include "symstring.h"
#include "symtab.h"

/* data for processing compressed proofs */
struct proof {
/* labels used in the proof which are not in the mandatory hypothesis */
  struct symstring dependencies;
/* tagged proof steps */
  struct symstringArray tags;
};

void
proofInit(struct proof* prf);

void
proofClean(struct proof* prf);

extern const size_t symbol_none_id;
extern const size_t file_none_id;

struct verifier {
/* a table of symbols */
  struct symbolArray symbols;
/* for looking up symbols */
  struct symtree tab;
  struct symstringArray stmts;
  struct frameArray frames;
/* disjoint variable restrictions currently in scope */
  struct size_tArray disjoint1;
  struct size_tArray disjoint2;
/* number of restrictions added since the beginning of each scope level */
  struct size_tArray disjointScope;
/* floating and essential hypotheses currently in scope */
  struct symstring hypotheses;
/* variables currently in scope */
  struct symstring variables;
/* reverse polish notation stack for verifying proofs */
  struct symstringArray stack;
/* the file currently being verified */
  struct reader* r;
/* a special file with id 0 */
  struct charstring file_none;
/* a list of filenames */
  struct charstringArray files;
/* id of the current file */
  size_t rId;
/* nesting level */
  size_t scope;
/* count of symbols parsed */
  size_t symCount[symType_size];
/* error from most recent operation */
  enum error err;
/* the number of errors reported */
  size_t errc;
/* logging verbosity */
  size_t verb;
/* number of hash collisions encountered */
  size_t hashc;
/* to do: have a dynamic array of errors */
};

void
verifierInit(struct verifier* vrf);

void
verifierClean(struct verifier* vrf);

void
verifierEmptyStack(struct verifier* vrf);

size_t
verifierGetSymId(struct verifier* vrf, const char* sym);

/* return the symId of the symbol added */
size_t
verifierAddSymbolExplicit(struct verifier* vrf, const char* sym,
  struct symtree* t, enum symType type, int isActive, int isTyped,
  size_t scope, size_t stmt, size_t frame, size_t file, size_t line,
  size_t offset, uint32_t hash);

size_t
verifierAddSymbol(struct verifier* vrf, const char* sym, enum symType type);

size_t
verifierAddConstant(struct verifier* vrf, const char* sym);

size_t
verifierAddVariable(struct verifier* vrf, const char* sym);

/* return the id of the statement */
size_t
verifierAddStatement(struct verifier* vrf, struct symstring* stmt);

/* add every pair of variables in stmt as a disjoint variable restriction  */
void
verifierAddDisjoint(struct verifier* vrf, struct symstring* stmt);

size_t
verifierAddFloating(struct verifier* vrf, const char* sym, 
  struct symstring* stmt);

size_t
verifierAddEssential(struct verifier* vrf, const char* sym,
  struct symstring* stmt);

/* return the id of the frame */
size_t
verifierAddFrame(struct verifier* vrf, struct frame* frm);

size_t
verifierAddAssertion(struct verifier* vrf, const char* sym,
  struct symstring* stmt);

size_t
verifierAddProvable(struct verifier* vrf, const char* sym,
  struct symstring* stmt, struct frame* frm);

void
verifierDeactivateSymbols(struct verifier* vrf);

void
verifierGetVariables(struct verifier* vrf, struct symstring* set,
  const struct symstring* stmt);

void
verifierMakeFrame(struct verifier* vrf, struct frame* frm, 
  const struct symstring* stmt);

int
verifierIsValidDisjointPairSubstitution(struct verifier* vrf,
  const struct frame* ctx, const struct frame* frm,
  const struct substitution* sub, size_t v1,
 size_t v2);

int
verifierIsValidSubstitution(struct verifier* vrf, const struct frame* ctx,
  const struct frame* frm, const struct substitution* sub);

void
verifierUnify(struct verifier* vrf, struct substitution* sub, 
  const struct symstring* a, const struct symstring* floating);

void
verifierApplyAssertion(struct verifier* vrf, const struct frame* ctx,
  size_t symId);

void
verifierCheckProof(struct verifier* vrf, const struct symstring* thm);

char*
verifierParseSymbol(struct verifier* vrf, int* isEndOfStatement, char end);

void
verifierParseStatementContent(struct verifier* vrf, struct symstring* stmt,
  char end);

void 
verifierParseConstants(struct verifier* vrf);

void
verifierParseVariables(struct verifier* vrf);

void
verifierParseDisjoints(struct verifier* vrf, struct symstring* stmt);

/* stmt must be initialized */
void
verifierParseFloating(struct verifier* vrf, struct symstring* stmt);

void
verifierParseEssential(struct verifier* vrf, struct symstring* stmt);

void
verifierParseAssertion(struct verifier* vrf, struct symstring* stmt);

void
verifierParseProofSymbol(struct verifier* vrf, const struct frame* ctx,
  int* isEndOfProof);

void
verifierParseProof(struct verifier* vrf, const struct frame* ctx);

void
verifierParseProvable(struct verifier* vrf, struct symstring* stmt,
  struct frame* frm);

void
verifierParseUnlabelledStatement(struct verifier* vrf, int* isEndOfScope,
 const char* tok);

void
verifierParseLabelledStatement(struct verifier* vrf, const char* tok);

void
verifierParseStatement(struct verifier* vrf, int* isEndOfScope);

void
verifierParseBlock(struct verifier* vrf);

void
verifierBeginReadingFile(struct verifier* vrf, struct reader* r);

void
verifierSetVerbosity(struct verifier* vrf, size_t verb);

void
verifierCompile(struct verifier* vrf, const char* in);

#endif
