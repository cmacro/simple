#include "verifier.h"
#include "hash.h"
#include "logger.h"

DEFINE_ARRAY(symbol)

const char* symTypeStrings[symType_size] = {
  "none",
  "constant",
  "variable",
  "disjoint",
  "floating",
  "essential",
  "assertion",
  "provable"
};

const char*
symTypeString(enum symType type)
{
  return symTypeStrings[type];
}

const size_t symbol_none_id = 0;
const size_t file_none_id = 0;

void
proofInit(struct proof* prf)
{
  symstringInit(&prf->dependencies);
  symstringArrayInit(&prf->tags, 1);
}

void
proofClean(struct proof* prf)
{
  size_t i;
  for (i = 0; i < prf->tags.size; i++) {
    symstringClean(&prf->tags.vals[i]);
  }
  symstringArrayClean(&prf->tags);
  symstringClean(&prf->dependencies);
}

void
verifierInit(struct verifier* vrf)
{
  size_t i;
  symbolArrayInit(&vrf->symbols, 1);
/* add symbol_none. This is not added to tab */
  struct symbol none;
  symbolInit(&none);
  charArrayAppend(&none.sym, "$none", 5 + 1);
  symbolArrayAdd(&vrf->symbols, none);
  symtreeInit(&vrf->tab);
  // verifierAddSymbolExplicit(vrf, "$none", symType_none, 0, 0, 0, 0, 0, 0, 0, 0,
  //  hash_murmur3("$none", 5, 0));
  symstringArrayInit(&vrf->stmts, 1);
  frameArrayInit(&vrf->frames, 1);
  size_tArrayInit(&vrf->disjoint1, 1);
  size_tArrayInit(&vrf->disjoint2, 1);
  size_tArrayInit(&vrf->disjointScope, 1);
  symstringInit(&vrf->hypotheses);
  symstringInit(&vrf->variables);
  symstringArrayInit(&vrf->stack, 1);
  charstringArrayInit(&vrf->files, 1);
/* add 'none' file */
  charstringInit(&vrf->file_none);
  charArrayAppend(&vrf->file_none, "", 1);
  charstringArrayAdd(&vrf->files, vrf->file_none);
  vrf->rId = file_none_id;
  vrf->scope = 0;
  for (i = 0; i < symType_size; i++) {
    vrf->symCount[i] = 0;
  }
  vrf->err = error_none;
  vrf->errc = 0;
  vrf->verb = 1;
  vrf->hashc = 0;
}

void
verifierClean(struct verifier* vrf)
{
  size_t i;
  for (i = 0; i < vrf->files.size; i++) {
    charstringClean(&vrf->files.vals[i]);
  }
  charstringArrayClean(&vrf->files);
  for (i = 0; i < vrf->stack.size; i++) {
    symstringClean(&vrf->stack.vals[i]);
  }
  symstringArrayClean(&vrf->stack);
  symstringClean(&vrf->variables);
  symstringClean(&vrf->hypotheses);
  size_tArrayClean(&vrf->disjointScope);
  size_tArrayClean(&vrf->disjoint2);
  size_tArrayClean(&vrf->disjoint1);
  for (i = 0; i < vrf->frames.size; i++) {
    frameClean(&vrf->frames.vals[i]);
  }
  frameArrayClean(&vrf->frames);
  for (i = 0; i < vrf->stmts.size; i++) {
    symstringClean(&vrf->stmts.vals[i]);
  }
  symstringArrayClean(&vrf->stmts);
  symtreeClean(&vrf->tab);
  for (i = 0; i < vrf->symbols.size; i++) {
    symbolClean(&vrf->symbols.vals[i]);
  }
  symbolArrayClean(&vrf->symbols);
  vrf->r = NULL;
}

void
verifierEmptyStack(struct verifier* vrf)
{
  size_t i;
  for (i = 0; i < vrf->stack.size; i++) {
    symstringClean(&vrf->stack.vals[i]);
  }
  symstringArrayEmpty(&vrf->stack);
}

static const char whitespace[] = " \t\r\f\n";

void
verifierSetError(struct verifier* vrf, enum error err)
{
  vrf->err = err;
  vrf->errc++;
/* to do: add vrf->errorsum for more details on the error */
}

size_t
verifierGetSymId(struct verifier* vrf, const char* sym)
{
  DEBUG_ASSERT(sym, "given symbol is NULL");
/* linear search */
  // size_t i;
  // for (i = 0; i < vrf->symbols.size; i++) {
  //   const struct symbol* s = &vrf->symbols.vals[i];
  //   DEBUG_ASSERT(s->sym.vals, 
  //     "symbol from symbol table is NULL");
  //   if ((strcmp(s->sym.vals, sym) == 0) && s->isActive) {
  //     return i;
  //   }
  // }
/* binary tree search */
  uint32_t hash = hash_murmur3(sym, strlen(sym), 0);
  struct symtree* t = symtreeFind(&vrf->tab, hash);
  if (t->node.h == hash) {
/* this is either a match or a hash collision */
    struct symnode* n = &t->node;
    while (n != NULL) {
      const struct symbol* s = &vrf->symbols.vals[n->symId];
      if (s->isActive) {
        if (strcmp(s->sym.vals, sym) == 0) {
          return n->symId;
        }
      }
      n = n->next;
    }
  }
  return symbol_none_id;
}

const char*
verifierGetSymName(const struct verifier* vrf, size_t symId)
{
  return vrf->symbols.vals[symId].sym.vals;
}

const char*
verifierPrintSym(const struct verifier* vrf, struct charArray* msg,
  const struct symstring* str)
{
  size_t i;
  for (i = 0; i < str->size; i++) {
    const char* name = verifierGetSymName(vrf, str->vals[i]);
    charArrayAppend(msg, name, strlen(name));
    charArrayAdd(msg, ' ');
  }
/* get rid of the extra space */
  if (msg->size > 0) {
    msg->size--;
  }
  charArrayAdd(msg, '\0');
  return msg->vals;
}

const char*
verifierPrintFrame(const struct verifier* vrf, struct charArray* msg,
  const struct frame* frm)
{
  // const char* h1 = "frame of ";
  const char* h2 = "mandatory hypotheses:\n";
  const char* h3 = "disjoint variable restrictions:\n";
  size_t i;
  // charArrayAppend(msg, h1, strlen(h1));
  // charArrayAppend(msg, sym->sym.vals, strlen(sym->sym.vals));
  charArrayAppend(msg, h2, strlen(h2));
  for (i = 0; i < frm->stmts.size; i++) {
    const struct symbol* s = &vrf->symbols.vals[frm->stmts.vals[i]];
    const char* type = symTypeString(s->type);
    const char* name = s->sym.vals;
    charArrayAppend(msg, type, strlen(type));
    charArrayAdd(msg, ' ');
    charArrayAppend(msg, name, strlen(name));
    charArrayAppend(msg, ": ", 2);
    verifierPrintSym(vrf, msg, &vrf->stmts.vals[s->stmt]);
/* get rid of the \0 */
    msg->size--;
    charArrayAdd(msg, '\n');
  }
  charArrayAppend(msg, h3, strlen(h3));
  for (i = 0; i < frm->disjoint1.size; i++) {
    const char* var1 = verifierGetSymName(vrf, frm->disjoint1.vals[i]);
    const char* var2 = verifierGetSymName(vrf, frm->disjoint2.vals[i]);
    charArrayAppend(msg, var1, strlen(var1));
    charArrayAdd(msg, ' ');
    charArrayAppend(msg, var2, strlen(var2));
    charArrayAdd(msg, '\n');
  }
  charArrayAdd(msg, '\0');
  return msg->vals;
}

size_t
verifierGetFileId(const struct verifier* vrf, const char* file)
{
  DEBUG_ASSERT(file, "file is NULL");
  size_t i;
/* don't compare with file_none */
  for (i = 1; i < vrf->files.size; i++) {
    if (strcmp(vrf->files.vals[i].vals, file) == 0) {
      return i;
    }
  }
/* reserved value */
  return 0;
}

int
verifierIsType(const struct verifier* vrf, size_t symId, enum symType type)
{
  DEBUG_ASSERT(symId < vrf->symbols.size, "invalid symId");
  return vrf->symbols.vals[symId].type == type;
}

size_t
verifierPreprocAddFile(struct verifier* vrf, const char* filename)
{
  size_t fid = verifierGetFileId(vrf, filename);
/* did we already add the file? */
  if (fid != file_none_id) { return fid; }
/* we add the new file */
  size_t len = strlen(filename) + 1;
  struct charstring f;
  charArrayInit(&f, len);
  charArrayAppend(&f, filename, len);
  charstringArrayAdd(&vrf->files, f);
  return vrf->files.size - 1;
}

void
verifierPreprocBeginReading(struct verifier* vrf, size_t rId, size_t line)
{
  DEBUG_ASSERT(rId < vrf->files.size, "%lu is an invalid file", rId);
  vrf->rId = rId;
/* reset the line number and offset */
  vrf->r->line = line;
  vrf->r->offset = 0;
}

/* check the label contains only alphanumeric characters and -, _, and .. */
int
verifierIsValidLabel(const struct verifier* vrf, const char* lab)
{
  const char* valid = "abcdefghijklmnopqrstuvwxyz"
  "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789-_.";
  (void) vrf;
  size_t i;
  const size_t len = strlen(lab);
  for (i = 0; i < len; i++) {
    if (!strchr(valid, lab[i])) { return 0; }
  }
  return 1;
}

/* note: this should not be called except from AddSymbol */
/* In unit testing, it is better to AddSymbol(), then set the relevant */
/* symbol data manually. */
/* fix me: specify the tree to insert in */
size_t
verifierAddSymbolExplicit(struct verifier* vrf, const char* sym, 
  struct symtree* t, enum symType type, int isActive, int isTyped,
  size_t scope, size_t stmt, size_t frame, size_t file, size_t line,
  size_t offset, uint32_t hash)
{
  size_t symId = vrf->symbols.size;
/* symIds begin at 1 because 0 is reserved for symbol_none_id. */
/* symbol_none_id is used in symtree to represent empty nodes. Adding 0 */
/* could cause a leak */
  DEBUG_ASSERT(symId != symbol_none_id, "tried adding symbol_none");
  struct symbol s;
  symbolInit(&s);
  size_t len = strlen(sym);
/* append the sym and \0 */
  charArrayAppend(&s.sym, sym, len + 1);
  s.type = type;
  s.isActive = isActive;
  s.isTyped = isTyped;
  s.scope = scope;
  s.stmt = stmt;
  s.frame = frame;
  s.file = file;
  s.line = line;
  s.offset = offset;
  symbolArrayAdd(&vrf->symbols, s);
  vrf->symCount[type]++;
  symtreeInsert(t, hash, symId);
  return symId;
}

size_t
verifierAddSymbol(struct verifier* vrf, const char* sym, enum symType type)
{
/* symbol names cannot contain $ */
  if (strchr(sym, '$')) {
    H_LOG_ERR(vrf, error_invalidSymbol, 1, "%s contains $", sym);
    return symbol_none_id;
  }
  if ((type == symType_floating) || (type == symType_essential)
    || (type == symType_assertion) || (type == symType_provable)) {
    if (!verifierIsValidLabel(vrf, sym)) {
      H_LOG_ERR(vrf, error_invalidLabel, 1,
        "label %s contains an invalid character", sym);
      return symbol_none_id;
    }
  }
/* determine if sym is a duplicate symbol */
  uint32_t hash = hash_murmur3(sym, strlen(sym), 0);
  struct symtree* t = symtreeFind(&vrf->tab, hash);
  DEBUG_ASSERT(t->node.symId < vrf->symbols.size, "invalid symId");
  const struct symbol* s = &vrf->symbols.vals[t->node.symId];
  if (s->isActive && t->node.h == hash) {
    if (strcmp(s->sym.vals, sym) == 0) {
/* to do: print file and line num */
      H_LOG_ERR(vrf, error_duplicateSymbol, 1, "%s was declared before", sym);
      return symbol_none_id;
    } else {
/* we have a hash collision. Record and move on */
      vrf->hashc++;
      H_LOG_INFO(vrf, 5, "hash collision with %s and %s", s->sym.vals, sym);
    }
  }
/* add the symbol */
/* we add directly to symtree t to avoid doing another search */
  size_t symId = verifierAddSymbolExplicit(vrf, sym, t, type, 1, 0,
    vrf->scope, vrf->stmts.size, vrf->frames.size, vrf->rId,
    vrf->r->line, vrf->r->offset, hash);
  if (type == symType_floating || type == symType_essential) {
    symstringAdd(&vrf->hypotheses, symId);
  } else if (type == symType_variable) {
    symstringAdd(&vrf->variables, symId);
  }
  return symId;
}

size_t
verifierAddConstant(struct verifier* vrf, const char* sym)
{
  DEBUG_ASSERT(vrf->rId < vrf->files.size, "invalid file id");
  return verifierAddSymbol(vrf, sym, symType_constant);
}

size_t
verifierAddVariable(struct verifier* vrf, const char* sym)
{
  return verifierAddSymbol(vrf, sym, symType_variable);
}

size_t
verifierAddStatement(struct verifier* vrf, struct symstring* stmt)
{
  symstringArrayAdd(&vrf->stmts, *stmt);
  return vrf->stmts.size - 1;
}

/* add every pair of variables */
void
verifierAddDisjoint(struct verifier* vrf, struct symstring* stmt)
{
  size_t i, j;
  DEBUG_ASSERT(vrf->disjointScope.size > 0, "disjointScope empty");
  H_LOG_INFO(vrf, 4,
    "adding entries for each disjoint variable restriction pair");
  if (stmt->size > 0) {
    for (i = 0; i < stmt->size - 1; i++) {
      for (j = i + 1; j < stmt->size; j++) {
        size_tArrayAdd(&vrf->disjoint1, stmt->vals[i]);
        size_tArrayAdd(&vrf->disjoint2, stmt->vals[j]);
        vrf->disjointScope.vals[vrf->disjointScope.size - 1]++;
        vrf->symCount[symType_disjoint]++;
        H_LOG_INFO(vrf, 5, "added $d %s %s $.", 
          verifierGetSymName(vrf, stmt->vals[i]),
          verifierGetSymName(vrf, stmt->vals[j]));
      }
    }
  }
/* clean up the stmt */
  symstringClean(stmt);
}

size_t
verifierAddFloating(struct verifier* vrf, const char* sym, 
  struct symstring* stmt)
{
  size_t symId = verifierAddSymbol(vrf, sym, symType_floating);
  vrf->symbols.vals[symId].stmt = verifierAddStatement(vrf, stmt);
  if (stmt->size >= 2) {
    vrf->symbols.vals[stmt->vals[1]].isTyped = 1;
  }
  return symId;
}

size_t
verifierAddEssential(struct verifier* vrf, const char* sym,
 struct symstring* stmt)
{
  size_t symId = verifierAddSymbol(vrf, sym, symType_essential);
  vrf->symbols.vals[symId].stmt = verifierAddStatement(vrf, stmt);
  return symId;
}

size_t
verifierAddFrame(struct verifier* vrf, struct frame* frm)
{
  frameArrayAdd(&vrf->frames, *frm);
  return vrf->frames.size - 1;
}

size_t
verifierAddAssertion(struct verifier* vrf, const char* sym, 
  struct symstring* stmt)
{
  size_t symId = verifierAddSymbol(vrf, sym, symType_assertion);
  vrf->symbols.vals[symId].stmt = verifierAddStatement(vrf, stmt);
  struct frame frm; 
  frameInit(&frm);
  verifierMakeFrame(vrf, &frm, stmt);
  vrf->symbols.vals[symId].frame = verifierAddFrame(vrf, &frm);
  return symId;
}

size_t
verifierAddProvable(struct verifier* vrf, const char* sym,
  struct symstring* stmt, struct frame* frm)
{
  size_t symId = verifierAddSymbol(vrf, sym, symType_provable);
  vrf->symbols.vals[symId].stmt = verifierAddStatement(vrf, stmt);
  vrf->symbols.vals[symId].frame = verifierAddFrame(vrf, frm);
  return symId;
}

void
verifierDeactivateDisjointVariableRestrictions(struct verifier* vrf)
{
  size_t i;
  DEBUG_ASSERT(vrf->disjointScope.size > 0, "disjointScope empty");
  size_t end = vrf->disjointScope.vals[vrf->disjointScope.size - 1];
  for (i = 0; i < end; i++) {
    vrf->disjoint1.size--;
    vrf->disjoint2.size--;
  }
  vrf->disjointScope.size--;
}

/* deactivate non-global symbols in the current nesting level */
void
verifierDeactivateSymbols(struct verifier* vrf)
{
  H_LOG_INFO(vrf, 4, "deactivating symbols going out of scope");
  size_t i;
  size_t scope = vrf->scope;
  struct symstring* hypotheses = &vrf->hypotheses;
  for (i = hypotheses->size; i > 0; i--) {
    struct symbol* sym = &vrf->symbols.vals[hypotheses->vals[i - 1]];
    if (sym->scope < scope) { break; }
    if (sym->type == symType_floating) {
/* untype the variable referenced by the floating hypothesis */
      size_t var = vrf->stmts.vals[sym->stmt].vals[1];
      vrf->symbols.vals[var].isTyped = 0;
    }
    sym->isActive = 0;
    hypotheses->size--;
  }
  struct symstring* variables = &vrf->variables;
  for (i = variables->size; i > 0; i--) {
    struct symbol* sym = &vrf->symbols.vals[variables->vals[i - 1]];
    if (sym->scope < scope) { break; }
    sym->isActive = 0;
    variables->size--;
  }
}

/* add the set of variables in str to set */
void
verifierGetVariables(struct verifier* vrf, struct symstring* set, 
  const struct symstring* str)
{
  vrf->err = error_none;
  size_t i;
  for (i = 0; i < str->size; i++) {
    size_t sym = str->vals[i];
    DEBUG_ASSERT(sym < vrf->symbols.size, "invalid symId %lu, size is %lu",
      sym, vrf->symbols.size);
    if (vrf->symbols.vals[sym].type != symType_variable) { continue; }
    if (symstringIsIn(set, sym)) { continue; }
    symstringAdd(set, sym);
  }
}

/* make the mandatory frame for the assertion stmt. frm->stmts will be in */
/* reverse order i.e. frm->stmts.vals[0] will be the last item */
void
verifierMakeFrame(struct verifier* vrf, struct frame* frm,
  const struct symstring* stmt)
{
  size_t i;
/* add disjoints */
  for (i = 0; i < vrf->disjoint1.size; i++) {
    frameAddDisjoint(frm, vrf->disjoint1.vals[i], vrf->disjoint2.vals[i]);
  }
/* the set of mandatory variables */
  struct symstring varset;
  symstringInit(&varset);
/* add the variables referenced in the assertion */
  verifierGetVariables(vrf, &varset, stmt);
/* look at the hypotheses in reverse order (latest first) */
  struct symstring* hypotheses = &vrf->hypotheses;
  for (i = hypotheses->size; i > 0; i--) {
    size_t symId = hypotheses->vals[i - 1];
    DEBUG_ASSERT(symId < vrf->symbols.size, "invalid symId");
    struct symbol* hypothesis = &vrf->symbols.vals[symId];
    if (hypothesis->type == symType_floating) {
/* get the variable symbol of the floating */
      size_t varId = vrf->stmts.vals[hypothesis->stmt].vals[1];
      if (symstringIsIn(&varset, varId)) {
/* we found a mandatory hypothesis */
        symstringAdd(&frm->stmts, symId);
      }
    } else if (hypothesis->type == symType_essential) {
/* add variables referenced in the hypothesis to varset */
      verifierGetVariables(vrf, &varset, &vrf->stmts.vals[hypothesis->stmt]);
      symstringAdd(&frm->stmts, symId);
    }
  }
  symstringClean(&varset);
}

/* From the metamath specification: */
/* */
/* Each substitution made in a proof must be checked to verify that any */
/* disjoint variable restrictions are satisfied, as follows. */
/* If two variables replaced by a substitution exist in a mandatory $d */
/* statement of the assertion referenced, the two expressions resulting */
/* from the substitution must meet satisfy the following conditions. */
/* First, the two ex- pressions must have no variables in common. Second, */
/* each possible pair of variables, one from each expression, must exist in */
/* an active $d statement of the $p statement containing the proof. */
/* */
/* ctx is the frame of the theorem being proved */
/* frm is the frame of the assertion or theorem being applied */
/* v1 and v2 are indices into sub->vars */
/* v1 and v2 must refer to variables with the disjoint restriction in frame */
/* frm. */
int
verifierIsValidDisjointPairSubstitution(struct verifier* vrf,
  const struct frame* ctx, const struct frame* frm,
  const struct substitution* sub, size_t v1, size_t v2)
{
  vrf->err = error_none;
  DEBUG_ASSERT(sub->vars.size == sub->subs.size, "invalid substitution");
  DEBUG_ASSERT(v1 < sub->vars.size, "invalid variable index to substitution");
  DEBUG_ASSERT(v2 < sub->vars.size, "invalid variable index to substitution");
  size_t varId1 = sub->vars.vals[v1];
  size_t varId2 = sub->vars.vals[v2];
  DEBUG_ASSERT(varId1 < vrf->symbols.size, "invalid varId %lu", varId1);
  DEBUG_ASSERT(varId2 < vrf->symbols.size, "invalid varId %lu", varId2);
  DEBUG_ASSERT(frameAreDisjoint(frm, varId1, varId2),
    "%s and %s are not disjoint", verifierGetSymName(vrf, varId1),
    verifierGetSymName(vrf, varId2));
  struct symstring s1, s2;
  symstringInit(&s1);
  symstringInit(&s2);
/* check the substitution has no common variables */
  verifierGetVariables(vrf, &s1, &sub->subs.vals[v1]);
  verifierGetVariables(vrf, &s2, &sub->subs.vals[v2]);
  if (symstringIsIntersecting(&s1, &s2)) {
  /* to do: pretty-print the intersecting set */
    H_LOG_ERR(vrf, error_invalidSubstitutionOfDisjoint, 1,
      "disjoint variables %s and %s share a variable in their " 
      "substitutions", verifierGetSymName(vrf, varId1), 
      verifierGetSymName(vrf, varId2));
  }
/* don't do this if there already was an error above. */
/* Check each pair of variables from s1 and s2 have the disjoint variable */
/* restriction on them inside the context */
  size_t i, j;
  for (i = 0; i < s1.size; i++) {
    if (vrf->err) { break; }
    for (j = 0; j < s2.size; j++) {
      if (!frameAreDisjoint(ctx, s1.vals[i], s2.vals[j])) {
/* to do: say which assertion / theorem */
        H_LOG_ERR(vrf, error_missingDisjointRestriction, 1,
        "the variables %s and %s should be disjoint", 
          verifierGetSymName(vrf, s1.vals[i]),
          verifierGetSymName(vrf, s2.vals[j]));
        break;
      }
    }
    if (vrf->err) {
/* fix me: roll my own printf to avoid doing this */
      struct charArray msg;
      charArrayInit(&msg, 1);
      H_LOG_INFO(vrf, 2, "the current frame is\n%s",
       verifierPrintFrame(vrf, &msg, frm));
      charArrayClean(&msg);
    }
  }
/* clean up */
  symstringClean(&s2);
  symstringClean(&s1);
  return !(vrf->err);
}

/* ensure that the substitution respects the disjoint-variable restriction */
/* ctx is the frame of the theorem being proved */
/* frm is the frame of the assertion or theorem being applied */
int
verifierIsValidSubstitution(struct verifier* vrf, const struct frame* ctx,
  const struct frame* frm, const struct substitution* sub)
{
  vrf->err = error_none;
  if (sub->vars.size == 0) { return 1; }
  size_t i, j;
  for (i = 0; i < sub->vars.size - 1; i++) {
    for (j = i + 1; j < sub->vars.size; j++) {
      if (!frameAreDisjoint(frm, sub->vars.vals[i], sub->vars.vals[j])) { 
        continue;
      }
      if (!verifierIsValidDisjointPairSubstitution(vrf, ctx, frm, sub, i, j)) {
        return 0;
      }
    }
  }
  return 1;
}

void
verifierPushSymbol(struct verifier* vrf, size_t symId)
{
  DEBUG_ASSERT(symId < vrf->symbols.size, "invalid symId");
  const struct symbol* sym = &vrf->symbols.vals[symId];
  DEBUG_ASSERT(sym->stmt < vrf->stmts.size, "invalid statement");
  const struct symstring* stmt = &vrf->stmts.vals[sym->stmt];
  struct symstring entry;
  symstringInit(&entry);
  symstringAppend(&entry, stmt);
  symstringArrayAdd(&vrf->stack, entry);
}

/* pop the top of the stack. The caller is responsible for cleaning the */
/* returned symstring */
struct symstring
verifierPop(struct verifier* vrf)
{
  struct symstring str;
  vrf->err = error_none;
  if (vrf->stack.size == 0) {
/* to do: ... in proof of what? */
    H_LOG_ERR(vrf, error_stackUnderflow, 1,
      "stack is empty");
    return str;
  }
  str = vrf->stack.vals[vrf->stack.size - 1];
  vrf->stack.size--;
  return str;
}

/* get substitution for floating to match a. */
void
verifierUnify(struct verifier* vrf, struct substitution* sub,
 const struct symstring* a, const struct symstring* floating)
{
  if (floating->size != 2) {
    struct charArray ca;
    charArrayInit(&ca, 1);
    H_LOG_ERR(vrf, error_invalidFloatingStatement, 1,
     "cannot unify with an invalid floating statement %s",
     verifierPrintSym(vrf, &ca, floating));
    return;
  }
  DEBUG_ASSERT(a->size >= 1, "cannot unify empty string");
  if (a->vals[0] != floating->vals[0]) {
    struct charArray ca1, ca2;
    charArrayInit(&ca1, 1);
    charArrayInit(&ca2, 1);
    H_LOG_ERR(vrf, error_mismatchedType, 1,
      "cannot unify %s with %s", verifierPrintSym(vrf, &ca1, a),
      verifierPrintSym(vrf, &ca2, floating));
    charArrayClean(&ca1);
    charArrayClean(&ca2);
  }
  struct symstring str;
  symstringInit(&str);
/* get rid of the first constant symbol (the type symbol) */
  size_tArrayAppend(&str, &a->vals[1], a->size - 1);
  substitutionAdd(sub, floating->vals[1], &str);
}

/* use an assertion or a theorem. Pop the appropriate number of entries, */
/* type-check, do unification and push the result */
/* ctx is the frame of the theorem being proved */
void
verifierApplyAssertion(struct verifier* vrf, const struct frame* ctx,
  size_t symId)
{
  size_t i;
  vrf->err = error_none;
  DEBUG_ASSERT(symId < vrf->symbols.size, "invalid symId %lu", symId);
  const struct symbol* sym = &vrf->symbols.vals[symId];
  DEBUG_ASSERT(sym->frame < vrf->frames.size, "invalid frame %lu", sym->frame);
/* frame of the assertion or theorem being applied */
  const struct frame* frm = &vrf->frames.vals[sym->frame];
  const size_t argc = frm->stmts.size;
/* we pop into this array. The last one out is the first argument to the */
/* assertion. */
  struct symstringArray args;
  symstringArrayInit(&args, argc);
  for (i = 0; i < argc; i++) {
/* note: popping and adding to the array causes the order of arguments to be */
/* reversed */
    struct symstring str = verifierPop(vrf);
    if (vrf->err) { break; }
    symstringArrayAdd(&args, str);
  }
/* get the patterns to match with the arguments, i.e. the mandatory */
/* hypotheses of the assertion */
/* note: frm->stmts are in reverse order */
  struct symstringArray pats;
  symstringArrayInit(&pats, 1);
  for (i = 0; i < argc; i++) {
    struct symstring str;
    symstringInit(&str);
    size_t frmSymId = frm->stmts.vals[i];
    size_t stmtId = vrf->symbols.vals[frmSymId].stmt;
    symstringAppend(&str, &vrf->stmts.vals[stmtId]);
    symstringArrayAdd(&pats, str);
  }
/* create the substitution by unifying $f statements */
  struct substitution sub;
  substitutionInit(&sub);
  for (i = 0; i < args.size; i++) {
    if (!verifierIsType(vrf, frm->stmts.vals[argc - 1 - i],
      symType_floating)) {
      continue;
    }
/* reverse the order of args and pats, then unify */
    verifierUnify(vrf, &sub, &args.vals[args.size - 1 - i],
     &pats.vals[argc - 1 - i]);
  }
/* check that the disjoint-variable restrictions are satisfied. If invalid, */
/* vrf->err will be set */
  verifierIsValidSubstitution(vrf, ctx, frm, &sub);
/* apply the substitution to $e hypotheses and check if they match the args */
  for (i = 0; i < args.size; i++) {
    if (vrf->err) { break; }
    if (!verifierIsType(vrf, frm->stmts.vals[argc - 1 - i],
      symType_essential)) {
      continue;
    }
    substitutionApply(&sub, &pats.vals[argc - 1 - i]);
    if (!symstringIsEqual(&args.vals[args.size - 1 - i],
      &pats.vals[argc - 1 - i])) {
      struct charArray ca1, ca2;
      charArrayInit(&ca1, 1);
      charArrayInit(&ca2, 1);
      H_LOG_ERR(vrf, error_mismatchedEssentialHypothesis, 1,
        "the argument %s does not match hypothesis %s",
        verifierPrintSym(vrf, &ca1, &args.vals[args.size - 1 - i]),
        verifierPrintSym(vrf, &ca2, &pats.vals[argc - 1 - i]));
      charArrayClean(&ca1);
      charArrayClean(&ca2);
    }
  }
/* build the result to push */
  struct symstring res;
  symstringInit(&res);
  symstringAppend(&res, &vrf->stmts.vals[sym->stmt]);
  substitutionApply(&sub, &res);
  symstringArrayAdd(&vrf->stack, res);
/* clean up */
  substitutionClean(&sub);
  for (i = 0; i < pats.size; i++) {
    symstringClean(&pats.vals[i]);
  }
  symstringArrayClean(&pats);
  for (i = 0; i < args.size; i++) {
    symstringClean(&args.vals[i]);
  }
  symstringArrayClean(&args);
}

/* apply the label with symId to the current proof. If it is $f or $e, */
/* we push it to the stack. If it is $a or $p, we apply the assertion and */
/* push the result */
/* ctx is the frame of the theorem being proved */
void
verifierApplySymbolToProof(struct verifier* vrf, const struct frame* ctx,
  size_t symId)
{
  DEBUG_ASSERT(symId < vrf->symbols.size, "invalid symId");
  const struct symbol* sym = &vrf->symbols.vals[symId];
  if ((sym->type == symType_floating) 
    || (sym->type == symType_essential)) {
/* push floating or essential on the stack */
    verifierPushSymbol(vrf, symId);
  } else if ((sym->type == symType_provable) 
    || (sym->type == symType_assertion)) {
    verifierApplyAssertion(vrf, ctx, symId);
  } else {
    H_LOG_ERR(vrf, error_invalidSymbolInProof, 1,
      "%s is %s statement", verifierGetSymName(vrf, symId),
      symTypeString(sym->type));
  }
}

/* report errors if the proof is wrong */
void
verifierCheckProof(struct verifier* vrf, const struct symstring* thm)
{
  if (vrf->stack.size > 1) {
/* to do: show which terms are unused */
    H_LOG_ERR(vrf, error_unusedTermInProof, 1,
      "the proof contains unused terms");
  }
  if (vrf->stack.size == 0) {
    H_LOG_ERR(vrf, error_incorrectProof, 1, "the proof is empty");
  } else if (!symstringIsEqual(&vrf->stack.vals[0], thm)) {
    struct charArray res, theorem;
    charArrayInit(&res, 1);
    charArrayInit(&theorem, 1);
    H_LOG_ERR(vrf, error_incorrectProof, 1,
      "%s was derived but the proof requires %s",
      verifierPrintSym(vrf, &res, &vrf->stack.vals[0]),
      verifierPrintSym(vrf, &theorem, thm));
    charArrayClean(&res);
    charArrayClean(&theorem);
  }
}

/* check all variables in the statement are typed */
int
verifierIsTyped(struct verifier* vrf, struct symstring* stmt)
{
  size_t i;
  vrf->err = error_none;
  for (i = 0; i < stmt->size; i++) {
    const size_t symId = stmt->vals[i];
    DEBUG_ASSERT(symId < vrf->symbols.size, "invalid symId");
    const struct symbol* sym = &vrf->symbols.vals[symId];
    if (sym->type != symType_variable) { continue; }
    if (!sym->isTyped) {
      H_LOG_ERR(vrf, error_untypedVariable, 1,
        "The symbol %s is untyped", verifierGetSymName(vrf, symId));
      return 0;
    }
  }
  return 1;
}

char* 
verifierParseSymbol(struct verifier* vrf, int* isEndOfStatement, char end)
{
  char* tok;
  vrf->err = error_none;
  *isEndOfStatement = 0;
  readerSkip(vrf->r, whitespace);
/* check for end of file */
  if (vrf->r->err == error_endOfString || vrf->r->err == error_endOfFile) {
    H_LOG_ERR(vrf, error_unterminatedStatement, 1,
      "reached end of file before end of statement");
    return NULL;
  }
  tok = readerGetToken(vrf->r, whitespace);
/* check for end of statement */
  if (tok[0] == '$') {
    size_t len = strlen(tok);
    if (len != 2) {
      H_LOG_ERR(vrf, error_invalidKeyword, 1, 
        "%s is not a valid keyword", tok);
    }
    if (tok[1] == end) {
      *isEndOfStatement = 1;
    } else {
      H_LOG_ERR(vrf, error_unexpectedKeyword, 1,
        "expected $%c instead of %s", end, tok);
    }
    return tok;
  }
/* check for end of file */
  if (vrf->r->err == error_endOfString || vrf->r->err == error_endOfFile) {
    H_LOG_ERR(vrf, error_unterminatedStatement, 1,
    "reached end of file before $%c", end);
    return tok;
  }
  return tok;
}

/* Parse the comment of the form '$( filename line $)' written by the */
/* preprocessor to indicate change of file */
void
verifierParsePreprocFile(struct verifier* vrf)
{
  vrf->err = error_none;
  int isEndOfStatement = 0;
/* get the filename */
  char* tok = verifierParseSymbol(vrf, &isEndOfStatement, ')');
  if (vrf->r->err) { return; }
  if (isEndOfStatement) {
/* fix me: should we keep track of the file line of the raw preprocessed file to */
/* report errors like this? */
    H_LOG_ERR(vrf, error_expectedFilename, 1, "a filename must follow $(");
    return;
  }
  size_t rId = verifierPreprocAddFile(vrf, tok);
  tok = verifierParseSymbol(vrf, &isEndOfStatement, ')');
  if (vrf->r->err) { return; }
  if (isEndOfStatement) {
    H_LOG_ERR(vrf, error_expectedLineNumber, 1,
      "a line number must follow filename");
  }
/* convert the string to a number */
/* fix me: do error checking? */
  size_t line = strtoul(tok, NULL, 10);
  verifierPreprocBeginReading(vrf, rId, line);
/* go to the end of the comment, ignoring anything else without reporting */
  while (1) {
    readerFind(vrf->r, "$");
    tok = readerGetToken(vrf->r, whitespace);
    if (vrf->r->err) { break; }
    if (strlen(tok) != 2) { continue; }
    if (tok[1] == ')') { break; }
  }
}

/* parses statements (excluding proofs) of the form */
/* <constant> <symbol list>. */
/* fix me: remove definition & type checks and let the caller handle them. */
void
verifierParseStatementContent(struct verifier* vrf, struct symstring* stmt,
  char end)
{
  vrf->err = error_none;
  char* tok;
  int isEndOfStatement;
  size_t symId;
  while (1) {
    tok = verifierParseSymbol(vrf, &isEndOfStatement, end);
    if (vrf->err) { break; }
    DEBUG_ASSERT(tok, "tok is NULL");
    if (isEndOfStatement) { break; }
    symId = verifierGetSymId(vrf, tok);
    if (symId == symbol_none_id) {
      H_LOG_ERR(vrf, error_undefinedSymbol, 1, "%s was not defined", tok);
    }
    symstringAdd(stmt, symId);
  }
}

void
verifierParseConstants(struct verifier* vrf)
{
  vrf->err = error_none;
  int isEndOfStatement;
  char* tok;
  while (!vrf->err) {
    tok = verifierParseSymbol(vrf, &isEndOfStatement, '.');
    if (vrf->err) { return; }
    if (isEndOfStatement) { break; }
    verifierAddConstant(vrf, tok);
  }
}

void
verifierParseVariables(struct verifier* vrf)
{
  vrf->err = error_none;
  int isEndOfStatement;
  char* tok;
  while (!vrf->err) {
    tok = verifierParseSymbol(vrf, &isEndOfStatement, '.');
    if (vrf->err) { return; }
    if (isEndOfStatement) { break; }
    verifierAddVariable(vrf, tok);
  }
}

void
verifierParseDisjoints(struct verifier* vrf, struct symstring* stmt)
{
  size_t i;
  verifierParseStatementContent(vrf, stmt, '.');
  verifierIsTyped(vrf, stmt);
/* all symbols must be variables */
  for (i = 0; i < stmt->size; i++) {
    if (!verifierIsType(vrf, stmt->vals[i], symType_variable)) {
      H_LOG_ERR(vrf, error_expectedVariableSymbol, 1,
        "%s is not a variable", verifierGetSymName(vrf, stmt->vals[i]));
    }
  }
}

void 
verifierParseFloating(struct verifier* vrf, struct symstring* stmt)
{
  verifierParseStatementContent(vrf, stmt, '.');
  if (stmt->size != 2) {
    H_LOG_ERR(vrf, error_invalidFloatingStatement, 1,
      "invalid floating statement has size %lu, expected 2", stmt->size);
    return;
  }
  DEBUG_ASSERT(stmt->vals[0] < vrf->symbols.size, "invalid symId at vals[0]");
  if (!verifierIsType(vrf, stmt->vals[0], symType_constant)) {
    H_LOG_ERR(vrf, error_expectedConstantSymbol, 1, 
      "%s is not a constant symbol", 
      verifierGetSymName(vrf, stmt->vals[0]));
  }
  DEBUG_ASSERT(stmt->vals[1] < vrf->symbols.size, "invalid symId at vals[1]");
  if (!verifierIsType(vrf, stmt->vals[1], symType_variable)) {
    H_LOG_ERR(vrf, error_expectedVariableSymbol, 1, 
      "%s is not a variable symbol",
      verifierGetSymName(vrf, stmt->vals[1]));
  }
}

void
verifierParseEssential(struct verifier* vrf, struct symstring* stmt)
{
  H_LOG_INFO(vrf, 5, "parsing essential statement");
  verifierParseStatementContent(vrf, stmt, '.');
  H_LOG_INFO(vrf, 6, "done parsing essential statement, checking size");
  if (stmt->size == 0) {
    H_LOG_ERR(vrf, error_invalidEssentialStatement, 1, 
      "essential statements must at least contain a constant symbol");
    return;
  }
  H_LOG_INFO(vrf, 6, "checking first symbol is a constant");
  DEBUG_ASSERT(stmt->vals[0] < vrf->symbols.size, "invalid symId");
  if (!verifierIsType(vrf, stmt->vals[0], symType_constant)) {
    H_LOG_ERR(vrf, error_expectedConstantSymbol, 1, 
      "%s is not a constant symbol",
      verifierGetSymName(vrf, stmt->vals[0]));
  }
  H_LOG_INFO(vrf, 6, "checking variables are typed");
  verifierIsTyped(vrf, stmt);
}

void
verifierParseAssertion(struct verifier* vrf, struct symstring* stmt)
{
  verifierParseStatementContent(vrf, stmt, '.');
  if (stmt->size == 0) {
    H_LOG_ERR(vrf, error_invalidAssertionStatement, 1,
      "an assertion must have at least a constant symbol");
    return;
  }
  if (!verifierIsType(vrf, stmt->vals[0], symType_constant)) {
    H_LOG_ERR(vrf, error_expectedConstantSymbol, 1,
     "%s is not a constant symbol",
      verifierGetSymName(vrf, stmt->vals[0]));
  }
  verifierIsTyped(vrf, stmt);
}

/* ctx is the frame of the theorem being proved */
void
verifierParseProofSymbol(struct verifier* vrf, const struct frame* ctx,
  int* isEndOfProof)
{
  char* tok;
  vrf->err = error_none;
  *isEndOfProof = 0;
  tok = verifierParseSymbol(vrf, isEndOfProof, '.');
  if (vrf->err) { return; }
  if (*isEndOfProof) { return; }
  size_t symId = verifierGetSymId(vrf, tok);
  if (symId == symbol_none_id) { 
    H_LOG_ERR(vrf, error_undefinedSymbol, 1, "%s was not defined", tok);
    return;
  }
  verifierApplySymbolToProof(vrf, ctx, symId);
}

/* thm is the theorem to prove */
void
verifierParseProof(struct verifier* vrf, const struct frame* ctx)
{
  vrf->err = error_none;
  int isEndOfProof = 0;
  verifierEmptyStack(vrf);
  while (!vrf->err && !isEndOfProof) {
    verifierParseProofSymbol(vrf, ctx, &isEndOfProof);
  }
}

/* collect the dependencies. We are past the left parenthesis */
void
verifierParseCompressedProofHeader(struct verifier* vrf, struct proof* prf)
{
  char* tok;
  while (1) {
    readerSkip(vrf->r, whitespace);
    tok = readerGetToken(vrf->r, whitespace);
    if (vrf->r->err) { break; }
    size_t len = strlen(tok);
    if ((len == 1) && (tok[0] == ')')) {
      break;
    }
/* we have a label for adding to dependencies */
    size_t symId = verifierGetSymId(vrf, tok);
    if (symId == symbol_none_id) {
      H_LOG_ERR(vrf, error_undefinedSymbol, 1, "%s was not defined", tok);
      continue;
    }
    symstringAdd(&prf->dependencies, symId);
  }
}

/* get the next number represented in the compressed proof. */
/* fix me: how much error checking should be done on compressed proofs? */
size_t
verifierParseCompressedProofNumber(struct verifier* vrf, int* isEndOfProof,
  int* isTagged)
{
  const char* base5 = "UVWXY";
  const char* base20 = "ABCDEFGHIJKLMNOPQRST";
  size_t num = 0;
  int c;
  *isEndOfProof = 0;
  *isTagged = 0;
  vrf->err = error_none;
  while (1) {
    readerSkip(vrf->r, whitespace);
    c = readerGet(vrf->r);
    if (c == '$') {
      c = readerGet(vrf->r);
      if (c == '.') {
        *isEndOfProof = 1;
/* note: 0 is unrepresentable in this compression scheme, so it could be */
/* used instead of isEndOfProof */
        return 0;
      }
    }
    if (vrf->r->err) {
      H_LOG_ERR(vrf, error_unterminatedCompressedProof, 1,
        "compressed proof ended before $.");
      return 0;
    }
    if (strchr(base5, c)) {
      num = num * 5 + (c - 'U' + 1);
    } else if (strchr(base20, c)) {
      num = num * 20 + (c - 'A' + 1);
/* prepare to look at the next non-whitespace */
      readerSkip(vrf->r, whitespace);
      if (readerPeek(vrf->r) == 'Z') {
/* get rid of the 'Z' */
        readerGet(vrf->r);
        *isTagged = 1;
      }
      return num;
    } else {
      H_LOG_ERR(vrf, error_invalidCharacterInCompressedProof, 1,
        "%c is invalid", c);
/* try again until a valid character is found */
    }
  }
}

/* ctx is the frame of the theorem being proved */
void
verifierParseCompressedProof(struct verifier* vrf, const struct frame* ctx)
{
  verifierEmptyStack(vrf);
  struct proof prf;
  proofInit(&prf);
  verifierParseCompressedProofHeader(vrf, &prf);
  int isEndOfProof = 0;
  while (1) {
    int isTagged = 0;
    int isTagRef = 0;
    size_t i =
      verifierParseCompressedProofNumber(vrf, &isEndOfProof, &isTagged);
    if (isEndOfProof) { break; }
    size_t symId = symbol_none_id;
    size_t k = prf.tags.size;
    size_t m = ctx->stmts.size;
    size_t n = prf.dependencies.size;
/* decode the number. Let m be the number of mandatory hypotheses and let n */
/* be the number of labels in the header. If 1 <= i <= m, i refers to */
/* the i-th mandatory hypothesis. If m + 1 <= i <= m + n, i refers to the */
/* i - m th label in the header. If m + n + 1 <= i, i refers to the */
/* i - (m + n) th tagged step of the proof. */
    if ((1 <= i) && (i <= m)) {
/* the frame is stored in reverse order */
      symId = ctx->stmts.vals[m - i];
    } else if ((m + 1 <= i) && (i <= m + n)) {
      symId = prf.dependencies.vals[i - (m + 1)];
    } else if ((m + n + 1 <= i) && (i <= m + n + k)) {
/* we have a tag reference */
      isTagRef = 1;
    } else {
      H_LOG_ERR(vrf, error_invalidTagReferenceInCompressedProof, 1,
        "the compressed proof contains an invalid reference");
    }
    if (symId != symbol_none_id) {
      verifierApplySymbolToProof(vrf, ctx, symId);
    } else if (isTagRef) {
/* push the symstring to the stack */
      struct symstring tag;
      symstringInit(&tag);
      symstringAppend(&tag, &prf.tags.vals[i - (m + n + 1)]);
      symstringArrayAdd(&vrf->stack, tag);
    }
    if (isTagged) {
/* add the current result to the tagged list */
      struct symstring tag;
      symstringInit(&tag);
      symstringAppend(&tag, &vrf->stack.vals[vrf->stack.size - 1]);
      symstringArrayAdd(&prf.tags, tag);
    }
  }
  proofClean(&prf);
}

void
verifierParseProvable(struct verifier* vrf, struct symstring* stmt, 
  struct frame* ctx)
{
  verifierParseStatementContent(vrf, stmt, '=');
  verifierIsTyped(vrf, stmt);
  verifierMakeFrame(vrf, ctx, stmt);
/* check if we have a compressed proof */
  readerSkip(vrf->r, whitespace);
  if (readerPeek(vrf->r) == '(') {
/* get rid of the ( */
    readerGet(vrf->r);
    verifierParseCompressedProof(vrf, ctx);
  } else {
    verifierParseProof(vrf, ctx);
  }
  verifierCheckProof(vrf, stmt);
}

/* parse $c, $v, or $d statements, or a ${ block. */
void
verifierParseUnlabelledStatement(struct verifier* vrf, int* isEndOfScope,
  const char* tok)
{
  size_t len = strlen(tok);
  if (len != 2) {
    H_LOG_ERR(vrf, error_invalidKeyword, 1, "%s is not a keyword", tok);
    return;
  }
  if (tok[1] == '(') {
/* this is a special comment left by the preprocessor to indicate change of */
/* filename */
    verifierParsePreprocFile(vrf);
  } else if (tok[1] == 'c') {
    verifierParseConstants(vrf);
  } else if (tok[1] == 'v') {
    verifierParseVariables(vrf);
  } else if (tok[1] == 'd') {
    struct symstring stmt;
    symstringInit(&stmt);
    verifierParseDisjoints(vrf, &stmt);
    verifierAddDisjoint(vrf, &stmt);
  } else if (tok[1] == '{') {
    verifierParseBlock(vrf);
  } else if (tok[1] == '}') {
    *isEndOfScope = 1;
  } else {
    H_LOG_ERR(vrf, error_unexpectedKeyword, 1,
      "expected $c, $v, $d, ${, or $} instead of %s", tok);
  }
}

/* parse $f, $e, $a, or $p after the label */
/* to do: error check on $f and handle case when stmt.size != 2, otherwise */
/* it will trigger an assert */
/* to do: validate label token */
void
verifierParseLabelledStatement(struct verifier* vrf, const char* tok)
{
  vrf->err = error_none;
  readerSkip(vrf->r, whitespace);
  char* keyword = readerGetToken(vrf->r, whitespace);
  if (vrf->r->err) {
    H_LOG_ERR(vrf, error_expectedKeyword, 1,
      "expected keyword after label %s", tok);
    return;
  }
  if (keyword[0] != '$') {
    H_LOG_ERR(vrf, error_expectedKeyword, 1,
      "expected a keyword after the label %s instead of %s", tok, keyword);
    return;
  }
  size_t len = strlen(keyword);
  if (len != 2) {
    H_LOG_ERR(vrf, error_invalidKeyword, 1,
    "%s is not a valid keyword", keyword);
    return;
  }
  enum symType type = symType_none;
  if (keyword[1] == 'f') {
    type = symType_floating;
    struct symstring stmt;
    symstringInit(&stmt);
    verifierParseFloating(vrf, &stmt);
    verifierAddFloating(vrf, tok, &stmt);
  } else if (keyword[1] == 'e') {
    type = symType_essential;
    struct symstring stmt;
    symstringInit(&stmt);
    verifierParseEssential(vrf, &stmt);
    verifierAddEssential(vrf, tok, &stmt);
  } else if (keyword[1] == 'a') {
    type = symType_assertion;
    struct symstring stmt;
    symstringInit(&stmt);
    verifierParseAssertion(vrf, &stmt);
    verifierAddAssertion(vrf, tok, &stmt);
  } else if (keyword[1] == 'p') {
    type = symType_provable;
    struct symstring stmt;
    symstringInit(&stmt);
/* create the frame for this theorem */
    struct frame ctx; 
    frameInit(&ctx);
    verifierParseProvable(vrf, &stmt, &ctx);
    verifierAddProvable(vrf, tok, &stmt, &ctx);
  }
  if (type == symType_none) {
    H_LOG_ERR(vrf, error_unexpectedKeyword, 1,
      "expected $f, $e, $a, or $p instead of %s", keyword);
  }
}

void
verifierParseStatement(struct verifier* vrf, int* isEndOfScope)
{
  vrf->err = error_none;
  char* tok;
  *isEndOfScope = 0;
/* the beginning of the statement. Get the keyword or the label */
  readerSkip(vrf->r, whitespace);
  tok = readerGetToken(vrf->r, whitespace);
/* check if we are at the end of file */
  if (vrf->r->err) {
    *isEndOfScope = 1;
/* the file must end with a new line character, except for empty files. */
/* note: strchr(s, 0) is always true. vrf->r->last is initialized to 0, so */
/* empty files shouldn't raise this error */
    if (vrf->scope != 1) {
      H_LOG_ERR(vrf, error_unterminatedScope, 1,
        "expected $} before end of file");
    }
    if (!strchr("\n\f", vrf->r->last)) {
      H_LOG_ERR(vrf, error_expectedNewLine, 1,
        "expected new line at the end of file");
    }
    return;
  }
  if (tok[0] == '$') {
/* we have a keyword */
    verifierParseUnlabelledStatement(vrf, isEndOfScope, tok);
  } else {
/* we have a label */
/* copy the label, because reader will change tok */
    struct charArray key;
    charArrayInit(&key, strlen(tok) + 1);
/* to do: this is error prone. Define a charstring struct */
    charArrayAppend(&key, tok, strlen(tok) + 1);
    verifierParseLabelledStatement(vrf, key.vals);
    charArrayClean(&key);
  }
}

void
verifierParseBlock(struct verifier* vrf)
{
  int isEndOfScope = 0;
  vrf->scope++;
  size_tArrayAdd(&vrf->disjointScope, 0);
  while (!isEndOfScope) {
    verifierParseStatement(vrf, &isEndOfScope);
  }
/* deactivate local symbols and restrictions in the current nesting level */
  verifierDeactivateSymbols(vrf);
  verifierDeactivateDisjointVariableRestrictions(vrf);
/* go back to the previous nesting level */
  vrf->scope--;
}

void
verifierBeginReadingFile(struct verifier* vrf, struct reader* r)
{
  vrf->r = r;
}

void
verifierSetVerbosity(struct verifier* vrf, size_t verb)
{
  vrf->verb = verb;
}

/* to do: have an output file, for compressed proofs */
void
verifierCompile(struct verifier* vrf, const char* in)
{
  FILE* fin = fopen(in, "r");
  if (!fin) {
    G_LOG_ERR(vrf, error_failedFileOpen, "failed to open input file %s", in);
    return;
  }
  struct reader r;
  readerInitFile(&r, fin, in);
  verifierBeginReadingFile(vrf, &r);
  verifierParseBlock(vrf);
  readerClean(&r);
  fclose(fin);
}
