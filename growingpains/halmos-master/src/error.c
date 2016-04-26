#include "error.h"

/* check this matches with enum error in error.h by looking at line number */
static const char* errorStrings[error_size] = {
  "none",
/* internal errors */
  "endOfString",
  "endOfFile",
  "failedFileOpen",
  "failedFileClose",
  "invalidReaderMode",
/* preprocessor errors */
  "nestedComment",
  "unterminatedComment",
  "expectedClosingBracket",
  "unterminatedFileInclusion",
  "failedOpenFile",
/* external (user) errors */
  "expectedNewLine",
  "expectedConstantSymbol",
  "expectedVariableSymbol",
  "expectedFloatingSymbol",
  "expectedKeyword",
  "unexpectedKeyword",
  "unterminatedStatement",
  "unterminatedScope",
  "untypedVariable",
  "invalidSymbol",
  "invalidLabel",
  "undefinedSymbol",
  "duplicateSymbol",
  "duplicateFile",
  "invalidKeyword",
  "invalidFloatingStatement",
  "invalidEssentialStatement",
  "invalidAssertionStatement",
  "stackUnderflow",
  "mismatchedType",
  "mismatchedEssentialHypothesis",
  "invalidSubstitutionOfDisjoint",
  "missingDisjointRestriction",
  "invalidSymbolInProof",
  "unusedTermInProof",
  "incorrectProof",
  "invalidCompressedProof",
  "invalidCharacterInCompressedProof",
  "unterminatedCompressedProof",
  "invalidTagReferenceInCompressedProof",
  "invalidFile",
  "expectedFilename",
  "unexpectedFilename"
/* error_size */
};

const char*
errorString(enum error err)
{
  return errorStrings[err];
}
