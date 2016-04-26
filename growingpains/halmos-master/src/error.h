#ifndef _HALMOSERROR_H_
#define _HALMOSERROR_H_

enum error {
  error_none = 0,
/* internal errors */
  error_endOfString,
  error_endOfFile,
  error_failedFileOpen,
  error_failedFileClose,
  error_invalidReaderMode,
/* preprocessor errors */
  error_nestedComment,
  error_unterminatedComment,
  error_expectedClosingBracket,
  error_unterminatedFileInclusion,
  error_failedOpenFile,
/* external (user) errors */
  error_expectedNewLine,
  error_expectedConstantSymbol,
  error_expectedVariableSymbol,
  error_expectedFloatingSymbol,
  error_expectedKeyword,
  error_unexpectedKeyword,
  error_unterminatedStatement,
  error_unterminatedScope,
  error_untypedVariable,
  error_invalidSymbol,
  error_invalidLabel,
  error_undefinedSymbol,
  error_duplicateSymbol,
  error_duplicateFile,
  error_invalidKeyword,
  error_invalidFloatingStatement,
  error_invalidEssentialStatement,
  error_invalidAssertionStatement,
  error_stackUnderflow,
  error_mismatchedType,
  error_mismatchedEssentialHypothesis,
  error_invalidSubstitutionOfDisjoint,
  error_missingDisjointRestriction,
  error_invalidSymbolInProof,
  error_unusedTermInProof,
  error_incorrectProof,
  error_invalidCompressedProof,
  error_invalidCharacterInCompressedProof,
  error_unterminatedCompressedProof,
  error_invalidTagReferenceInCompressedProof,
  error_invalidFile,
  error_expectedFilename,
  error_expectedLineNumber,
  error_size
};

const char* errorString(enum error err);

#endif
