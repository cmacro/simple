#include "dbg.h"
#include "halmos.h"
#include "preproc.h"
#include "verifier.h"
#include <errno.h>
#include <stdio.h>
#include <time.h>

static const char* flags[halmosflag_size] = {
  "",
  "--verbose",
  "--summary",
  "--preproc",
  "--no-preproc",
  "--no-verify",
  "--report-count",
  "--report-hash",
  "--report-time",
  "--help",
  // "--include",
};

static const size_t flagsArgc[halmosflag_size] = {
  0, /* none */
  1, /* verbose */
  0, /* summary */
  1, /* preproc - the output file */
  0, /* no-preproc */
  0, /* no-verify */
  0, /* report-count */
  0, /* report-hash */
  0, /* report-time */
  // 0, /* include */
};

static const char* help =
"NAME\n"
"\thalmos, a compiler for the halmos language\n"
"\n"
"SYNOPSIS\n"
"\thalmos [optons] file\n"
"\n"
"DESCRIPTION\n";
void
halmosInit(struct halmos* h)
{
  size_t i;
  for (i = 0; i < halmosflag_size; i++) {
    h->flags[i] = 0;
    h->flagsArgv[i] = NULL;
  }
}

void
halmosClean(struct halmos* h)
{
  (void) h;
}

void
halmosCompile(struct halmos* h, const char* filename)
{
  size_t i;
  struct preproc p;
  struct verifier vrf;
  double ptime = 0.0;
  double vrftime = 0.0;
  verifierInit(&vrf);
  preprocInit(&p);
  if (h->flags[halmosflag_help]) {
    printf("%s", help);
    h->flags[halmosflag_no_preproc] = 1;
    h->flags[halmosflag_no_verify] = 1;
  }
  if (h->flags[halmosflag_verbose]) {
    errno = 0;
    size_t verb = strtoul(h->flagsArgv[halmosflag_verbose][0], NULL, 10);
    if (errno) {
      printf("%s requires a positive integer\n", flags[halmosflag_verbose]);
      h->flags[halmosflag_no_preproc] = 1;
      h->flags[halmosflag_no_verify] = 1;
    } else {
      verifierSetVerbosity(&vrf, verb);
    }
  }
  if (!h->flags[halmosflag_no_preproc]) {
    printf("------preproc\n");
    printf("------%s\n", filename);
    clock_t start = clock();
    preprocCompile(&p, filename, "out.mm");
    clock_t end = clock();
    ptime = ((double) end - start) / CLOCKS_PER_SEC;
    /* don't verify if preproc failed */
    if (p.errCount > 0) {
      h->flags[halmosflag_no_verify] = 1;
    }
    printf("Found %lu errors\n", p.errCount);
  }
/* don't compile if preproc was specified */
  if (!h->flags[halmosflag_preproc] && !h->flags[halmosflag_no_verify]) {
    printf("------verifier\n");
    clock_t start = clock();
    verifierCompile(&vrf, "out.mm");
    clock_t end = clock();
    vrftime = ((double) end - start) / CLOCKS_PER_SEC;
    printf("Found %lu errors\n", vrf.errc);
  }
  if (h->flags[halmosflag_summary]) {
    printf("------summary\n");
    h->flags[halmosflag_report_count] = 1;
    h->flags[halmosflag_report_hash] = 1;
    h->flags[halmosflag_report_time] = 1;
  }
  if (h->flags[halmosflag_report_count]) {
    printf("------symbol count\n");
    for (i = symType_constant; i < symType_size; i++) {
      printf("Parsed %lu %s symbols\n", vrf.symCount[i], symTypeString(i));
    }
  }
  if (h->flags[halmosflag_report_hash]) {
    printf("------hash collision count\nFound %lu collisions\n", vrf.hashc);
  }
  if (h->flags[halmosflag_report_time]) {
    printf("------processing time\npreprocessing: %lf sec\n"
      "verification: %lf sec\n", ptime, vrftime);
  }
  preprocClean(&p);
  verifierClean(&vrf);
}

enum halmosflag
halmosParseFlag(struct halmos* h, const char* tok)
{
  size_t i;
  for (i = 0; i < halmosflag_size; i++) {
    if (strcmp(flags[i], tok) == 0) {
      h->flags[i] = 1;
      return i;
    }
  }
  return halmosflag_none;
}

int
halmosMain(int argc, char* argv[])
{
  if (argc < 2) {
    printf("Usage: halmos [flags] <filename>\n");
    return 0;
  }
  struct halmos h;
  halmosInit(&h);
  int i;
  for (i = 1; i < argc - 1; i++) {
    enum halmosflag flag = halmosParseFlag(&h, argv[i]);
    if (flag == halmosflag_none) {
      printf("unrecognized flag %s\n", argv[i]);
      return 0;
    }
    if (flagsArgc[flag] > 0) {
      h.flagsArgv[flag] = &argv[i + 1];
    }
    i += flagsArgc[flag];
    if (i >= argc - 1) {
      printf("%s takes %lu arguments\n", flags[flag], flagsArgc[flag]);
      return 0;
    }
  }
  if (i > argc) {
    printf("Missing input file\n");
    return 0;
  }
  halmosCompile(&h, argv[argc - 1]);
  halmosClean(&h);
  return 0;
}
