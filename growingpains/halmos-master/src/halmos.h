#ifndef _HALMOSHALMOS_H_
#define _HALMOSHALMOS_H_

enum halmosflag {
  halmosflag_none = 0,
  halmosflag_verbose, /* set the verbosity */
  halmosflag_summary, /* print summary at the end */
  halmosflag_preproc, /* preprocessing only */
  halmosflag_no_preproc, /* disable preprocessing */
  halmosflag_no_verify, /* disable verification */
  halmosflag_report_count, /* report count of symbols by type */
  halmosflag_report_hash, /* report count of hash collisions */
  halmosflag_report_time, /* report the processing time spent */
  halmosflag_help, /* show help message */
  // halmosflag_include,
  halmosflag_size
};

struct halmos {
  char flags[halmosflag_size];
  char** flagsArgv[halmosflag_size];
};

void
halmosInit(struct halmos* h);

void
halmosClean(struct halmos* h);

void
halmosCompile(struct halmos* h, const char* filename);

/* the front end */
int
halmosMain(int argc, char* argv[]);

#endif
