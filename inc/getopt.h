#pragma once

#define no_argument        (0)
#define required_argument  (1)
#define optional_argument  (2)

struct option {
  const char *name;
  int has_arg;
  int *flag;
  int val;
};

extern int optind, opterr, optopt;
extern char *optarg;

int getopt_long(int argc, char *const argv[], const char *optstring,
                const struct option *longopts, int *longindex);
