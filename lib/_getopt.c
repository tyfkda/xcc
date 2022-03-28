#include "getopt.h"

#include <stdbool.h>
#include <stddef.h>  // NULL
#include <stdio.h>
#include <string.h>

int optind, optopt;
int opterr = 1;
char *optarg;

static int short_idx;

#define ERROR(...)  do { if (opterr) fprintf(stderr, __VA_ARGS__); } while (0)

int getopt_long(int argc, char *const argv[], const char *optstring,
                const struct option *longopts, int *longindex) {
  if (optind == 0) {
    optind = 1;
  }

  if (optind >= argc)
    return -1;

  optarg = NULL;
  optopt = 0;
  char *p;
  int opt;
  if (short_idx > 0) {
    p = &argv[optind][short_idx];
    opt = *p++;
  } else {
    p = argv[optind];
    if (*p != '-')
      return -1;
    opt = *(++p);
    ++p;
  }

  if (short_idx == 0 && opt == '-') {
    ++optind;
    // Check long options.
    if (longopts != NULL) {
      for (int idx = 0; longopts->name != NULL; ++longopts, ++idx) {
        size_t len = strlen(longopts->name);
        if (strncmp(p, longopts->name, len) == 0) {
          opt = longopts->val;
          char *q = p + len;
          char c = *q;
          if (longopts->has_arg) {
            if (c != '\0') {
              if (c != '=')
                continue;
              optarg = q + 1;
            } else if (optind < argc) {
              optarg = argv[optind++];
            } else {
              ERROR("%s: option '--%s' requires an argument\n", argv[0], longopts->name);
              break;
            }
          } else {
            if (c != '\0') {
              if (c != '=')
                continue;
              ERROR("%s: option '--%s' doesn't allow an argument\n", argv[0], longopts->name);
              break;
            }
          }

          if (longindex != NULL)
            *longindex = idx;
          return opt;
        }
      }
    }

    if (longopts->name == NULL) {
      // No matches.
      ERROR("%s: unrecognized option '--%s'\n", argv[0], p);
    }
  } else {
    // Check short options.
    for (; *optstring != '\0'; ++optstring) {
      char s = *optstring;
      if (s == ':')
        continue;
      if (opt != s)
        continue;

      if (optstring[1] == ':') {
        char c = *p;
        if (c != '\0') {
          optarg = c == '=' ? p + 1 : p;
        } else if (optind + 1 < argc) {
          optarg = argv[++optind];
        } else {
          ERROR("%s: option requires an argument -- '%c'\n", argv[0], opt);
          break;
        }
        short_idx = 0;
        ++optind;
      } else {
        if (*p == '\0') {
          short_idx = 0;
          ++optind;
        } else {
          short_idx = p - argv[optind];
        }
      }
      return opt;
    }

    if (*p == '\0') {
      short_idx = 0;
      ++optind;
    } else {
      short_idx = p - argv[optind];
    }
    if (*optstring == '\0') {
      // No matches.
      ERROR("%s: invalid option -- '%c'\n", argv[0], opt);
    }
  }

  optopt = opt;
  opt = '?';
  return opt;
}
