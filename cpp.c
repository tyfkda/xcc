#include "ctype.h"
#include "libgen.h"  // dirname
#include "stdarg.h"
#include "stdio.h"
#include "stdlib.h"
#include "string.h"

#include "util.h"

const char *skip_whitespaces(const char *s) {
  while (isspace(*s))
    ++s;
  return s;
}

const char *keyword(const char *s, const char *word) {
  size_t len = strlen(word);
  if (strncmp(s, word, len) != 0 || !isspace(s[len]))
    return NULL;
  return skip_whitespaces(s + (len + 1));
}

const char *find_directive(const char *line) {
  const char *p = skip_whitespaces(line);
  if (*p != '#')
    return NULL;
  return skip_whitespaces(p + 1);
}

void pp(FILE *fp, const char *filename);

void handle_include(const char *p, const char *srcname) {
  char close;
  switch (*p++) {
  case '"':
    close = '"';
    break;
  case '<':
    close = '>';
    break;
  default:
    error("syntax error");
    break;
  }

  const char *q;
  for (q = p; *q != close; ++q) {
    if (*q == '\0')
      error("not closed");
  }

  char *dir = dirname(strdup_(srcname));
  char *fn;
  if (*p == '/') {
    fn = strndup_(p, q - p);
  } else {
    size_t dirlen = strlen(dir);
    size_t fnlen = q - p;
    fn = malloc(dirlen + fnlen + 2);
    strcpy(fn, dir);
    strcpy(fn + dirlen, "/");
    memcpy(fn + dirlen + 1, p, fnlen);
    fn[dirlen + 1 + fnlen] = '\0';
  }

  FILE *fp = fopen(fn, "r");
  if (fp == NULL)
    error("Cannot open file: %s", fn);
  printf("/* \"%s\" start */\n", fn);
  pp(fp, fn);
  printf("/* \"%s\" end */\n", fn);
  fclose(fp);
}

void pp(FILE *fp, const char *filename) {
  char *line = NULL;
  size_t capa = 0;

  for (;;) {
    if (getline_(&line, &capa, fp) == EOF)
      break;

    // Find '#'
    const char *directive = find_directive(line);
    if (directive == NULL) {
      printf("%s\n", line);
      continue;
    }

    const char *next;
    if ((next = keyword(directive, "include")) != NULL) {
      handle_include(next, filename);
    } else {
      printf("unknown directive: %s", directive);
    }
  }
  free(line);
}

int main(int argc, char* argv[]) {
  if (argc > 1) {
    for (int i = 1; i < argc; ++i) {
      const char *filename = argv[i];
      FILE *fp = fopen(filename, "rb");
      if (fp == NULL)
        error("Cannot open file: %s\n", argv[i]);
      pp(fp, filename);
      fclose(fp);
    }
  } else {
    pp(stdin, "*stdin*");
  }
  return 0;
}
