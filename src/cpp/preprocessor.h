#pragma once

#include <stdbool.h>
#include <stdio.h>  // FILE*

typedef struct Stream Stream;

enum IncludeOrder {
  INC_NORMAL,
  INC_SYSTEM,
  INC_AFTER,
};

void init_preprocessor(FILE *ofp);
void set_preserve_comment(bool enable);
void preprocess(FILE *fp, const char *filename);

char *find_include_file(const char **pp, Stream *stream, bool is_next);

void define_macro(const char *arg);  // "FOO" or "BAR=QUX"
void undef_macro(const char *begin, const char *end);
void add_inc_path(enum IncludeOrder order, const char *path);

// Handle preprocessor directives and
// returns non-directive next line.
//   NULL: EOF
const char *get_processed_next_line(void);
