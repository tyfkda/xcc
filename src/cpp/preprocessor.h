#pragma once

#include <stdbool.h>
#include <stdio.h>  // FILE*

enum IncludeOrder {
  INC_NORMAL,
  INC_SYSTEM,
  INC_AFTER,
};

void init_preprocessor(FILE *ofp);
void set_preserve_comment(bool enable);
void preprocess(FILE *fp, const char *filename);

void define_macro(const char *arg);  // "FOO" or "BAR=QUX"
void add_inc_path(enum IncludeOrder order, const char *path);

// Handle preprocessor directives and
// returns non-directive next line.
//   NULL: EOF
const char *get_processed_next_line(void);
