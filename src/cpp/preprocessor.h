#pragma once

#include <stdio.h>  // FILE*

enum IncludeOrder {
  INC_NORMAL,
  INC_SYSTEM,
  INC_AFTER,
};

void init_preprocessor(FILE *ofp);
int preprocess(FILE *fp, const char *filename);

void define_macro(const char *arg);  // "FOO" or "BAR=QUX"
void add_inc_path(enum IncludeOrder order, const char *path);
