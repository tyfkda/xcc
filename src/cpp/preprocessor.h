#pragma once

#include <stdio.h>  // FILE*

void init_preprocessor(FILE *ofp);
int preprocess(FILE *fp, const char *filename);

void define_macro(const char *arg);  // "FOO" or "BAR=QUX"
void add_system_inc_path(const char *path);
