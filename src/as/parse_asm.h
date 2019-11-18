// Parser for assembler

#pragma once

#include <stdbool.h>
#include <stdio.h>

#include "inst.h"

typedef struct Vector Vector;

typedef struct Line {
  const char *rawline;
  const char *label;

  Inst inst;

  enum DirectiveType dir;
  const char *directive_line;
} Line;

extern int current_section;  // enum SectionType
extern bool err;

Line *parse_line(const char *rawline);
void handle_directive(enum DirectiveType dir, const char *p, Vector **section_irs);
