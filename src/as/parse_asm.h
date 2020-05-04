// Parser for assembler

#pragma once

#include <stdbool.h>

#include "inst.h"  // Inst, DirectiveType

typedef struct Name Name;
typedef struct Table Table;
typedef struct Token Token;
typedef struct Vector Vector;

typedef struct ParseInfo {
  const char *filename;
  int lineno;
  const char *rawline;
  const char *p;

  Token *token;
  const char *next;
} ParseInfo;

typedef struct Line {
  const Name *label;
  Inst inst;
  enum DirectiveType dir;
} Line;

extern int current_section;  // enum SectionType
extern bool err;

Line *parse_line(ParseInfo *info);
void handle_directive(ParseInfo *info, enum DirectiveType dir, Vector **section_irs,
                      Table *label_table);
void parse_error(const ParseInfo *info, const char *message);
