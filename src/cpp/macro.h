#pragma once

#include <stdbool.h>

typedef struct Name Name;
typedef struct StringBuffer StringBuffer;
typedef struct Token Token;
typedef struct Vector Vector;

enum SegmentKind {
  SK_TEXT,
  SK_PARAM,
};

typedef struct {
  enum SegmentKind kind;
  union {
    const char *text;
    int param;
  };
} Segment;

typedef struct {
  Vector *params;  // <const char*>
  bool va_args;
  Vector *segments;  // <Segment*>
} Macro;

Macro *new_macro(Vector *params, bool va_args, Vector *segments);
Macro *new_macro_single(const char *text);

void expand(Macro *macro, const Token *token, Vector *args, const Name *name, StringBuffer *sb);
