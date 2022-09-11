#pragma once

#include <stdbool.h>

typedef struct Name Name;
typedef struct StringBuffer StringBuffer;
typedef struct Token Token;
typedef struct Vector Vector;

enum SegmentKind {
  SK_TEXT,
  SK_PARAM,
  SK_STRINGIFY,
};

typedef struct {
  enum SegmentKind kind;
  union {
    const char *text;
    int param;
  };
} Segment;

typedef struct Macro {
  Vector *params;  // <const char*>
  bool va_args;
  Vector *segments;  // <Segment*>
} Macro;

Macro *new_macro(Vector *params, bool va_args, Vector *segments);
Macro *new_macro_single(const char *text);

bool expand_macro(Macro *macro, Vector *args, const Token *token, StringBuffer *sb);

//

void macro_add(const Name *name, Macro *macro);
Macro *macro_get(const Name *name);
void macro_delete(const Name *name);
