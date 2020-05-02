#pragma once

#include <stdbool.h>

typedef struct Name Name;
typedef struct StringBuffer StringBuffer;
typedef struct Vector Vector;

enum SegmentType {
  ST_TEXT,
  ST_PARAM,
};

typedef struct {
  enum SegmentType type;
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

void expand(Macro *macro, Vector *args, const Name *name, StringBuffer *sb);
