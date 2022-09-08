#pragma once

#include <stdbool.h>

typedef struct Name Name;
typedef struct StringBuffer StringBuffer;
typedef struct Table Table;
typedef struct Token Token;
typedef struct Vector Vector;

typedef struct Macro {
  Table *param_table;  // key=variable name, value=parameter index
  int params_len;  // -1 => no param macro
  bool va_args;
  Vector *body;  // <Token*>
} Macro;

Macro *new_macro(Vector *params, bool va_args, Vector *body);
bool expand_macro(Macro *macro, Vector *args, const Token *token, Vector *expanded);

//

void macro_add(const Name *name, Macro *macro);
Macro *macro_get(const Name *name);
void macro_delete(const Name *name);
