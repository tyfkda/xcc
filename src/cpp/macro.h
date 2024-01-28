#pragma once

typedef struct Name Name;
typedef struct Table Table;
typedef struct Vector Vector;

typedef struct Macro {
  Table *param_table;  // key=variable name, value=parameter index
  int params_len;  // -1 => no param macro
  const Name *vaargs_ident;
  Vector *body;  // <Token*>
} Macro;

Macro *new_macro(Vector *params, const Name *vaargs_ident, Vector *body);

//

void macro_add(const Name *name, Macro *macro);
Macro *macro_get(const Name *name);
void macro_delete(const Name *name);
void macro_expand(Vector *tokens);
