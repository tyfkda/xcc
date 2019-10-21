#include "../../config.h"
#include "var.h"

#include <assert.h>
#include <stdlib.h>  // malloc
#include <string.h>

#include "fe_misc.h"  // alloc_dummy_ident
#include "table.h"
#include "type.h"
#include "util.h"

Vector *static_vars;

int var_find(const Vector *vars, const Name *name) {
  for (int i = 0, len = vars->len; i < len; ++i) {
    VarInfo *info = vars->data[i];
    if (info->ident != NULL && equal_name(info->ident->ident, name))
      return i;
  }
  return -1;
}

VarInfo *var_add(Vector *vars, const Token *token, Type *type, int storage) {
  assert(token == NULL || var_find(vars, token->ident) < 0);
  VarInfo *varinfo = calloc_or_die(sizeof(*varinfo));
  varinfo->ident = token;
  varinfo->type = type;
  varinfo->storage = storage;
  vec_push(vars, varinfo);
  return varinfo;
}

// Global

Scope *global_scope;
static Table global_var_table;

void init_global(void) {
  global_scope = new_scope(NULL);
  global_scope->vars = new_vector();
  table_init(&global_var_table);
}

static VarInfo *define_global(const Token *token, Type *type, int storage) {
  assert(token != NULL);
  const Name *name = token->ident;
  VarInfo *varinfo = table_get(&global_var_table, name);
  if (varinfo != NULL) {
    if (!(varinfo->storage & VS_EXTERN)) {
      assert(storage & VS_EXTERN);
      return varinfo;
    }
    varinfo->ident = token;
    varinfo->type = type;
    varinfo->storage = storage;
    varinfo->global.init = NULL;
  } else {
    // `static' is different meaning for global and local variable.
    varinfo = var_add(global_scope->vars, token, type, storage & ~VS_STATIC);
    varinfo->storage = storage;
    table_put(&global_var_table, name, varinfo);
  }
  return varinfo;
}

// Scope

Scope *new_scope(Scope *parent) {
  Scope *scope = calloc_or_die(sizeof(*scope));
  scope->parent = parent;
  scope->vars = NULL;
  return scope;
}

bool is_global_scope(Scope *scope) {
  assert(scope->parent != NULL || scope == global_scope);  // Global scope is only one.
  return scope->parent == NULL;
}

VarInfo *scope_find(Scope *scope, const Name *name, Scope **pscope) {
  VarInfo *varinfo = NULL;
  for (; scope != NULL; scope = scope->parent) {
    if (is_global_scope(scope)) {
      varinfo = table_get(&global_var_table, name);
      break;
    }

    if (scope->vars != NULL) {
      int idx = var_find(scope->vars, name);
      if (idx >= 0) {
        varinfo = scope->vars->data[idx];
        break;
      }
    }
  }
  if (pscope != NULL)
    *pscope = scope;
  return varinfo;
}

VarInfo *scope_add(Scope *scope, const Token *name, Type *type, int storage) {
  assert(name != NULL);
  if (is_global_scope(scope))
    return define_global(name, type, storage);

  if (scope->vars == NULL)
    scope->vars = new_vector();

  VarInfo *varinfo = var_add(scope->vars, name, type, storage);
  if (storage & VS_STATIC) {
    // Add corresponding static variable.
    assert(static_vars != NULL);
    varinfo->static_.svar = var_add(static_vars, alloc_dummy_ident(), type, storage);
  }
  return varinfo;
}

StructInfo *find_struct(Scope *scope, const Name *name, Scope **pscope) {
  for (; scope != NULL; scope = scope->parent) {
    if (scope->struct_table == NULL)
      continue;
    StructInfo *sinfo = table_get(scope->struct_table, name);
    if (sinfo != NULL) {
      if (pscope != NULL)
        *pscope = scope;
      return sinfo;
    }
  }
  return NULL;
}

void define_struct(Scope *scope, const Name *name, StructInfo *sinfo) {
  if (scope->struct_table == NULL)
    scope->struct_table = alloc_table();
  table_put(scope->struct_table, name, sinfo);
}

Type *find_typedef(Scope *scope, const Name *name, Scope **pscope) {
  for (; scope != NULL; scope = scope->parent) {
    Type *type;
    if (scope->typedef_table != NULL && (type = table_get(scope->typedef_table, name)) != NULL) {
      if (pscope != NULL)
        *pscope = scope;
      return type;
    }

    // Shadowed by variable?
    if (scope->vars != NULL && var_find(scope->vars, name) >= 0)
      break;
  }
  return NULL;
}

bool add_typedef(Scope *scope, const Name *name, Type *type) {
  if (scope->typedef_table != NULL) {
    if (table_get(scope->typedef_table, name) != NULL)
      return false;
  } else {
    scope->typedef_table = alloc_table();
  }
  table_put(scope->typedef_table, name, (void*)type);
  return true;
}

EnumInfo *find_enum(Scope *scope, const Name *name) {
  for (; scope != NULL; scope = scope->parent) {
    if (scope->enum_table == NULL)
      continue;
    EnumInfo *einfo = table_get(scope->enum_table, name);
    if (einfo != NULL)
      return einfo;
  }
  return NULL;
}

void define_enum(Scope *scope, const Name *name, EnumInfo *einfo) {
  if (scope->enum_table == NULL)
    scope->enum_table = alloc_table();
  table_put(scope->enum_table, name, einfo);
}
