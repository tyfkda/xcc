#include "../../config.h"
#include "var.h"

#include <assert.h>
#include <stdlib.h>  // malloc
#include <string.h>

#include "ast.h"
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
  scope->vars = new_vector();
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

    int idx = var_find(scope->vars, name);
    if (idx >= 0) {
      varinfo = scope->vars->data[idx];
      break;
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

  VarInfo *varinfo = var_add(scope->vars, name, type, storage);
  if (storage & VS_STATIC) {
    // Add corresponding static variable.
    assert(static_vars != NULL);
    varinfo->static_.svar = var_add(static_vars, alloc_dummy_ident(), type, storage);
  }
  return varinfo;
}

typedef struct {
  enum TypeTagKind kind;
  void *info;  // StructInfo* or EnumInfo*
} TypeTag;

enum TagDefineResult define_type_tag(Scope *scope, const Name *name, enum TypeTagKind kind, void *info) {
  Table *table = scope->type_tag_table;
  if (table == NULL)
    scope->type_tag_table = table = alloc_table();
  TypeTag *typetag = table_get(table, name);
  if (typetag == NULL) {
    typetag = calloc_or_die(sizeof(*typetag));
    typetag->kind = kind;
    table_put(table, name, typetag);
  } else {
    if (typetag->kind != kind)
      return TAGRESULT_CONFLICT;
    if (typetag->info != NULL)
      return TAGRESULT_DUPLICATED;
  }
  typetag->info = info;
  return TAGRESULT_SUCCESS;
}

StructInfo *find_struct(Scope *scope, const Name *name, Scope **pscope) {
  for (; scope != NULL; scope = scope->parent) {
    if (scope->type_tag_table == NULL)
      continue;
    TypeTag *typetag = table_get(scope->type_tag_table, name);
    if (typetag != NULL) {
      if (pscope != NULL)
        *pscope = scope;
      return typetag->kind == TAG_STRUCT ? typetag->info : NULL;
    }
  }
  if (pscope != NULL)
    *pscope = NULL;
  return NULL;
}

EnumInfo *find_enum(Scope *scope, const Name *name, Scope **pscope) {
  for (; scope != NULL; scope = scope->parent) {
    if (scope->type_tag_table == NULL)
      continue;
    TypeTag *typetag = table_get(scope->type_tag_table, name);
    if (typetag != NULL) {
      if (pscope != NULL)
        *pscope = scope;
      return typetag->kind == TAG_ENUM ? typetag->info : NULL;
    }
  }
  if (pscope != NULL)
    *pscope = NULL;
  return NULL;
}

Type *find_typedef(Scope *scope, const Name *name, Scope **pscope) {
  for (; scope != NULL; scope = scope->parent) {
    Type *type;
    if (scope->typedef_table != NULL && table_try_get(scope->typedef_table, name, (void**)&type)) {
      if (pscope != NULL)
        *pscope = scope;
      return type;
    }

    // Shadowed by variable?
    if (var_find(scope->vars, name) >= 0)
      break;
  }
  if (pscope != NULL)
    *pscope = NULL;
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
