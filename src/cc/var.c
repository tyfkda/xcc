#include "var.h"

#include <assert.h>
#include <stdlib.h>  // malloc
#include <string.h>

#include "lexer.h"
#include "table.h"
#include "type.h"
#include "util.h"

static VarInfo *define_global(const Name *name, const Type *type, int storage, const Token *ident);

int var_find(const Vector *vars, const Name *name) {
  for (int i = 0, len = vars->len; i < len; ++i) {
    VarInfo *info = vars->data[i];
    if (info->name != NULL && equal_name(info->name, name))
      return i;
  }
  return -1;
}

VarInfo *var_add(Vector *vars, const Name *name, const Type *type, int storage,
                 const Token *ident) {
  if (name != NULL) {
    int idx = var_find(vars, name);
    if (idx >= 0)
      parse_error(ident, "`%.*s' already defined", name->bytes, name->chars);
  }

  VarInfo *varinfo = calloc(1, sizeof(*varinfo));
  varinfo->name = name;
  varinfo->type = type;
  varinfo->storage = storage;
  if (storage & VS_STATIC)
    varinfo->static_.gvar = define_global(alloc_label(), type, storage, NULL);
  vec_push(vars, varinfo);
  return varinfo;
}

// Global

Scope *global_scope;

void init_global(void) {
  global_scope = calloc(1, sizeof(*global_scope));
  global_scope->parent = NULL;
  global_scope->vars = new_vector();
}

static VarInfo *define_global(const Name *name, const Type *type, int storage, const Token *ident) {
  assert(name != NULL);
  VarInfo *varinfo = scope_find(global_scope, name, NULL);
  if (varinfo != NULL) {
    if (!(varinfo->storage & VS_EXTERN)) {
      if (!(storage & VS_EXTERN))
        parse_error(ident, "`%.*s' already defined", name->bytes, name->chars);
      return varinfo;
    }
    varinfo->name = name;
    varinfo->type = type;
    varinfo->storage = storage;
    varinfo->global.init = NULL;
  } else {
    // `static' is different meaning for global and local variable.
    varinfo = var_add(global_scope->vars, name, type, storage & ~VS_STATIC, ident);
    varinfo->storage = storage;
  }
  return varinfo;
}

// Scope

Scope *new_scope(Scope *parent, Vector *vars) {
  Scope *scope = malloc(sizeof(*scope));
  scope->parent = parent;
  scope->vars = vars;
  scope->struct_table = NULL;
  scope->typedef_table = NULL;
  scope->enum_table = NULL;
  return scope;
}

bool is_global_scope(Scope *scope) {
  assert(scope->parent != NULL || scope == global_scope);  // Global scope is only one.
  return scope->parent == NULL;
}

VarInfo *scope_find(Scope *scope, const Name *name, Scope **pscope) {
  VarInfo *varinfo = NULL;
  for (;; scope = scope->parent) {
    if (scope == NULL)
      break;
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

VarInfo *scope_add(Scope *scope, const Token *ident, const Type *type, int storage) {
  assert(ident != NULL);
  if (is_global_scope(scope))
    return define_global(ident->ident, type, storage, ident);

  if (scope->vars == NULL)
    scope->vars = new_vector();
  return var_add(scope->vars, ident->ident, type, storage, ident);
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
  if (scope->struct_table == NULL) {
    scope->struct_table = malloc(sizeof(*scope->struct_table));
    table_init(scope->struct_table);
  }
  table_put(scope->struct_table, name, sinfo);
}

const Type *find_typedef(Scope *scope, const Name *name, Scope **pscope) {
  for (; scope != NULL; scope = scope->parent) {
    if (scope->typedef_table == NULL)
      continue;
    const Type *type = table_get(scope->typedef_table, name);
    if (type != NULL) {
      if (pscope != NULL)
        *pscope = scope;
      return type;
    }
  }
  return NULL;
}

bool add_typedef(Scope *scope, const Name *name, const Type *type) {
  if (scope->typedef_table != NULL) {
    if (table_get(scope->typedef_table, name) != NULL)
      return false;
  } else {
    scope->typedef_table = malloc(sizeof(*scope->typedef_table));
    table_init(scope->typedef_table);
  }
  table_put(scope->typedef_table, name, (void*)type);
  return true;
}

Type *find_enum(Scope *scope, const Name *name) {
  for (; scope != NULL; scope = scope->parent) {
    if (scope->enum_table == NULL)
      continue;
    Type *type = table_get(scope->enum_table, name);
    if (type != NULL)
      return type;
  }
  return NULL;
}

Type *define_enum(Scope *scope, const Name *name) {
  Type *type = create_enum_type(name);
  if (name != NULL) {
    if (scope->enum_table == NULL) {
      scope->enum_table = malloc(sizeof(*scope->enum_table));
      table_init(scope->enum_table);
    }
    table_put(scope->enum_table, name, type);
  }
  return type;
}

// Misc.

void ensure_struct(Type *type, const Token *token, Scope *scope) {
  assert(type->kind == TY_STRUCT);
  if (type->struct_.info == NULL) {
    StructInfo *sinfo = find_struct(scope, type->struct_.name, NULL);
    if (sinfo == NULL)
      parse_error(token, "Accessing unknown struct(%.*s)'s member", type->struct_.name->bytes,
                  type->struct_.name->chars);
    type->struct_.info = sinfo;
  }

  // Recursively.
  StructInfo *sinfo = type->struct_.info;
  for (int i = 0; i < sinfo->members->len; ++i) {
    VarInfo *varinfo = sinfo->members->data[i];
    if (varinfo->type->kind == TY_STRUCT)
      ensure_struct((Type*)varinfo->type, token, scope);
  }
}

const VarInfo *search_from_anonymous(const Type *type, const Name *name, const Token *ident,
                                     Vector *stack) {
  assert(type->kind == TY_STRUCT);
  const Vector *members = type->struct_.info->members;
  for (int i = 0, len = members->len; i < len; ++i) {
    const VarInfo *member = members->data[i];
    if (member->name != NULL) {
      if (equal_name(member->name, name)) {
        vec_push(stack, (void*)(long)i);
        return member;
      }
    } else if (member->type->kind == TY_STRUCT) {
      vec_push(stack, (void*)(intptr_t)i);
      const VarInfo *submember = search_from_anonymous(member->type, name, ident, stack);
      if (submember != NULL)
        return submember;
      vec_pop(stack);
    }
  }
  return NULL;
}
