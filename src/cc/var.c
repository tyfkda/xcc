#include "var.h"

#include <assert.h>
#include <stdlib.h>  // malloc
#include <string.h>

#include "lexer.h"
#include "table.h"
#include "util.h"

static VarInfo *define_global(const Name *name, const Type *type, int flag, const Token *ident);

int var_find(const Vector *vars, const Name *name) {
  for (int i = 0, len = vars->len; i < len; ++i) {
    VarInfo *info = vars->data[i];
    if (info->name != NULL && equal_name(info->name, name))
      return i;
  }
  return -1;
}

VarInfo *var_add(Vector *vars, const Name *name, const Type *type, int flag, const Token *ident) {
  const Name *label = NULL;
  VarInfo *ginfo = NULL;
  if (name != NULL) {
    int idx = var_find(vars, name);
    if (idx >= 0)
      parse_error(ident, "`%.*s' already defined", name->bytes, name->chars);
    if (flag & VF_STATIC) {
      label = alloc_label();
      ginfo = define_global(label, type, flag, NULL);
    }
  }

  VarInfo *info = malloc(sizeof(*info));
  info->name = name;
  info->type = type;
  info->flag = flag;
  info->local.label = label;
  info->reg = NULL;
  vec_push(vars, info);
  return ginfo != NULL ? ginfo : info;
}

// Global

Scope *global_scope;

void init_global(void) {
  global_scope = calloc(1, sizeof(*global_scope));
  global_scope->parent = NULL;
  global_scope->vars = new_vector();
}

static VarInfo *define_global(const Name *name, const Type *type, int flag, const Token *ident) {
  assert(name != NULL);
  VarInfo *varinfo = scope_find(global_scope, name, NULL);
  if (varinfo != NULL) {
    if (!(varinfo->flag & VF_EXTERN)) {
      if (!(flag & VF_EXTERN))
        parse_error(ident, "`%.*s' already defined", name->bytes, name->chars);
      return varinfo;
    }
    varinfo->name = name;
    varinfo->type = type;
    varinfo->flag = flag;
    varinfo->global.init = NULL;
  } else {
    // `static' is different meaning for global and local variable.
    varinfo = var_add(global_scope->vars, name, type, flag & ~VF_STATIC, ident);
    varinfo->flag = flag;
  }
  return varinfo;
}

// Scope

Scope *new_scope(Scope *parent, Vector *vars) {
  Scope *scope = malloc(sizeof(*scope));
  scope->parent = parent;
  scope->vars = vars;
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

VarInfo *scope_add(Scope *scope, const Token *ident, const Type *type, int flag) {
  assert(ident != NULL);
  if (is_global_scope(scope))
    return define_global(ident->ident, type, flag, ident);

  if (scope->vars == NULL)
    scope->vars = new_vector();
  return var_add(scope->vars, ident->ident, type, flag, ident);
}
