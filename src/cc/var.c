#include "var.h"

#include <stdlib.h>  // malloc
#include <string.h>

#include "lexer.h"
#include "table.h"
#include "util.h"

int var_find(Vector *lvars, const Name *name) {
  for (int i = 0, len = lvars->len; i < len; ++i) {
    VarInfo *info = (VarInfo*)lvars->data[i];
    if (info->name != NULL && equal_name(info->name, name))
      return i;
  }
  return -1;
}

VarInfo *var_add(Vector *lvars, const Token *ident, const Type *type, int flag) {
  // init is only for static local variable.
  const Name *name = NULL;
  const Name *label = NULL;
  VarInfo *ginfo = NULL;
  if (ident != NULL) {
    name = ident->ident;
    int idx = var_find(lvars, name);
    if (idx >= 0)
      parse_error(ident, "`%.*s' already defined", name->bytes, name->chars);
    if (flag & VF_STATIC) {
      label = alloc_label();
      ginfo = define_global(type, flag, NULL, label);
    }
  }

  VarInfo *info = malloc(sizeof(*info));
  info->name = name;
  info->type = type;
  info->flag = flag;
  info->local.label = label;
  info->reg = NULL;
  vec_push(lvars, info);
  return ginfo != NULL ? ginfo : info;
}

Vector *extract_varinfo_types(Vector *params) {
  Vector *param_types = NULL;
  if (params != NULL) {
    param_types = new_vector();
    for (int i = 0, len = params->len; i < len; ++i)
      vec_push(param_types, ((VarInfo*)params->data[i])->type);
  }
  return param_types;
}

// Global

Vector *gvar_names;
static Table gvar_table;

VarInfo *find_global(const Name *name) {
  return table_get(&gvar_table, name);
}

VarInfo *define_global(const Type *type, int flag, const Token *ident, const Name *name) {
  if (name == NULL)
    name = ident->ident;
  VarInfo *varinfo = find_global(name);
  if (varinfo != NULL && !(varinfo->flag & VF_EXTERN)) {
    if (!(flag & VF_EXTERN))
      parse_error(ident, "`%.*s' already defined", name->bytes, name->chars);
    return varinfo;
  }
  VarInfo *varinfo2 = malloc(sizeof(*varinfo2));
  varinfo2->name = name;
  varinfo2->type = type;
  varinfo2->flag = flag;
  varinfo2->global.init = NULL;
  table_put(&gvar_table, name, varinfo2);
  if (varinfo == NULL)
    vec_push(gvar_names, name);
  return varinfo2;
}

// Scope

Scope *new_scope(Scope *parent, Vector *vars) {
  Scope *scope = malloc(sizeof(*scope));
  scope->parent = parent;
  scope->vars = vars;
  return scope;
}

VarInfo *scope_find(Scope **pscope, const Name *name) {
  Scope *scope = *pscope;
  VarInfo *varinfo = NULL;
  for (;; scope = scope->parent) {
    if (scope == NULL)
      break;
    if (scope->vars != NULL) {
      int idx = var_find(scope->vars, name);
      if (idx >= 0) {
        varinfo = (VarInfo*)scope->vars->data[idx];
        break;
      }
    }
  }
  *pscope = scope;
  return varinfo;
}
