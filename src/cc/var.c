#include "var.h"

#include <stdlib.h>  // malloc
#include <string.h>

#include "lexer.h"
#include "util.h"

int var_find(Vector *lvars, const char *name) {
  for (int i = 0, len = lvars->len; i < len; ++i) {
    VarInfo *info = (VarInfo*)lvars->data[i];
    if (info->name != NULL && strcmp(info->name, name) == 0)
      return i;
  }
  return -1;
}

VarInfo *var_add(Vector *lvars, const Token *ident, const Type *type, int flag) {
  // init is only for static local variable.
  const char *name = NULL;
  const char *label = NULL;
  VarInfo *ginfo = NULL;
  if (ident != NULL) {
    name = ident->ident;
    int idx = var_find(lvars, name);
    if (idx >= 0)
      parse_error(ident, "`%s' already defined", name);
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
  info->offset = -1;
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

Map *gvar_map;

VarInfo *find_global(const char *name) {
  return (VarInfo*)map_get(gvar_map, name);
}

VarInfo *define_global(const Type *type, int flag, const Token *ident, const char *name) {
  if (name == NULL)
    name = ident->ident;
  VarInfo *varinfo = find_global(name);
  if (varinfo != NULL && !(varinfo->flag & VF_EXTERN)) {
    if (flag & VF_EXTERN)
      return varinfo;
    parse_error(ident, "`%s' already defined", name);
  }
  varinfo = malloc(sizeof(*varinfo));
  varinfo->name = name;
  varinfo->type = type;
  varinfo->flag = flag;
  varinfo->global.init = NULL;
  varinfo->offset = 0;
  map_put(gvar_map, name, varinfo);
  return varinfo;
}

// Scope

Scope *new_scope(Scope *parent, Vector *vars) {
  Scope *scope = malloc(sizeof(*scope));
  scope->parent = parent;
  scope->vars = vars;
  return scope;
}

VarInfo *scope_find(Scope **pscope, const char *name) {
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

// Function

Function *new_func(const Type *type, const char *name, Vector *params) {
  Function *func = malloc(sizeof(*func));
  func->type = type;
  func->name = name;
  func->params = params;

  func->top_scope = NULL;
  func->all_scopes = new_vector();
  func->frame_size = 0;
  func->used_reg_bits = 0;
  func->bbcon = NULL;
  func->ret_bb = NULL;
  return func;
}
