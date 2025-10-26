#include "../config.h"
#include "wcc.h"

#include <alloca.h>
#include <assert.h>
#include <stdlib.h>  // free, strtol
#include <string.h>

#include "ast.h"
#include "cc_misc.h"  // is_function_omitted
#include "fe_misc.h"  // curfunc, curscope
#include "table.h"
#include "type.h"
#include "util.h"
#include "var.h"
#include "wasm.h"
#include "wasm_obj.h"

int cur_depth;
static int break_depth;
static int continue_depth;

static void gen_compare_expr(enum ExprKind kind, Expr *lhs, Expr *rhs, bool needval) {
  assert(lhs->type->kind == rhs->type->kind || !needval);
  assert(is_prim_type(lhs->type) || !needval);

  gen_expr(lhs, needval);
  if (needval && is_const(rhs) && is_fixnum(rhs->type) && rhs->fixnum == 0 && kind == EX_EQ) {
    ADD_CODE(type_size(lhs->type) <= I32_SIZE ? OP_I32_EQZ : OP_I64_EQZ);
    return;
  }
  gen_expr(rhs, needval);
  if (!needval)
    return;

  int index = 0;
  if (is_flonum(lhs->type)) {
    index = lhs->type->flonum.kind >= FL_DOUBLE ? 5 : 4;
  } else {
    index = (!is_fixnum(lhs->type) || lhs->type->fixnum.is_unsigned ? 2 : 0) +
            (type_size(lhs->type) > I32_SIZE ? 1 : 0);
  }

  static const unsigned char OpTable[][6] = {
    {OP_I32_EQ, OP_I32_NE, OP_I32_LT_S, OP_I32_LE_S, OP_I32_GE_S, OP_I32_GT_S},
    {OP_I64_EQ, OP_I64_NE, OP_I64_LT_S, OP_I64_LE_S, OP_I64_GE_S, OP_I64_GT_S},
    {OP_I32_EQ, OP_I32_NE, OP_I32_LT_U, OP_I32_LE_U, OP_I32_GE_U, OP_I32_GT_U},
    {OP_I64_EQ, OP_I64_NE, OP_I64_LT_U, OP_I64_LE_U, OP_I64_GE_U, OP_I64_GT_U},
    {OP_F32_EQ, OP_F32_NE, OP_F32_LT, OP_F32_LE, OP_F32_GE, OP_F32_GT},
    {OP_F64_EQ, OP_F64_NE, OP_F64_LT, OP_F64_LE, OP_F64_GE, OP_F64_GT},
  };

  ADD_CODE(OpTable[index][kind - EX_EQ]);
}

void gen_cond(Expr *cond, bool tf, bool needval) {
  enum ExprKind ck = cond->kind;
  switch (ck) {
  case EX_FIXNUM:
    {
      Expr *zero = new_expr_fixlit(&tyInt, NULL, 0);
      gen_compare_expr(tf ? EX_NE : EX_EQ, cond, zero, needval);
    }
    break;
  case EX_EQ:
  case EX_NE:
  case EX_LT:
  case EX_GT:
  case EX_LE:
  case EX_GE:
    if (!tf) {
      if (ck <= EX_NE)
        ck = (EX_EQ + EX_NE) - ck;
      else
        ck = EX_LT + ((ck - EX_LT) ^ 2);
    }
    gen_compare_expr(ck, cond->bop.lhs, cond->bop.rhs, needval);
    break;
  case EX_LOGAND:
  case EX_LOGIOR:
    {
      bool logand = ck == EX_LOGAND;
      gen_cond(cond->bop.lhs, logand, true);
      ADD_CODE(OP_IF, needval ? WT_I32 : WT_VOID);
      ++cur_depth;
      gen_cond(cond->bop.rhs, tf, needval);
      if (needval) {
        ADD_CODE(OP_ELSE);
        ADD_CODE(OP_I32_CONST, tf ^ logand);
      }
      ADD_CODE(OP_END);
      --cur_depth;
    }
    break;
  case EX_COMMA:
    gen_expr(cond->bop.lhs, false);
    gen_cond(cond->bop.rhs, tf, needval);
    break;
  default: assert(false); break;
  }
}

static void gen_cond_jmp(Expr *cond, bool tf, uint32_t depth) {
  gen_cond(cond, tf, true);
  ADD_CODE(OP_BR_IF);
  ADD_ULEB128(depth);
}

static void gen_switch_table_jump(Stmt *stmt, Expr *value, Fixnum min, Fixnum max,
                                  int default_index) {
  Vector *cases = stmt->switch_.cases;
  int case_count = cases->len;

  unsigned int vrange = max - min + 1;
  bool use_alloca = vrange <= 64;
  int *table = use_alloca ? alloca(sizeof(*table) * vrange)
                          : malloc_or_die(sizeof(*table) * vrange);
  for (unsigned int i = 0; i < vrange; ++i)
    table[i] = default_index;
  for (int i = 0; i < case_count; ++i) {
    Stmt *c = cases->data[i];
    if (c->case_.value != NULL) {
      int index = c->case_.block_index;
      if (index < 0)
        index = ~index;
      table[c->case_.value->fixnum - min] = index;
    }
  }

  gen_expr(value, true);
  bool is_i64 = type_size(value->type) > I32_SIZE;
  if (min != 0) {
    ADD_CODE(is_i64 ? OP_I64_CONST : OP_I32_CONST);
    ADD_LEB128(min);
    ADD_CODE(is_i64 ? OP_I64_SUB : OP_I32_SUB);
  }
  if (is_i64)
    ADD_CODE(OP_I32_WRAP_I64);
  ADD_CODE(OP_BR_TABLE);
  ADD_ULEB128(vrange);
  for (unsigned int i = 0; i < vrange; ++i)
    ADD_ULEB128(table[i]);
  ADD_ULEB128(default_index);

  if (!use_alloca)
    free(table);
}

static void squash_cases(Vector *cases) {
  int case_count = cases->len;
  int index = 0;
  for (int i = 0; i < case_count; ++i) {
    Stmt *c = cases->data[i];
    Stmt *prev;
    if (i == 0 || (prev = cases->data[i - 1])->case_.stmt != c) {
      if (i != 0)
        ++index;
      c->case_.block_index = index;
    } else {
      c->case_.block_index = ~index;
    }
  }
}

static void gen_switch(Stmt *stmt) {
  int save_depth = break_depth;
  break_depth = cur_depth;

  ADD_CODE(OP_BLOCK, WT_VOID);
  Vector *cases = stmt->switch_.cases;
  squash_cases(cases);
  int case_count = cases->len;
  int block_count = 0;
  for (int i = 0; i < case_count; ++i) {
    Stmt *c = cases->data[i];
    if (c->case_.block_index >= 0) {
      ADD_CODE(OP_BLOCK, WT_VOID);
      ++block_count;
    }
  }
  cur_depth += block_count + 1;

  Expr *value = stmt->switch_.value;
  if (value->kind == EX_COMMA) {
    gen_expr(value, false);
    value = value->bop.rhs;
  }
  // Must be simple expression, because this is evaluated multiple times.
  assert(is_const(value) || value->kind == EX_VAR);
  assert(is_fixnum(value->type));

  int default_index = block_count;
  Fixnum min = INTPTR_MAX;
  Fixnum max = INTPTR_MIN;
  for (int i = 0; i < case_count; ++i) {
    Stmt *c = cases->data[i];
    if (c->case_.value == NULL) {
      default_index = c->case_.block_index;
      if (default_index < 0)
        default_index = ~default_index;
      continue;
    }
    Fixnum v = c->case_.value->fixnum;
    if (v < min)
      min = v;
    if (v > max)
      max = v;
  }

  if (case_count >= 4 && (max - min) / 2 <= case_count) {
    gen_switch_table_jump(stmt, value, min, max, default_index);
  } else {
    bool is_i64 = type_size(value->type) > I32_SIZE;
    unsigned char op_const = is_i64 ? OP_I64_CONST : OP_I32_CONST;
    unsigned char op_eq = is_i64 ? OP_I64_EQ : OP_I32_EQ;
    for (int i = 0; i < case_count; ++i) {
      Stmt *c = cases->data[i];
      if (c->case_.value == NULL)  // default.
        continue;
      gen_expr(value, true);
      ADD_CODE(op_const);
      ADD_LEB128(c->case_.value->fixnum);
      ADD_CODE(op_eq, OP_BR_IF);
      int index = c->case_.block_index;
      if (index < 0)
        index = ~index;
      ADD_ULEB128(index);
    }
    // Jump to default.
    ADD_CODE(OP_BR);
    ADD_ULEB128(default_index);
  }

  // Body.
  gen_stmt(stmt->switch_.body, false);

  ADD_CODE(OP_END);
  --cur_depth;
  assert(cur_depth == break_depth);
  break_depth = save_depth;
}

static void gen_case(Stmt *stmt, bool is_last) {
  if (stmt->case_.block_index >= 0) {
    ADD_CODE(OP_END);
    --cur_depth;
  }
  assert(cur_depth >= 0);
  gen_stmt(stmt->case_.stmt, is_last);
}

static void gen_while(Stmt *stmt) {
  bool infinite_loop = false;
  Expr *cond = stmt->while_.cond;
  if (is_const(cond)) {
    if (!is_const_truthy(cond))
      return;
    infinite_loop = true;
  }

  int save_break = break_depth;
  int save_continue = continue_depth;
  break_depth = cur_depth;
  continue_depth = cur_depth + 1;

  ADD_CODE(OP_BLOCK, WT_VOID);
  ADD_CODE(OP_LOOP, WT_VOID);
  cur_depth += 2;
  if (!infinite_loop)
    gen_cond_jmp(cond, false, 1);
  gen_stmt(stmt->while_.body, false);
  ADD_CODE(OP_BR, 0);
  ADD_CODE(OP_END);
  ADD_CODE(OP_END);
  cur_depth -= 2;
  break_depth = save_break;
  continue_depth = save_continue;
}

static void gen_do_while(Stmt *stmt) {
  bool infinite_loop = false, no_loop = false;
  Expr *cond = stmt->while_.cond;
  if (is_const(cond)) {
    if (is_const_truthy(cond))
      infinite_loop = true;
    else
      no_loop = true;
  }

  int save_break = break_depth;
  int save_continue = continue_depth;
  break_depth = cur_depth;
  continue_depth = cur_depth + 2;

  ADD_CODE(OP_BLOCK, WT_VOID);
  ADD_CODE(OP_LOOP, WT_VOID);
  ADD_CODE(OP_BLOCK, WT_VOID);
  cur_depth += 3;
  gen_stmt(stmt->while_.body, false);
  ADD_CODE(OP_END);
  --cur_depth;
  if (no_loop)
    ADD_CODE(OP_BR, 1);
  else if (infinite_loop)
    ADD_CODE(OP_BR, 0);
  else
    gen_cond_jmp(cond, true, 0);
  ADD_CODE(OP_END);
  ADD_CODE(OP_END);
  cur_depth -= 2;
  break_depth = save_break;
  continue_depth = save_continue;
}

static void gen_for(Stmt *stmt) {
  if (stmt->for_.pre != NULL)
    gen_expr_stmt(stmt->for_.pre);

  bool infinite_loop = false;
  Expr *cond = stmt->for_.cond;
  if (cond == NULL) {
    infinite_loop = true;
  } else if (is_const(cond)) {
    if (!is_const_truthy(cond))
      return;
    infinite_loop = true;
  }

  int save_break = break_depth;
  int save_continue = continue_depth;
  break_depth = cur_depth;
  continue_depth = cur_depth + 2;

  ADD_CODE(OP_BLOCK, WT_VOID);
  ADD_CODE(OP_LOOP, WT_VOID);
  ADD_CODE(OP_BLOCK, WT_VOID);
  cur_depth += 3;
  if (!infinite_loop)
    gen_cond_jmp(cond, false, 2);
  gen_stmt(stmt->for_.body, false);
  ADD_CODE(OP_END);
  --cur_depth;
  if (stmt->for_.post != NULL)
    gen_expr_stmt(stmt->for_.post);
  ADD_CODE(OP_BR, 0);
  ADD_CODE(OP_END);
  ADD_CODE(OP_END);
  cur_depth -= 2;
  break_depth = save_break;
  continue_depth = save_continue;
}

static void gen_break(void) {
  assert(cur_depth > break_depth);
  ADD_CODE(OP_BR);
  ADD_ULEB128(cur_depth - break_depth - 1);
}

static void gen_continue(void) {
  assert(cur_depth > continue_depth);
  ADD_CODE(OP_BR);
  ADD_ULEB128(cur_depth - continue_depth - 1);
}

static void gen_block(Stmt *stmt, bool is_last) {
  assert(stmt->kind == ST_BLOCK);
  // AST may moved, so code generation traversal may differ from lexical scope chain.
  Scope *bak_curscope = curscope;
  if (stmt->block.scope != NULL)
    curscope = stmt->block.scope;
  gen_stmts(stmt->block.stmts, is_last);
  if (stmt->block.scope != NULL)
    curscope = bak_curscope;
}

static void gen_return(Stmt *stmt, bool is_last) {
  assert(curfunc != NULL);
  if (stmt->return_.val != NULL) {
    Expr *val = stmt->return_.val;
    const Type *rettype = val->type;
    if (is_prim_type(rettype) || rettype->kind == TY_VOID) {
      gen_expr(val, true);
    } else {
      assert(rettype->kind == TY_STRUCT);
      FuncInfo *finfo = table_get(&func_info_table, curfunc->ident->ident);
      assert(finfo != NULL);
      if (finfo->flag & FF_INLINING) {
        gen_lval(val);  // Put a pointer to the top of stack.
      } else if (is_small_struct(rettype)) {
        gen_lval(val);
        const Type *et = get_small_struct_elem_type(rettype);
        gen_load(et);
      } else {
        ADD_CODE(OP_LOCAL_GET, 0);  // Local #0 is the pointer for result.
        gen_lval(val);
        ADD_CODE(OP_I32_CONST);
        ADD_LEB128(type_size(rettype));
        ADD_CODE(OP_0xFC, OPFC_MEMORY_COPY, 0, 0);
        // Struct result is stored, and not return the value.
      }
    }
  }

  FuncInfo *finfo = table_get(&func_info_table, curfunc->ident->ident);
  assert(finfo != NULL);
  if (!is_last) {
    if (finfo->bpname != NULL || finfo->lspname != NULL || finfo->flag & FF_INLINING) {
      assert(cur_depth > 0);
      ADD_CODE(OP_BR);
      ADD_ULEB128(cur_depth - 1);
    } else {
      ADD_CODE(OP_RETURN);
    }
  }
}

static void gen_if(Stmt *stmt, bool is_last) {
  if (is_const(stmt->if_.cond)) {
    if (is_const_truthy(stmt->if_.cond)) {
      gen_stmt(stmt->if_.tblock, is_last);
    } else if (stmt->if_.fblock != NULL) {
      gen_stmt(stmt->if_.fblock, is_last);
    }
    return;
  }

  unsigned char wt = WT_VOID;
  if (is_last && check_funcend_return(stmt))
    wt = get_func_ret_wtype(curfunc->type->func.ret);

  gen_cond(stmt->if_.cond, true, true);
  ADD_CODE(OP_IF, wt);
  ++cur_depth;
  gen_stmt(stmt->if_.tblock, is_last);
  if (stmt->if_.fblock != NULL) {
    ADD_CODE(OP_ELSE);
    gen_stmt(stmt->if_.fblock, is_last);
  }
  ADD_CODE(OP_END);
  --cur_depth;
}

static void gen_vardecl(VarDecl *decl) {
  assert(decl->init_stmt != NULL);
  VarInfo *varinfo = decl->varinfo;
  assert(varinfo != NULL);
  gen_clear_local_var(varinfo);
  gen_stmt(decl->init_stmt, false);
}

void gen_expr_stmt(Expr *expr) {
  gen_expr(expr, false);
}

static void gen_asm(Stmt *stmt) {
  assert(stmt->asm_.templates->len == 1);
  assert(stmt->asm_.outputs == NULL);
  assert(stmt->asm_.inputs == NULL);

  // Assume non-digit character is at the end.
  const char *buf = stmt->asm_.templates->data[0];
  for (const char *p = skip_whitespaces(buf);;) {
    char *next;
    long op = strtol(p, &next, 10);
    if (next == p)
      break;
    ADD_CODE(op);

    p = skip_whitespaces(next);
    if (*p != ',')
      break;
    p = skip_whitespaces(p + 1);
  }
}

void gen_stmt(Stmt *stmt, bool is_last) {
  if (stmt == NULL)
    return;

  switch (stmt->kind) {
  case ST_EMPTY: break;
  case ST_EXPR:  gen_expr_stmt(stmt->expr); break;
  case ST_RETURN:  gen_return(stmt, is_last); break;
  case ST_BLOCK:  gen_block(stmt, is_last); break;
  case ST_IF:  gen_if(stmt, is_last); break;
  case ST_SWITCH:  gen_switch(stmt); break;
  case ST_CASE:  gen_case(stmt, is_last); break;
  case ST_WHILE:  gen_while(stmt); break;
  case ST_DO_WHILE:  gen_do_while(stmt); break;
  case ST_FOR:  gen_for(stmt); break;
  case ST_BREAK:  gen_break(); break;
  case ST_CONTINUE:  gen_continue(); break;
  case ST_LABEL: gen_stmt(stmt->label.stmt, is_last); break;
  case ST_VARDECL:  gen_vardecl(stmt->vardecl); break;
  case ST_ASM:  gen_asm(stmt); break;
  case ST_GOTO: assert(false); break;
  }
}

void gen_stmts(Vector *stmts, bool is_last) {
  assert(stmts != NULL);
  for (int i = 0, len = stmts->len; i < len; ++i) {
    Stmt *stmt = stmts->data[i];
    if (stmt == NULL)
      continue;
    gen_stmt(stmt, is_last && i == len - 1);
  }
}

static inline uint32_t calc_frame_size(
    Function *func, unsigned int local_counts[4], FuncInfo *finfo) {
  const Type *functype = func->type;
  unsigned int param_count = functype->func.params != NULL ? functype->func.params->len : 0;
  const Name *va_args_name = functype->func.vaargs ? alloc_name(VA_ARGS_NAME, NULL, false) : NULL;
  uint32_t frame_size = 0;
  for (int i = 0; i < func->scopes->len; ++i) {
    Scope *scope = func->scopes->data[i];
    for (int j = 0; j < scope->vars->len; ++j) {
      VarInfo *varinfo = scope->vars->data[j];
      const Type *type = varinfo->type;
      int param_index = -1;
      if (i == 0 && varinfo->storage & VS_PARAM) {
        if (va_args_name != NULL && equal_name(varinfo->ident->ident, va_args_name)) {
          param_index = param_count;
        } else if (param_count > 0) {
          int k = get_funparam_index(func, varinfo->ident->ident);
          if (k >= 0)
            param_index = k;
        }
      }

      if (!is_local_storage(varinfo)) {
        // Static entity is allocated in global, not on stack.
        // Extern doesn't have its entity.
        // Enum members are replaced to constant value.
        continue;
      }
      if (type->kind == TY_FUNC) {
        // Function declaration has no entity.
        continue;
      }

      if ((varinfo->storage & VS_REF_TAKEN) ||
          (param_index < 0 && !is_prim_type(type)) ||
          (param_index >= 0 && is_small_struct(type))) {
        size_t size = type_size(type);
        if (size < 1)
          size = 1;
        frame_size = ALIGN(frame_size, align_size(type)) + size;
      } else if (is_prim_type(type)) {
        if (param_index < 0) {
          unsigned char wt = to_wtype(type);
          assert(WT_F64 <= wt && wt <= WT_I32);
          int index = WT_I32 - wt;
          local_counts[index] += 1;
        }
      }
    }
  }
  if (frame_size > 0 || (finfo->flag & FF_STACK_MODIFIED)) {
    frame_size = ALIGN(frame_size, STACK_ALIGN);

    // Allocate a variable for base pointer in function top scope.
    const Token *bpident = alloc_dummy_ident();
    finfo->bpname = bpident->ident;

    scope_add(func->scopes->data[0], bpident, &tySize, 0);
    local_counts[WT_I32 - WT_I32] += 1;
  }
  return frame_size;
}

static inline void assign_variable_index_or_offsets(
    Function *func, unsigned int ret_param, size_t frame_size, unsigned int local_indices[4]) {
  const Type *functype = func->type;
  unsigned int param_count = functype->func.params != NULL ? functype->func.params->len : 0;
  const Name *va_args_name = functype->func.vaargs ? alloc_name(VA_ARGS_NAME, NULL, false) : NULL;
  uint32_t frame_offset = 0;
  for (int i = 0; i < func->scopes->len; ++i) {
    Scope *scope = func->scopes->data[i];
    for (int j = 0; j < scope->vars->len; ++j) {
      VarInfo *varinfo = scope->vars->data[j];
      if (!is_local_storage(varinfo))
        continue;

      VReg *vreg = calloc_or_die(sizeof(*vreg));
      varinfo->local.vreg = vreg;
      int param_index = -1;
      if (i == 0 && varinfo->storage & VS_PARAM) {
        if (va_args_name != NULL && equal_name(varinfo->ident->ident, va_args_name)) {
          param_index = param_count;
        } else if (param_count > 0) {
          int k = get_funparam_index(func, varinfo->ident->ident);
          if (k >= 0)
            param_index = k;
        }
      }
      vreg->param_index = ret_param + param_index;
      const Type *type = varinfo->type;
      size_t size = type_size(type), align = align_size(type);
      bool prim = is_prim_type(type);
      if (param_index >= 0) {
        vreg->prim.local_index = vreg->param_index;
        bool small_struct = is_small_struct(type);
        if (small_struct || varinfo->storage & VS_REF_TAKEN) {
          frame_offset = ALIGN(frame_offset, align);
          vreg->non_prim.offset = frame_offset - frame_size;
          if (size < 1)
            size = 1;
          frame_offset += size;
        }
      } else {
        if ((prim && varinfo->storage & VS_REF_TAKEN) ||  // `&` taken wasm local var.
            !prim) {  // non-prim variable (not function parameter)
          frame_offset = ALIGN(frame_offset, align);
          vreg->non_prim.offset = frame_offset - frame_size;
          if (size < 1)
            size = 1;
          frame_offset += size;
        } else {  // Not `&` taken, wasm local var.
          unsigned char wt = to_wtype(type);
          int index = WT_I32 - wt;
          vreg->prim.local_index = local_indices[index]++;
        }
      }
    }
  }
}

static inline uint32_t allocate_local_variables(Function *func, DataStorage *data) {
  const Type *functype = func->type;
  const Type *rettype = functype->func.ret;
  unsigned int ret_param = rettype->kind == TY_STRUCT && !is_small_struct(rettype) ? 1 : 0;

  unsigned int local_counts[4];  // I32, I64, F32, F64
  memset(local_counts, 0, sizeof(local_counts));

  FuncInfo *finfo = table_get(&func_info_table, func->ident->ident);
  assert(finfo != NULL);

  size_t frame_size = calc_frame_size(func, local_counts, finfo);

  unsigned int local_index_count = 0;
  for (int i = 0; i < 4; ++i) {
    if (local_counts[i] > 0)
      ++local_index_count;
  }
  data_uleb128(data, -1, local_index_count);
  int variadic = func->type->func.vaargs;
  unsigned int param_count = functype->func.params != NULL ? functype->func.params->len : 0;
  unsigned int local_indices[4];
  for (int i = 0; i < 4; ++i) {
    unsigned int count = local_counts[i];
    if (count > 0) {
      data_uleb128(data, -1, count);
      data_push(data, WT_I32 - i);
    }
    local_indices[i] = i == 0 ? ret_param + variadic + param_count
                              : local_indices[i - 1] + local_counts[i - 1];
  }

  assign_variable_index_or_offsets(func, ret_param, frame_size, local_indices);

  assert(((frame_size + finfo->stack_work_size) & (STACK_ALIGN - 1)) == 0);
  return frame_size + finfo->stack_work_size;
}

static inline void setup_base_pointer(
    Function *func, uint32_t frame_size, Expr **pbpvar, Expr **plspvar, Expr **pgspvar) {
  FuncInfo *finfo = table_get(&func_info_table, func->ident->ident);
  assert(finfo != NULL);
  const Name *bpname = finfo->bpname;
  Expr *bpvar = NULL;
  if (bpname != NULL)
    bpvar = new_expr_variable(bpname, &tyVoidPtr, NULL, func->scopes->data[0]);
  const Name *lspname = finfo->lspname;
  Expr *lspvar = NULL;
  if (lspname != NULL)
    lspvar = new_expr_variable(lspname, &tyVoidPtr, NULL, func->scopes->data[0]);
  Expr *gspvar = NULL;
  if (bpname != NULL || lspname != NULL) {
    gspvar = get_sp_var();
    // local.bp = global.sp;
    if (bpvar != NULL)
      gen_expr_stmt(new_expr_bop(EX_ASSIGN, &tyVoid, NULL, bpvar, gspvar));
    // global.sp = local.bp - frame_size;
    if (frame_size > 0) {
      Expr *result = new_expr_bop(EX_SUB, &tyVoidPtr, NULL, bpvar != NULL ? bpvar : gspvar,
                                  new_expr_fixlit(&tySize, NULL, frame_size));
      if (lspvar == NULL) {
        result = new_expr_bop(EX_ASSIGN, &tyVoid, NULL, gspvar, result);
      } else {
        result = new_expr_bop(EX_COMMA, &tyVoid, NULL,
                              new_expr_bop(EX_ASSIGN, &tyVoid, NULL, lspvar, result),
                              new_expr_bop(EX_ASSIGN, &tyVoid, NULL, gspvar, lspvar));
      }
      gen_expr_stmt(result);
    }
  }

  *pbpvar = bpvar;
  *plspvar = lspvar;
  *pgspvar = gspvar;
}

static inline void move_params_to_stack_frame(Function *func) {
  if (func->params != NULL) {
    const Vector *params = func->params;
    for (int i = 0, param_count = params->len; i < param_count; ++i) {
      VarInfo *varinfo = params->data[i];
      const Type *type = varinfo->type;
      if (is_small_struct(type)) {
        // Store small struct passed by value to stack frame.
        VReg *vreg = varinfo->local.vreg;
        assert(vreg != NULL);
        gen_bpofs(vreg->non_prim.offset);
        ADD_CODE(OP_LOCAL_GET);
        ADD_ULEB128(vreg->prim.local_index);
        gen_store(get_small_struct_elem_type(type));
      } else if (is_stack_param(type)) {
        // Nothing.
      } else if (varinfo->storage & VS_REF_TAKEN) {
        // Store ref-taken parameters to stack frame.
        VReg *vreg = varinfo->local.vreg;
        gen_bpofs(vreg->non_prim.offset);
        ADD_CODE(OP_LOCAL_GET);
        ADD_ULEB128(vreg->prim.local_index);
        gen_store(type);
      }
    }
  }
}

static inline void gen_func_body(Function *func, Expr *bpvar, Expr *lspvar) {
  const Type *functype = func->type;
  // Statements
  if (bpvar != NULL || lspvar != NULL) {
    unsigned char wt = get_func_ret_wtype(functype->func.ret);
    ADD_CODE(OP_BLOCK, wt);
    cur_depth += 1;
  }
  gen_stmt(func->body_block, true);

  {
    Vector *stmts = func->body_block->block.stmts;
    if (stmts->len > 0) {
      Stmt *last = stmts->data[stmts->len - 1];
      if (last->kind != ST_ASM && functype->func.ret->kind != TY_VOID &&
          !check_funcend_return(func->body_block)) {
        assert(func->body_block->reach & REACH_STOP);
        ADD_CODE(OP_UNREACHABLE);
      }
    }
  }
}

static inline void epilogue(Function *func, uint32_t frame_size, Expr *bpvar, Expr *lspvar, Expr *gspvar) {
  // Epilogue
  if (bpvar != NULL) {
    ADD_CODE(OP_END);
    cur_depth -= 1;

    // Restore stack pointer: global.sp = bp;
    gen_expr_stmt(new_expr_bop(EX_ASSIGN, &tyVoid, NULL, gspvar, bpvar));
  } else if (lspvar != NULL) {
    ADD_CODE(OP_END);
    cur_depth -= 1;

    FuncInfo *finfo = table_get(&func_info_table, func->ident->ident);
    assert(!(finfo->flag & FF_STACK_MODIFIED));
    assert(frame_size > 0);
    // Restore stack pointer: global.sp = local.sp + frame_size;
    gen_expr_stmt(new_expr_bop(EX_ASSIGN, &tyVoid, NULL, gspvar,
                               new_expr_bop(EX_ADD, &tyVoidPtr, NULL, lspvar,
                                            new_expr_fixlit(&tySize, NULL, frame_size))));
  }
}

static void gen_defun(Function *func) {
  if (func->scopes == NULL)  // Prototype definition
    return;

  VarInfo *funcvi = scope_find(global_scope, func->ident->ident, NULL);
  if (is_function_omitted(funcvi))
    return;

  DataStorage *code = malloc_or_die(sizeof(*code));
  data_init(code);
  data_open_chunk(code);

  FuncExtra *extra = func->extra;
  assert(extra != NULL);
  extra->code = code;
  func->extra = extra;
  uint32_t frame_size = allocate_local_variables(func, code);

  curfunc = func;
  curcodeds = code;

  // Prologue
  Expr *bpvar, *lspvar, *gspvar;
  setup_base_pointer(func, frame_size, &bpvar, &lspvar, &gspvar);
  move_params_to_stack_frame(func);

  gen_func_body(func, bpvar, lspvar);

  epilogue(func, frame_size, bpvar, lspvar, gspvar);

  ADD_CODE(OP_END);

  size_t before = code->len;
  data_close_chunk(code, -1);
  extra->offset = code->len - before;

  curfunc = NULL;
  curcodeds = NULL;
  assert(cur_depth == 0);
}

static void gen_decl(Declaration *decl) {
  if (decl == NULL)
    return;

  switch (decl->kind) {
  case DCL_DEFUN:
    gen_defun(decl->defun.func);
    break;
  case DCL_ASM:
    assert(false);
    break;
  }
}

void gen(Vector *decls) {
  if (decls == NULL)
    return;

  for (int i = 0, len = decls->len; i < len; ++i) {
    Declaration *decl = decls->data[i];
    if (decl == NULL)
      continue;
    gen_decl(decl);
  }
}
