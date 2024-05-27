#include "../../config.h"
#include "parser.h"

#include <assert.h>
#include <inttypes.h>  // PRId64
#include <limits.h>
#include <stdbool.h>
#include <stdlib.h>  // malloc
#include <string.h>

#include "ast.h"
#include "fe_misc.h"
#include "initializer.h"
#include "lexer.h"
#include "table.h"
#include "type.h"
#include "util.h"
#include "var.h"

static Table builtin_expr_ident_table;

static Expr *parse_unary(void);

void add_builtin_expr_ident(const char *str, BuiltinExprProc *proc) {
  const Name *name = alloc_name(str, NULL, false);
  table_put(&builtin_expr_ident_table, name, proc);
}

Vector *parse_args(Token **ptoken) {
  Vector *args = new_vector();
  Token *token;
  if ((token = match(TK_RPAR)) == NULL) {
    for (;;) {
      Expr *arg = parse_assign();
      vec_push(args, arg);
      if ((token = match(TK_RPAR)) != NULL)
        break;
      if (!consume(TK_COMMA, "`,' or `)` expected"))
        break;
    }
  }

  *ptoken = token;
  return args;
}

static Expr *parse_funcall(Expr *func) {
  Token *token;
  Vector *args = parse_args(&token);

  assert(curfunc != NULL);
  curfunc->flag |= FUNCF_HAS_FUNCALL;

  check_funcall_args(func, args, curscope);
  Type *functype = get_callee_type(func->type);
  if (functype == NULL) {
    parse_error(PE_NOFATAL, func->token, "Cannot call except function");
    return func;
  }

  Type *rettype = functype->func.ret;
  ensure_struct(rettype, token, curscope);

  if (func->kind == EX_VAR && is_global_scope(func->var.scope)) {
    VarInfo *varinfo = scope_find(func->var.scope, func->var.name, NULL);
    assert(varinfo != NULL);
    if (satisfy_inline_criteria(varinfo))
      return new_expr_inlined(token, varinfo->name, rettype, args,
                              embed_inline_funcall(varinfo));
  }

  return new_expr_funcall(token, func, rettype, args);
}

static Expr *parse_array_index(const Token *token, Expr *expr) {
  Expr *index = parse_expr();
  consume(TK_RBRACKET, "`]' expected");
  expr = str_to_char_array_var(curscope, expr);
  index = str_to_char_array_var(curscope, index);
  if (!ptr_or_array(expr->type)) {
    if (!ptr_or_array(index->type)) {
      parse_error(PE_NOFATAL, expr->token, "array or pointer required for `['");
      return expr;
    }
    Expr *tmp = expr;
    expr = index;
    index = tmp;
  }
  if (!is_fixnum(index->type->kind)) {
    parse_error(PE_NOFATAL, index->token, "int required for `['");
  } else {
    expr = new_expr_addsub(EX_ADD, token, expr, index);
  }
  return new_expr_deref(token, expr);
}

static Expr *parse_member_access(Expr *target, Token *acctok) {
  Token *ident = consume(TK_IDENT, "member name expected");

  // Find member's type from struct info.
  Type *type = target->type;
  switch (acctok->kind) {
  case TK_DOT:
    if (type->kind != TY_STRUCT) {
      parse_error(PE_NOFATAL, acctok, "`.' for non struct value");
      // Suppose `dot` is mistakenly used instead of `arrow`, continue parsing for pointer type.
      if (!(ptr_or_array(type) && (type = type->pa.ptrof, type->kind == TY_STRUCT)))
        return target;
    }
    break;
  case TK_ARROW:
    if (!ptr_or_array(type)) {
      parse_error(PE_NOFATAL, acctok, "`->' for non pointer value");
      // Suppose `arrow` is mistakenly used instead of `dot`, continue parsing for struct type.
      if (type->kind != TY_STRUCT)
        return target;  // Error is already reported in above, so return here.
    } else {
      type = type->pa.ptrof;
    }
    if (type->kind != TY_STRUCT) {
      parse_error(PE_NOFATAL, acctok, "`->' for non struct value");
      return target;
    }
    break;
  default: assert(false); break;
  }
  if (ident == NULL)
    return target;

  ensure_struct(type, ident, curscope);

  int index = find_struct_member(type->struct_.info, ident->ident);
  if (index >= 0) {
    const MemberInfo *minfo = &type->struct_.info->members[index];
    Type *type = acctok->kind == TK_DOT ? qualified_type(minfo->type, target->type->qualifier)
                                        : minfo->type;
    return new_expr_member(acctok, type, target, ident->ident, minfo);
  } else {
    Vector *stack = new_vector();
    const MemberInfo *member = search_from_anonymous(type, ident->ident, ident, stack);
    if (member == NULL) {
      parse_error(PE_NOFATAL, ident, "`%.*s' doesn't exist in the struct", NAMES(ident->ident));
      return target;
    }
    Expr *p = target;
    Token *tok = acctok;
    for (int i = 0; i < stack->len; ++i) {
      int index = (int)(long long)stack->data[i];
      const MemberInfo *minfo = &type->struct_.info->members[index];
      type = qualified_type(minfo->type, type->qualifier);
      const Name *member_name = NULL;
      if (i == stack->len - 1) {  // Last one must be specified member.
        assert(equal_name(minfo->name, ident->ident));
        member_name = ident->ident;
      }
      p = new_expr_member(tok, type, p, member_name, minfo);
      if (tok->kind != TK_DOT)
        tok = alloc_token(TK_DOT, acctok->line, acctok->begin, acctok->end);
    }
    return p;
  }
}

static void parse_enum_members(Type *type) {
  assert(type != NULL && type->kind == TY_FIXNUM && type->fixnum.kind == FX_ENUM);
  Type *ctype = qualified_type(type, TQ_CONST);
  int value = 0;
  while (!match(TK_RBRACE)) {
    Token *ident = consume(TK_IDENT, "ident expected");
    if (ident == NULL)
      break;
    if (match(TK_ASSIGN)) {
      Expr *expr = parse_const_fixnum();
      value = expr->fixnum;
    }

    if (scope_find(global_scope, ident->ident, NULL) != NULL) {
      parse_error(PE_NOFATAL, ident, "`%.*s' is already defined", NAMES(ident->ident));
    } else {
      define_enum_member(ctype, ident, value);
    }
    ++value;

    if (!match(TK_COMMA)) {
      if (fetch_token()->kind != TK_RBRACE) {
        parse_error(PE_NOFATAL, NULL, "`}' expected");
        break;
      }
    }
  }
}

static Type *parse_enum(void) {
  Token *ident = match(TK_IDENT);
  Type *type = ident != NULL ? find_enum(curscope, ident->ident) : NULL;
  if (match(TK_LBRACE)) {
    if (type != NULL)
      parse_error(PE_FATAL, ident, "Duplicate enum type");
    type = define_enum(curscope, ident != NULL ? ident->ident : NULL);
    parse_enum_members(type);
  } else {
    if (type == NULL)
      parse_error(PE_FATAL, ident, "Unknown enum type");
  }
  return type;
}

// Parse struct or union definition `{...}`
static StructInfo *parse_struct(bool is_union) {
  int count = 0;
  MemberInfo *members = NULL;
  Token *flex_arr_mem = NULL;  // Flexible array member appeared.
  while (!match(TK_RBRACE)) {
    if (flex_arr_mem != NULL) {
      parse_error(PE_NOFATAL, flex_arr_mem, "Flexible array meber must be last element");
      flex_arr_mem = NULL;
    }

    Type *rawType = NULL;
    do {
      int storage;
      Token *ident;
      Type *type = parse_var_def(&rawType, &storage, &ident);
      if (type == NULL) {
        parse_error(PE_FATAL, NULL, "type expected");
        break;
      }

      if (!not_void(type, NULL))
        type = &tyInt;  // Deceive to continue compiling.
      ensure_struct(type, ident, curscope);
      Expr *bit = NULL;
#ifndef __NO_BITFIELD
      if (type->kind == TY_FIXNUM) {
        if (match(TK_COLON))
          bit = parse_const_fixnum();
      }
#endif
      // Allow ident to be null for anonymous struct member or bitfield, otherwise raise error.
      if (ident == NULL && type->kind != TY_STRUCT && bit == NULL)
        parse_error(PE_NOFATAL, NULL, "ident expected");
#ifndef __NO_BITFIELD
      if (bit != NULL) {
        if (bit->fixnum < 0) {
          parse_error(PE_NOFATAL, NULL, "illegal bit width");
          bit = NULL;
        } else if (bit->fixnum > (Fixnum)(type_size(type) * CHAR_BIT)) {  // TODO: target CHAR_BIT
          parse_error(PE_NOFATAL, NULL, "bit width exceeds");
          bit = NULL;
        } else if (bit->fixnum == 0 && ident != NULL) {
          parse_error(PE_NOFATAL, NULL, "bit width zero with name");
        }
      }
#endif

      switch (type->kind) {
      case TY_ARRAY:
#ifndef __NO_VLA
        if (type->pa.vla != NULL) {
          parse_error(PE_NOFATAL, ident, "VLA not allowed in struct/union");
          // To continue compile.
          type->pa.vla = NULL;
          type->pa.length = 1;
        }
#endif
        if (type->pa.length == LEN_UND) {
          assert(ident != NULL);
          flex_arr_mem = ident;
          type->pa.length = LEN_FAM;
        }
        break;
      case TY_STRUCT:
        assert(type->struct_.info != NULL);
        if (type->struct_.info->is_flexible) {
          assert(ident != NULL);
          flex_arr_mem = ident;
        }
        break;
      default:  break;
      }

      const Name *name = ident != NULL ? ident->ident : NULL;
      if (name != NULL) {
        for (int i = 0; i < count; ++i) {
          const MemberInfo *minfo = &members[i];
          if (minfo->name != NULL && equal_name(minfo->name, name)) {
            parse_error(PE_NOFATAL, ident, "`%.*s' already defined", NAMES(name));
            name = NULL;  // Avoid conflict.
            break;
          }
        }
      }

      members = realloc_or_die(members, sizeof(*members) * (count + 1));
      MemberInfo *p = &members[count++];
      p->name = name;
      p->type = type;
      p->offset = 0;
#ifndef __NO_BITFIELD
      p->bitfield.width = bit == NULL ? -1 : bit->fixnum;
#endif
    } while (flex_arr_mem == NULL && match(TK_COMMA));
    consume(TK_SEMICOL, "`;' expected");
  }
  return create_struct_info(members, count, is_union, flex_arr_mem != NULL);
}

#define ASSERT_PARSE_ERROR(cond, tok, ...)  do { if (!(cond)) parse_error(PE_FATAL, tok, __VA_ARGS__); } while (0)

Type *parse_raw_type(int *pstorage) {
  static const char MULTIPLE_STORAGE_SPECIFIED[] = "multiple storage specified";
  static const char MULTIPLE_QUALIFIER_SPECIFIED[] = "multiple qualifier specified";
  static const char ILLEGAL_TYPE_COMBINATION[] = "illegal type combination";

  Type *type = NULL;

  TypeCombination tc = {0};
  Token *tok = NULL;
  for (;;) {
    if (tok != NULL)
      check_type_combination(&tc, tok);  // Check for last token
    tok = match(-1);
    switch (tok->kind) {
    case TK_UNSIGNED:
      ++tc.unsigned_num;
      continue;
    case TK_SIGNED:
      ++tc.signed_num;
      continue;
    case TK_STATIC:
      ASSERT_PARSE_ERROR((tc.storage & ~VS_INLINE) == 0, tok, MULTIPLE_STORAGE_SPECIFIED);
      tc.storage |= VS_STATIC;
      continue;
    case TK_INLINE:
      ASSERT_PARSE_ERROR((tc.storage & ~(VS_STATIC | VS_EXTERN)) == 0, tok, MULTIPLE_STORAGE_SPECIFIED);
      tc.storage |= VS_INLINE;
      continue;
    case TK_EXTERN:
      ASSERT_PARSE_ERROR((tc.storage & ~VS_INLINE) == 0, tok, MULTIPLE_STORAGE_SPECIFIED);
      tc.storage |= VS_EXTERN;
      continue;
    case TK_TYPEDEF:
      ASSERT_PARSE_ERROR(tc.storage == 0, tok, MULTIPLE_STORAGE_SPECIFIED);
      tc.storage |= VS_TYPEDEF;
      continue;
    case TK_AUTO:
      ASSERT_PARSE_ERROR((tc.storage & ~VS_REGISTER) == 0, tok, MULTIPLE_STORAGE_SPECIFIED);
      tc.storage |= VS_AUTO;
      continue;
    case TK_REGISTER:
      ASSERT_PARSE_ERROR((tc.storage & ~VS_AUTO) == 0, tok, MULTIPLE_STORAGE_SPECIFIED);
      tc.storage |= VS_REGISTER;
      continue;
    case TK_CONST:
      ASSERT_PARSE_ERROR((tc.qualifier & TQ_CONST) == 0, tok, MULTIPLE_QUALIFIER_SPECIFIED);
      tc.qualifier |= TQ_CONST;
      continue;
    case TK_VOLATILE:
      ASSERT_PARSE_ERROR((tc.qualifier & TQ_VOLATILE) == 0, tok, MULTIPLE_QUALIFIER_SPECIFIED);
      tc.qualifier |= TQ_VOLATILE;
      continue;
    case TK_RESTRICT:
      ASSERT_PARSE_ERROR((tc.qualifier & TQ_RESTRICT) == 0, tok, MULTIPLE_QUALIFIER_SPECIFIED);
      tc.qualifier |= TQ_RESTRICT;
      continue;
    case TK_CHAR:
      ++tc.char_num;
      continue;
    case TK_SHORT:
      ++tc.short_num;
      continue;
    case TK_INT:
      ++tc.int_num;
      continue;
    case TK_LONG:
      ++tc.long_num;
      continue;
    case TK_FLOAT:
      ++tc.float_num;
      continue;
    case TK_DOUBLE:
      ++tc.double_num;
      continue;
    default: break;
    }

    if (type != NULL) {
      unget_token(tok);
      break;
    }

    if (tok->kind == TK_STRUCT || tok->kind == TK_UNION) {
      if (!no_type_combination(&tc, 0, 0))
        parse_error(PE_NOFATAL, tok, ILLEGAL_TYPE_COMBINATION);

      bool is_union = tok->kind == TK_UNION;
      const Name *name = NULL;
      Token *ident;
      if ((ident = match(TK_IDENT)) != NULL)
        name = ident->ident;

      StructInfo *sinfo = NULL;
      if (match(TK_LBRACE)) {  // Definition
        sinfo = parse_struct(is_union);
        if (name != NULL) {
          Scope *scope;
          StructInfo *exist = find_struct(curscope, name, &scope);
          if (exist != NULL && scope == curscope)
            parse_error(PE_NOFATAL, ident, "`%.*s' already defined", NAMES(name));
          else
            define_struct(curscope, name, sinfo);
        }
      } else {
        if (name != NULL) {
          sinfo = find_struct(curscope, name, NULL);
          if (sinfo != NULL) {
            if (sinfo->is_union != is_union)
              parse_error(PE_NOFATAL, tok, "Wrong tag for `%.*s'", NAMES(name));
          }
        }
      }

      if (name == NULL && sinfo == NULL)
        parse_error(PE_FATAL, NULL, "Illegal struct/union usage");

      type = create_struct_type(sinfo, name, tc.qualifier);
    } else if (tok->kind == TK_ENUM) {
      if (!no_type_combination(&tc, 0, 0))
        parse_error(PE_NOFATAL, tok, ILLEGAL_TYPE_COMBINATION);

      type = parse_enum();
    } else if (tok->kind == TK_BOOL) {
      if (!no_type_combination(&tc, 0, 0))
        parse_error(PE_NOFATAL, tok, ILLEGAL_TYPE_COMBINATION);

      type = &tyBool;
    } else if (tok->kind == TK_IDENT) {
      if (no_type_combination(&tc, 0, 0)) {
        Token *next = match(-1);
        if (next->kind != TK_COLON)
          type = find_typedef(curscope, tok->ident, NULL);
        unget_token(next);
      }
    } else if (tok->kind == TK_VOID) {
      type = tc.qualifier & TQ_CONST ? &tyConstVoid : &tyVoid;
    }
    if (type == NULL) {
      unget_token(tok);
      break;
    }
  }

  if (type != NULL) {
    if (tc.qualifier != 0)
      type = qualified_type(type, tc.qualifier);
  } else if (!no_type_combination(&tc, ~0, ~0)) {
    if (tc.float_num > 0) {
      type = &tyFloat;
    } else if (tc.double_num > 0) {
      type = (tc.long_num > 0 ? &tyLDouble : &tyDouble);
    } else {
      enum FixnumKind fk = (tc.char_num > 0)  ? FX_CHAR
                         : (tc.short_num > 0) ? FX_SHORT
                                              : kLongKinds[tc.long_num];
      type = get_fixnum_type(fk, tc.unsigned_num > 0, tc.qualifier);
    }
  }

  if (pstorage != NULL)
    *pstorage = tc.storage;

  return type;
}

Type *parse_type_suffix(Type *type) {
  if (type == NULL)
    return NULL;

  if (!match(TK_LBRACKET))
    return type;
  ssize_t length = -1;
  if (match(TK_RBRACKET)) {
    // Arbitrary size.
  } else {
    Expr *expr = parse_const_fixnum();
    if (expr->fixnum < 0)
      parse_error(PE_NOFATAL, expr->token, "Array size must be greater than 0, but %" PRId64,
                  expr->fixnum);
    length = expr->fixnum;
    consume(TK_RBRACKET, "`]' expected");
  }
  return arrayof(parse_type_suffix(type), length);
}

// <pointer> ::= * {<type-qualifier>}* {<pointer>}?
Type *parse_pointer(Type *type) {
  if (type == NULL)
    return NULL;

  for (;;) {
    if (match(TK_CONST)) {
      if (type->qualifier & TQ_CONST)
        parse_error(PE_NOFATAL, NULL, "multiple `const' specified");
      else
        type = qualified_type(type, TQ_CONST);
      continue;
    }
    if (match(TK_VOLATILE)) {
      if (type->qualifier & TQ_VOLATILE)
        parse_error(PE_NOFATAL, NULL, "multiple `volatile' specified");
      else
        type = qualified_type(type, TQ_VOLATILE);
      continue;
    }
    if (match(TK_RESTRICT)) {
      if (type->qualifier & TQ_RESTRICT)
        parse_error(PE_NOFATAL, NULL, "multiple `restrict' specified");
      else
        type = qualified_type(type, TQ_RESTRICT);
      continue;
    }

    if (!match(TK_MUL))
      break;
    type = ptrof(type);
  }

  return type;
}

static const char kConstIntExpected[] = "constant integer expected";

static ssize_t parse_array_size(Expr **pvla) {
  ssize_t length = LEN_UND;

  Expr *expr = parse_expr();
  switch (expr->kind) {
  case EX_FIXNUM:
    length = expr->fixnum;
    break;
  case EX_VAR:
    {
      VarInfo *varinfo = scope_find(expr->var.scope, expr->var.name, NULL);
      assert(varinfo != NULL);
      if (expr->type->qualifier & TQ_CONST) {
        switch (expr->type->kind) {
        case TY_FLONUM:
          parse_error(PE_WARNING, expr->token, kConstIntExpected);
          // Fallthrough
        case TY_FIXNUM:
          {
            Initializer *init = is_global_scope(expr->var.scope) ? varinfo->global.init
                                                                  : varinfo->local.init;
            assert(init != NULL);
            assert(init->kind == IK_SINGLE);
            switch (init->single->kind) {
            case EX_FIXNUM:
              length = init->single->fixnum;
              break;
#ifndef __NO_FLONUM
            case EX_FLONUM:
              length = init->single->flonum;
              break;
#endif
            // TODO: Compound literal?
            default:  assert(false); break;
            }
          }
          break;
        default:
          parse_error(PE_NOFATAL, expr->token, kConstIntExpected);
          length = 1;
          break;
        }
        break;
      }
    }
    // Fallthrough
  default:
    if (is_fixnum(expr->type->kind)) {
      *pvla = expr;
      length = LEN_VLA;
    } else {
      parse_error(PE_NOFATAL, expr->token, kConstIntExpected);
      length = 1;
    }
    break;
  }

  if (length < 0 && *pvla == NULL) {
    parse_error(PE_NOFATAL, expr->token, "Array size must be greater than 0, but %zd", length);
    length = 1;
  }
  return length;
}

static Vector *parsing_funparams;

// <direct-declarator> ::= <identifier>
//                       | ( <declarator> )
//                       | <direct-declarator> [ {<constant-expression>}? ]
//                       | <direct-declarator> ( <parameter-type-list> )
//                       | <direct-declarator> ( {<identifier>}* )
static Type *parse_direct_declarator_suffix(Type *type) {
  Token *tok;
  if ((tok = match(TK_LBRACKET)) != NULL) {
    if (match(TK_RESTRICT))
      type = qualified_type(type, TQ_RESTRICT);

    ssize_t length = LEN_UND;
    Expr *vla = NULL;
    if (match(TK_RBRACKET)) {
      // Arbitrary size.
    } else {
      // Arrow `arr[static expr]` (C99) (just ignored).
      for (;;) {
        const Token *tok = fetch_token();
        enum TokenKind kind = tok->kind;
        if (!(kind == TK_STATIC || kind == TK_CONST || kind == TK_VOLATILE || kind == TK_RESTRICT))
          break;
        consume(kind, NULL);
      }

      Token *var = match(TK_IDENT);
      int index;
      if (var != NULL && parsing_funparams != NULL &&
          (index = var_find(parsing_funparams, var->ident)) >= 0) {
        VarInfo *varinfo = parsing_funparams->data[index];
        vla = new_expr_variable(var->ident, varinfo->type, var, curscope);  // TODO: curscopeはその関数のルートスコープにする必要がある
      } else {
        if (var != NULL)
          unget_token(var);
        length = parse_array_size(&vla);
      }

      consume(TK_RBRACKET, "`]' expected");
    }
    const Type *basetype = type;
    Type *subtype = parse_direct_declarator_suffix(type);
#ifndef __NO_VLA
    if (vla == NULL && subtype->kind == TY_ARRAY && subtype->pa.vla != NULL) {
      // If subtype is VLA, then make parent VLA, too.
      vla = new_expr_fixlit(&tySize, tok, length);
    }
    if (vla != NULL) {
      // VLA: Convert most outer type as a pointer, not an array.
      type = arrayof(subtype, LEN_VLA);
      type->pa.vla = vla;
      type->qualifier |= TQ_CONST;  // Make VLA is not modified.
    } else
#endif
    {
      type = arrayof(subtype, length);
      if (basetype->qualifier & TQ_CONST)
        type->qualifier |= TQ_CONST;
    }

    // Flexible array struct not allowd.
    if (basetype->kind == TY_STRUCT) {
      if (basetype->struct_.info != NULL && basetype->struct_.info->is_flexible)
        parse_error(PE_NOFATAL, tok, "using flexible array as an array not allowed.");
    }
  } else if (match(TK_LPAR)) {
    bool vaargs;
    Vector *param_vars = parse_funparams(&vaargs);
    Type *rettype = parse_direct_declarator_suffix(type);

    Vector *param_types = extract_varinfo_types(param_vars);
    type = new_func_type(rettype, param_types, vaargs);
    type->func.param_vars = param_vars;
  }
  return type;
}
static Type *parse_direct_declarator(Type *type, Token **pident) {
  Token *ident = NULL;
  if (match(TK_LPAR)) {
    Type *ret = type;
    Type *placeholder = calloc_or_die(sizeof(*placeholder));
    assert(placeholder != NULL);
    memcpy(placeholder, type, sizeof(*placeholder));

    type = parse_declarator(placeholder, &ident);
    consume(TK_RPAR, "`)' expected");

    Type *inner = parse_direct_declarator_suffix(ret);
    memcpy(placeholder, inner, sizeof(*placeholder));
  } else {
    ident = match(TK_IDENT);
    type = parse_direct_declarator_suffix(type);
  }

  if (pident != NULL)
    *pident = ident;

  return type;
}

// <declarator> ::= {<pointer>}? <direct-declarator>
Type *parse_declarator(Type *rawtype, Token **pident) {
  Type *type = parse_pointer(rawtype);
  return parse_direct_declarator(type, pident);
}

Type *parse_var_def(Type **prawType, int *pstorage, Token **pident) {
  Type *rawType = prawType != NULL ? *prawType : NULL;
  if (rawType == NULL) {
    rawType = parse_raw_type(pstorage);
    if (rawType == NULL)
      return NULL;
    if (prawType != NULL)
      *prawType = rawType;
  }

  return parse_declarator(rawType, pident);
}

Vector *parse_funparams(bool *pvaargs) {
  Vector *bak_parsing_funparams = parsing_funparams;
  parsing_funparams = NULL;

  Vector *vars = NULL;
  bool vaargs = false;
  if (match(TK_RPAR)) {
    // Arbitrary funparams.
    vaargs = true;
  } else {
    parsing_funparams = vars = new_vector();
    for (;;) {
      if (match(TK_ELLIPSIS)) {
        vaargs = true;
        consume(TK_RPAR, "`)' expected");
        break;
      }

      int storage;
      Token *ident;
      Type *type = parse_var_def(NULL, &storage, &ident);
      if (type == NULL) {
        parse_error(PE_FATAL, NULL, "type expected");
      } else {
        if (storage & VS_STATIC)
          parse_error(PE_NOFATAL, ident, "`static' for function parameter");
        if (storage & VS_EXTERN)
          parse_error(PE_NOFATAL, ident, "`extern' for function parameter");
        if (storage & VS_TYPEDEF)
          parse_error(PE_NOFATAL, ident, "`typedef' for function parameter");

        if (vars->len == 0) {
          if (type->kind == TY_VOID) {  // fun(void)
            if (ident != NULL || !match(TK_RPAR))
              parse_error(PE_FATAL, NULL, "`)' expected");
            break;
          }
        } else {
          if (!not_void(type, NULL))
            type = &tyInt;  // Deceive to continue compiling.
        }

        // Treat array or function as its pointer type automatically.
        switch (type->kind) {
        case TY_ARRAY:
#ifndef __NO_VLA
          // Keep VLAs to calculate array size in `parse_defun`.
          if (type->pa.vla != NULL)
            ;
          else
#endif
            type = array_to_ptr(type);
          break;
        case TY_FUNC:   type = ptrof(type); break;
        default: break;
        }

        if (type->kind == TY_STRUCT) {
          if (type->struct_.info != NULL && type->struct_.info->is_flexible)
            parse_error(PE_NOFATAL, ident, "using flexible array as a parameter not allowed");
        }

        if (ident != NULL && var_find(vars, ident->ident) >= 0)
          parse_error(PE_NOFATAL, ident, "`%.*s' already defined", NAMES(ident->ident));
        else
          var_add(vars, ident != NULL ? ident->ident : NULL, type, storage | VS_PARAM);
      }
      if (match(TK_RPAR))
        break;
      if (!consume(TK_COMMA, "`,' or `)' expected"))
        break;
    }
  }
  *pvaargs = vaargs;
  parsing_funparams = bak_parsing_funparams;
  return vars;
}

static Expr *parse_compound_literal(Type *type) {
  Token *token = fetch_token();
  Initializer *init = parse_initializer();

  if (type->kind == TY_ARRAY)
    type = fix_array_size(type, init);

  Expr *var = alloc_tmp_var(curscope, type);
  Vector *inits = NULL;
  if (is_global_scope(curscope)) {
    // Global variable initializer is flattened in `check_vardecl()`
    const Name *name = var->var.name;
    VarInfo *varinfo = scope_find(curscope, name, NULL);
    assert(varinfo != NULL);
    varinfo->storage |= VS_STATIC;
    varinfo->global.init = init;
  } else {
    init = flatten_initializer(type, init);
    inits = assign_initial_value(var, init, NULL);
  }

  return new_expr_complit(type, token, var, inits, init);
}

static Expr *parse_prim(void) {
  Token *tok;
  if ((tok = match(TK_LPAR)) != NULL) {
    int storage;
    Type *type = parse_var_def(NULL, &storage, NULL);
    if (type != NULL) {  // Compound literal
      consume(TK_RPAR, "`)' expected");
      if (fetch_token()->kind != TK_LBRACE) {
        parse_error(PE_NOFATAL, NULL, "`{' expected");
        return new_expr_variable(alloc_label(), type, tok, curscope);  // Dummy
      }
      return parse_compound_literal(type);
    } else if (match(TK_LBRACE)) {  // ({})
      // gcc extension: Statement expression.
      Stmt *block = parse_block(tok, NULL);
      consume(TK_RPAR, "`)' expected");
      return new_expr_block(block);
    } else {
      Expr *expr = parse_expr();
      consume(TK_RPAR, "No close paren");
      return expr;
    }
  }

  {
    static const struct {
      enum TokenKind tk;
      enum FixnumKind fx;
      bool is_unsigned;
    } TABLE[] = {
      {TK_CHARLIT, FX_CHAR, false},
      {TK_INTLIT, FX_INT, false},
      {TK_LONGLIT, FX_LONG, false},
      {TK_LLONGLIT, FX_LLONG, false},
      {TK_UCHARLIT, FX_CHAR, true},
      {TK_UINTLIT, FX_INT, true},
      {TK_ULONGLIT, FX_LONG, true},
      {TK_ULLONGLIT, FX_LLONG, true},
#ifndef __NO_WCHAR
      {TK_WCHARLIT, FX_INT, true},  // Uses 32-bit wchar_t internally
#endif
    };
    for (int i = 0; i < (int)ARRAY_SIZE(TABLE); ++i) {
      if ((tok = match(TABLE[i].tk)) != NULL) {
        Type *type = get_fixnum_type(TABLE[i].fx, TABLE[i].is_unsigned, 0);
        Fixnum fixnum = tok->fixnum;
        return new_expr_fixlit(type, tok, fixnum);
      }
    }
  }
#ifndef __NO_FLONUM
  if ((tok = match(TK_FLOATLIT)) != NULL) {
    return new_expr_flolit(&tyFloat, tok, tok->flonum);
  }
  if ((tok = match(TK_DOUBLELIT)) != NULL) {
    return new_expr_flolit(&tyDouble, tok, tok->flonum);
  }
  if ((tok = match(TK_LDOUBLELIT)) != NULL) {
    return new_expr_flolit(&tyLDouble, tok, tok->flonum);
  }
#endif

  if ((tok = match(TK_STR)) != NULL)
    return new_expr_str(tok, tok->str.buf, tok->str.len, tok->str.kind);

  if ((tok = match(TK_FUNCNAME)) != NULL) {
    if (curfunc == NULL) {
      parse_error(PE_NOFATAL, tok, "must be inside function");
      static const char nulstr[] = "";
      return new_expr_str(tok, nulstr, 0, STR_CHAR);
    }

    // Make nul-terminated function name.
    size_t len = curfunc->name->bytes;
    char *str = malloc_or_die(len + 1);
    memcpy(str, curfunc->name->chars, len);
    str[len] = '\0';
    return new_expr_str(tok, str, len + 1, STR_CHAR);
  }

  Token *ident = consume(TK_IDENT, "Number or Ident or open paren expected");
  if (ident == NULL)
    return new_expr_fixlit(&tyInt, NULL, 0);  // Dummy.

  const Name *name = ident->ident;
  BuiltinExprProc *proc = table_get(&builtin_expr_ident_table, name);
  if (proc != NULL)
    return (*proc)(ident);
  Scope *scope;
  VarInfo *varinfo = scope_find(curscope, name, &scope);
  Type *type;
  if (varinfo != NULL) {
    if (varinfo->storage & VS_ENUM_MEMBER)
      return new_expr_fixlit(varinfo->type, ident, varinfo->enum_member.value);
    type = varinfo->type;
  } else {
    parse_error(PE_NOFATAL, ident, "`%.*s' undeclared", NAMES(ident->ident));
    type = &tyInt;
    scope = curscope;
    add_var_to_scope(scope, ident, type, 0);
  }
  return new_expr_variable(name, type, ident, scope);
}

static Expr *parse_postfix_cont(Expr *expr) {
  for (;;) {
    Token *tok;
    if (match(TK_LPAR))
      expr = parse_funcall(expr);
    else if ((tok = match(TK_LBRACKET)) != NULL)
      expr = parse_array_index(tok, expr);
    else if ((tok = match(TK_DOT)) != NULL || (tok = match(TK_ARROW)) != NULL)
      expr = parse_member_access(expr, tok);
    else if ((tok = match(TK_INC)) != NULL) {
      not_const(expr->type, tok);
      expr = incdec_of(EX_POSTINC, expr, tok);
    } else if ((tok = match(TK_DEC)) != NULL) {
      not_const(expr->type, tok);
      expr = incdec_of(EX_POSTDEC, expr, tok);
    } else
      return expr;
  }
}
static Expr *parse_postfix(void) {
  Expr *expr = parse_prim();
  return parse_postfix_cont(expr);
}

static Expr *parse_sizeof(const Token *token) {
  Type *type = NULL;
  const Token *tok;
  if ((tok = match(TK_LPAR)) != NULL) {
    type = parse_var_def(NULL, NULL, NULL);
    if (type != NULL) {
      consume(TK_RPAR, "`)' expected");
#ifndef __NO_VLA
      Expr *vla_size = calc_vla_size(type);
      if (vla_size != NULL)
        return new_expr_bop(EX_COMMA, &tySize, token, vla_size, calc_type_size(type));
#endif
    } else {
      unget_token((Token*)tok);
      Expr *expr = parse_unary();
      not_bitfield_member(expr);
      type = expr->type;
      tok = expr->token;
    }
  } else {
    Expr *expr = parse_unary();
    not_bitfield_member(expr);
    type = expr->type;
    tok = expr->token;
  }
  assert(type != NULL);
  ensure_struct(type, token, curscope);
#ifndef __NO_VLA
  if (ptr_or_array(type) && type->pa.vla != NULL)
    return calc_type_size(type);
#endif
  if (type->kind == TY_ARRAY) {
    if (type->pa.length == -1) {
      // TODO: assert `export` modifier.
      parse_error(PE_NOFATAL, tok, "size unknown");
      type->pa.length = 1;  // Continue parsing.
    }
    assert(type->pa.length >= 0);
  }

  const Fixnum size = token->kind == TK_SIZEOF ? type_size(type) : align_size(type);
  return new_expr_fixlit(&tySize, token, size);
}

static Expr *parse_cast_expr(void) {
  Token *lpar;
  if ((lpar = match(TK_LPAR)) != NULL) {
    int storage;
    const Token *token = fetch_token();
    Type *type = parse_var_def(NULL, &storage, NULL);
    if (type != NULL) {  // Cast
      consume(TK_RPAR, "`)' expected");

      Token *token2 = fetch_token();
      if (token2->kind == TK_LBRACE) {
        Expr *complit = parse_compound_literal(type);
        // Make compound literal priority as primary expression.
        return parse_postfix_cont(complit);
      }

      Expr *sub = parse_cast_expr();
      sub = str_to_char_array_var(curscope, sub);
      check_cast(type, sub->type, is_zero(sub), true, token);

      // Do not reduce cast expression using `make_cast`
      // because it ignores `(int)x = 1`.

      if (type->kind != TY_VOID && is_const(sub))
        return make_cast(type, token, sub, true);
      return sub->type->kind != TY_VOID ? new_expr_cast(type, token, sub) : sub;
    }
    unget_token(lpar);
  }
  return parse_unary();
}

static Expr *parse_unary(void) {
  Token *tok;
  if ((tok = match(TK_ADD)) != NULL) {
    Expr *expr = parse_cast_expr();
    if (!is_number(expr->type)) {
      parse_error(PE_NOFATAL, tok, "Cannot apply `+' except number types");
      return expr;
    }
    if (is_fixnum(expr->type->kind))
      expr = promote_to_int(expr);
    if (is_const(expr))
      return expr;
    return new_expr_unary(EX_POS, expr->type, tok, expr);
  }

  if ((tok = match(TK_SUB)) != NULL) {
    Expr *expr = parse_cast_expr();
    if (!is_number(expr->type)) {
      parse_error(PE_NOFATAL, tok, "Cannot apply `-' except number types");
      return expr;
    }

    Type *type = expr->type;
    if (is_fixnum(type->kind)) {
      expr = promote_to_int(expr);
      type = expr->type;
      if (type->fixnum.is_unsigned)
        type = get_fixnum_type(type->fixnum.kind, false, type->qualifier);  // Get signed type.
    }
    if (is_const(expr)) {
#ifndef __NO_FLONUM
      if (is_flonum(type)) {
        expr->flonum = -expr->flonum;
        return expr;
      }
#endif
      assert(is_fixnum(type->kind));
      expr->fixnum = wrap_value(-expr->fixnum, type_size(type), false);
      expr->type = type;
      return expr;
    }
    return new_expr_unary(EX_NEG, type, tok, expr);
  }

  if ((tok = match(TK_NOT)) != NULL) {
    Expr *expr = parse_cast_expr();
    if (!is_number(expr->type) && !ptr_or_array(expr->type)) {
      parse_error(PE_NOFATAL, tok, "Cannot apply `!' except number or pointer types");
      return new_expr_fixlit(&tyBool, tok, false);
    }
    return make_not_expr(expr);
  }

  if ((tok = match(TK_TILDA)) != NULL) {
    Expr *expr = parse_cast_expr();
    if (!is_fixnum(expr->type->kind)) {
      parse_error(PE_NOFATAL, tok, "Cannot apply `~' except integer");
      return new_expr_fixlit(&tyInt, expr->token, 0);
    }
    expr = promote_to_int(expr);
    if (is_const(expr)) {
      Type *type = expr->type;
      expr->fixnum = wrap_value(~expr->fixnum, type_size(type), type->fixnum.is_unsigned);
      return expr;
    }
    return new_expr_unary(EX_BITNOT, expr->type, tok, expr);
  }

  if ((tok = match(TK_AND)) != NULL) {
    Expr *expr = parse_cast_expr();
    assert(expr->type != NULL);
#ifndef __NO_BITFIELD
    if (expr->kind == EX_MEMBER) {
      const MemberInfo *minfo = expr->member.info;
      if (minfo->bitfield.width > 0)
        parse_error(PE_NOFATAL, tok, "Cannot take reference for bitfield");
    }
#endif
    expr = str_to_char_array_var(curscope, expr);
    return make_refer(tok, expr);
  }

  if ((tok = match(TK_MUL)) != NULL) {
    Expr *expr = parse_cast_expr();
    Type *type = expr->type;
    assert(type != NULL);
    switch (type->kind) {
    case TY_PTR: case TY_ARRAY:
      type = type->pa.ptrof;
      break;
    case TY_FUNC:
      break;
    default:
      parse_error(PE_NOFATAL, tok, "Cannot dereference raw type");
      return expr;
    }
    expr = str_to_char_array_var(curscope, expr);
    return new_expr_unary(EX_DEREF, type, tok, expr);
  }

  if ((tok = match(TK_INC)) != NULL) {
    Expr *expr = parse_unary();
    not_const(expr->type, tok);
    return incdec_of(EX_PREINC, expr, tok);
  }

  if ((tok = match(TK_DEC)) != NULL) {
    Expr *expr = parse_unary();
    not_const(expr->type, tok);
    return incdec_of(EX_PREDEC, expr, tok);
  }

  if ((tok = match(TK_SIZEOF)) != NULL ||
      (tok = match(TK_ALIGNOF)) != NULL)
    return parse_sizeof(tok);

  return parse_postfix();
}

static Expr *parse_mul(void) {
  Expr *expr = parse_cast_expr();

  for (;;) {
    enum ExprKind kind;
    Token *tok;
    if ((tok = match(TK_MUL)) != NULL)
      kind = EX_MUL;
    else if ((tok = match(TK_DIV)) != NULL)
      kind = EX_DIV;
    else if ((tok = match(TK_MOD)) != NULL)
      kind = EX_MOD;
    else
      return expr;

    Expr *lhs = expr, *rhs = parse_cast_expr();
    expr = new_expr_num_bop(kind, tok, lhs, rhs);
  }
}

static Expr *parse_add(void) {
  Expr *expr = parse_mul();

  for (;;) {
    enum ExprKind kind;
    Token *tok;
    if ((tok = match(TK_ADD)) != NULL)
      kind = EX_ADD;
    else if ((tok = match(TK_SUB)) != NULL)
      kind = EX_SUB;
    else
      return expr;

    Expr *lhs = expr, *rhs = parse_mul();
    expr = new_expr_addsub(kind, tok, lhs, rhs);
  }
}

static Expr *parse_shift(void) {
  Expr *expr = parse_add();

  for (;;) {
    enum ExprKind kind;
    Token *tok;
    if ((tok = match(TK_LSHIFT)) != NULL)
      kind = EX_LSHIFT;
    else if ((tok = match(TK_RSHIFT)) != NULL)
      kind = EX_RSHIFT;
    else
      return expr;

    Expr *lhs = expr, *rhs = parse_add();
    if (!is_fixnum(lhs->type->kind) ||
        !is_fixnum(rhs->type->kind))
      parse_error(PE_FATAL, tok, "Cannot use `%.*s' except numbers.", (int)(tok->end - tok->begin), tok->begin);

    if (is_const(lhs) && is_const(rhs)) {
      Type *type = lhs->type;
      if (type->fixnum.kind < FX_INT)
        type = get_fixnum_type(FX_INT, type->fixnum.is_unsigned, type->qualifier);
      Fixnum value;
      if (type->fixnum.is_unsigned) {
        UFixnum lval = lhs->fixnum;
        UFixnum rval = rhs->fixnum;
        value = kind == EX_LSHIFT ? lval << rval : lval >> rval;
      } else {
        Fixnum lval = lhs->fixnum;
        Fixnum rval = rhs->fixnum;
        value = kind == EX_LSHIFT ? lval << rval : lval >> rval;
      }
      value = wrap_value(value, type_size(type), type->fixnum.is_unsigned);
      expr = new_expr_fixlit(type, tok, value);
    } else {
      lhs = promote_to_int(lhs);
      expr = new_expr_bop(kind, lhs->type, tok, lhs, rhs);
    }
  }
}

static Expr *parse_cmp(void) {
  Expr *expr = parse_shift();

  for (;;) {
    enum ExprKind kind;
    Token *tok;
    if ((tok = match(TK_LT)) != NULL)
      kind = EX_LT;
    else if ((tok = match(TK_GT)) != NULL)
      kind = EX_GT;
    else if ((tok = match(TK_LE)) != NULL)
      kind = EX_LE;
    else if ((tok = match(TK_GE)) != NULL)
      kind = EX_GE;
    else
      return expr;

    Expr *lhs = expr, *rhs = parse_shift();
    lhs = str_to_char_array_var(curscope, lhs);
    rhs = str_to_char_array_var(curscope, rhs);
    expr = new_expr_cmp(kind, tok, lhs, rhs);
  }
}

static Expr *parse_eq(void) {
  Expr *expr = parse_cmp();

  for (;;) {
    enum ExprKind kind;
    Token *tok;
    if ((tok = match(TK_EQ)) != NULL)
      kind = EX_EQ;
    else if ((tok = match(TK_NE)) != NULL)
      kind = EX_NE;
    else
      return expr;

    Expr *lhs = expr, *rhs = parse_cmp();
    lhs = str_to_char_array_var(curscope, lhs);
    rhs = str_to_char_array_var(curscope, rhs);
    expr = new_expr_cmp(kind, tok, lhs, rhs);
  }
}

static Expr *parse_and(void) {
  Expr *expr = parse_eq();
  for (;;) {
    Token *tok;
    if ((tok = match(TK_AND)) == NULL)
      return expr;

    Expr *lhs = expr, *rhs = parse_eq();
    expr = new_expr_int_bop(EX_BITAND, tok, lhs, rhs);
  }
}

static Expr *parse_xor(void) {
  Expr *expr = parse_and();
  for (;;) {
    Token *tok;
    if ((tok = match(TK_HAT)) == NULL)
      return expr;

    Expr *lhs = expr, *rhs = parse_and();
    expr = new_expr_int_bop(EX_BITXOR, tok, lhs, rhs);
  }
}

static Expr *parse_or(void) {
  Expr *expr = parse_xor();
  for (;;) {
    Token *tok;
    if ((tok = match(TK_OR)) == NULL)
      return expr;

    Expr *lhs = expr, *rhs = parse_xor();
    expr = new_expr_int_bop(EX_BITOR, tok, lhs, rhs);
  }
}

static Expr *parse_logand(void) {
  Expr *expr = parse_or();
  Token *tok = match(TK_LOGAND);
  if (tok != NULL) {
    expr = make_cond(expr);
    do {
      Expr *rhs = make_cond(parse_or());
      if (expr->kind == EX_FIXNUM)
        expr = expr->fixnum == 0 ? expr : rhs;
      else
        expr = new_expr_bop(EX_LOGAND, &tyBool, tok, expr, rhs);
    } while ((tok = match(TK_LOGAND)) != NULL);
  }
  return expr;
}

static Expr *parse_logior(void) {
  Expr *expr = parse_logand();
  Token *tok = match(TK_LOGIOR);
  if (tok != NULL) {
    expr = make_cond(expr);
    do {
      Expr *rhs = make_cond(parse_logand());
      if (expr->kind == EX_FIXNUM)
        expr = expr->fixnum != 0 ? expr : rhs;
      else
        expr = new_expr_bop(EX_LOGIOR, &tyBool, tok, expr, rhs);
    } while ((tok = match(TK_LOGIOR)) != NULL);
  }
  return expr;
}

static Expr *parse_conditional(void) {
  Expr *expr = parse_logior();
  for (;;) {
    const Token *tok;
    if ((tok = match(TK_QUESTION)) == NULL)
      return expr;
    Expr *tval = parse_expr();
    consume(TK_COLON, "`:' expected");
    Expr *fval = parse_conditional();

    tval = str_to_char_array_var(curscope, tval);
    fval = str_to_char_array_var(curscope, fval);

    Type *type;
    type = choose_ternary_result_type(tval, fval);
    if (type == NULL) {
      parse_error(PE_NOFATAL, tok, "lhs and rhs must be same type");
      type = tval->type;  // Dummy to continue.
    } else {
      if (is_fixnum(type->kind) && type->fixnum.kind < FX_INT)
        type = &tyInt;
      if (type->kind != TY_VOID) {
        tval = make_cast(type, tval->token, tval, false);
        fval = make_cast(type, fval->token, fval, false);
      }
    }

    expr = make_cond(expr);
    if (expr->kind == EX_FIXNUM)
      expr = expr->fixnum != 0 ? tval : fval;
    else
      expr = new_expr_ternary(tok, expr, tval, fval, type);
  }
}

Expr *parse_assign(void) {
  static const enum TokenKind kAssignWithOps[] = {
    TK_ASSIGN,
    TK_ADD_ASSIGN, TK_SUB_ASSIGN,
    TK_MUL_ASSIGN, TK_DIV_ASSIGN,
    TK_MOD_ASSIGN, TK_AND_ASSIGN, TK_OR_ASSIGN, TK_HAT_ASSIGN,
    TK_LSHIFT_ASSIGN, TK_RSHIFT_ASSIGN,
  };

  Expr *expr = parse_conditional();

  for (int i = 0; i < (int)ARRAY_SIZE(kAssignWithOps); ++i) {
    Token *tok;
    if ((tok = match(kAssignWithOps[i])) != NULL) {
      Expr *lhs = expr, *rhs = parse_assign();

      check_lval(tok, lhs, "Cannot assign");
      not_const(lhs->type, tok);

      switch (lhs->type->kind) {
      case TY_ARRAY:
      case TY_FUNC:
        parse_error(PE_NOFATAL, tok, "Cannot assign to %s", lhs->type->kind == TY_ARRAY ? "array" : "function");
        return expr;
      default: break;
      }

      if (tok->kind == TK_ASSIGN) {
        rhs = str_to_char_array_var(curscope, rhs);
        if (lhs->type->kind == TY_STRUCT) {  // Struct assignment requires same type.
          if (!same_type_without_qualifier(lhs->type, rhs->type, true))
            parse_error(PE_NOFATAL, tok, "Cannot assign to incompatible struct");
        } else {  // Otherwise, cast-ability required.
          rhs = make_cast(lhs->type, tok, rhs, false);
        }
#ifndef __NO_BITFIELD
        if (lhs->kind == EX_MEMBER) {
          const MemberInfo *minfo = lhs->member.info;
          if (minfo->bitfield.width > 0)
            return assign_to_bitfield(tok, lhs, rhs, minfo);
        }
#endif
        return new_expr_bop(EX_ASSIGN, lhs->type, tok, lhs, rhs);
      }

      return transform_assign_with(tok, lhs, rhs);
    }
  }
  return expr;
}

Expr *parse_const_fixnum(void) {
  Expr *expr = parse_conditional();
  if (is_const(expr) && is_fixnum(expr->type->kind))
    return expr;
  parse_error(PE_NOFATAL, expr->token, "constant integer expected");
  return new_expr_fixlit(&tyInt, expr->token, 1);
}

Expr *parse_expr(void) {
  Expr *expr = parse_assign();
  Expr *last = expr;
  const Token *tok;
  while ((tok = match(TK_COMMA)) != NULL) {
    Expr *next_expr = parse_assign();
    if (is_const(expr))
      expr = last = next_expr;
    else if (is_const(next_expr))
      last = next_expr;
    else
      expr = last = new_expr_bop(EX_COMMA, next_expr->type, tok, expr, next_expr);
  }
  if (expr != last)
    expr = new_expr_bop(EX_COMMA, last->type, tok, expr, last);
  return expr;
}
