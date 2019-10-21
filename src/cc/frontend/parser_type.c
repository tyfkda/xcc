#include "../../config.h"
#include "parser.h"

#include <assert.h>
#include <inttypes.h>  // PRId64
#include <string.h>

#include "ast.h"
#include "fe_misc.h"
#include "lexer.h"
#include "table.h"
#include "type.h"
#include "util.h"
#include "var.h"

extern bool parsing_stmt;

static void parse_enum_members(Type *type) {
  assert(type != NULL && type->kind == TY_FIXNUM && type->fixnum.kind == FX_ENUM);
  Type *ctype = qualified_type(type, TQ_CONST);
  Vector *members = type->fixnum.enum_.info->members;
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
    EnumMemberInfo *m = calloc_or_die(sizeof(*m));
    m->name = ident->ident;
    m->value = value;
    vec_push(members, m);
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
  Token *tagname = match(TK_IDENT);
  EnumInfo *einfo = tagname != NULL ? find_enum(curscope, tagname->ident, NULL) : NULL;
  Type *type;
  if (match(TK_LBRACE)) {
    if (einfo != NULL) {
      parse_error(PE_NOFATAL, tagname, "Duplicate enum type");
    } else {
      einfo = malloc_or_die(sizeof(*einfo));
      einfo->members = new_vector();
      if (tagname != NULL)
        define_enum(curscope, tagname->ident, einfo);
    }
    type = create_enum_type(einfo, tagname != NULL ? tagname->ident : NULL);
    parse_enum_members(type);
  } else {
    if (einfo == NULL) {
      if (tagname == NULL) {
        parse_error(PE_NOFATAL, NULL, "ident expected");
      } else {
        // Imcomplete enum (enum with unknown name): Create silently.
        define_enum(curscope, tagname->ident, NULL);
      }
    }
    type = create_enum_type(NULL, tagname != NULL ? tagname->ident : NULL);
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
      parse_error(PE_NOFATAL, flex_arr_mem, "Flexible array member must be last element");
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
      if (!ensure_type_info(type, ident, curscope, true))
        continue;
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
          parse_error(PE_NOFATAL, bit->token, "illegal bit width");
          bit = NULL;
        } else if (bit->fixnum > (Fixnum)(type_size(type) * TARGET_CHAR_BIT)) {
          parse_error(PE_NOFATAL, bit->token, "bit width exceeds");
          bit = NULL;
        } else if (bit->fixnum == 0 && ident != NULL) {
          parse_error(PE_NOFATAL, bit->token, "bit width zero with name");
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
      case TY_AUTO:
        parse_error(PE_NOFATAL, ident, "auto type not allowed in struct/union");
        type = &tyInt;
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
      memset(p, 0, sizeof(*p));
      p->name = name;
      p->type = type;
      p->offset = 0;
#ifndef __NO_BITFIELD
      if (bit == NULL) {
        p->bitfield.active = false;
        p->bitfield.width = 0;
      } else {
        p->bitfield.active = true;
        p->bitfield.width = bit->fixnum;
      }
#endif
    } while (flex_arr_mem == NULL && match(TK_COMMA));
    consume(TK_SEMICOL, "`;' expected");
  }
  return create_struct_info(members, count, is_union, flex_arr_mem != NULL);
}

static Type *parse_typeof(const Token *tok) {
  consume(TK_LPAR, "`(' expected");
  Type *type = parse_var_def(NULL, NULL, NULL);
  if (type == NULL) {
    Expr *e = parse_expr();
    if (e == NULL) {
      parse_error(PE_FATAL, tok, "type expected");
      type = &tyInt;  // Dummy.
    } else {
      type = e->type;
    }
  }
  consume(TK_RPAR, "`)' expected");
  return type;
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
      ASSERT_PARSE_ERROR((tc.storage & ~(VS_STATIC | VS_EXTERN)) == 0, tok,
                         MULTIPLE_STORAGE_SPECIFIED);
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
    case TK_CHAR: ++tc.char_num; continue;
    case TK_SHORT: ++tc.short_num; continue;
    case TK_INT: ++tc.int_num; continue;
    case TK_LONG: ++tc.long_num; continue;
    case TK_FLOAT: ++tc.float_num; continue;
    case TK_DOUBLE: ++tc.double_num; continue;
    default: break;
    }

    if (type != NULL) {
      unget_token(tok);
      break;
    }

    switch (tok->kind) {
    case TK_STRUCT: case TK_UNION:
      {
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
          parse_error(PE_FATAL, tok, "Illegal struct/union usage");

        type = create_struct_type(sinfo, name, tc.qualifier);
      }
      break;
    case TK_ENUM:
      if (!no_type_combination(&tc, 0, 0))
        parse_error(PE_NOFATAL, tok, ILLEGAL_TYPE_COMBINATION);

      type = parse_enum();
      break;
    case TK_BOOL:
      if (!no_type_combination(&tc, 0, 0))
        parse_error(PE_NOFATAL, tok, ILLEGAL_TYPE_COMBINATION);

      type = &tyBool;
      break;
    case TK_IDENT:
      if (no_type_combination(&tc, 0, 0)) {
        if (!parsing_stmt || fetch_token()->kind != TK_COLON)
          type = find_typedef(curscope, tok->ident, NULL);
      }
      break;
    case TK_VOID:
      type = tc.qualifier & TQ_CONST ? &tyConstVoid : &tyVoid;
      break;
    case TK_AUTO_TYPE:
      if (!no_type_combination(&tc, 0, 0))
        parse_error(PE_NOFATAL, tok, ILLEGAL_TYPE_COMBINATION);

      type = calloc_or_die(sizeof(*type));
      type->kind = TY_AUTO;
      break;
    case TK_TYPEOF:
      type = parse_typeof(tok);
      break;
    default: break;
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
static Type *parse_pointer(Type *type) {
  if (type == NULL)
    return NULL;

  for (;;) {
    const Token *tok;
    if ((tok = match(TK_CONST)) != NULL) {
      if (type->qualifier & TQ_CONST)
        parse_error(PE_NOFATAL, tok, "multiple `const' specified");
      else
        type = qualified_type(type, TQ_CONST);
      continue;
    }
    if ((tok = match(TK_VOLATILE)) != NULL) {
      if (type->qualifier & TQ_VOLATILE)
        parse_error(PE_NOFATAL, tok, "multiple `volatile' specified");
      else
        type = qualified_type(type, TQ_VOLATILE);
      continue;
    }
    if ((tok = match(TK_RESTRICT)) != NULL) {
      if (type->qualifier & TQ_RESTRICT)
        parse_error(PE_NOFATAL, tok, "multiple `restrict' specified");
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
    if (is_fixnum(expr->type)) {
      *pvla = expr;
      mark_var_used(expr);
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
        vla = new_expr_variable(var->ident, varinfo->type, var, curscope);  // TODO: Use function root scope.
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
  } else if (match(TK_LPAR)) {
    bool vaargs;
    Vector *param_vars = parse_funparams(&vaargs);
    Type *rettype = parse_direct_declarator_suffix(type);
    if (rettype->kind == TY_AUTO) {
      parse_error(PE_NOFATAL, NULL, "auto type not allowed in function return type");
      rettype = &tyInt;
    }

    Vector *param_types = extract_varinfo_types(param_vars);
    type = new_func_type(rettype, param_types, vaargs);
    type->func.param_vars = param_vars;
  }
  return type;
}
Type *parse_direct_declarator(Type *type, Token **pident) {
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
        case TY_AUTO:
          parse_error(PE_NOFATAL, ident, "auto type not allowed in function parameter");
          type = &tyInt;
          break;
        default: break;
        }

        ensure_type_info(type, ident, curscope, false);

        if (type->kind == TY_STRUCT) {
          if (type->struct_.info != NULL && type->struct_.info->is_flexible)
            parse_error(PE_NOFATAL, ident, "using flexible array as a parameter not allowed");
        }

        if (ident != NULL && var_find(vars, ident->ident) >= 0)
          parse_error(PE_NOFATAL, ident, "`%.*s' already defined", NAMES(ident->ident));
        else
          var_add(vars, ident, type, storage | VS_PARAM);
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
