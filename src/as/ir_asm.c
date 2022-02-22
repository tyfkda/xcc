#include "ir_asm.h"

#include <assert.h>
#include <stdlib.h>  // malloc

#include "gen_section.h"
#include "inst.h"
#include "table.h"
#include "util.h"

#define BYTE_SIZE  (1)
#define WORD_SIZE  (2)
#define LONG_SIZE  (4)
#define QUAD_SIZE  (8)

static LabelInfo *new_label(int section, uintptr_t address) {
  LabelInfo *info = malloc(sizeof(*info));
  info->section = section;
  info->flag = 0;
  info->address = address;
  return info;
}

bool add_label_table(Table *label_table, const Name *label, int section, bool define, bool global) {
  LabelInfo *info = table_get(label_table, label);
  if (info != NULL) {
    if (define) {
      if ((info->flag & LF_DEFINED) != 0) {
        fprintf(stderr, "`%.*s' already defined\n", label->bytes, label->chars);
        return false;
      }
      info->address = 1;
      info->section = section;
    }
  } else {
    info = new_label(section, 0);
    table_put(label_table, label, info);
  }
  if (define)
    info->flag |= LF_DEFINED;
  if (global)
    info->flag |= LF_GLOBAL;
  return true;
}

IR *new_ir_label(const Name *label) {
  IR *ir = malloc(sizeof(*ir));
  ir->kind = IR_LABEL;
  ir->label = label;
  return ir;
}

IR *new_ir_code(const Code *code) {
  IR *ir = malloc(sizeof(*ir));
  ir->kind = IR_CODE;
  ir->code = *code;
  return ir;
}

IR *new_ir_data(const void *data, size_t size) {
  IR *ir = malloc(sizeof(*ir));
  ir->kind = IR_DATA;
  ir->data.len = size;
  ir->data.buf = (unsigned char*)data;
  return ir;
}

IR *new_ir_bss(size_t size) {
  IR *ir = malloc(sizeof(*ir));
  ir->kind = IR_BSS;
  ir->bss = size;
  return ir;
}

IR *new_ir_align(int align) {
  IR *ir = malloc(sizeof(*ir));
  ir->kind = IR_ALIGN;
  ir->align = align;
  return ir;
}

IR *new_ir_expr(enum IrKind kind, const Expr *expr) {
  IR *ir = malloc(sizeof(*ir));
  ir->kind = kind;
  ir->expr = expr;
  return ir;
}

static uintptr_t align_next_section(enum SectionType sec, uintptr_t address) {
  size_t align = section_aligns[sec];
  if (align > 1)
    address = ALIGN(address, align);
  return address;
}

bool calc_label_address(uintptr_t start_address, Vector **section_irs, Table *label_table) {
  bool settle = true;
  uintptr_t address = start_address;
  for (int sec = 0; sec < SECTION_COUNT; ++sec) {
    address = align_next_section(sec, address);
    section_start_addresses[sec] = address;

    Vector *irs = section_irs[sec];
    for (int i = 0, len = irs->len; i < len; ++i) {
      IR *ir = irs->data[i];
      ir->address = address;
      switch (ir->kind) {
      case IR_LABEL:
        {
          LabelInfo *info;
          if (!table_try_get(label_table, ir->label, (void**)&info)) {
            fprintf(stderr, "[%.*s] not found\n", ir->label->bytes, ir->label->chars);
            assert(!"Unexpected");
          } else {
            info->address = address;
          }
        }
        break;
      case IR_CODE:
        address += ir->code.len;
        break;
      case IR_DATA:
        address += ir->data.len;
        break;
      case IR_BSS:
        address += ir->bss;
        break;
      case IR_ALIGN:
        ir->address = address = ALIGN(address, ir->align);
        if ((size_t)ir->align > section_aligns[sec]) {
          section_aligns[sec] = ir->align;
          settle = false;
        }
        break;
      case IR_EXPR_BYTE:
        address += BYTE_SIZE;
        break;
      case IR_EXPR_WORD:
        address += WORD_SIZE;
        break;
      case IR_EXPR_LONG:
        address += LONG_SIZE;
        break;
      case IR_EXPR_QUAD:
        address += QUAD_SIZE;
        break;
      default:  assert(false); break;
      }
    }
  }
  return settle;
}

static void put_value(unsigned char *p, intptr_t value, int size) {
  for (int i = 0; i < size; ++i) {
    *p++ = value;
    value >>= 8;
  }
}

static void put_unresolved(Table *table, const Name *label) {
  table_put(table, label, (void*)label);
}

typedef struct {
  const Name *label;
  intptr_t offset;
} Value;

static Value calc_expr(Table *label_table, const Expr *expr) {
  assert(expr != NULL);
  switch (expr->kind) {
  case EX_LABEL:
    return (Value){.label = expr->label, .offset = 0};
  case EX_FIXNUM:
    return (Value){.label = NULL, .offset = expr->fixnum};
  case EX_ADD:
  case EX_SUB:
  case EX_MUL:
  case EX_DIV:
    {
      Value lhs = calc_expr(label_table, expr->bop.lhs);
      Value rhs = calc_expr(label_table, expr->bop.rhs);
      if (rhs.label != NULL) {
        if (expr->kind == EX_SUB && lhs.label != NULL) {
          LabelInfo *llabel, *rlabel;
          if (table_try_get(label_table, lhs.label, (void**)&llabel) &&
              table_try_get(label_table, rhs.label, (void**)&rlabel)) {
            return (Value){.label = NULL, .offset = llabel->address - rlabel->address};
          } else {
            error("Unresolved");
          }
        }
        if (expr->kind != EX_ADD || lhs.label != NULL) {
          error("Illegal expression");
          return rhs;
        }
        // offset + label
        return (Value){.label = rhs.label, .offset = lhs.offset + rhs.offset};
      }
      if (lhs.label != NULL) {
        if (expr->kind != EX_ADD) {
          error("Illegal expression");
          return lhs;
        }
        // label + offset
        return (Value){.label = lhs.label, .offset = lhs.offset + rhs.offset};
      }

      assert(lhs.label == NULL && rhs.label == NULL);
      switch (expr->kind) {
      case EX_ADD:  lhs.offset += rhs.offset; break;
      case EX_SUB:  lhs.offset -= rhs.offset; break;
      case EX_MUL:  lhs.offset *= rhs.offset; break;
      case EX_DIV:  lhs.offset /= rhs.offset; break;
      default: assert(false); break;
      }
      return lhs;
    }

  case EX_POS:
  case EX_NEG:
    {
      Value value = calc_expr(label_table, expr->unary.sub);
      if (value.label != NULL) {
        error("Illegal expression");
      }
      if (expr->kind == EX_NEG)
        value.offset = -value.offset;
      return value;
    }

  default: assert(false); break;
  }
  return (Value){.label = NULL, .offset = 0};
}

bool resolve_relative_address(Vector **section_irs, Table *label_table, Vector *unresolved) {
  Table unresolved_labels;
  table_init(&unresolved_labels);
  if (unresolved != NULL)
    vec_clear(unresolved);
  bool size_upgraded = false;
  for (int sec = 0; sec < SECTION_COUNT; ++sec) {
    Vector *irs = section_irs[sec];
    uintptr_t start_address = irs->len > 0 ? ((IR*)irs->data[0])->address : 0;
    for (int i = 0, len = irs->len; i < len; ++i) {
      IR *ir = irs->data[i];
      uintptr_t address = ir->address;
      switch (ir->kind) {
      case IR_CODE:
        {
          Inst *inst = ir->code.inst;
          switch (inst->op) {
          case LEA:
            if (inst->src.type == INDIRECT &&
                inst->src.indirect.reg.no == RIP &&
                inst->src.indirect.offset->kind != EX_FIXNUM) {
              Value value = calc_expr(label_table, inst->src.indirect.offset);
              if (value.label != NULL) {
                LabelInfo *label_info = table_get(label_table, value.label);
                if (unresolved != NULL) {
                  if (label_info == NULL) {
                    UnresolvedInfo *info = malloc(sizeof(*info));
                    info->kind = UNRES_EXTERN_PC32;
                    info->label = value.label;
                    info->src_section = sec;
                    info->offset = address + 3 - start_address;
                    info->add = value.offset - 4;
                    vec_push(unresolved, info);
                    break;
                  } else if (label_info->section != sec) {
                    Vector *irs2 = section_irs[label_info->section];
                    uintptr_t dst_start_address = irs2->len > 0 ? ((IR *)irs2->data[0])->address : 0;

                    UnresolvedInfo *info = malloc(sizeof(*info));
                    info->kind = UNRES_OTHER_SECTION;
                    info->label = value.label;
                    info->src_section = sec;
                    info->offset = address + 3 - start_address;
                    info->add = label_info->address + value.offset - dst_start_address - 4;
                    vec_push(unresolved, info);
                    break;
                  }
                } else {
                  if (label_info == NULL) {
                    put_unresolved(&unresolved_labels, value.label);
                    break;
                  }
                }
                assert(label_info != NULL);
                value.offset += label_info->address;
              }
              intptr_t offset = value.offset - ((intptr_t)address + ir->code.len);
              put_value(ir->code.buf + 3, offset, sizeof(int32_t));
            }
            break;
          case JMP:
          case JO: case JNO: case JB:  case JAE:
          case JE: case JNE: case JBE: case JA:
          case JS: case JNS: case JP:  case JNP:
          case JL: case JGE: case JLE: case JG:
            if (inst->src.type == DIRECT) {
              Value value = calc_expr(label_table, inst->src.direct.expr);
              if (value.label != NULL) {
                LabelInfo *label_info = table_get(label_table, value.label);
                if (label_info == NULL) {
                  assert(!"Not handled");
                  break;
                } else {
                  value.offset += label_info->address;
                }
              }

              intptr_t offset = value.offset - ((intptr_t)address + ir->code.len);
              bool long_offset = ir->code.flag & INST_LONG_OFFSET;
              if (!long_offset) {
                if (!is_im8(offset)) {
                  // Change to long offset, and recalculate.
                  ir->code.flag |= INST_LONG_OFFSET;
                  if (inst->op == JMP)
                    MAKE_CODE(inst, &ir->code, 0xe9, IM32(-1));
                  else
                    MAKE_CODE(inst, &ir->code, 0x0f, 0x80 + (inst->op - JO), IM32(-1));
                  size_upgraded = true;
                } else {
                  put_value(ir->code.buf + 1, offset, sizeof(int8_t));
                }
              } else {
                if (!is_im32(offset))
                  error("Jump offset too far (over 32bit)");

                int d = inst->op == JMP ? 1 : 2;
                put_value(ir->code.buf + d, offset, sizeof(int32_t));
              }
            }
            break;
          case CALL:
            if (inst->src.type == DIRECT) {
              Value value = calc_expr(label_table, inst->src.direct.expr);
              if (value.label != NULL) {
                LabelInfo *label_info = table_get(label_table, value.label);
                if (label_info == NULL) {
                  if (unresolved == NULL) {
                    put_unresolved(&unresolved_labels, value.label);
                    break;
                  }
                  UnresolvedInfo *info = malloc(sizeof(*info));
                  info->kind = UNRES_EXTERN;
                  info->label = value.label;
                  info->src_section = sec;
                  info->offset = address + 1 - start_address;
                  info->add = value.offset - 4;
                  vec_push(unresolved, info);
                  break;
                }
                value.offset += label_info->address;
              }
              intptr_t offset = value.offset - ((intptr_t)address + ir->code.len);
              put_value(ir->code.buf + 1, offset, sizeof(int32_t));
            }
            break;
          default:
            break;
          }
        }
        break;
      case IR_EXPR_BYTE:
      case IR_EXPR_WORD:
      case IR_EXPR_LONG:
      case IR_EXPR_QUAD:
        {
          Value value = calc_expr(label_table, ir->expr);
          if (value.label != NULL) {
            if (unresolved != NULL) {
              UnresolvedInfo *info = malloc(sizeof(*info));
              info->kind = UNRES_ABS64;  // TODO:
              info->label = value.label;
              info->src_section = sec;
              info->offset = address - start_address;
              info->add = value.offset;
              vec_push(unresolved, info);
              break;
            }
            LabelInfo *label_info = table_get(label_table, value.label);
            if (label_info == NULL) {
              put_unresolved(&unresolved_labels, value.label);
              break;
            }
            value.offset += label_info->address;
          }
          put_value(ir->code.buf + 3, value.offset, sizeof(int32_t));
        }
        break;
      case IR_LABEL:
      case IR_DATA:
      case IR_BSS:
      case IR_ALIGN:
        break;
      default:  assert(false); break;
      }
    }
  }

  if (unresolved_labels.count > 0 && unresolved == NULL) {
    for (int i = 0; ;) {
      const Name *name;
      void *dummy;
      i = table_iterate(&unresolved_labels, i, &name, &dummy);
      if (i < 0)
        break;
      fprintf(stderr, "Undefined reference: `%.*s'\n", name->bytes, name->chars);
    }
    exit(1);
  }

  return !size_upgraded;
}

void emit_irs(Vector **section_irs, Table *label_table) {
  for (int sec = 0; sec < SECTION_COUNT; ++sec) {
    Vector *irs = section_irs[sec];
    for (int i = 0, len = irs->len; i < len; ++i) {
      IR *ir = irs->data[i];
      switch (ir->kind) {
      case IR_LABEL:
        break;
      case IR_CODE:
        add_code(ir->code.buf, ir->code.len);
        break;
      case IR_DATA:
        add_section_data(sec, ir->data.buf, ir->data.len);
        break;
      case IR_BSS:
        add_bss(ir->bss);
        break;
      case IR_ALIGN:
        align_section_size(sec, ir->align);
        break;
      case IR_EXPR_BYTE:
      case IR_EXPR_WORD:
      case IR_EXPR_LONG:
      case IR_EXPR_QUAD:
        {
          Value value = calc_expr(label_table, ir->expr);
          if (value.label != NULL) {
            LabelInfo *label_info = table_get(label_table, value.label);
            if (label_info != NULL) {
              value.offset += label_info->address;
            }
          }
          int size = 1 << (ir->kind - IR_EXPR_BYTE);
          add_section_data(sec, &value.offset, size);  // TODO: Target endian
        }
        break;
      default:  assert(false); break;
      }
    }
  }
}
