#include "../../../config.h"
#include "ir_asm.h"

#include <assert.h>

#include "inst.h"
#include "parse_asm.h"
#include "table.h"
#include "util.h"

static void sec_add_data(SectionInfo *section, const void *data, size_t bytes) {
  assert(!(section->flag & SF_BSS));
  data_append(section->ds, data, bytes);
}

static void sec_add_code(SectionInfo *section, const void *buf, size_t bytes) {
  sec_add_data(section, buf, bytes);
}

static void sec_add_bss(SectionInfo *section, size_t size) {
  assert(section->flag & SF_BSS);
  section->bss_size += size;
}

static void sec_align_size(SectionInfo *section, size_t align) {
  if (align > section->align)
    section->align = align;

  if (section->flag & SF_BSS)
    section->bss_size = ALIGN(section->bss_size, align);
  else
    data_align(section->ds, align);
}

bool calc_label_address(uint64_t start_address, Vector *sections, Table *label_table) {
  bool settle = true;
  uint64_t address = start_address;
  for (int sec = 0; sec < sections->len; ++sec) {
    SectionInfo *section = sections->data[sec];
    address = ALIGN(address, section->align);
    section->start_address = address;

    Vector *irs = section->irs;
    for (int i = 0, len = irs->len; i < len; ++i) {
      IR *ir = irs->data[i];
      ir->address = address;
      switch (ir->kind) {
      case IR_LABEL:
        {
          LabelInfo *info;
          if (!table_try_get(label_table, ir->label, (void**)&info)) {
            fprintf(stderr, "[%.*s] not found\n", NAMES(ir->label));
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
      case IR_ZERO:
        address += ir->zero;
        break;
      case IR_ALIGN:
        ir->address = address = ALIGN(address, ir->align);
        if ((size_t)ir->align > section->align) {
          section->align = ir->align;
          settle = false;
        }
        break;
      case IR_EXPR_BYTE:
        address += BYTE_SIZE;
        break;
      case IR_EXPR_SHORT:
        address += SHORT_SIZE;
        break;
      case IR_EXPR_LONG:
        address += LONG_SIZE;
        break;
      case IR_EXPR_QUAD:
        address += QUAD_SIZE;
        break;
      }
    }
  }
  return settle;
}

bool resolve_relative_address(Vector *sections, Table *label_table, Vector *unresolved) {
  assert(unresolved != NULL);
  vec_clear(unresolved);
  bool size_upgraded = false;
  for (int sec = 0; sec < sections->len; ++sec) {
    SectionInfo *section = sections->data[sec];
    Vector *irs = section->irs;
    uint64_t start_address = irs->len > 0 ? ((IR*)irs->data[0])->address : 0;
    for (int i = 0, len = irs->len; i < len; ++i) {
      IR *ir = irs->data[i];
      uint64_t address = ir->address;
      switch (ir->kind) {
      case IR_CODE:
        {
          Inst *inst = ir->code.inst;
          switch (inst->op) {
          case ADD_I:
            if (inst->opr[2].type == DIRECT) {
              ExprWithFlag *expr = &inst->opr[2].direct.expr;
              Value value = calc_expr(label_table, expr->expr);
              if (value.label != NULL) {
                if (expr->flag == LF_PAGEOFF || expr->flag == (LF_GOT | LF_PAGEOFF)) {
                  UnresolvedInfo *info = malloc_or_die(sizeof(*info));
                  info->kind = expr->flag == LF_PAGEOFF ? UNRES_PCREL_LO : UNRES_GOT_LO;
                  info->label = value.label;
                  info->src_section = section;
                  info->offset = address - start_address;
                  info->add = value.offset;
                  vec_push(unresolved, info);
                }
              }
            }
            break;
          case LDR:
          case STR:
            if (inst->opr[1].type == INDIRECT && inst->opr[1].indirect.offset.expr != NULL) {
              ExprWithFlag *offset = &inst->opr[1].indirect.offset;
              Value value = calc_expr(label_table, offset->expr);
              if (value.label != NULL) {
                static const int table[][2] = {
                  {LF_GOT, UNRES_GOT_HI},
                  {LF_GOT | LF_PAGEOFF, UNRES_GOT_LO},
                };
                size_t i;
                for (i = 0; i < ARRAY_SIZE(table); ++i) {
                  if (table[i][0] == offset->flag)
                    break;
                }
                if (i >= ARRAY_SIZE(table)) {
                  break;
                }

                UnresolvedInfo *info = malloc_or_die(sizeof(*info));
                info->kind = table[i][1];
                info->label = value.label;
                info->src_section = section;
                info->offset = address - start_address;
                info->add = value.offset;
                vec_push(unresolved, info);
              }
            }
            break;
          case ADRP:
            if (inst->opr[1].type == DIRECT) {
              ExprWithFlag *expr = &inst->opr[1].direct.expr;
              Value value = calc_expr(label_table, expr->expr);
              if (value.label != NULL) {
                static const int table[][2] = {
                  {0, UNRES_PCREL_HI},
                  {LF_PAGE, UNRES_PCREL_HI},
                  {LF_GOT | LF_PAGE, UNRES_GOT_HI},
                };
                size_t i;
                for (i = 0; i < ARRAY_SIZE(table); ++i) {
                  if (table[i][0] == expr->flag)
                    break;
                }
                if (i >= ARRAY_SIZE(table)) {
                  break;
                }

                UnresolvedInfo *info = malloc_or_die(sizeof(*info));
                info->kind = table[i][1];
                info->label = value.label;
                info->src_section = section;
                info->offset = address - start_address;
                info->add = value.offset;
                vec_push(unresolved, info);
              }
            }
            break;
          case B:
          case BEQ: case BNE: case BHS: case BLO:
          case BMI: case BPL: case BVS: case BVC:
          case BHI: case BLS: case BGE: case BLT:
          case BGT: case BLE: case BAL: case BNV:
            if (inst->opr[0].type == DIRECT) {
              Value value = calc_expr(label_table, inst->opr[0].direct.expr.expr);
              if (value.label != NULL) {
                LabelInfo *label_info = table_get(label_table, value.label);
                if (label_info == NULL) {
                  /*UnresolvedInfo *info = malloc_or_die(sizeof(*info));
                  info->kind = UNRES_EXTERN;
                  info->label = value.label;
                  info->src_section = section;
                  info->offset = address - start_address;
                  info->add = value.offset - 4;
                  vec_push(unresolved, info);
                  break;*/
                  assert(false);
                } else {
                  value.offset += label_info->address;
                }
              }

              int64_t offset = value.offset - address;
              if (inst->op == B) {
                if (offset >= (1L << 27) || offset < -(1L << 27) || (offset & 3) != 0)
                  error("Jump offset too far (over 32bit)");

                Code *code = &ir->code;
                uint32_t *buf = (uint32_t*)code->buf;
                *buf = (*buf & 0xfc000000) | ((offset >> 2) & ((1U << 26) - 1));
              } else {
                if (offset >= (1L << 20) || offset < -(1L << 20) || (offset & 3) != 0)
                  error("Jump offset too far (over 32bit)");

                Code *code = &ir->code;
                uint32_t *buf = (uint32_t*)code->buf;
                *buf = (*buf & 0xff00001f) | ((offset & ((1U << 21) - 1)) << (5 - 2));
              }
            }
            break;

          case CBZ: case CBNZ:
            if (inst->opr[1].type == DIRECT) {
              Value value = calc_expr(label_table, inst->opr[1].direct.expr.expr);
              if (value.label != NULL) {
                LabelInfo *label_info = table_get(label_table, value.label);
                if (label_info == NULL) {
                  /*UnresolvedInfo *info = malloc_or_die(sizeof(*info));
                  info->kind = UNRES_EXTERN;
                  info->label = value.label;
                  info->src_section = section;
                  info->offset = address - start_address;
                  info->add = value.offset - 4;
                  vec_push(unresolved, info);
                  break;*/
                  assert(false);
                } else {
                  value.offset += label_info->address;
                }
              }

              int64_t offset = value.offset - address;
              {
                if (offset >= (1L << 20) || offset < -(1L << 20) || (offset & 3) != 0)
                  error("Jump offset too far (over 32bit)");

                Code *code = &ir->code;
                uint32_t *buf = (uint32_t*)code->buf;
                *buf = (*buf & 0xff00001f) | ((offset & ((1U << 21) - 1)) << (5 - 2));
              }
            }
            break;

          case BL:
            if (inst->opr[0].type == DIRECT) {
              Value value = calc_expr(label_table, inst->opr[0].direct.expr.expr);
              if (value.label != NULL) {
                UnresolvedInfo *info = malloc_or_die(sizeof(*info));
                info->kind = UNRES_CALL;
                info->label = value.label;
                info->src_section = section;
                info->offset = address - start_address;
                info->add = value.offset;
                vec_push(unresolved, info);
              }
            }
            break;
          default:
            break;
          }
        }
        break;
      case IR_EXPR_BYTE:
      case IR_EXPR_SHORT:
      case IR_EXPR_LONG:
      case IR_EXPR_QUAD:
        {
          Value value = calc_expr(label_table, ir->expr.expr);
          assert(value.label != NULL);
          UnresolvedInfo *info = malloc_or_die(sizeof(*info));
          info->kind = UNRES_ABS64;  // TODO:
          info->label = value.label;
          info->src_section = section;
          info->offset = address - start_address;
          info->add = value.offset;
          ir->expr.addend = value.offset;
          vec_push(unresolved, info);
        }
        break;
      case IR_LABEL:
      case IR_DATA:
      case IR_BSS:
      case IR_ZERO:
      case IR_ALIGN:
        break;
      }
    }
  }

  return !size_upgraded;
}

void emit_irs(Vector *sections) {
  for (int sec = 0; sec < sections->len; ++sec) {
    SectionInfo *section = sections->data[sec];
    Vector *irs = section->irs;
    for (int i = 0, len = irs->len; i < len; ++i) {
      IR *ir = irs->data[i];
      switch (ir->kind) {
      case IR_LABEL:
        break;
      case IR_CODE:
        sec_add_code(section, ir->code.buf, ir->code.len);
        break;
      case IR_DATA:
        sec_add_data(section, ir->data.buf, ir->data.len);
        break;
      case IR_BSS:
        sec_add_bss(section, ir->bss);
        break;
      case IR_ZERO:
        if (section->flag & SF_BSS) {
          sec_add_bss(section, ir->zero);
        } else {
          void *buf = calloc_or_die(ir->zero);
          sec_add_data(section, buf, ir->zero);
        }
        break;
      case IR_ALIGN:
        sec_align_size(section, ir->align);
        break;
      case IR_EXPR_BYTE:
      case IR_EXPR_SHORT:
      case IR_EXPR_LONG:
      case IR_EXPR_QUAD:
        {
#if XCC_TARGET_PLATFORM == XCC_PLATFORM_APPLE
          int64_t value = ir->expr.addend;
#else
          int64_t value = 0;
#endif
          int size = 1 << (ir->kind - IR_EXPR_BYTE);
          sec_add_data(section, &value, size);  // TODO: Target endian
        }
        break;
      }
    }
  }
}
