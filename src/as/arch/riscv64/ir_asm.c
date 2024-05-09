#include "../../../config.h"
#include "ir_asm.h"

#include <assert.h>
#include <stdint.h>

#include "gen_section.h"
#include "inst.h"
#include "parse_asm.h"
#include "riscv64_code.h"
#include "table.h"
#include "util.h"

#define BYTE_SIZE   (1)
#define SHORT_SIZE  (2)
#define LONG_SIZE   (4)
#define QUAD_SIZE   (8)

IR *new_ir_label(const Name *label) {
  IR *ir = malloc_or_die(sizeof(*ir));
  ir->kind = IR_LABEL;
  ir->label = label;
  return ir;
}

IR *new_ir_code(const Code *code) {
  IR *ir = malloc_or_die(sizeof(*ir));
  ir->kind = IR_CODE;
  ir->code = *code;
  return ir;
}

IR *new_ir_data(const void *data, size_t size) {
  IR *ir = malloc_or_die(sizeof(*ir));
  ir->kind = IR_DATA;
  ir->data.len = size;
  ir->data.buf = (unsigned char*)data;
  return ir;
}

IR *new_ir_bss(size_t size) {
  IR *ir = malloc_or_die(sizeof(*ir));
  ir->kind = IR_BSS;
  ir->bss = size;
  return ir;
}

IR *new_ir_align(int align) {
  IR *ir = malloc_or_die(sizeof(*ir));
  ir->kind = IR_ALIGN;
  ir->align = align;
  return ir;
}

IR *new_ir_expr(enum IrKind kind, const Expr *expr) {
  IR *ir = malloc_or_die(sizeof(*ir));
  ir->kind = kind;
  ir->expr = expr;
  return ir;
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

static bool make_bxx_long(IR *ir) {
  if (ir->code.flag & INST_LONG_OFFSET)
    return false;

  Code *code = &ir->code;
  assert(code->len == 2);
  uint16_t *buf = (uint16_t*)code->buf;
  assert((*buf & 0xc003) == 0xc001);
  uint16_t s = *buf;

  Inst *inst = code->inst;
  // Change to long offset, and recalculate.
  code->flag |= INST_LONG_OFFSET;
  code->len = 0;

  uint16_t rs = ((s >> 7) & 7) + 8;
  assert(inst->op == BEQ || inst->op == BNE);
  int offset = 0;  // Offset is set at next iteration.
  W_BXX(inst->op - BEQ, rs, 0, offset);
  return true;
}

static bool make_jmp_long(IR *ir) {
  if (ir->code.flag & INST_LONG_OFFSET)
    return false;

  Code *code = &ir->code;
  Inst *inst = code->inst;
  // Change to long offset, and recalculate.
  code->flag |= INST_LONG_OFFSET;
  code->len = 0;
  assert(inst->op == J);

  W_JAL(ZERO, 0);
  return true;
}

bool resolve_relative_address(Vector **section_irs, Table *label_table, Vector *unresolved) {
  assert(unresolved != NULL);
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
          case LA:
            assert(inst->opr3.type == DIRECT);
            if (inst->opr2.type == DIRECT) {
              Value value = calc_expr(label_table, inst->opr2.direct.expr);
              if (value.label != NULL) {
                uintptr_t offset = address - start_address;
                UnresolvedInfo *info;
                info = calloc_or_die(sizeof(*info));
                info->kind = UNRES_RISCV_PCREL_HI20;
                info->label = value.label;
                info->src_section = sec;
                info->offset = offset;
                info->add = value.offset;
                vec_push(unresolved, info);

                info = calloc_or_die(sizeof(*info));
                info->kind = UNRES_RISCV_RELAX;
                info->label = value.label;
                info->src_section = sec;
                info->offset = offset;
                info->add = value.offset;
                vec_push(unresolved, info);

                // hilabel points to AUIPC instruction, just above one.
                assert(inst->opr3.direct.expr->kind == EX_LABEL);
                const Name *hilabel = inst->opr3.direct.expr->label;
                info = calloc_or_die(sizeof(*info));
                info->kind = UNRES_RISCV_PCREL_LO12_I;
                info->label = hilabel;
                info->src_section = sec;
                info->offset = offset + 4;
                info->add = 0;
                vec_push(unresolved, info);

                info = calloc_or_die(sizeof(*info));
                info->kind = UNRES_RISCV_RELAX;
                info->label = hilabel;
                info->src_section = sec;
                info->offset = offset + 4;
                info->add = 0;
                vec_push(unresolved, info);
                break;
              }
            }
            break;
          case J:
            if (inst->opr1.type == DIRECT) {
              int64_t target_address = 0;
              Value value = calc_expr(label_table, inst->opr1.direct.expr);
              if (value.label != NULL) {
                // Put rela even if the label is defined in the same object file.
                UnresolvedInfo *info;
                info = calloc_or_die(sizeof(*info));
                info->kind = (ir->code.flag & INST_LONG_OFFSET) ? UNRES_RISCV_JAL : UNRES_RISCV_RVC_JUMP;
                info->label = value.label;
                info->src_section = sec;
                info->offset = address - start_address;
                info->add = value.offset;
                vec_push(unresolved, info);

                LabelInfo *label_info = table_get(label_table, value.label);
                if (label_info != NULL)
                  target_address = label_info->address + value.offset;
              }

              if (target_address != 0) {
                Code *code = &ir->code;
                int64_t offset = target_address - VOIDP2INT(address);
                bool is_long = ir->code.flag & INST_LONG_OFFSET;
                if (!is_long) {
                  assert(code->len == 2);
                  if (offset < (1 << 11) && offset >= -(1 << 11)) {
                    uint16_t *buf = (uint16_t*)code->buf;
                    // Compressed: imm[11|4|9:8|10|6|7|3:1|5]
                    buf[0] = (buf[0] & 0xe383) | SWIZZLE_C_J(offset);
                  } else {
                    size_upgraded |= make_jmp_long(ir);
                  }
                } else {
                  if (offset < (1 << 20) && offset >= -(1 << 20)) {
                    uint32_t *buf = (uint32_t*)code->buf;
                    // jal: imm[20|10:1|11|19:12]
                    buf[0] = (buf[0] & 0x000007ff) | (IMM(offset, 20, 20) << 31) | (IMM(offset, 10, 1) << 21) |
                      (IMM(offset, 11, 11) << 20) | (IMM(offset, 19, 12) << 12);
                  } else {
                    // Linker extends the branch instruction to long offset?
                    assert(false);
                  }
                }
              }
            }
            break;
          case BEQ: case BNE: case BLT: case BGE: case BLTU: case BGEU:
            if (inst->opr3.type == DIRECT) {
              bool comp = false;
              int64_t target_address = 0;
              Value value = calc_expr(label_table, inst->opr3.direct.expr);
              if (value.label != NULL) {
                LabelInfo *label_info = table_get(label_table, value.label);
                if (label_info != NULL)
                  target_address = label_info->address + value.offset;

                int64_t offset = target_address - VOIDP2INT(address);
                comp = inst->opr2.reg.no == 0 && is_rvc_reg(inst->opr1.reg.no) &&
                  (inst->op == BEQ || inst->op == BNE) &&
                  offset < (1 << 7) && offset >= -(1 << 7);

                assert(inst->opr2.type == REG);
                // Put rela even if the label is defined in the same object file.
                UnresolvedInfo *info;
                info = calloc_or_die(sizeof(*info));
                info->kind = comp ? UNRES_RISCV_RVC_BRANCH : UNRES_RISCV_BRANCH;
                info->label = value.label;
                info->src_section = sec;
                info->offset = address - start_address;
                info->add = value.offset;
                vec_push(unresolved, info);
              }

              if (target_address != 0) {
                Code *code = &ir->code;
                int64_t offset = target_address - VOIDP2INT(address);
                bool is_long = ir->code.flag & INST_LONG_OFFSET;
                if (!is_long) {
                  assert(code->len == 2);
                  if (offset < (1 << 7) && offset >= -(1 << 7)) {
                    uint16_t *buf = (uint16_t*)code->buf;
                    assert((*buf & 0xc003) == 0xc001);
                    *buf = (*buf & 0xe383) | SWIZZLE_C_BXX(offset);
                  } else {
                    size_upgraded |= make_bxx_long(ir);
                  }
                } else {
                  if (offset < (1 << 11) && offset >= -(1 << 11)) {
                    assert(code->len == 4);
                    uint32_t *buf = (uint32_t*)code->buf;
                    // STYPE
                    buf[0] = (buf[0] & 0x01fff07f) | SWIZZLE_BXX(offset);
                  } else {
                    // Linker extends the branch instruction to long offset?
                    // assert(false);
                  }
                }
              }
            }
            break;
          case CALL:
            if (inst->opr1.type == DIRECT) {
              Value value = calc_expr(label_table, inst->opr1.direct.expr);
              if (value.label != NULL) {
                // Put rela even if the label is defined in the same object file.
                UnresolvedInfo *info;
                info = calloc_or_die(sizeof(*info));
                info->kind = UNRES_RISCV_CALL;
                info->label = value.label;
                info->src_section = sec;
                info->offset = address - start_address;
                info->add = value.offset;
                vec_push(unresolved, info);

                info = calloc_or_die(sizeof(*info));
                info->kind = UNRES_RISCV_RELAX;
                info->label = value.label;
                info->src_section = sec;
                info->offset = address - start_address;
                info->add = value.offset;
                vec_push(unresolved, info);
                break;
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
          Value value = calc_expr(label_table, ir->expr);
          assert(value.label != NULL);
          UnresolvedInfo *info = malloc_or_die(sizeof(*info));
          info->kind = UNRES_ABS64;  // TODO:
          info->label = value.label;
          info->src_section = sec;
          info->offset = address - start_address;
          info->add = value.offset;
          vec_push(unresolved, info);
        }
        break;
      case IR_LABEL:
      case IR_DATA:
      case IR_BSS:
      case IR_ALIGN:
        break;
      }
    }
  }

  return !size_upgraded;
}

void emit_irs(Vector **section_irs) {
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
      case IR_EXPR_SHORT:
      case IR_EXPR_LONG:
      case IR_EXPR_QUAD:
        {
          int64_t zero = 0;
          int size = 1 << (ir->kind - IR_EXPR_BYTE);
          add_section_data(sec, &zero, size);  // TODO: Target endian
        }
        break;
      }
    }
  }
}
