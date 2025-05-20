#include "../../../config.h"
#include "ir_asm.h"

#include <assert.h>
#include <inttypes.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

#include "inst.h"
#include "parse_asm.h"
#include "riscv64_code.h"
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

static bool make_bxx_long(IR *ir) {
  if (ir->code.flag & INST_LONG_OFFSET)
    return false;

  Code *code = &ir->code;
  assert(code->len == 2);
  uint16_t *buf = (uint16_t*)code->buf;
  assert((*buf & 0xc003) == 0xc001);  // c.beqz or c.bnez
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
          case LA:
            assert(inst->opr[2].type == DIRECT);
            if (inst->opr[1].type == DIRECT) {
              Value value = calc_expr(label_table, inst->opr[1].direct.expr);
              if (value.label != NULL) {
                uint64_t offset = address - start_address;
                UnresolvedInfo *info;
                info = calloc_or_die(sizeof(*info));
                info->kind = UNRES_PCREL_HI;
                info->label = value.label;
                info->src_section = section;
                info->offset = offset;
                info->add = value.offset;
                vec_push(unresolved, info);

                info = calloc_or_die(sizeof(*info));
                info->kind = UNRES_RISCV_RELAX;
                info->label = value.label;
                info->src_section = section;
                info->offset = offset;
                info->add = value.offset;
                vec_push(unresolved, info);

                // hilabel points to AUIPC instruction, just above one.
                assert(inst->opr[2].direct.expr->kind == EX_LABEL);
                const Name *hilabel = inst->opr[2].direct.expr->label.name;
                info = calloc_or_die(sizeof(*info));
                info->kind = UNRES_PCREL_LO;
                info->label = hilabel;
                info->src_section = section;
                info->offset = offset + 4;
                info->add = 0;
                vec_push(unresolved, info);

                info = calloc_or_die(sizeof(*info));
                info->kind = UNRES_RISCV_RELAX;
                info->label = hilabel;
                info->src_section = section;
                info->offset = offset + 4;
                info->add = 0;
                vec_push(unresolved, info);
                break;
              }
            }
            break;
          case J:
            if (inst->opr[0].type == DIRECT) {
              uint64_t target_address = 0;
              Value value = calc_expr(label_table, inst->opr[0].direct.expr);
              if (value.label != NULL) {
                // Put rela even if the label is defined in the same object file.
                UnresolvedInfo *info;
                info = calloc_or_die(sizeof(*info));
                info->kind = (ir->code.flag & INST_LONG_OFFSET) ? UNRES_RISCV_JAL : UNRES_RISCV_RVC_JUMP;
                info->label = value.label;
                info->src_section = section;
                info->offset = address - start_address;
                info->add = value.offset;
                vec_push(unresolved, info);

                LabelInfo *label_info = table_get(label_table, value.label);
                if (label_info != NULL)
                  target_address = label_info->address + value.offset;
              }

              if (target_address != 0) {
                Code *code = &ir->code;
                int64_t offset = target_address - address;
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
                    buf[0] = (buf[0] & 0x000007ff) | (IMM(offset, 20, 20) << 31) |
                             (IMM(offset, 10, 1) << 21) | (IMM(offset, 11, 11) << 20) |
                             (IMM(offset, 19, 12) << 12);
                  } else {
                    // Linker extends the branch instruction to long offset?
                    fprintf(stderr, "jump offset too large: %+" PRId64 "\n", offset);
                    exit(1);
                  }
                }
              }
            }
            break;
          case BEQ: case BNE: case BLT: case BGE: case BLTU: case BGEU:
            if (inst->opr[2].type == DIRECT) {
              bool comp = false;
              uint64_t target_address = 0;
              Value value = calc_expr(label_table, inst->opr[2].direct.expr);
              if (value.label != NULL) {
                LabelInfo *label_info = table_get(label_table, value.label);
                if (label_info != NULL)
                  target_address = label_info->address + value.offset;

                int64_t offset = target_address - address;
                comp = inst->opr[1].reg.no == 0 && is_rvc_reg(inst->opr[0].reg.no) &&
                       (inst->op == BEQ || inst->op == BNE) && offset < (1 << 8) &&
                       offset >= -(1 << 8);

                assert(inst->opr[1].type == REG);
                // Put rela even if the label is defined in the same object file.
                UnresolvedInfo *info;
                info = calloc_or_die(sizeof(*info));
                info->kind = comp ? UNRES_RISCV_RVC_BRANCH : UNRES_RISCV_BRANCH;
                info->label = value.label;
                info->src_section = section;
                info->offset = address - start_address;
                info->add = value.offset;
                vec_push(unresolved, info);
              } else {
                target_address = address + value.offset;
              }

              if (target_address != 0) {
                Code *code = &ir->code;
                int64_t offset = target_address - address;
                bool is_long = ir->code.flag & INST_LONG_OFFSET;
                if (!is_long) {
                  assert(code->len == 2);
                  if (offset < (1 << 8) && offset >= -(1 << 8)) {
                    uint16_t *buf = (uint16_t*)code->buf;
                    assert((*buf & 0xc003) == 0xc001);  // c.beqz or c.bnez
                    *buf = (*buf & 0xe383) | SWIZZLE_C_BXX(offset);
                  } else {
                    size_upgraded |= make_bxx_long(ir);
                  }
                } else {
                  if (offset < (1 << 12) && offset >= -(1 << 12)) {
                    assert(code->len == 4);
                    uint32_t *buf = (uint32_t*)code->buf;
                    // STYPE
                    buf[0] = (buf[0] & 0x01fff07f) | SWIZZLE_BXX(offset);
                  } else {
                    extern unsigned char *asm_bxx(Inst *inst, Code *code);

                    // Insert `j` instruction and skip it with inverted conditional branch.
                    Expr *destination = inst->opr[2].direct.expr;
                    enum Opcode inv = ((inst->op - BEQ) ^ 1) + BEQ;  // BEQ <=> BNE, BLT <=> BGE, BLTU <=> BGEU
                    inst->op = inv;

                    Expr *skip = new_expr(EX_FIXNUM);
                    inst->opr[2].direct.expr = skip;
                    ir->code.flag &= ~INST_LONG_OFFSET;
                    ir->code.len = 0;
                    asm_bxx(inst, &ir->code);  // Reassemble the instruction.

                    // `J`
                    Code jcode, *code = &jcode;  // `code` is referred in instruction macro.
                    {
                      Inst *inst = calloc_or_die(sizeof(*inst));
                      inst->op = J;
                      inst->opr[0].type = DIRECT;
                      inst->opr[0].direct.expr = destination;
                      memset(code, 0, sizeof(*code));
                      W_JAL(ZERO, 0);
                      code->flag = INST_LONG_OFFSET;  // Start with long offset.
                    }
                    IR *jmp = new_ir_code(code);
                    jmp->address = address;  // Temporary address.
                    vec_insert(irs, i + 1, jmp);

                    skip->fixnum = ir->code.len + jcode.len;  // Skip `J` instruction.
                    size_upgraded = true;  // Force retry.
                  }
                }
              }
            }
            break;
          case CALL:
            if (inst->opr[0].type == DIRECT) {
              Value value = calc_expr(label_table, inst->opr[0].direct.expr);
              if (value.label != NULL) {
                // Put rela even if the label is defined in the same object file.
                UnresolvedInfo *info;
                info = calloc_or_die(sizeof(*info));
                info->kind = UNRES_CALL;
                info->label = value.label;
                info->src_section = section;
                info->offset = address - start_address;
                info->add = value.offset;
                vec_push(unresolved, info);

                info = calloc_or_die(sizeof(*info));
                info->kind = UNRES_RISCV_RELAX;
                info->label = value.label;
                info->src_section = section;
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
          Value value = calc_expr(label_table, ir->expr.expr);
          assert(value.label != NULL);
          UnresolvedInfo *info = malloc_or_die(sizeof(*info));
          info->kind = UNRES_ABS64;  // TODO:
          info->label = value.label;
          info->src_section = section;
          info->offset = address - start_address;
          info->add = value.offset;
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
          int64_t zero = 0;
          int size = 1 << (ir->kind - IR_EXPR_BYTE);
          sec_add_data(section, &zero, size);  // TODO: Target endian
        }
        break;
      }
    }
  }
}
