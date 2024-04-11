#include "../../../config.h"
#include "ir_asm.h"

#include <assert.h>
#include <stdlib.h>  // exit

#include "gen_section.h"
#include "inst.h"
#include "parse_asm.h"
#include "table.h"
#include "util.h"

#define BYTE_SIZE   (1)
#define SHORT_SIZE  (2)
#define LONG_SIZE   (4)
#define QUAD_SIZE   (8)

static LabelInfo *new_label(int section, uintptr_t address) {
  LabelInfo *info = malloc_or_die(sizeof(*info));
  info->section = section;
  info->flag = 0;
  info->address = address;
  info->kind = LK_NONE;
  return info;
}

LabelInfo *add_label_table(Table *label_table, const Name *label, int section, bool define, bool global) {
  LabelInfo *info = table_get(label_table, label);
  if (info != NULL) {
    if (define) {
      if ((info->flag & LF_DEFINED) != 0) {
        fprintf(stderr, "`%.*s' already defined\n", NAMES(label));
        return NULL;
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
  return info;
}

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

bool resolve_relative_address(Vector **section_irs, Table *label_table, Vector *unresolved) {
  assert(unresolved != NULL);
  Table unresolved_labels;
  table_init(&unresolved_labels);
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
          case CALL:
            if (inst->opr1.type == DIRECT) {
              Value value = calc_expr(label_table, inst->opr1.direct.expr);
              if (value.label != NULL) {
                // Put rela even if the label is defined in the same object file.
                UnresolvedInfo *info = malloc_or_die(sizeof(*info));
                info->kind = UNRES_RISCV_CALL;
                info->label = value.label;
                info->src_section = sec;
                info->offset = address - start_address;
                info->add = value.offset;
                vec_push(unresolved, info);

                info = malloc_or_die(sizeof(*info));
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
      default:
        break;
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
      fprintf(stderr, "Undefined reference: `%.*s'\n", NAMES(name));
    }
    exit(1);
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
