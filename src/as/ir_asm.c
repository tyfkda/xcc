#include "ir_asm.h"

#include <assert.h>
#include <stdlib.h>  // malloc

#include "gen.h"
#include "inst.h"
#include "util.h"

#define WORD_SIZE  (8)

IR *new_ir_label(const char *label) {
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

IR *new_ir_abs_quad(const char *label) {
  IR *ir = malloc(sizeof(*ir));
  ir->kind = IR_ABS_QUAD;
  ir->label = label;
  return ir;
}

static uintptr_t align_next_section(enum SectionType sec, uintptr_t address) {
  static const int kAlignTable[] = {0, 4096, 16};
  int align = kAlignTable[sec];
  if (align > 1)
    address = ALIGN(address, align);
  return address;
}

void calc_label_address(uintptr_t start_address, Vector **section_irs, Map *label_map) {
  uintptr_t address = start_address;
  for (int sec = 0; sec < SECTION_COUNT; ++sec) {
    address = align_next_section(sec, address);

    Vector *irs = section_irs[sec];
    for (int i = 0, len = irs->len; i < len; ++i) {
      IR *ir = irs->data[i];
      ir->address = address;
      switch (ir->kind) {
      case IR_LABEL:
        map_put(label_map, ir->label, (void*)address);
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
        address = ALIGN(address, ir->align);
        break;
      case IR_ABS_QUAD:
        address += WORD_SIZE;
        break;
      default:  assert(false); break;
      }
    }
  }
}

static void put_value(unsigned char *p, intptr_t value, int size) {
  for (int i = 0; i < size; ++i) {
    *p++ = value;
    value >>= 8;
  }
}

static void put_unresolved(Map **pp, const char *label) {
  Map *map = *pp;
  if (map == NULL)
    *pp = map = new_map();

  void *dummy;
  if (map_try_get(map, label, &dummy))
    return;
  map_put(map, label, NULL);
}

bool resolve_relative_address(Vector **section_irs, Map *label_map) {
  Map *unresolved_labels = NULL;
  bool size_upgraded = false;
  for (int sec = 0; sec < SECTION_COUNT; ++sec) {
    Vector *irs = section_irs[sec];
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
                inst->src.indirect.offset == 0 &&
                inst->src.indirect.label != NULL) {
              void *dst;
              if (map_try_get(label_map, inst->src.indirect.label, &dst)) {
                intptr_t offset = (intptr_t)dst - ((intptr_t)address + ir->code.len);
                put_value(ir->code.buf + 3, offset, sizeof(int32_t));
              } else {
                put_unresolved(&unresolved_labels, inst->src.indirect.label);
              }
            }
            break;
          case JMP:
          case JO: case JNO: case JB:  case JAE:
          case JE: case JNE: case JBE: case JA:
          case JS: case JNS: case JP:  case JNP:
          case JL: case JGE: case JLE: case JG:
            if (inst->src.type == LABEL) {
              void *dst;
              if (map_try_get(label_map, inst->src.label, &dst)) {
                intptr_t offset = (intptr_t)dst - ((intptr_t)address + ir->code.len);
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
              } else {
                put_unresolved(&unresolved_labels, inst->src.label);
              }
            }
            break;
          case CALL:
           if (inst->src.type == LABEL) {
              void *dst;
              if (map_try_get(label_map, inst->src.label, &dst)) {
                intptr_t offset = (intptr_t)dst - ((intptr_t)address + ir->code.len);
                put_value(ir->code.buf + 1, offset, sizeof(int32_t));
              } else {
                put_unresolved(&unresolved_labels, inst->src.label);
              }
            }
            break;
          default:
            break;
          }
        }
        break;
      case IR_ABS_QUAD:
        {
          void *dst;
          if (!map_try_get(label_map, ir->label, &dst))
            put_unresolved(&unresolved_labels, ir->label);
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

  if (unresolved_labels != NULL) {
    for (int i = 0, len = unresolved_labels->keys->len; i < len; ++i) {
      const char *label = unresolved_labels->keys->data[i];
      fprintf(stderr, "Undefined reference: `%s'\n", label);
    }
    exit(1);
  }

  return !size_upgraded;
}

void emit_irs(Vector **section_irs, Map *label_map) {
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
      case IR_ABS_QUAD:
        {
          void *dst;
          bool result = map_try_get(label_map, ir->label, &dst);
          UNUSED(result);
          assert(result);
          assert(sizeof(dst) == WORD_SIZE);
          add_section_data(sec, &dst, sizeof(dst));  // TODO: Target endian
        }
        break;
      default:  assert(false); break;
      }
    }
  }
}
