#include "ir_asm.h"

#include <assert.h>
#include <stdlib.h>  // malloc

#include "gen.h"
#include "inst.h"
#include "util.h"

#define WORD_SIZE  (8)

IR *new_ir_label(const char *label) {
  IR *ir = malloc(sizeof(*ir));
  ir->type = IR_LABEL;
  ir->u.label = label;
  return ir;
}

IR *new_ir_code(const Code *code) {
  IR *ir = malloc(sizeof(*ir));
  ir->type = IR_CODE;
  ir->u.code = *code;
  return ir;
}

IR *new_ir_data(const void *data, size_t size) {
  IR *ir = malloc(sizeof(*ir));
  ir->type = IR_DATA;
  ir->u.data.len = size;
  ir->u.data.buf = (unsigned char*)data;
  return ir;
}

IR *new_ir_bss(size_t size) {
  IR *ir = malloc(sizeof(*ir));
  ir->type = IR_BSS;
  ir->u.bss = size;
  return ir;
}

IR *new_ir_align(int align) {
  IR *ir = malloc(sizeof(*ir));
  ir->type = IR_ALIGN;
  ir->u.align = align;
  return ir;
}

IR *new_ir_abs_quad(const char *label) {
  IR *ir = malloc(sizeof(*ir));
  ir->type = IR_ABS_QUAD;
  ir->u.label = label;
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
  map_clear(label_map);
  uintptr_t address = start_address;
  for (int sec = 0; sec < SECTION_COUNT; ++sec) {
    address = align_next_section(sec, address);

    Vector *irs = section_irs[sec];
    for (int i = 0, len = irs->len; i < len; ++i) {
      IR *ir = irs->data[i];
      switch (ir->type) {
      case IR_LABEL:
        map_put(label_map, ir->u.label, (void*)address);
        break;
      case IR_CODE:
        address += ir->u.code.len;
        break;
      case IR_DATA:
        address += ir->u.data.len;
        break;
      case IR_BSS:
        address += ir->u.bss;
        break;
      case IR_ALIGN:
        address = ALIGN(address, ir->u.align);
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

void emit_irs(uintptr_t start_address, Vector **section_irs, Map *label_map) {
  Map *unresolved_labels = NULL;

  uintptr_t address = start_address;
  for (int sec = 0; sec < SECTION_COUNT; ++sec) {
    address = align_next_section(sec, address);

    Vector *irs = section_irs[sec];
    for (int i = 0, len = irs->len; i < len; ++i) {
      IR *ir = irs->data[i];
      switch (ir->type) {
      case IR_LABEL:
        break;
      case IR_CODE:
        {
          Inst *inst = ir->u.code.inst;
          switch (inst->op) {
          case LEA:
            if (inst->src.type == INDIRECT &&
                inst->src.u.indirect.reg.no == RIP &&
                inst->src.u.indirect.offset == 0 &&
                inst->src.u.indirect.label != NULL) {
              void *dst;
              if (map_try_get(label_map, inst->src.u.indirect.label, &dst)) {
                put_value(ir->u.code.buf + 3, (intptr_t)dst - ((intptr_t)address + ir->u.code.len), sizeof(int32_t));
              } else {
                put_unresolved(&unresolved_labels, inst->src.u.indirect.label);
              }
            }
            break;
          case JMP:
          case JO: case JNO: case JB:  case JAE:
          case JE: case JNE: case JBE: case JA:
          case JS: case JNS: case JP:  case JNP:
          case JL: case JGE: case JLE: case JG:
          case CALL:
            if (inst->src.type == LABEL) {
              void *dst;
              if (map_try_get(label_map, inst->src.u.label, &dst)) {
                int d = (inst->op == JMP || inst->op == CALL) ? 1 : 2;
                put_value(ir->u.code.buf + d, (intptr_t)dst - ((intptr_t)address + ir->u.code.len), sizeof(int32_t));
              } else {
                put_unresolved(&unresolved_labels, inst->src.u.label);
              }
            }
            break;
          default:
            break;
          }

          add_code(ir->u.code.buf, ir->u.code.len);
          address += ir->u.code.len;
        }
        break;
      case IR_DATA:
        add_section_data(sec, ir->u.data.buf, ir->u.data.len);
        address += ir->u.data.len;
        break;
      case IR_BSS:
        add_bss(ir->u.bss);
        address += ir->u.bss;
        break;
      case IR_ALIGN:
        align_section_size(sec, ir->u.align);
        address = ALIGN(address, ir->u.align);
        break;
      case IR_ABS_QUAD:
        {
          void *dst;
          if (map_try_get(label_map, ir->u.label, &dst)) {
            unsigned char buf[WORD_SIZE];
            put_value(buf, (uintptr_t)dst, WORD_SIZE);
            add_section_data(sec, buf, sizeof(buf));
          } else {
            put_unresolved(&unresolved_labels, ir->u.label);
          }
          address += WORD_SIZE;
        }
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
}
