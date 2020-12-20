#pragma once

#include <stdint.h>
#include <stdio.h>

typedef struct {
  char magic[4];
  uint32_t binary_version;
} WASM_HEADER;

#define SEC_TYPE      (1)
#define SEC_IMPORT    (2)
#define SEC_FUNC      (3)
#define SEC_GLOBAL    (6)
#define SEC_EXPORT    (7)
#define SEC_CODE      (10)

// Wasm opcode
#define OP_BLOCK          (0x02)
#define OP_LOOP           (0x03)
#define OP_IF             (0x04)
#define OP_ELSE           (0x05)
#define OP_END            (0x0b)
#define OP_BR             (0x0c)
#define OP_BR_IF          (0x0d)
#define OP_RETURN         (0x0f)
#define OP_CALL           (0x10)
#define OP_DROP           (0x1a)
#define OP_LOCAL_GET      (0x20)
#define OP_LOCAL_SET      (0x21)
#define OP_LOCAL_TEE      (0x22)
#define OP_GLOBAL_GET     (0x23)
#define OP_GLOBAL_SET     (0x24)
#define OP_I32_CONST      (0x41)
#define OP_I32_EQZ        (0x45)
#define OP_I32_EQ         (0x46)
#define OP_I32_NE         (0x47)
#define OP_I32_LT_S       (0x48)
#define OP_I32_LT_U       (0x49)
#define OP_I32_GT_S       (0x4a)
#define OP_I32_GT_U       (0x4b)
#define OP_I32_LE_S       (0x4c)
#define OP_I32_LE_U       (0x4d)
#define OP_I32_GE_S       (0x4e)
#define OP_I32_GE_U       (0x4f)
#define OP_I32_ADD        (0x6a)
#define OP_I32_SUB        (0x6b)
#define OP_I32_MUL        (0x6c)
#define OP_I32_DIV_S      (0x6d)
#define OP_I32_REM_S      (0x6f)

// Types
#define WT_VOID           (0x40)
#define WT_FUNC           (0x60)
#define WT_I32            (0x7f)

void emit_wasm_header(FILE *ofp);
