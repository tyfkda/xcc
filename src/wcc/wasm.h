#pragma once

#include <stdint.h>
#include <stdio.h>

#define WASM_BINARY_MAGIC  {'\0', 'a', 's', 'm'}
#define WASM_BINARY_VERSION  (1)

typedef struct {
  char magic[4];
  uint32_t binary_version;
} WasmHeader;

#define SEC_CUSTOM      (0)
#define SEC_TYPE        (1)
#define SEC_IMPORT      (2)
#define SEC_FUNC        (3)
#define SEC_TABLE       (4)
#define SEC_MEMORY      (5)
#define SEC_GLOBAL      (6)
#define SEC_EXPORT      (7)
#define SEC_START       (8)
#define SEC_ELEM        (9)
#define SEC_CODE        (10)
#define SEC_DATA        (11)
#define SEC_DATA_COUNT  (12)
#define SEC_TAG         (13)

// Import and export kind
#define IMPORT_FUNC    (0)
#define IMPORT_TABLE   (1)
#define IMPORT_MEMORY  (2)
#define IMPORT_GLOBAL  (3)

// Wasm opcode
#define OP_UNREACHABLE    (0x00)
#define OP_NOP            (0x01)
#define OP_BLOCK          (0x02)
#define OP_LOOP           (0x03)
#define OP_IF             (0x04)
#define OP_ELSE           (0x05)
#define OP_TRY            (0x06)
#define OP_CATCH          (0x07)
#define OP_THROW          (0x08)
#define OP_RETHROW        (0x09)
#define OP_END            (0x0b)
#define OP_BR             (0x0c)
#define OP_BR_IF          (0x0d)
#define OP_BR_TABLE       (0x0e)
#define OP_RETURN         (0x0f)
#define OP_CALL           (0x10)
#define OP_CALL_INDIRECT  (0x11)
#define OP_CATCH_ALL      (0x19)
#define OP_DROP           (0x1a)
#define OP_SELECT         (0x1b)
#define OP_LOCAL_GET      (0x20)
#define OP_LOCAL_SET      (0x21)
#define OP_LOCAL_TEE      (0x22)
#define OP_GLOBAL_GET     (0x23)
#define OP_GLOBAL_SET     (0x24)
#define OP_I32_LOAD       (0x28)
#define OP_I64_LOAD       (0x29)
#define OP_F32_LOAD       (0x2a)
#define OP_F64_LOAD       (0x2b)
#define OP_I32_LOAD8_S    (0x2c)
#define OP_I32_LOAD8_U    (0x2d)
#define OP_I32_LOAD16_S   (0x2e)
#define OP_I32_LOAD16_U   (0x2f)
#define OP_I32_STORE      (0x36)
#define OP_I64_STORE      (0x37)
#define OP_F32_STORE      (0x38)
#define OP_F64_STORE      (0x39)
#define OP_I32_STORE8     (0x3a)
#define OP_I32_STORE16    (0x3b)
#define OP_MEMORY_SIZE    (0x3f)
#define OP_MEMORY_GROW    (0x40)
#define OP_I32_CONST      (0x41)
#define OP_I64_CONST      (0x42)
#define OP_F32_CONST      (0x43)
#define OP_F64_CONST      (0x44)
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
#define OP_I64_EQZ        (0x50)
#define OP_I64_EQ         (0x51)
#define OP_I64_NE         (0x52)
#define OP_I64_LT_S       (0x53)
#define OP_I64_LT_U       (0x54)
#define OP_I64_GT_S       (0x55)
#define OP_I64_GT_U       (0x56)
#define OP_I64_LE_S       (0x57)
#define OP_I64_LE_U       (0x58)
#define OP_I64_GE_S       (0x59)
#define OP_I64_GE_U       (0x5a)
#define OP_F32_EQ         (0x5b)
#define OP_F32_NE         (0x5c)
#define OP_F32_LT         (0x5d)
#define OP_F32_GT         (0x5e)
#define OP_F32_LE         (0x5f)
#define OP_F32_GE         (0x60)
#define OP_F64_EQ         (0x61)
#define OP_F64_NE         (0x62)
#define OP_F64_LT         (0x63)
#define OP_F64_GT         (0x64)
#define OP_F64_LE         (0x65)
#define OP_F64_GE         (0x66)
#define OP_I32_ADD        (0x6a)
#define OP_I32_SUB        (0x6b)
#define OP_I32_MUL        (0x6c)
#define OP_I32_DIV_S      (0x6d)
#define OP_I32_DIV_U      (0x6e)
#define OP_I32_REM_S      (0x6f)
#define OP_I32_REM_U      (0x70)
#define OP_I32_AND        (0x71)
#define OP_I32_OR         (0x72)
#define OP_I32_XOR        (0x73)
#define OP_I32_SHL        (0x74)
#define OP_I32_SHR_S      (0x75)
#define OP_I32_SHR_U      (0x76)
#define OP_I64_ADD        (0x7c)
#define OP_I64_SUB        (0x7d)
#define OP_I64_MUL        (0x7e)
#define OP_I64_DIV_S      (0x7f)
#define OP_I64_DIV_U      (0x80)
#define OP_I64_REM_S      (0x81)
#define OP_I64_REM_U      (0x82)
#define OP_I64_AND        (0x83)
#define OP_I64_OR         (0x84)
#define OP_I64_XOR        (0x85)
#define OP_I64_SHL        (0x86)
#define OP_I64_SHR_S      (0x87)
#define OP_I64_SHR_U      (0x88)
#define OP_F32_ADD        (0x92)
#define OP_F32_SUB        (0x93)
#define OP_F32_MUL        (0x94)
#define OP_F32_DIV        (0x95)
#define OP_F64_CEIL       (0x9b)
#define OP_F64_FLOOR      (0x9c)
#define OP_F64_SQRT       (0x9f)
#define OP_F64_ADD        (0xa0)
#define OP_F64_SUB        (0xa1)
#define OP_F64_MUL        (0xa2)
#define OP_F64_DIV        (0xa3)
#define OP_F64_COPYSIGN   (0xa6)
#define OP_I32_WRAP_I64       (0xa7)  // i32 <- i64
#define OP_I32_TRUNC_F32_S    (0xa8)  // i32 <- f32
#define OP_I32_TRUNC_F32_U    (0xa9)  // i32 <- f32
#define OP_I32_TRUNC_F64_S    (0xaa)  // i32 <- f64
#define OP_I32_TRUNC_F64_U    (0xab)  // i32 <- f64
#define OP_I64_EXTEND_I32_S   (0xac)  // i64 <- i32
#define OP_I64_EXTEND_I32_U   (0xad)  // i64 <- i32
#define OP_I64_TRUNC_F32_S    (0xae)  // i64 <- f32
#define OP_I64_TRUNC_F32_U    (0xaf)  // i64 <- f32
#define OP_I64_TRUNC_F64_S    (0xb0)  // i64 <- f64
#define OP_I64_TRUNC_F64_U    (0xb1)  // i64 <- f64
#define OP_F32_CONVERT_I32_S  (0xb2)  // f32 <- i32
#define OP_F32_CONVERT_I32_U  (0xb3)  // f32 <- i32
#define OP_F32_CONVERT_I64_S  (0xb4)  // f32 <- i64
#define OP_F32_CONVERT_I64_U  (0xb5)  // f32 <- i64
#define OP_F32_DEMOTE_F64     (0xb6)  // f32 <- f64
#define OP_F64_CONVERT_I32_S  (0xb7)  // f64 <- i32
#define OP_F64_CONVERT_I32_U  (0xb8)  // f64 <- i32
#define OP_F64_CONVERT_I64_S  (0xb9)  // f64 <- i64
#define OP_F64_CONVERT_I64_U  (0xba)  // f64 <- i64
#define OP_F64_PROMOTE_F32    (0xbb)  // f64 <- f32
#define OP_EXTENSION      (0xfc)

#define OPEX_MEMORY_COPY  (0x0a)
#define OPEX_MEMORY_FILL  (0x0b)

// Types
#define WT_VOID           (0x40)
#define WT_FUNC           (0x60)
#define WT_FUNCREF        (0x70)
#define WT_F64            (0x7c)
#define WT_F32            (0x7d)
#define WT_I64            (0x7e)
#define WT_I32            (0x7f)

#define MEMORY_PAGE_SIZE  65536
