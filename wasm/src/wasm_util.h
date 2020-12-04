#pragma once

// Wasm opcode
#define OP_END            (0x0b)
#define OP_DROP           (0x1a)
#define OP_LOCAL_GET      (0x20)
#define OP_LOCAL_SET      (0x21)
#define OP_LOCAL_TEE      (0x22)
#define OP_I32_CONST      (0x41)
#define OP_I32_ADD        (0x6a)
#define OP_I32_SUB        (0x6b)
#define OP_I32_MUL        (0x6c)
#define OP_I32_DIV_S      (0x6d)
#define OP_I32_REM_S      (0x6f)

// Types
#define WT_FUNC           (0x60)
#define WT_I32            (0x7f)
