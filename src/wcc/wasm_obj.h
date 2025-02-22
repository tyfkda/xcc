#pragma once

#include <stdbool.h>
#include <stddef.h>  // size_t
#include <stdint.h>

typedef struct Name Name;
typedef struct Vector Vector;

#define LINKING_VERSION   (2)

enum LinkingType {
  LT_WASM_SEGMENT_INFO        = 5,
  LT_WASM_INIT_FUNCS          = 6,
  LT_WASM_COMDAT_INFO         = 7,
  LT_WASM_SYMBOL_TABLE        = 8,
};

enum SymInfoKind {
  SIK_SYMTAB_FUNCTION         = 0,
  SIK_SYMTAB_DATA             = 1,
  SIK_SYMTAB_GLOBAL           = 2,
  SIK_SYMTAB_SECTION          = 3,
  SIK_SYMTAB_EVENT            = 4,
  SIK_SYMTAB_TABLE            = 5,
};

// enum SegmentFlags
#define WASM_SEG_FLAG_STRINGS       (1 << 0)
#define WASM_SEG_FLAG_TLS           (1 << 1)
#define WASM_SEG_FLAG_RETAIN        (1 << 2)

// enum SymFlags
#define WASM_SYM_BINDING_WEAK       (1 << 0)
#define WASM_SYM_BINDING_LOCAL      (1 << 1)
#define WASM_SYM_VISIBILITY_HIDDEN  (1 << 2)
#define WASM_SYM_UNDEFINED          (1 << 4)
#define WASM_SYM_EXPORTED           (1 << 5)
#define WASM_SYM_EXPLICIT_NAME      (1 << 6)
#define WASM_SYM_NO_STRIP           (1 << 7)
#define WASM_SYM_TLS                (1 << 8)
#define WASM_SYM_ABSOLUTE           (1 << 9)

enum RelocType {
  R_WASM_FUNCTION_INDEX_LEB    = 0,
  R_WASM_TABLE_INDEX_SLEB      = 1,
  R_WASM_TABLE_INDEX_I32       = 2,
  R_WASM_MEMORY_ADDR_LEB       = 3,
  R_WASM_MEMORY_ADDR_SLEB      = 4,
  R_WASM_MEMORY_ADDR_I32       = 5,
  R_WASM_TYPE_INDEX_LEB        = 6,
  R_WASM_GLOBAL_INDEX_LEB      = 7,
  R_WASM_FUNCTION_OFFSET_I32   = 8,
  R_WASM_SECTION_OFFSET_I32    = 9,
  R_WASM_TAG_INDEX_LEB         = 10,  // R_WASM_EVENT_INDEX_LEB
  R_WASM_GLOBAL_INDEX_I32      = 13,
  R_WASM_MEMORY_ADDR_LEB64     = 14,
  R_WASM_MEMORY_ADDR_SLEB64    = 15,
  R_WASM_MEMORY_ADDR_I64       = 16,
  R_WASM_TABLE_INDEX_SLEB64    = 18,
  R_WASM_TABLE_INDEX_I64       = 19,
  R_WASM_TABLE_NUMBER_LEB      = 20,
};

typedef struct {
  uint32_t offset;  // from its function top (not section top).
  uint32_t index;
  int32_t addend;
  uint8_t type;     // enum RelocType
} RelocInfo;

typedef struct {
  unsigned char *start;
  size_t size;
  uint8_t id;
} WasmSection;

typedef struct {
  const Name *module_name;
  const Name *name;
  enum SymInfoKind kind;
  uint32_t flags;
  uint32_t local_index;  // Local index.
  union {
    struct {
      uint32_t type_index;
      uint32_t indirect_index;
    } func;
    struct {
      uint8_t wtype;
      uint8_t mut;
      union {
        int64_t ivalue;
#ifndef __NO_FLONUM
        float f32value;
        double f64value;
#endif
      };
    } global;
    struct {
      uint32_t offset;
      uint32_t size;
      uint32_t p2align;
      uint32_t address;
    } data;
    struct {
      uint32_t index;
    } tag;
  };

  uint32_t combined_index;
} SymbolInfo;

typedef struct WasmObj {
  unsigned char *buffer;
  size_t bufsiz;

  WasmSection *sections;
  int section_count;
  uint32_t version;

  struct {
    Vector *functions;  // <SymbolInfo*>
    Vector *globals;    // <SymbolInfo*>
    bool table_funcref;
  } import;
  struct {
    Vector *symtab;     // <SymbolInfo*>
    Vector *init_funcs;  // <SymbolInfo*>
  } linking;
  Vector *types;  // <int>
  struct {
    Vector *types;
    uint32_t count;
  } func;
  struct {
    struct DataSegmentForLink *segments;
    uint32_t count;
  } data;
  struct {
    struct TagData *data;
    uint32_t count;
  } tag;
  struct {
    struct ElemSegmentForLink *segments;
    uint32_t count;
  } elem;
  struct {
    RelocInfo *relocs;
    uint32_t section_index;
    uint32_t count;
  } reloc[2];  // 0=code, 1=data
} WasmObj;
