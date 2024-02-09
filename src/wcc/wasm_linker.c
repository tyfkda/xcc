#include "../../config.h"
#include "wasm_linker.h"

#include <assert.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "table.h"
#include "util.h"
#include "wasm.h"
#include "wasm_obj.h"
#include "wcc.h"

static int64_t read_leb128(unsigned char *p, unsigned char **next) {
  int64_t result = 0;
  int shift = 0;
  for (;;) {
    unsigned char byte = *p++;
    result |= (byte & 0x7f) << shift;
    if ((byte & 0x80) == 0) {
      if ((byte & 0x40) != 0)
        result -= (int64_t)1 << shift;
      break;
    }
    shift += 7;
  }
  *next = p;
  return result;
}

static uint64_t read_uleb128(unsigned char *p, unsigned char **next) {
  uint64_t result = 0;
  int shift = 0;
  for (;;) {
    unsigned char byte = *p++;
    result |= (byte & 0x7f) << shift;
    if ((byte & 0x80) == 0)
      break;
    shift += 7;
  }
  *next = p;
  return result;
}

static bool match_string(unsigned char *p, const char *str, unsigned char **next) {
  size_t len = read_uleb128(p, &p);
  if (strncmp(str, (char*)p, len) != 0 || str[len] != '\0')
    return false;
  *next = p + len;
  return true;
}

static const Name *read_wasm_string(unsigned char *p, unsigned char **next) {
  size_t len = read_uleb128(p, &p);
  *next = p + len;
  return alloc_name((char*)p, (char*)p + len, false);
}

static uint8_t read_type(unsigned char *p, unsigned char **next) {
  uint8_t type = *p++;
  switch (type) {
  case WT_I32:
  case WT_I64:
  case WT_F32:
  case WT_F64:
    break;
  case WT_FUNCREF:
    {
      uint32_t flag = read_uleb128(p, &p);
      uint32_t initial = read_uleb128(p, &p);
      UNUSED(flag);
      UNUSED(initial);
    }
    break;
  default:
    error("illegal type: 0x%02x", type);
    break;
  }
  *next = p;
  return type;
}

static void wasmobj_init(WasmObj *wasmobj) {
  memset(wasmobj, 0, sizeof(*wasmobj));
  wasmobj->import.functions = new_vector();
  wasmobj->import.globals = new_vector();
  wasmobj->linking.symtab = new_vector();
}

static WasmSection *find_section(WasmObj *wasmobj, uint8_t secid) {
  for (int i = 0; i < wasmobj->section_count; ++i) {
    WasmSection *sec = &wasmobj->sections[i];
    if (sec->id == secid)
      return sec;
  }
  return NULL;
}

// Returns type index vector.
static void read_type_section(WasmObj *wasmobj, unsigned char *p) {
  static const char kMalformed[] = "malformed type section";

  Vector *type_indices = new_vector();
  uint32_t num = read_uleb128(p, &p);
  for (uint32_t i = 0; i < num; ++i) {
    uint8_t wtype = *p++;
    if (wtype != WT_FUNC) {
      error(kMalformed);
    }

    unsigned char *start = p;
    uint32_t num_params = read_uleb128(p, &p);
    p += num_params;
    uint32_t num_results = read_uleb128(p, &p);
    p += num_results;

    int index = getsert_func_type(start, p - start, true);
    vec_push(type_indices, INT2VOIDP(index));
  }
  wasmobj->types = type_indices;
}

static void read_import_section(WasmObj *wasmobj, unsigned char *p) {
  uint32_t num = read_uleb128(p, &p);
  for (uint32_t i = 0; i < num; ++i) {
    const Name *module_name = read_wasm_string(p, &p);
    const Name *name = read_wasm_string(p, &p);
    uint8_t kind = *p++;
    switch (kind) {
    case IMPORT_FUNC:
      {
        uint32_t type_index = read_uleb128(p, &p);
        uint32_t index = wasmobj->import.functions->len;

        SymbolInfo *sym = calloc_or_die(sizeof(*sym));
        sym->module_name = module_name;
        sym->name = name;
        sym->kind = SIK_SYMTAB_FUNCTION;
        sym->flags = 0;
        sym->local_index = index;
        sym->func.type_index = type_index;
        vec_push(wasmobj->import.functions, sym);
      }
      break;
    case IMPORT_TABLE:
      {
        uint8_t wtype = read_type(p, &p);
        UNUSED(wtype);
      }
      break;
    case IMPORT_MEMORY:
      {
        // TODO:
        uint32_t index = read_uleb128(p, &p);
        uint32_t size = read_uleb128(p, &p);
        UNUSED(index);
        UNUSED(size);
      }
      break;
    case IMPORT_GLOBAL:
      {
        uint8_t wtype = read_type(p, &p);
        uint8_t mut = *p++;
        uint32_t index = wasmobj->import.globals->len;

        SymbolInfo *sym = calloc_or_die(sizeof(*sym));
        sym->module_name = module_name;
        sym->name = name;
        sym->kind = SIK_SYMTAB_GLOBAL;
        sym->flags = 0;
        sym->local_index = index;
        sym->global.wtype = wtype;
        sym->global.mut = mut;
        vec_push(wasmobj->import.globals, sym);
      }
      break;
    default:
      error("Illegal import kind: %d", kind);
      break;
    }
  }
}

typedef struct DataSegmentForLink {
  unsigned char *content;
  uint32_t start;
  uint32_t size;
} DataSegmentForLink;

static void read_data_section(WasmObj *wasmobj, unsigned char *p) {
  DataSegmentForLink *segments = NULL;
  uint32_t count = 0;
  count = read_uleb128(p, &p);
  segments = calloc_or_die(sizeof(*segments) * count);
  for (uint32_t i = 0; i < count; ++i) {
    read_uleb128(p, &p);  // flag
    DataSegmentForLink *segment = &segments[i];
    if (*p++ != OP_I32_CONST || (segment->start = read_uleb128(p, &p), *p++ != OP_END)) {
      error("malformed data section");
    }
    uint32_t size = read_uleb128(p, &p);
    segment->size = size;
    segment->content = p;

    p += segment->size;
  }

  wasmobj->data.segments = segments;
  wasmobj->data.count = count;
}

typedef struct ElemSegmentForLink {
  uint32_t *content;
  uint32_t start;
  uint32_t count;
} ElemSegmentForLink;

static void read_elem_section(WasmObj *wasmobj, unsigned char *p) {
  ElemSegmentForLink *segments = NULL;
  uint32_t count = 0;
  count = read_uleb128(p, &p);
  segments = calloc_or_die(sizeof(*segments) * count);
  for (uint32_t i = 0; i < count; ++i) {
    read_uleb128(p, &p);  // flag
    ElemSegmentForLink *segment = &segments[i];
    if (*p++ != OP_I32_CONST || (segment->start = read_uleb128(p, &p), *p++ != OP_END)) {
      error("malformed elem section");
    }
    uint32_t count = read_uleb128(p, &p);
    uint32_t *content = calloc_or_die(sizeof(*content) * count);
    for (uint32_t j = 0; j < count; ++j) {
      uint32_t index = read_uleb128(p, &p);
      content[j] = index;
    }

    segment->count = count;
    segment->content = content;
  }

  wasmobj->elem.segments = segments;
  wasmobj->elem.count = count;
}

static void read_linking(WasmObj *wasmobj, unsigned char *p, unsigned char *end) {
  uint32_t version = read_uleb128(p, &p);

  if (version != LINKING_VERSION) {
    error("unsupported linking version: %d\n", version);
  }

  Vector *import_symbols[] = {
    [SIK_SYMTAB_FUNCTION] = wasmobj->import.functions,
    [SIK_SYMTAB_GLOBAL] = wasmobj->import.globals,
  };

  while (p < end) {
    enum LinkingType linking_type = *p++;
    uint32_t payload_len = read_uleb128(p, &p);
    unsigned char *next = p + payload_len;

    switch (linking_type) {
    case LT_WASM_SYMBOL_TABLE:
      {
        uint32_t count = read_uleb128(p, &p);
        for (uint32_t i = 0; i < count; ++i) {
          enum SymInfoKind kind = *p++;
          uint32_t flags = read_uleb128(p, &p);

          switch (kind) {
          case SIK_SYMTAB_FUNCTION:
          case SIK_SYMTAB_GLOBAL:
            {
              SymbolInfo *sym = NULL;

              uint32_t index = read_uleb128(p, &p);
              Vector *import = import_symbols[kind];
              if (index < (uint32_t)import->len && !(flags & WASM_SYM_EXPLICIT_NAME)) {
                sym = import->data[index];
              } else {
                const Name *name = read_wasm_string(p, &p);
                sym = calloc_or_die(sizeof(*sym));
                sym->module_name = NULL;
                sym->name = name;
              }
              sym->kind = kind;
              sym->flags = flags;
              sym->local_index = index;
              vec_push(wasmobj->linking.symtab, sym);
            }
            break;
          case SIK_SYMTAB_DATA:
            {
              const Name *symname = read_wasm_string(p, &p);
              uint32_t index = 0;
              uint32_t offset = 0;
              uint32_t size = 0;
              if (!(flags & WASM_SYM_UNDEFINED)) {
                index = read_uleb128(p, &p);
                offset = read_uleb128(p, &p);
                size = read_uleb128(p, &p);
              }

              SymbolInfo *sym = calloc_or_die(sizeof(*sym));
              sym->module_name = NULL;
              sym->name = symname;
              sym->kind = kind;
              sym->flags = flags;
              sym->local_index = index;
              sym->data.offset = offset;
              sym->data.size = size;
              sym->data.address = 0;
              vec_push(wasmobj->linking.symtab, sym);
            }
            break;

          default:
            error("linking not handled: %d", linking_type);
            break;
          }
        }
      }
      break;
    case LT_WASM_SEGMENT_INFO:
      {
        if (wasmobj->linking.symtab == NULL) {
          error("segment info must be after symbol table");
        }

        Vector *symtab = wasmobj->linking.symtab;
        uint32_t count = read_uleb128(p, &p);
        uint32_t index = 0;
        for (uint32_t i = 0; i < count; ++i) {
          const Name *name = read_wasm_string(p, &p);
          uint32_t p2align = read_uleb128(p, &p);
          /*uint32_t flags =*/ read_uleb128(p, &p);  // bit0=WASM_SEGMENT_FLAG_STRINGS, bit1=WASM_SEGMENT_FLAG_TLS

          // Search correspoinding symbol.
          for (; index < (uint32_t)symtab->len; ++index) {
            SymbolInfo *sym = symtab->data[index];
            if (sym->kind == SIK_SYMTAB_DATA)
              break;
          }
          if (index >= (uint32_t)symtab->len) {
            error("no symbol for segment: %d, %.*s", i, NAMES(name));
          }
          SymbolInfo *sym = symtab->data[index];
          sym->data.p2align = p2align;
          ++index;
        }
      }
      break;

    default:
      error("linking not handled: %d", linking_type);
      break;
    }

    p = next;
  }
}

static void read_reloc(WasmObj *wasmobj, unsigned char *p, int is_data) {
  uint32_t section_index = read_uleb128(p, &p);
  uint32_t count = read_uleb128(p, &p);

  if (section_index >= (uint32_t)wasmobj->section_count ||
      wasmobj->sections[section_index].id != (is_data ? SEC_DATA : SEC_CODE)) {
    error("invalid section for relocation: section index=%d", section_index);
  }

  RelocInfo *relocs = NULL;
  if (count > 0) {
    relocs = calloc_or_die(sizeof(*relocs) * count);
    for (uint32_t i = 0; i < count; ++i) {
      uint8_t type = *p++;
      uint32_t offset = read_uleb128(p, &p);
      uint32_t index = read_uleb128(p, &p);

      int32_t addend = 0;
      switch (type) {
      case R_WASM_MEMORY_ADDR_LEB:
      case R_WASM_MEMORY_ADDR_SLEB:
      case R_WASM_MEMORY_ADDR_I32:
      case R_WASM_MEMORY_ADDR_LEB64:
      case R_WASM_MEMORY_ADDR_SLEB64:
      case R_WASM_MEMORY_ADDR_I64:
      case R_WASM_FUNCTION_OFFSET_I32:
      case R_WASM_SECTION_OFFSET_I32:
        addend = read_leb128(p, &p);
        break;
      default: break;
      }

      RelocInfo *p = &relocs[i];
      p->type = type;
      p->offset = offset;
      p->index = index;
      p->addend = addend;
    }
  }

  wasmobj->reloc[is_data].relocs = relocs;
  wasmobj->reloc[is_data].section_index = section_index;
  wasmobj->reloc[is_data].count = count;
}

static WasmObj *read_wasm(FILE *fp, const char *filename, size_t filesize) {
  static const char MAGIC[] = WASM_BINARY_MAGIC;

  WasmHeader header;
  if (fread(&header, sizeof(header), 1, fp) != 1 ||
      memcmp(header.magic, MAGIC, sizeof(header.magic)) != 0 ||
      header.binary_version != WASM_BINARY_VERSION) {
    return false;
  }

  size_t bufsize = filesize - sizeof(header);
  void *buffer = malloc_or_die(bufsize);
  size_t readsize = fread(buffer, 1, bufsize, fp);
  if (readsize != bufsize) {
    fprintf(stderr, "load failed: %s\n", filename);
    return false;
  }

  WasmObj *wasmobj = calloc_or_die(sizeof(*wasmobj));
  wasmobj_init(wasmobj);
  wasmobj->version = header.binary_version;
  wasmobj->buffer = buffer;
  wasmobj->bufsiz = bufsize;

  // Enumerate sections.
  WasmSection *sections = NULL;
  int linking_section_index = -1;
  wasmobj->section_count = 0;
  {
    unsigned char *p = wasmobj->buffer;
    unsigned char *end = p + wasmobj->bufsiz;
    while (p < end) {
      uint8_t id = *p++;
      if (id > SEC_TAG) {
        error("invalid section id: %d\n", id);
      }

      size_t size = read_uleb128(p, &p);
      int count = ++wasmobj->section_count;
      wasmobj->sections = sections = realloc_or_die(sections, sizeof(WasmSection) * count);
      WasmSection *sec = &sections[count - 1];
      sec->start = p;
      sec->size = size;
      sec->id = id;

      switch (id) {
      case SEC_TYPE:
        read_type_section(wasmobj, p);
        break;
      case SEC_IMPORT:
        read_import_section(wasmobj, p);
        break;
      case SEC_DATA:
        read_data_section(wasmobj, p);
        break;
      case SEC_ELEM:
        read_elem_section(wasmobj, p);
        break;
      case SEC_CUSTOM:
        {
          unsigned char *q;
          if (match_string(p, "linking", &q)) {
            linking_section_index = count;
            read_linking(wasmobj, q, sec->start + size);
          } else if (match_string(p, "reloc.CODE", &q)) {
            read_reloc(wasmobj, q, 0);
          } else if (match_string(p, "reloc.DATA", &q)) {
            read_reloc(wasmobj, q, 1);
          }
        }
        break;
      default: break;
      }

      p += size;
    }
  }

  if (linking_section_index < 0) {
    fprintf(stderr, "no linking section: %s\n", filename);
    return NULL;
  }

  return wasmobj;
}

//

struct File {
  const char *filename;
  union {
    WasmObj *wasmobj;
  };
};

static int resolve_symbols_wasmobj(WasmLinker *linker, WasmObj *wasmobj) {
  int err_count = 0;
  Vector *symtab = wasmobj->linking.symtab;
  for (int i = 0; i < symtab->len; ++i) {
    SymbolInfo *sym = symtab->data[i];
    if (sym->flags & WASM_SYM_UNDEFINED) {
      SymbolInfo *pre;
      if (!table_try_get(&linker->defined, sym->name, (void**)&pre) || pre == NULL)
        table_put(&linker->unresolved, sym->name, (void*)sym);
    } else if (!(sym->flags & (WASM_SYM_BINDING_LOCAL | WASM_SYM_VISIBILITY_HIDDEN))) {
      SymbolInfo *defsym = table_get(&linker->defined, sym->name);
      if (defsym != NULL && !(defsym->flags & WASM_SYM_BINDING_WEAK) &&
          !(sym->flags & WASM_SYM_BINDING_WEAK)) {
        fprintf(stderr, "duplicate symbol: %.*s\n", NAMES(sym->name));
        ++err_count;
      } else {
        table_put(&linker->defined, sym->name, (void*)sym);
        table_delete(&linker->unresolved, sym->name);
      }
    }
  }
  return err_count;
}

static bool resolve_symbols(WasmLinker *linker) {
  int err_count = 0;

  // Traverse all wasmobj files and enumerate defined and unresolved symbols.
  for (int i = 0; i < linker->files->len; ++i) {
    File *file = linker->files->data[i];
    err_count += resolve_symbols_wasmobj(linker, file->wasmobj);
  }

  // Enumerate unresolved: import
  const Name *wasi_module_name = alloc_name(WASI_MODULE_NAME, NULL, false);
  uint32_t unresolved_func_count = 0;
  const Name *name;
  SymbolInfo *sym;
  for (int it = 0; (it = table_iterate(&linker->unresolved, it, &name, (void**)&sym)) != -1; ) {
    if (sym == NULL) {
      fprintf(stderr, "Unresolved: %.*s\n", NAMES(name));
      ++err_count;
      continue;
    }
    switch (sym->kind) {
    default: assert(false); // Fallthrough to suppress warning.
    case SIK_SYMTAB_FUNCTION:
      if (sym->module_name != NULL && equal_name(sym->module_name, wasi_module_name)) {
        sym->combined_index = unresolved_func_count++;
        break;
      }

      if (sym->module_name != NULL)
        fprintf(stderr, "Unresolved: %.*s.%.*s\n", NAMES(sym->module_name), NAMES(name));
      else
        fprintf(stderr, "Unresolved: %.*s\n", NAMES(name));
      ++err_count;
      break;

    case SIK_SYMTAB_DATA:
    case SIK_SYMTAB_GLOBAL:
      if (equal_name(name, linker->sp_name) || equal_name(name, linker->curbrk_name)) {
        // TODO: Check type, etc.
        table_delete(&linker->unresolved, name);
        table_put(&linker->defined, name, (void*)sym);
        break;
      }

      fprintf(stderr, "Unresolved: %.*s\n", NAMES(name));
      ++err_count;
      break;
    }
  }
  linker->unresolved_func_count = unresolved_func_count;

  return err_count == 0;
}

static void renumber_symbols(WasmLinker *linker) {
  // Enumerate defined functions and data.
  uint32_t defined_count[] = {
    [SIK_SYMTAB_FUNCTION] = linker->unresolved_func_count,
    [SIK_SYMTAB_DATA] = 0,
  };
  for (int i = 0; i < linker->files->len; ++i) {
    WasmObj *wasmobj = ((File*)linker->files->data[i])->wasmobj;
    Vector *symtab = wasmobj->linking.symtab;
    uint32_t import_count[3];
    import_count[SIK_SYMTAB_FUNCTION] = wasmobj->import.functions->len;
    import_count[SIK_SYMTAB_DATA] = 0;
    for (int j = 0; j < symtab->len; ++j) {
      SymbolInfo *sym = symtab->data[j];
      if (sym->flags & WASM_SYM_UNDEFINED)
        continue;
      switch (sym->kind) {
      default: assert(false); // Fallthrough to suppress warning.
      case SIK_SYMTAB_FUNCTION:
      case SIK_SYMTAB_DATA:
        sym->combined_index = sym->local_index + defined_count[sym->kind] - import_count[sym->kind];
        break;
      case SIK_SYMTAB_GLOBAL:
        // Handled differently (just below).
        break;
      }
    }

    // Increment count_table according to defined counts.
    static const int kSecTable[] = {SEC_FUNC, SEC_DATA};
    for (size_t i = 0; i < sizeof(kSecTable) / sizeof(kSecTable[0]); ++i) {
      int secidx = kSecTable[i];
      WasmSection *sec = find_section(wasmobj, secidx);
      if (sec != NULL) {
        unsigned char *p = sec->start;
        uint32_t num = read_uleb128(p, &p);
        defined_count[i] += num;
      }
    }
  }

  // Globals.
  {
    const Name *name;
    SymbolInfo *sym;
    uint32_t index = 0;
    for (int it = 0; (it = table_iterate(&linker->defined, it, &name, (void**)&sym)) != -1; ) {
      if (sym->kind != SIK_SYMTAB_GLOBAL)
        continue;
      sym->combined_index = index++;
    }
  }
}

static void renumber_func_types(WasmLinker *linker) {
  for (int i = 0; i < linker->files->len; ++i) {
    WasmObj *wasmobj = ((File*)linker->files->data[i])->wasmobj;
    Vector *type_indices = wasmobj->types;
    Vector *symtab = wasmobj->linking.symtab;
    for (int j = 0; j < symtab->len; ++j) {
      SymbolInfo *sym = symtab->data[j];
      if (sym->kind != SIK_SYMTAB_FUNCTION)
        continue;
      if (sym->func.type_index >= (uint32_t)type_indices->len)
        error("illegal type index for %.*s: %d\n", NAMES(sym->name), sym->func.type_index);
      sym->func.type_index = VOIDP2INT(type_indices->data[sym->func.type_index]);
    }
  }
}

static void remap_data_address(WasmLinker *linker) {
  uint32_t address = 0;
  for (int i = 0; i < linker->files->len; ++i) {
    WasmObj *wasmobj = ((File*)linker->files->data[i])->wasmobj;
    address = ALIGN(address, 16);  // TODO:
    uint32_t max = address;
    for (uint32_t j = 0; j < wasmobj->data.count; ++j) {
      DataSegmentForLink *d = &wasmobj->data.segments[j];
      d->start += address;
      uint32_t end = d->start + d->size;
      if (end > max)
        max = end;
    }
    address = max;

    Vector *symtab = wasmobj->linking.symtab;
    for (int k = 0; k < symtab->len; ++k) {
      SymbolInfo *sym = symtab->data[k];
      if (sym->kind != SIK_SYMTAB_DATA || sym->flags & WASM_SYM_UNDEFINED)
        continue;
      if (sym->local_index >= wasmobj->data.count)
        error("illegal index for data segment %.*s: %d\n", NAMES(sym->name), sym->local_index);
      DataSegmentForLink *d = &wasmobj->data.segments[sym->local_index];
      uint32_t addr = d->start + sym->data.offset;
      sym->data.address = addr;
    }
  }
  linker->data_end_address = address;
}

static void renumber_indirect_functions(WasmLinker *linker) {
  Table *indirect_functions = &linker->indirect_functions;
  for (int i = 0; i < linker->files->len; ++i) {
    WasmObj *wasmobj = ((File*)linker->files->data[i])->wasmobj;

    uint32_t segnum = wasmobj->elem.count;
    ElemSegmentForLink *segments = wasmobj->elem.segments;
    for (uint32_t i = 0; i < segnum; ++i) {
      ElemSegmentForLink *segment = &segments[i];
      uint32_t count = segment->count;
      for (uint32_t j = 0; j < count; ++j) {
        uint32_t index = segment->content[j];
        SymbolInfo *sym = NULL;
        {
          Vector *symtab = wasmobj->linking.symtab;
          for (int k = 0; k < symtab->len; ++k) {
            SymbolInfo *p = symtab->data[k];
            if (p->kind == SIK_SYMTAB_FUNCTION && p->local_index == index) {
              sym = p;
              break;
            }
          }
        }
        if (sym == NULL) {
          error("indirect function not found: %d", index);
        }
        if (!(sym->flags & WASM_SYM_BINDING_LOCAL)) {
          const Name *name = sym->name;
          if (!table_try_get(&linker->defined, name, (void**)&sym)) {
            if (!table_try_get(&linker->unresolved, name, (void**)&sym)) {
              error("indirect function not found: %.*s", NAMES(name));
            }
          }
        }
        table_put(indirect_functions, sym->name, sym);
      }
    }
  }

  const Name *name;
  SymbolInfo *sym;
  uint32_t index = INDIRECT_FUNCTION_TABLE_START_INDEX;
  for (int it = 0; (it = table_iterate(indirect_functions, it, &name, (void**)&sym)) != -1; ) {
    sym->func.indirect_index = index++;
  }
}

static void put_varint32(unsigned char *p, int32_t x, RelocInfo *reloc) {
  if (!(p[0] & 0x80) || !(p[1] & 0x80) || !(p[2] & 0x80) || !(p[3] & 0x80) || (p[4] & 0x80))
    error("Illegal reloc varint32: at 0x%x", reloc->offset);

  for (uint32_t count = 0; ; ++count) {
    assert(count <= 5);
    if (!(*p & 0x80)) {
      assert(x <= 0x7f);
      *p = x & 0x7f;
      break;
    }
    *p++ = (x & 0x7f) | 0x80;
    x >>= 7;
  }
}

static void put_varuint32(unsigned char *p, uint32_t x, RelocInfo *reloc) {
  if (!(p[0] & 0x80) || !(p[1] & 0x80) || !(p[2] & 0x80) || !(p[3] & 0x80) || (p[4] & 0x80))
    error("Illegal reloc varuint32: at 0x%x", reloc->offset);

  for (uint32_t count = 0; ; ++count) {
    assert(count <= 5);
    if (!(*p & 0x80)) {
      assert(x <= 0x7f);
      *p = x & 0x7f;
      break;
    }
    *p++ = (x & 0x7f) | 0x80;
    x >>= 7;
  }
}

static void put_i32(unsigned char *p, int32_t x) {
  for (int i = 0; i < 4; ++i) {
    *p++ = x;
    x >>= 8;
  }
}

static void apply_relocation(WasmLinker *linker) {
  for (int i = 0; i < linker->files->len; ++i) {
    WasmObj *wasmobj = ((File*)linker->files->data[i])->wasmobj;
    Vector *symtab = wasmobj->linking.symtab;
    for (int j = 0; j < 2; ++j) {
      uint32_t count = wasmobj->reloc[j].count;
      if (count == 0)
        continue;

      uint32_t section_index = wasmobj->reloc[j].section_index;
      WasmSection *sec = &wasmobj->sections[section_index];
      RelocInfo *relocs = wasmobj->reloc[j].relocs;
      for (uint32_t k = 0; k < count; ++k) {
        RelocInfo *p = &relocs[k];
        unsigned char *q = sec->start + p->offset;
        switch (p->type) {
        case R_WASM_TYPE_INDEX_LEB:
          {
            assert(wasmobj->types != NULL);
            if (p->index >= (uint32_t)wasmobj->types->len)
              error("illegal type index: %d", p->index);
            uint32_t index = VOIDP2INT(wasmobj->types->data[p->index]);
            put_varuint32(q, index, p);
          }
          continue;
        default: break;
        }

        // Symbol resolution.
        if (p->index >= (uint32_t)symtab->len)
          error("illegal index for reloc: %d", p->index);
        SymbolInfo *sym = symtab->data[p->index];
        SymbolInfo *target = sym;
        if (!table_try_get(&linker->defined, sym->name, (void**)&target))
          table_try_get(&linker->unresolved, sym->name, (void**)&target);

        switch (p->type) {
        case R_WASM_FUNCTION_INDEX_LEB:
        case R_WASM_GLOBAL_INDEX_LEB:
          put_varuint32(q, target->combined_index, p);
          break;
        case R_WASM_MEMORY_ADDR_LEB:
          put_varuint32(q, target->data.address, p);
          break;
        case R_WASM_MEMORY_ADDR_I32:
          put_i32(q, target->data.address + p->addend);
          break;
        case R_WASM_TABLE_INDEX_SLEB:
          put_varint32(q, target->func.indirect_index, p);
          break;
        case R_WASM_TABLE_INDEX_I32:
          put_i32(q, target->func.indirect_index);
          break;
        default:
          error("Relocation not handled: type=%d", p->type);
          break;
        }
      }
    }
  }
}

//

void linker_init(WasmLinker *linker) {
  memset(linker, 0, sizeof(*linker));
  linker->files = new_vector();

  table_init(&linker->defined);
  table_init(&linker->unresolved);
  table_init(&linker->indirect_functions);

  linker->sp_name = alloc_name(SP_NAME, NULL, false);
  linker->curbrk_name = alloc_name(BREAK_ADDRESS_NAME, NULL, false);
}

bool read_wasm_obj(WasmLinker *linker, const char *filename) {
  FILE *fp;
  if (!is_file(filename) || (fp = fopen(filename, "r")) == NULL) {
    fprintf(stderr, "cannot open: %s\n", filename);
    return false;
  }

  fseek(fp, 0, SEEK_END);
  long filesize = ftell(fp);
  fseek(fp, 0, SEEK_SET);

  WasmObj *wasmobj = read_wasm(fp, filename, filesize);
  fclose(fp);
  if (wasmobj == NULL)
    return false;

  File *file = calloc_or_die(sizeof(*file));
  file->filename = filename;
  file->wasmobj = wasmobj;
  vec_push(linker->files, file);
  return true;
}

bool link_wasm_objs(WasmLinker *linker, Vector *exports) {
  for (int i = 0; i < exports->len; ++i) {
    const Name *name = exports->data[i];
    table_put(&linker->unresolved, name, NULL);
  }

  if (!resolve_symbols(linker))
    return false;

  remap_data_address(linker);
  renumber_symbols(linker);
  renumber_func_types(linker);
  renumber_indirect_functions(linker);
  apply_relocation(linker);

  return true;
}
