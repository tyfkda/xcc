#include "../config.h"
#include "wasm_linker.h"

#include <ar.h>
#include <assert.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <strings.h>

#include "archive.h"
#include "table.h"
#include "util.h"
#include "wasm.h"
#include "wasm_obj.h"
#include "wcc.h"

#define CODE  (curcodeds)

#define ADD_LEB128(x)  data_leb128(CODE, -1, x)
#define ADD_ULEB128(x) data_uleb128(CODE, -1, x)
#define ADD_VARUINT32(x)  data_varuint32(CODE, -1, x)

static const char LINKER_GENERATED_FILENAME[] = "*linker-generated*";

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
  wasmobj->import.tables = NULL;
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
        Vector *tables = wasmobj->import.tables;
        if (tables == NULL)
          wasmobj->import.tables = tables = new_vector();
        uint32_t index = tables->len;
        uint8_t wtype = read_type(p, &p);

        SymbolInfo *sym = calloc_or_die(sizeof(*sym));
        sym->module_name = module_name;
        sym->name = name;
        sym->kind = SIK_SYMTAB_TABLE;
        sym->flags = 0;
        sym->local_index = index;
        sym->table.wtype = wtype;
        vec_push(tables, sym);
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

static void read_func_section(WasmObj *wasmobj, unsigned char *p) {
  uint32_t num = read_uleb128(p, &p);
  Vector *types = new_vector();
  for (uint32_t i = 0; i < num; ++i) {
    uint32_t index = read_uleb128(p, &p);
    vec_push(types, INT2VOIDP(index));
  }
  wasmobj->func.types = types;
  wasmobj->func.count = num;
}

typedef struct TagData {
  uint32_t attribute;
  uint32_t typeindex;
} TagData;

static void read_tag_section(WasmObj *wasmobj, unsigned char *p) {
  uint32_t num = read_uleb128(p, &p);
  TagData *data = malloc_or_die(sizeof(*data) * num);
  for (uint32_t i = 0; i < num; ++i) {
    TagData *d = &data[i];
    d->attribute = read_uleb128(p, &p);
    d->typeindex = read_uleb128(p, &p);
  }
  wasmobj->tag.data = data;
  wasmobj->tag.count = num;
}

typedef struct DataSegmentForLink {
  unsigned char *content;
  uint32_t start;
  uint32_t size;
} DataSegmentForLink;

static void read_data_section(WasmObj *wasmobj, unsigned char *p) {
  DataSegmentForLink *segments = NULL;
  uint32_t count = read_uleb128(p, &p);
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
    [SIK_SYMTAB_TABLE] = wasmobj->import.tables,
  };

  while (p < end) {
    enum LinkingType linking_type = *p++;
    uint32_t payload_len = read_uleb128(p, &p);
    unsigned char *next = p + payload_len;

    switch (linking_type) {
    case LT_WASM_SYMBOL_TABLE:
      {
        Vector *func_types = wasmobj->func.types;
        uint32_t count = read_uleb128(p, &p);
        for (uint32_t i = 0; i < count; ++i) {
          enum SymInfoKind kind = *p++;
          uint32_t flags = read_uleb128(p, &p);

          switch (kind) {
          case SIK_SYMTAB_FUNCTION:
          case SIK_SYMTAB_GLOBAL:
          case SIK_SYMTAB_TABLE:
            {
              SymbolInfo *sym = NULL;

              uint32_t index = read_uleb128(p, &p);
              Vector *import = import_symbols[kind];
              bool imported_symbol = import != NULL && index < (uint32_t)import->len;
              bool explicit_name = flags & WASM_SYM_EXPLICIT_NAME;
              sym = imported_symbol ? import->data[index] : calloc_or_die(sizeof(*sym));
              if (!imported_symbol || explicit_name) {
                const Name *name = read_wasm_string(p, &p);
                if (!imported_symbol)
                  sym->name = name;
              }
              sym->kind = kind;
              sym->flags = flags;
              sym->local_index = index;

              if (kind == SIK_SYMTAB_FUNCTION) {
                uint32_t func_type;
                if (index < (uint32_t)import->len) {
                  SymbolInfo *p = wasmobj->import.functions->data[index];
                  if (p->kind != SIK_SYMTAB_FUNCTION)
                    error("symbol is not function: %.*s", NAMES(sym->name));
                  func_type = p->func.type_index;
                } else {
                  uint32_t i = index - wasmobj->import.functions->len;
                  if (func_types == NULL || i >= (uint32_t)func_types->len)
                    error("illegal function type index: %.*s", NAMES(sym->name));
                  func_type = VOIDP2INT(func_types->data[i]);
                }
                sym->func.type_index = func_type;
              }

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
          case SIK_SYMTAB_EVENT:
            {
              uint32_t index = read_uleb128(p, &p);
              const Name *symname = read_wasm_string(p, &p);
              if (index >= wasmobj->tag.count)
                error("illegal tag index: %.*s", NAMES(symname));

              SymbolInfo *sym = calloc_or_die(sizeof(*sym));
              sym->module_name = NULL;
              sym->name = symname;
              sym->kind = kind;
              sym->flags = flags;
              sym->local_index = -1;  // Unused.
              sym->tag.index = index;
              vec_push(wasmobj->linking.symtab, sym);
            }
            break;

          default:
            error("symbol not handled: %d", kind);
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

    case LT_WASM_INIT_FUNCS:
      {
        Vector *init_funcs = new_vector();
        uint32_t count = read_uleb128(p, &p);
        for (uint32_t i = 0; i < count; ++i) {
          /*uint32_t priority =*/ read_uleb128(p, &p);  // TODO: Sort by priority.
          uint32_t index = read_uleb128(p, &p);
          assert(index < (uint32_t)wasmobj->linking.symtab->len);
          SymbolInfo *sym = wasmobj->linking.symtab->data[index];
          vec_push(init_funcs, sym);
        }
        wasmobj->linking.init_funcs = init_funcs;
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
      wasmobj->sections = sections;
      wasmobj->section_count = count;

      switch (id) {
      case SEC_TYPE:
        read_type_section(wasmobj, p);
        break;
      case SEC_IMPORT:
        read_import_section(wasmobj, p);
        break;
      case SEC_FUNC:
        read_func_section(wasmobj, p);
        break;
      case SEC_TAG:
        read_tag_section(wasmobj, p);
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
  enum {
    FK_WASMOBJ,
    FK_ARCHIVE,
  } kind;
  union {
    WasmObj *wasmobj;
    Archive *archive;
  };
};

static int resolve_symbols_wasmobj(WasmLinker *linker, WasmObj *wasmobj) {
  int err_count = 0;
  Vector *symtab = wasmobj->linking.symtab;
  for (int i = 0; i < symtab->len; ++i) {
    SymbolInfo *sym = symtab->data[i];
    if (sym->flags & WASM_SYM_UNDEFINED) {
      SymbolInfo *prev;
      if (!table_try_get(&linker->defined, sym->name, (void**)&prev) || prev == NULL) {
        if (!table_try_get(&linker->unresolved, sym->name, NULL))
          table_put(&linker->unresolved, sym->name, sym);
      } else if (sym->kind != prev->kind) {
        fprintf(stderr, "different symbol type: %.*s\n", NAMES(sym->name));
        ++err_count;
      }
    } else if (!(sym->flags & WASM_SYM_BINDING_LOCAL)) {
      SymbolInfo *prev, *unre;
      if ((prev = table_get(&linker->defined, sym->name)) != NULL &&
          !(prev->flags & WASM_SYM_BINDING_WEAK) && !(sym->flags & WASM_SYM_BINDING_WEAK)) {
        fprintf(stderr, "duplicate symbol: %.*s\n", NAMES(sym->name));
        ++err_count;
      } else if ((unre = table_get(&linker->unresolved, sym->name)) != NULL &&
                 unre->kind != sym->kind) {
        fprintf(stderr, "different symbol type: %.*s\n", NAMES(sym->name));
        ++err_count;
      } else {
        if (prev == NULL || !(sym->flags & WASM_SYM_BINDING_WEAK)) {
          assert(prev == NULL || prev->flags & WASM_SYM_BINDING_WEAK);
          table_put(&linker->defined, sym->name, sym);
        }
        table_delete(&linker->unresolved, sym->name);
      }
    }
  }
  return err_count;
}

static void *load_wasmobj(FILE *fp, const char *name, size_t size) {
  return read_wasm(fp, name, size);
}

static int resolve_symbols_archive(WasmLinker *linker, Archive *ar) {
  Table *unresolved = &linker->unresolved;
  Table *table = &ar->symbol_table;
  for (;;) {
    bool retry = false;
    const Name *name;
    for (int it = 0; (it = table_iterate(unresolved, it, &name, NULL)) != -1;) {
      ArSymbol *symbol;
      if (!table_try_get(table, name, (void**)&symbol))
        continue;
      table_delete(unresolved, name);

      WasmObj *wasmobj = load_archive_content(ar, symbol, load_wasmobj);
      if (wasmobj != NULL) {
        resolve_symbols_wasmobj(linker, wasmobj);
        retry = true;
        break;
      }
    }
    if (!retry)
      break;
  }
  return 0;
}

static void resolve_symbols_auto_fill(WasmLinker *linker) {
  const Name *call_ctors_name = alloc_name("__wasm_call_ctors", NULL, false);
  WasmObj *obj = NULL;
  if (table_get(&linker->unresolved, call_ctors_name) != NULL) {
    // Prepare linker generated wasmobj.
    obj = calloc_or_die(sizeof(*obj));
    wasmobj_init(obj);

    // Funcion type.
    obj->types = new_vector();
    {
      static const unsigned char functype[] = {0x00, 0x00};
      unsigned char *dup = malloc_or_die(sizeof(functype));
      memcpy(dup, functype, sizeof(functype));
      int index = getsert_func_type(dup, sizeof(functype), true);
      vec_push(obj->types, INT2VOIDP(index));
    }

    File *file = calloc_or_die(sizeof(*file));
    file->filename = LINKER_GENERATED_FILENAME;
    file->kind = FK_WASMOBJ;
    file->wasmobj = obj;
    vec_push(linker->files, file);

    // Register symbol `__wasm_call_ctors`.
    SymbolInfo *sym = calloc_or_die(sizeof(*sym));
    sym->kind = SIK_SYMTAB_FUNCTION;
    sym->module_name = NULL;
    sym->name = call_ctors_name;
    vec_push(obj->linking.symtab, sym);

    // Function
    int section_count = 1;
    WasmSection *sections = calloc_or_die(sizeof(*sections) * section_count);
    obj->section_count = section_count;
    obj->sections = sections;
    {
      WasmSection *p = &sections[0];
      p->id = SEC_CODE;
      p->start = NULL;
      p->size = 0;
    }
    obj->func.count = 1;
  }

  if (obj != NULL) {
    int err_count = resolve_symbols_wasmobj(linker, obj);
    UNUSED(err_count);
    assert(err_count == 0);
  }
}

static bool resolve_symbols(WasmLinker *linker) {
  int err_count = 0;

  // Traverse all wasmobj files and enumerate defined and unresolved symbols.
  for (int i = 0; i < linker->files->len; ++i) {
    File *file = linker->files->data[i];
    switch (file->kind) {
    case FK_WASMOBJ:
      err_count += resolve_symbols_wasmobj(linker, file->wasmobj);
      break;
    case FK_ARCHIVE:
      err_count += resolve_symbols_archive(linker, file->archive);
      break;
    }
  }

  resolve_symbols_auto_fill(linker);

  // Enumerate unresolved: import
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
      if (linker->options.allow_undefined || sym->flags & WASM_SYM_EXPLICIT_NAME) {
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
      if (equal_name(name, linker->sp_name) || equal_name(name, linker->heapbase_name)) {
        // TODO: Check type, etc.
        table_delete(&linker->unresolved, name);
        table_put(&linker->defined, name, (void*)sym);
        break;
      }
      // Fallthrough.
    case SIK_SYMTAB_EVENT:
      fprintf(stderr, "Unresolved: %.*s\n", NAMES(name));
      ++err_count;
      break;

    case SIK_SYMTAB_TABLE:
      if (equal_name(name, linker->indirect_function_table_name)) {
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

static void generate_init_funcs(WasmLinker *linker) {
  if (linker->files->len <= 0)
    return;

  // Assume the linker generated wasmobj is the last one.
  File *generated = linker->files->data[linker->files->len - 1];
  if (strcmp(generated->filename, LINKER_GENERATED_FILENAME) != 0)
    return;

  Vector *init_funcs = new_vector();
  for (int i = 0; i < linker->files->len; ++i) {
    File *file = linker->files->data[i];
    switch (file->kind) {
    case FK_WASMOBJ:
      {
        WasmObj *wasmobj = file->wasmobj;
        if (wasmobj->linking.init_funcs != NULL) {
          vec_concat(init_funcs, wasmobj->linking.init_funcs);
        }
      }
      break;
    case FK_ARCHIVE:
      FOREACH_FILE_ARCONTENT(file->archive, content, {
        WasmObj *wasmobj = content->obj;
        if (wasmobj->linking.init_funcs != NULL) {
          vec_concat(init_funcs, wasmobj->linking.init_funcs);
        }
      });
      break;
    }
  }

  // Generate
  assert(generated->kind == FK_WASMOBJ);
  WasmObj *wasmobj = generated->wasmobj;
  WasmSection *codesec = &wasmobj->sections[0];
  assert(codesec->id == SEC_CODE);
  DataStorage ds;
  data_init(&ds);
  data_uleb128(&ds, -1, 1);  // num functions
  {
    curcodeds = &ds;

    data_open_chunk(&ds);
    data_uleb128(&ds, -1, 0);  // Local variable count
    for (int i = 0; i < init_funcs->len; ++i) {
      SymbolInfo *syminfo = init_funcs->data[i];
      ADD_CODE(OP_CALL);
      ADD_ULEB128(syminfo->combined_index);
    }
    ADD_CODE(OP_END);
    data_close_chunk(&ds, -1);

    curcodeds = NULL;
  }
  codesec->start = ds.buf;
  codesec->size = ds.len;
}

static void renumber_symbols_wasmobj(WasmObj *wasmobj, uint32_t *defined_count) {
  Vector *symtab = wasmobj->linking.symtab;
  uint32_t import_count[6];
  import_count[SIK_SYMTAB_FUNCTION] = wasmobj->import.functions->len;
  import_count[SIK_SYMTAB_DATA] = 0;
  import_count[SIK_SYMTAB_TABLE] = wasmobj->import.tables != NULL ? wasmobj->import.tables->len : 0;
  for (int i = 0; i < symtab->len; ++i) {
    SymbolInfo *sym = symtab->data[i];
    if (sym->flags & WASM_SYM_UNDEFINED)
      continue;
    switch (sym->kind) {
    default: assert(false); // Fallthrough to suppress warning.
    case SIK_SYMTAB_FUNCTION:
    case SIK_SYMTAB_DATA:
    case SIK_SYMTAB_TABLE:
      sym->combined_index = sym->local_index + defined_count[sym->kind] - import_count[sym->kind];
      break;
    case SIK_SYMTAB_GLOBAL:
      // Handled differently (just below).
      break;
    case SIK_SYMTAB_EVENT:
      {
        if (sym->tag.index >= wasmobj->tag.count)
          error("illegal index for event: %.*s", NAMES(sym->name));

        const TagData *d = &wasmobj->tag.data[sym->tag.index];
        uint32_t typeindex = VOIDP2INT(wasmobj->types->data[d->typeindex]);
        TagInfo *ti = getsert_tag(sym->name, typeindex);
        sym->combined_index = ti->index;
      }
      break;
    }
  }

  // Increment count_table according to defined counts.
  defined_count[0] += wasmobj->func.count;
  defined_count[1] += wasmobj->data.count;
  // Assume table is not defined in wasmobj.
}

static void renumber_symbols(WasmLinker *linker) {
  // Enumerate defined functions and data.
  uint32_t defined_count[] = {
    [SIK_SYMTAB_FUNCTION] = linker->unresolved_func_count,
    [SIK_SYMTAB_DATA] = 0,
    [SIK_SYMTAB_TABLE] = 0,
  };
  for (int i = 0; i < linker->files->len; ++i) {
    File *file = linker->files->data[i];
    switch (file->kind) {
    case FK_WASMOBJ:
      renumber_symbols_wasmobj(file->wasmobj, defined_count);
      break;
    case FK_ARCHIVE:
      FOREACH_FILE_ARCONTENT(file->archive, content, {
        renumber_symbols_wasmobj(content->obj, defined_count);
      });
      break;
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

static void renumber_func_types_wasmobj(WasmObj *wasmobj) {
  Vector *type_indices = wasmobj->types;
  Vector *symtab = wasmobj->linking.symtab;
  for (int i = 0; i < symtab->len; ++i) {
    SymbolInfo *sym = symtab->data[i];
    if (sym->kind != SIK_SYMTAB_FUNCTION)
      continue;
    if (sym->func.type_index >= (uint32_t)type_indices->len)
      error("illegal type index for %.*s: %d\n", NAMES(sym->name), sym->func.type_index);
    sym->func.type_index = VOIDP2INT(type_indices->data[sym->func.type_index]);
  }
}

static void renumber_func_types(WasmLinker *linker) {
  for (int i = 0; i < linker->files->len; ++i) {
    File *file = linker->files->data[i];
    switch (file->kind) {
    case FK_WASMOBJ:
      renumber_func_types_wasmobj(file->wasmobj);
      break;
    case FK_ARCHIVE:
      FOREACH_FILE_ARCONTENT(file->archive, content, {
        renumber_func_types_wasmobj(content->obj);
      });
      break;
    }
  }
}

static uint32_t remap_data_address_wasmobj(WasmObj *wasmobj, uint32_t address) {
  // Detect max p2align.
  uint32_t p2align = 0;
  Vector *symtab = wasmobj->linking.symtab;
  for (int i = 0; i < symtab->len; ++i) {
    SymbolInfo *sym = symtab->data[i];
    if (sym->kind != SIK_SYMTAB_DATA || sym->flags & WASM_SYM_UNDEFINED)
      continue;
    if (sym->data.p2align > p2align)
      p2align = sym->data.p2align;
  }

  // Remap data segments.
  address = ALIGN(address, 1U << p2align);
  uint32_t max = address;
  for (uint32_t i = 0; i < wasmobj->data.count; ++i) {
    DataSegmentForLink *d = &wasmobj->data.segments[i];
    d->start += address;
    uint32_t end = d->start + d->size;
    if (end > max)
      max = end;
  }
  address = max;

  // Apply to data symbols.
  for (int i = 0; i < symtab->len; ++i) {
    SymbolInfo *sym = symtab->data[i];
    if (sym->kind != SIK_SYMTAB_DATA || sym->flags & WASM_SYM_UNDEFINED)
      continue;
    if (sym->local_index >= wasmobj->data.count)
      error("illegal index for data segment %.*s: %d\n", NAMES(sym->name), sym->local_index);
    DataSegmentForLink *d = &wasmobj->data.segments[sym->local_index];
    sym->data.address = d->start + sym->data.offset;
  }
  return address;
}

static uint32_t remap_data_address(WasmLinker *linker, uint32_t address) {
  for (int i = 0; i < linker->files->len; ++i) {
    File *file = linker->files->data[i];
    switch (file->kind) {
    case FK_WASMOBJ:
    address = remap_data_address_wasmobj(file->wasmobj, address);
      break;
    case FK_ARCHIVE:
      FOREACH_FILE_ARCONTENT(file->archive, content, {
        address = remap_data_address_wasmobj(content->obj, address);
      });
      break;
    }
  }
  return address;
}

static void renumber_indirect_functions_wasmobj(WasmLinker *linker, WasmObj *wasmobj) {
  Vector *indirect_functions = linker->indirect_functions;
  uint32_t segnum = wasmobj->elem.count;
  ElemSegmentForLink *segments = wasmobj->elem.segments;
  Vector *symtab = wasmobj->linking.symtab;
  for (uint32_t i = 0; i < segnum; ++i) {
    ElemSegmentForLink *segment = &segments[i];
    uint32_t count = segment->count;
    for (uint32_t j = 0; j < count; ++j) {
      uint32_t index = segment->content[j];
      SymbolInfo *sym = NULL;
      for (int k = 0; k < symtab->len; ++k) {
        SymbolInfo *p = symtab->data[k];
        if (p->kind == SIK_SYMTAB_FUNCTION && p->local_index == index) {
          sym = p;
          break;
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
      vec_push(indirect_functions, sym);
    }
  }
}

static void renumber_indirect_functions(WasmLinker *linker) {
  for (int i = 0; i < linker->files->len; ++i) {
    File *file = linker->files->data[i];
    switch (file->kind) {
    case FK_WASMOBJ:
      renumber_indirect_functions_wasmobj(linker, file->wasmobj);
      break;
    case FK_ARCHIVE:
      FOREACH_FILE_ARCONTENT(file->archive, content, {
        renumber_indirect_functions_wasmobj(linker, content->obj);
      });
      break;
    }
  }

  Vector *indirect_functions = linker->indirect_functions;
  for (int i = 0; i < indirect_functions->len; ++i) {
    SymbolInfo *sym = indirect_functions->data[i];
    sym->func.indirect_index = i + INDIRECT_FUNCTION_TABLE_START_INDEX;
  }
}

static void put_varint32(unsigned char *p, int32_t x, RelocInfo *reloc) {
  for (uint32_t count = 0; ; ++count) {
    if (!(*p & 0x80)) {
      if (x > 0x7f)
        error("Cannot fit reloc varint32: at 0x%x", reloc->offset);
      *p = x & 0x7f;
      break;
    }
    if (count >= 5)
      error("Malformed varint32: at 0x%x", reloc->offset);
    *p++ = (x & 0x7f) | 0x80;
    x >>= 7;
  }
}

static void put_varuint32(unsigned char *p, uint32_t x, RelocInfo *reloc) {
  for (uint32_t count = 0;; ++count) {
    if (!(*p & 0x80)) {
      if (x > 0x7f)
        error("Cannot fit reloc varuint32: at 0x%x", reloc->offset);
      *p = x & 0x7f;
      break;
    }
    if (count >= 5)
      error("Malformed varuint32: at 0x%x", reloc->offset);
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

static void apply_relocation_wasmobj(WasmLinker *linker, WasmObj *wasmobj) {
  Vector *symtab = wasmobj->linking.symtab;
  for (int i = 0; i < 2; ++i) {
    uint32_t count = wasmobj->reloc[i].count;
    if (count == 0)
      continue;

    uint32_t section_index = wasmobj->reloc[i].section_index;
    WasmSection *sec = &wasmobj->sections[section_index];
    RelocInfo *relocs = wasmobj->reloc[i].relocs;
    for (uint32_t j = 0; j < count; ++j) {
      RelocInfo *p = &relocs[j];
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

      enum ValueType { COMBIDX = 1, MEMADDR, FUNCIDX };
      enum DstType { VARU = 1, VARI, I32 };
      static const struct {
        enum ValueType val;
        enum DstType dst;
      } kTable[] = {
        [R_WASM_FUNCTION_INDEX_LEB] = { COMBIDX, VARU },
        [R_WASM_TABLE_INDEX_SLEB]   = { FUNCIDX, VARI },
        [R_WASM_TABLE_INDEX_I32]    = { FUNCIDX, I32 },
        [R_WASM_MEMORY_ADDR_LEB]    = { MEMADDR, VARU },
        [R_WASM_MEMORY_ADDR_SLEB]   = { MEMADDR, VARI },
        [R_WASM_MEMORY_ADDR_I32]    = { MEMADDR, I32 },
        [R_WASM_GLOBAL_INDEX_LEB]   = { COMBIDX, VARU },
        [R_WASM_TAG_INDEX_LEB]      = { COMBIDX, VARU },
        [R_WASM_TABLE_NUMBER_LEB]   = { COMBIDX, VARU },
      };

      unsigned int type = p->type;
      uint32_t value = 0;
      if (type >= ARRAY_SIZE(kTable) || kTable[type].val == 0) {
        error("Relocation not handled: type=%d", type);
      } else {
        switch (kTable[type].val) {
        case COMBIDX:  value = target->combined_index; break;
        case MEMADDR:  value = target->data.address + p->addend; break;
        case FUNCIDX:  value = target->func.indirect_index; break;
        default: assert(false); break;
        }
        switch (kTable[type].dst) {
        case VARU:  put_varuint32(q, value, p); break;
        case VARI:  put_varint32(q, value, p); break;
        case I32:   put_i32(q, value); break;
        default: assert(false); break;
        }
      }
    }
  }
}

static void apply_relocation(WasmLinker *linker) {
  for (int i = 0; i < linker->files->len; ++i) {
    File *file = linker->files->data[i];
    switch (file->kind) {
    case FK_WASMOBJ:
      apply_relocation_wasmobj(linker, file->wasmobj);
      break;
    case FK_ARCHIVE:
      FOREACH_FILE_ARCONTENT(file->archive, content, {
        apply_relocation_wasmobj(linker, content->obj);
      });
      break;
    }
  }
}

static void out_import_section(WasmLinker *linker) {
  DataStorage imports_section;
  data_init(&imports_section);
  data_open_chunk(&imports_section);
  data_open_chunk(&imports_section);
  uint32_t imports_count = 0;

  const Name *name;
  SymbolInfo *sym;
  for (int it = 0; (it = table_iterate(&linker->unresolved, it, &name, (void**)&sym)) != -1; ) {
    assert(sym != NULL);
    if (sym->kind != SIK_SYMTAB_FUNCTION)
      continue;
    const Name *modname = sym->module_name;
    if (modname == NULL)
      modname = alloc_name(linker->options.import_module_name, NULL, false);
    const Name *name = sym->name;

    data_string(&imports_section, modname->chars, modname->bytes);  // import module name
    data_string(&imports_section, name->chars, name->bytes);  // import name
    data_push(&imports_section, IMPORT_FUNC);  // import kind
    data_uleb128(&imports_section, -1, sym->func.type_index);  // import signature index
    ++imports_count;
  }

  if (imports_count > 0) {
    data_close_chunk(&imports_section, imports_count);
    data_close_chunk(&imports_section, -1);

    fputc(SEC_IMPORT, linker->ofp);
    fwrite(imports_section.buf, imports_section.len, 1, linker->ofp);
  }
}

static uint32_t out_function_section_wasmobj(WasmObj *wasmobj, DataStorage *functions_section) {
  Vector *symtab = wasmobj->linking.symtab;
  uint32_t function_count = 0;
  uint32_t import_function_count = wasmobj->import.functions->len;
  for (int j = 0; j < symtab->len; ++j) {
    SymbolInfo *sym = symtab->data[j];
    if (sym->kind != SIK_SYMTAB_FUNCTION || (sym->flags & WASM_SYM_UNDEFINED))
      continue;
    assert(sym->local_index >= import_function_count);
    uint32_t index = sym->local_index - import_function_count;
    if (index < function_count)
      continue;
    assert(index == function_count);  // Assume symtab order is arranged in ascending order.
    ++function_count;
    int type_index = sym->func.type_index;
    data_uleb128(functions_section, -1, type_index);  // function i signature index
  }
  return function_count;
}

static void out_function_section(WasmLinker *linker) {
  DataStorage functions_section;
  data_init(&functions_section);
  data_open_chunk(&functions_section);
  data_open_chunk(&functions_section);
  uint32_t function_count = 0;

  for (int i = 0; i < linker->files->len; ++i) {
    File *file = linker->files->data[i];
    switch (file->kind) {
    case FK_WASMOBJ:
      function_count += out_function_section_wasmobj(file->wasmobj, &functions_section);
      break;
    case FK_ARCHIVE:
      FOREACH_FILE_ARCONTENT(file->archive, content, {
        function_count += out_function_section_wasmobj(content->obj, &functions_section);
      });
      break;
    }
  }

  if (function_count > 0) {
    data_close_chunk(&functions_section, function_count);  // num functions
    data_close_chunk(&functions_section, -1);

    fputc(SEC_FUNC, linker->ofp);
    fwrite(functions_section.buf, functions_section.len, 1, linker->ofp);
  }
}

static inline bool funcref_table_exist(WasmLinker *linker) {
  return table_try_get(&linker->defined, linker->indirect_function_table_name, NULL);
}

static void out_table_section(WasmLinker *linker) {
  Vector *indirect_functions = linker->indirect_functions;
  if (indirect_functions->len == 0 && !funcref_table_exist(linker))
    return;

  DataStorage table_section;
  data_init(&table_section);
  data_open_chunk(&table_section);
  data_leb128(&table_section, -1, 1);  // num tables
  data_push(&table_section, WT_FUNCREF);
  data_push(&table_section, 0x00);  // limits: flags
  data_leb128(&table_section, -1,
              INDIRECT_FUNCTION_TABLE_START_INDEX + indirect_functions->len);  // initial
  data_close_chunk(&table_section, -1);

  fputc(SEC_TABLE, linker->ofp);
  fwrite(table_section.buf, table_section.len, 1, linker->ofp);
}

static void out_memory_section(WasmLinker *linker) {
  DataStorage memory_section;
  data_init(&memory_section);
  data_open_chunk(&memory_section);
  data_open_chunk(&memory_section);
  {
    uint32_t page_count = (linker->address_bottom + MEMORY_PAGE_SIZE - 1) / MEMORY_PAGE_SIZE;
    if (page_count <= 0)
      page_count = 1;
    data_uleb128(&memory_section, -1, 0);  // limits (no maximum page size)
    data_uleb128(&memory_section, -1, page_count);
    data_close_chunk(&memory_section, 1);  // count
    data_close_chunk(&memory_section, -1);
  }

  fputc(SEC_MEMORY, linker->ofp);
  fwrite(memory_section.buf, memory_section.len, 1, linker->ofp);
}

static void out_global_section(WasmLinker *linker) {
  DataStorage globals_section;
  data_init(&globals_section);
  data_open_chunk(&globals_section);
  data_open_chunk(&globals_section);
  uint32_t globals_count = 0;
  {
    const Name *name;
    SymbolInfo *sym;
    for (int it = 0; (it = table_iterate(&linker->defined, it, &name, (void**)&sym)) != -1; ) {
      if (sym->kind != SIK_SYMTAB_GLOBAL)
        continue;
      assert(sym->combined_index == globals_count);

      uint8_t wtype = sym->global.wtype;
      data_push(&globals_section, wtype);
      data_push(&globals_section, sym->global.mut);
      switch (wtype) {
      case WT_I32: case WT_I64:
        data_push(&globals_section, wtype == WT_I32 ? OP_I32_CONST : OP_I64_CONST);
        data_leb128(&globals_section, -1, sym->global.ivalue);
        data_push(&globals_section, OP_END);
        break;
#ifndef __NO_FLONUM
      case WT_F32:
        data_push(&globals_section, OP_F32_CONST);
        data_append(&globals_section, &sym->global.f32value, sizeof(sym->global.f32value));  // !Endian
        data_push(&globals_section, OP_END);
        break;
      case WT_F64:
        data_push(&globals_section, OP_F64_CONST);
        data_append(&globals_section, &sym->global.f64value, sizeof(sym->global.f64value));  // !Endian
        data_push(&globals_section, OP_END);
        break;
#endif
      default: assert(false); break;
      }
      ++globals_count;
    }
  }
  if (globals_count > 0) {
    data_close_chunk(&globals_section, globals_count);  // num functions
    data_close_chunk(&globals_section, -1);

    fputc(SEC_GLOBAL, linker->ofp);
    fwrite(globals_section.buf, globals_section.len, 1, linker->ofp);
  }
}

static void out_export_section(WasmLinker *linker, Table *exports) {
  DataStorage exports_section;
  data_init(&exports_section);
  data_open_chunk(&exports_section);
  data_open_chunk(&exports_section);
  int num_exports = 0;
  const Name *name;
  Table *table = linker->options.export_all ? &linker->defined : exports;
  for (int it = 0; (it = table_iterate(table, it, &name, NULL)) != -1; ) {
    SymbolInfo *sym = table_get(&linker->defined, name);
    if (sym == NULL) {
      error("Export: `%.*s' not found", NAMES(name));
    }

    switch (sym->kind) {
    case SIK_SYMTAB_FUNCTION:
    case SIK_SYMTAB_GLOBAL:
      data_string(&exports_section, name->chars, name->bytes);  // export name
      data_uleb128(&exports_section, -1,
                   sym->kind == SIK_SYMTAB_FUNCTION ? IMPORT_FUNC : IMPORT_GLOBAL);  // export kind
      data_uleb128(&exports_section, -1, sym->combined_index);  // export func index
      break;
    default:
      if (!linker->options.export_all)
        error("Cannot export symbol: `%.*s' is not function nor global", NAMES(name));
      continue;
    }
    ++num_exports;
  }
  {  // WASI requires `memory` export.
    static const char name[] = "memory";
    data_string(&exports_section, name, sizeof(name) - 1);  // export name
    data_uleb128(&exports_section, -1, IMPORT_MEMORY);  // export kind
    data_uleb128(&exports_section, -1, 0);  // export global index
    ++num_exports;
  }
  if (linker->options.export_table) {
    Vector *indirect_functions = linker->indirect_functions;
    if (indirect_functions->len > 0 || funcref_table_exist(linker)) {
      const Name *name = linker->indirect_function_table_name;
      data_string(&exports_section, name->chars, name->bytes);  // export name
      data_uleb128(&exports_section, -1, IMPORT_TABLE);  // export kind
      data_uleb128(&exports_section, -1, 0);  // export global index
      ++num_exports;
    }
  }
  data_close_chunk(&exports_section, num_exports);
  data_close_chunk(&exports_section, -1);

  fputc(SEC_EXPORT, linker->ofp);
  fwrite(exports_section.buf, exports_section.len, 1, linker->ofp);
}

static void out_elems_section(WasmLinker *linker) {
  Vector *indirect_functions = linker->indirect_functions;
  if (indirect_functions->len == 0)
    return;

  DataStorage elems_section;
  data_init(&elems_section);
  data_open_chunk(&elems_section);

  // Enumerate imported functions.
  data_leb128(&elems_section, -1, 1);  // num elem segments
  data_leb128(&elems_section, -1, 0);  // segment flags
  data_push(&elems_section, OP_I32_CONST);
  data_leb128(&elems_section, -1, INDIRECT_FUNCTION_TABLE_START_INDEX);  // start index
  data_push(&elems_section, OP_END);
  data_leb128(&elems_section, -1, indirect_functions->len);  // num elems
  for (int i = 0; i < indirect_functions->len; ++i) {
    SymbolInfo *sym = indirect_functions->data[i];
    data_leb128(&elems_section, -1, sym->combined_index);  // elem function index
  }
  data_close_chunk(&elems_section, -1);

  fputc(SEC_ELEM, linker->ofp);
  fwrite(elems_section.buf, elems_section.len, 1, linker->ofp);
}

static uint32_t out_code_section_wasmobj(WasmObj *wasmobj, DataStorage *codesec) {
  WasmSection *sec = find_section(wasmobj, SEC_CODE);
  if (sec == NULL)
    return 0;
  unsigned char *p = sec->start;
  assert(p != NULL);
  uint32_t num = read_uleb128(p, &p);
  data_append(codesec, p, sec->size - (p - sec->start));
  return num;
}

static void out_code_section(WasmLinker *linker) {
  DataStorage codesec;
  data_init(&codesec);
  data_open_chunk(&codesec);
  data_open_chunk(&codesec);

  uint32_t code_count = 0;
  for (int i = 0; i < linker->files->len; ++i) {
    File *file = linker->files->data[i];
    switch (file->kind) {
    case FK_WASMOBJ:
      code_count += out_code_section_wasmobj(file->wasmobj, &codesec);
      break;
    case FK_ARCHIVE:
      FOREACH_FILE_ARCONTENT(file->archive, content, {
        code_count += out_code_section_wasmobj(content->obj, &codesec);
      });
      break;
    }
  }

  data_close_chunk(&codesec, code_count);
  data_close_chunk(&codesec, -1);

  fputc(SEC_CODE, linker->ofp);
  fwrite(codesec.buf, codesec.len, 1, linker->ofp);
}

static uint32_t out_data_section_wasmobj(WasmObj *wasmobj, DataStorage *datasec) {
  DataSegmentForLink *segments = wasmobj->data.segments;
  uint32_t data_count = 0;
  for (uint32_t j = 0, count = wasmobj->data.count; j < count; ++j) {
    DataSegmentForLink *segment = &segments[j];
    uint32_t size = segment->size;
    const unsigned char *content = segment->content;
    uint32_t non_zero_size;
    for (non_zero_size = size; non_zero_size > 0; --non_zero_size) {
      if (content[non_zero_size - 1] != 0x00)
        break;
    }
    if (non_zero_size == 0)  // BSS
      continue;

    data_push(datasec, 0);  // flags
    // Init (address).
    uint32_t address = segment->start;
    data_push(datasec, OP_I32_CONST);
    data_leb128(datasec, -1, address);
    data_push(datasec, OP_END);
    // Content
    data_uleb128(datasec, -1, non_zero_size);
    data_append(datasec, segment->content, non_zero_size);
    ++data_count;
  }
  return data_count;
}

static void out_data_section(WasmLinker *linker) {
  DataStorage datasec;
  data_init(&datasec);
  data_open_chunk(&datasec);
  data_open_chunk(&datasec);

  uint32_t data_count = 0;
  for (int i = 0; i < linker->files->len; ++i) {
    File *file = linker->files->data[i];
    switch (file->kind) {
    case FK_WASMOBJ:
      data_count += out_data_section_wasmobj(file->wasmobj, &datasec);
      break;
    case FK_ARCHIVE:
      FOREACH_FILE_ARCONTENT(file->archive, content, {
        data_count += out_data_section_wasmobj(content->obj, &datasec);
      });
      break;
    }
  }
  data_close_chunk(&datasec, data_count);
  data_close_chunk(&datasec, -1);

  fputc(SEC_DATA, linker->ofp);
  fwrite(datasec.buf, datasec.len, 1, linker->ofp);
}

//

void linker_init(WasmLinker *linker) {
  memset(linker, 0, sizeof(*linker));
  linker->files = new_vector();

  table_init(&linker->defined);
  table_init(&linker->unresolved);
  linker->indirect_functions = new_vector();

  linker->sp_name = alloc_name(SP_NAME, NULL, false);
  linker->heapbase_name = alloc_name(HEAP_BASE_NAME, NULL, false);
  linker->indirect_function_table_name = alloc_name(INDIRECT_FUNCALL_TABLE_NAME, NULL, false);
}

bool read_wasm_obj(WasmLinker *linker, const char *filename) {
  File *file = calloc_or_die(sizeof(*file));
  file->filename = filename;

  char *ext = get_ext(filename);
  if (strcasecmp(ext, "o") == 0) {
    FILE *fp;
    if (!is_file(filename) || (fp = fopen(filename, "rb")) == NULL) {
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

    file->kind = FK_WASMOBJ;
    file->wasmobj = wasmobj;
    vec_push(linker->files, file);
  } else if (strcasecmp(ext, "a") == 0) {
    Archive *archive = load_archive(filename);
    if (archive == NULL) {
      fprintf(stderr, "load failed: %s\n", filename);
      return false;
    }
    file->kind = FK_ARCHIVE;
    file->archive = archive;
    vec_push(linker->files, file);
  } else {
    error("Unsupported file: %s", filename);
  }
  return true;
}

static void verbose_symbols(WasmObj *wasmobj, enum SymInfoKind kind) {
  Vector *symtab = wasmobj->linking.symtab;
  for (int j = 0; j < symtab->len; ++j) {
    SymbolInfo *sym = symtab->data[j];
    if (sym->flags & WASM_SYM_UNDEFINED || sym->kind != kind)
      continue;
    printf("%2d: %.*s\n", sym->combined_index, NAMES(sym->name));
  }
}

bool link_wasm_objs(WasmLinker *linker, Table *exports, uint32_t stack_size) {
  const Name *name;
  for (int it = 0; (it = table_iterate(exports, it, &name, NULL)) != -1; ) {
    table_put(&linker->unresolved, name, NULL);
  }

  if (!resolve_symbols(linker))
    return false;

  uint32_t data_end_address = remap_data_address(linker, stack_size);
  uint32_t address_bottom = ALIGN(data_end_address, 16);
  {
    SymbolInfo *spsym = table_get(&linker->defined, linker->sp_name);
    if (spsym != NULL) {
      if (spsym->kind != SIK_SYMTAB_GLOBAL)
        error("illegal symbol for stack pointer: %.*s", NAMES(linker->sp_name));
      spsym->global.ivalue = stack_size;
    }

    SymbolInfo *heapbasesym = table_get(&linker->defined, linker->heapbase_name);
    if (heapbasesym != NULL) {
      if (heapbasesym->kind != SIK_SYMTAB_DATA)
        error("illegal symbol for break address: %.*s", NAMES(linker->heapbase_name));
      heapbasesym->data.address = address_bottom;
      heapbasesym->data.offset = 0;
      heapbasesym->data.size = 0;
    }
  }
  renumber_symbols(linker);
  renumber_func_types(linker);
  renumber_indirect_functions(linker);
  apply_relocation(linker);

  generate_init_funcs(linker);

  linker->address_bottom = address_bottom;

  if (verbose) {
    const Name *name;
    SymbolInfo *sym;

    printf("### Functions\n");
    uint32_t imports_count = 0;
    // Import.
    for (int it = 0; (it = table_iterate(&linker->unresolved, it, &name, (void**)&sym)) != -1; ) {
      if (sym->kind != SIK_SYMTAB_FUNCTION)
        continue;
      const Name *modname = sym->module_name;
      assert(modname != NULL);
      const Name *name = sym->name;
      printf("%2d: %.*s.%.*s (import)\n", imports_count, NAMES(modname), NAMES(name));
      ++imports_count;
    }
    // Defined.
    for (int i = 0; i < linker->files->len; ++i) {
      File *file = linker->files->data[i];
      switch (file->kind) {
      case FK_WASMOBJ:
        verbose_symbols(file->wasmobj, SIK_SYMTAB_FUNCTION);
        break;
      case FK_ARCHIVE:
        FOREACH_FILE_ARCONTENT(file->archive, content, {
          verbose_symbols(content->obj, SIK_SYMTAB_FUNCTION);
        });
        break;
      }
    }

    printf("### Globals\n");
    for (int it = 0; (it = table_iterate(&linker->defined, it, &name, (void**)&sym)) != -1; ) {
      if (sym->kind != SIK_SYMTAB_GLOBAL)
        continue;
      printf("%2d: %.*s\n", sym->combined_index, NAMES(sym->name));
    }

    printf("### Data\n");
    for (int i = 0; i < linker->files->len; ++i) {
      File *file = linker->files->data[i];
      switch (file->kind) {
      case FK_WASMOBJ:
        verbose_symbols(file->wasmobj, SIK_SYMTAB_DATA);
        break;
      case FK_ARCHIVE:
        FOREACH_FILE_ARCONTENT(file->archive, content, {
          verbose_symbols(content->obj, SIK_SYMTAB_DATA);
        });
        break;
      }
    }
  }

  return true;
}

bool linker_emit_wasm(WasmLinker *linker, const char *ofn, Table *exports) {
  FILE *ofp = fopen(ofn, "wb");
  if (ofp == NULL) {
    fprintf(stderr, "cannot open: %s\n", ofn);
    return false;
  }

  linker->ofp = ofp;

  write_wasm_header(ofp);

  EmitWasm ew_body = {
    .ofp = ofp,
  };
  EmitWasm *ew = &ew_body;

  // Types.
  emit_type_section(ew);

  // Imports.
  out_import_section(linker);

  // Functions.
  out_function_section(linker);

  // Table.
  out_table_section(linker);

  // Memory.
  out_memory_section(linker);

  // Tag (must put earlier than Global section.)
  emit_tag_section(ew);

  // Globals.
  out_global_section(linker);

  // Exports.
  out_export_section(linker, exports);

  // Elements.
  out_elems_section(linker);

  // Code.
  out_code_section(linker);

  // Data.
  out_data_section(linker);

  fclose(ofp);

  return true;
}
