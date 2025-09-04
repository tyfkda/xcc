#include "../config.h"
#include "wasm_obj.h"

#include <assert.h>
#include <string.h>

#include "table.h"
#include "util.h"
#include "wasm.h"
#include "wcc.h"

int64_t read_leb128(unsigned char *p, unsigned char **next) {
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

uint64_t read_uleb128(unsigned char *p, unsigned char **next) {
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

bool match_string(unsigned char *p, const char *str, unsigned char **next) {
  size_t len = read_uleb128(p, &p);
  if (strncmp(str, (char*)p, len) != 0 || str[len] != '\0')
    return false;
  *next = p + len;
  return true;
}

const Name *read_wasm_string(unsigned char *p, unsigned char **next) {
  size_t len = read_uleb128(p, &p);
  *next = p + len;
  return alloc_name((char*)p, (char*)p + len, false);
}

uint8_t read_type(unsigned char *p, unsigned char **next) {
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

//

void wasmobj_init(WasmObj *wasmobj) {
  memset(wasmobj, 0, sizeof(*wasmobj));
  wasmobj->import.functions = new_vector();
  wasmobj->import.globals = new_vector();
  wasmobj->import.tables = NULL;
  wasmobj->linking.symtab = new_vector();
}

WasmSection *find_section(WasmObj *wasmobj, uint8_t secid) {
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

WasmObj *read_wasm(FILE *fp, const char *filename, size_t filesize) {
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
