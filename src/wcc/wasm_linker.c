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
      case SEC_IMPORT:
        read_import_section(wasmobj, p);
        break;
      case SEC_CUSTOM:
        {
          unsigned char *q;
          if (match_string(p, "linking", &q)) {
            linking_section_index = count;
            read_linking(wasmobj, q, sec->start + size);
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


void linker_init(WasmLinker *linker) {
  memset(linker, 0, sizeof(*linker));
  linker->files = new_vector();
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
