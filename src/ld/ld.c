#include <assert.h>
#include <elf.h>
#include <getopt.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "gen_section.h"
#include "table.h"
#include "util.h"

static const char kDefaultEntryName[] = "_start";

void *malloc_or_die(size_t size) {
  void *p = malloc(size);
  if (p != NULL)
    return p;
  fprintf(stderr, "memory overflow\n");
  exit(1);
}

void* read_from(FILE *fp, unsigned long offset, size_t size) {
  void *buf = malloc_or_die(size);
  if (fseek(fp, offset, SEEK_SET) != 0 ||
      fread(buf, 1, size, fp) != size) {
    free(buf);
    buf = NULL;
  }
  return buf;
}

static char *shtypename(int shtype) {
  switch (shtype) {
  case SHT_NULL: return "null";
  case SHT_PROGBITS: return "progbits";
  case SHT_SYMTAB: return "symtab";
  case SHT_STRTAB: return "strtab";
  case SHT_RELA: return "rela";
  case SHT_NOBITS: return "nobits";
  default:
    {
      static char buf[32];
      sprintf(buf, "type%d", shtype);
      return buf;
    }
  }
}

static Elf64_Shdr *read_all_section_headers(FILE *fp, Elf64_Ehdr *ehdr) {
  if (ehdr->e_shnum <= 0)
    return NULL;
  Elf64_Shdr *esecs = read_from(fp, ehdr->e_shoff, ehdr->e_shnum * sizeof(Elf64_Shdr));
  if (esecs == NULL) {
    perror("read section header failed");
  }
  return esecs;
}

static char *read_strtab(FILE *fp, Elf64_Shdr *sec) {
  assert(sec->sh_type == SHT_STRTAB);
  char *buf = read_from(fp, sec->sh_offset, sec->sh_size);
  if (buf == NULL) {
    perror("read strtab failed");
  }
  return buf;
}

typedef struct {
  FILE *fp;
  Elf64_Ehdr ehdr;
  Elf64_Shdr *shdrs;
  char *shstrtab;

  struct {
    Elf64_Shdr *shdr;
    Elf64_Sym *symtabs;
    Table names;  // <Elf64_Sym*>
    const char *str;
  } symtab;
} ElfObj;

static void elfobj_init(ElfObj *elfobj) {
  memset(elfobj, 0, sizeof(*elfobj));
  table_init(&elfobj->symtab.names);
}

static int find_section(ElfObj *elfobj, unsigned int section_type) {
  for (int i = 0; i < elfobj->ehdr.e_shnum; ++i) {
    Elf64_Shdr *p = &elfobj->shdrs[i];
    if (p->sh_type == section_type)
      return i;
  }
  return -1;
}

static bool load_symtab(ElfObj *elfobj) {
  int symtab_index = find_section(elfobj, SHT_SYMTAB);
  if (symtab_index < 0)
    return false;

  Elf64_Shdr *symtab_sec = &elfobj->shdrs[symtab_index];
  Elf64_Sym *symtabs = read_from(elfobj->fp, symtab_sec->sh_offset, symtab_sec->sh_size);
  if (symtabs == NULL)
    perror("read symtab failed");
  if (symtab_sec->sh_size % sizeof(Elf64_Sym) != 0)
    error("malformed symtab");

  Elf64_Shdr *strtab_sec = &elfobj->shdrs[symtab_sec->sh_link];
  const char *str = read_strtab(elfobj->fp, strtab_sec);
  if (str == NULL)
    error("read strtab failed");

  Table *names = &elfobj->symtab.names;
  size_t count = symtab_sec->sh_size / sizeof(Elf64_Sym);
  for (uint32_t i = 0; i < count; ++i) {
    Elf64_Sym *sym = &symtabs[i];
    unsigned char type = sym->st_info & 0x0f;
    if (type == STT_NOTYPE && str[sym->st_name] != '\0') {
      const Name *name = alloc_name(&str[sym->st_name], NULL, false);
      table_put(names, name, sym);
    }
  }

  elfobj->symtab.shdr = symtab_sec;
  elfobj->symtab.symtabs = symtabs;
  elfobj->symtab.str = str;
  return true;
}

static bool read_elf(ElfObj *elfobj, FILE *fp, const char *fn) {
  elfobj->fp = fp;
  ssize_t size = fread(&elfobj->ehdr, sizeof(elfobj->ehdr), 1, fp);
  if (size != 1 ||
      elfobj->ehdr.e_ident[0] != ELFMAG0 || elfobj->ehdr.e_ident[1] != ELFMAG1 ||
      elfobj->ehdr.e_ident[2] != ELFMAG2 || elfobj->ehdr.e_ident[3] != ELFMAG3) {
    fprintf(stderr, "no elf file: %s\n", fn);
    return false;
  }
  if (elfobj->ehdr.e_machine != EM_X86_64 || elfobj->ehdr.e_version != EV_CURRENT ||
      elfobj->ehdr.e_ehsize != sizeof(Elf64_Ehdr) || elfobj->ehdr.e_shentsize != sizeof(Elf64_Shdr) ||
      elfobj->ehdr.e_shnum < 1 || elfobj->ehdr.e_shstrndx >= elfobj->ehdr.e_shnum) {
    fprintf(stderr, "illegal elf: %s\n", fn);
    return false;
  }
  elfobj->shdrs = read_all_section_headers(fp, &elfobj->ehdr);
  if (elfobj->shdrs != NULL) {
    elfobj->shstrtab = read_strtab(fp, &elfobj->shdrs[elfobj->ehdr.e_shstrndx]);
    if (elfobj->shstrtab != NULL) {
      if (!load_symtab(elfobj))
        return false;
    }
  }
  return true;
}

static bool open_elf(const char *fn, ElfObj *elfobj) {
  FILE *fp = fopen(fn, "r");
  if (fp == NULL) {
    fprintf(stderr, "cannot open: %s\n", fn);
  } else {
    if (read_elf(elfobj, fp, fn))
      return true;
    fclose(fp);
    elfobj->fp = NULL;
  }
  return false;
}

static void close_elf(ElfObj *elfobj) {
  if (elfobj->fp != NULL) {
    fclose(elfobj->fp);
    elfobj->fp = NULL;
  }
}

void dump(FILE *fp, ElfObj *elfobj) {
  for (int i = 0; i < elfobj->ehdr.e_shnum; ++i) {
    Elf64_Shdr *p = &elfobj->shdrs[i];
    fprintf(fp, "%2d:%-16s type=%8s, offset=%lx, size=%lx\n", i, &elfobj->shstrtab[p->sh_name], shtypename(p->sh_type), p->sh_offset, p->sh_size);
  }

  {
    printf("Symbols: #%d\n", elfobj->symtab.names.count);
    const Name *name;
    Elf64_Sym *sym;
    for (int it = 0; (it = table_iterate(&elfobj->symtab.names, it, &name, (void**)&sym)) != -1; ) {
      unsigned char bind = sym->st_info >> 4;
      char *bindname;
      switch (bind) {
      case STB_LOCAL:  bindname = "LOCAL"; break;
      case STB_GLOBAL:  bindname = "GLOBAL"; break;
      default:  bindname = "?"; break;
      }
      printf("  %.*s: bind=%s\n", name->bytes, name->chars, bindname);
    }
  }
}

static Elf64_Sym *find_symbol(ElfObj *elfobj, const Name *name) {
  Elf64_Sym *sym = table_get(&elfobj->symtab.names, name);
  return sym;
}

static Elf64_Sym *find_symbol_from_all(Vector *elfobjs, const Name *name, ElfObj **pelfobj) {
  for (int i = 0; i < elfobjs->len; ++i) {
    ElfObj *elfobj = elfobjs->data[i];
    Elf64_Sym *sym = find_symbol(elfobj, name);
    if (sym != NULL && sym->st_shndx != 0) {
      if (pelfobj != NULL)
        *pelfobj = elfobj;
      return sym;
    }
  }
  return NULL;
}

static bool link_elfobjs(Vector *elfobjs, const Name *entry) {
  Table unresolved;
  table_init(&unresolved);
  table_put(&unresolved, entry, (void*)entry);

  for (int i = 0; i < elfobjs->len; ++i) {
    ElfObj *elfobj = elfobjs->data[i];

    for (Elf64_Half sec = 0; sec < elfobj->ehdr.e_shnum; ++sec) {
      Elf64_Shdr *shdr = &elfobj->shdrs[sec];
      switch (shdr->sh_type) {
      case SHT_PROGBITS:
        {
          Elf64_Xword size = shdr->sh_size;
          if (size <= 0)
            break;
          void *buf = read_from(elfobj->fp, shdr->sh_offset, size);
          if (buf == NULL) {
            perror("read error");
          }
          if (shdr->sh_flags & SHF_EXECINSTR) {
            add_code(buf, size);
          } else {
            int writable = shdr->sh_flags & SHF_WRITE;
            add_section_data(writable ? SEC_DATA : SEC_RODATA, buf, size);
          }
        }
        break;
      case SHT_NOBITS:
        {
          Elf64_Xword size = shdr->sh_size;
          if (size <= 0)
            break;
          add_bss(shdr->sh_size);
        }
        break;
      default: break;
      }
    }

    if (elfobj->symtab.shdr == NULL)
      continue;

    const char *str = elfobj->symtab.str;
    Table *names = &elfobj->symtab.names;
    const Name *name;
    Elf64_Sym *sym;
    for (int it = 0; (it = table_iterate(names, it, &name, (void**)&sym)) != -1; ) {
      unsigned char type = sym->st_info & 0x0f;
      if (type != STT_NOTYPE || str[sym->st_name] == '\0')
        continue;
      const Name *name = alloc_name(&str[sym->st_name], NULL, false);
      if (sym->st_shndx == 0) {
        if (find_symbol_from_all(elfobjs, name, NULL) == NULL)
          table_put(&unresolved, name, (void *)name);
      } else {
        table_delete(&unresolved, name);
      }
    }
  }

  if (unresolved.count > 0) {
    fprintf(stderr, "Unresolved: #%d\n", unresolved.count);
    const Name *name;
    void *dummy;
    for (int it = 0; (it = table_iterate(&unresolved, it, &name, &dummy)) != -1;) {
      fprintf(stderr, "  %.*s\n", name->bytes, name->chars);
    }
    return false;
  }

  return true;
}

int main(int argc, char *argv[]) {
  const char *ofn = NULL;
  const char *entry = kDefaultEntryName;

  struct option longopts[] = {
    {"version", no_argument, NULL, 'V'},
    {0},
  };
  int opt;
  int longindex;
  while ((opt = getopt_long(argc, argv, "Vo:e:", longopts, &longindex)) != -1) {
    switch (opt) {
    case 'V':
      show_version("ld");
      break;
    case 'o':
      ofn = optarg;
      break;
    case 'e':
      entry = optarg;
      break;
    default:
      fprintf(stderr, "Unknown option: %s\n", argv[optind]);
      return 1;
    }
  }

  int iarg = optind;
  if (iarg >= argc)
    error("no input");

  if (ofn == NULL)
    ofn = "a.out";

  Vector *elfobjs = new_vector();
  for (int i = iarg; i < argc; ++i) {
    ElfObj *elfobj = malloc_or_die(sizeof(*elfobj));
    elfobj_init(elfobj);
    if (!open_elf(argv[i], elfobj))
      return 1;
    dump(stdout, elfobj);
    vec_push(elfobjs, elfobj);
  }

  bool result = link_elfobjs(elfobjs, alloc_name(entry, NULL, false));
  for (int i = 0; i < elfobjs->len; ++i) {
    close_elf(elfobjs->data[i]);
  }
  return result ? 0 : 1;
}
