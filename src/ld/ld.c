#include <ar.h>
#include <assert.h>
#include <fcntl.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <strings.h>
#include <sys/stat.h>

#include "archive.h"
#include "elfobj.h"
#include "elfutil.h"
#include "gen_section.h"
#include "table.h"
#include "util.h"

static const char kDefaultEntryName[] = "_start";

#define PROG_START      (0x100)
#define START_ADDRESS   (0x01000000 + PROG_START)
#define LOAD_ADDRESS    START_ADDRESS
#define DATA_ALIGN      (0x1000)

//

typedef struct {
  ElfObj *elfobj;
  size_t size;
  char name[1];  // [sizeof(((struct ar_hdr*)0)->ar_name) + 1]
} ArContent;

ElfObj *load_archive_elfobj(Archive *ar, uint32_t offset) {
  Vector *contents = ar->contents;
  for (int i = 0; i < contents->len; i += 2) {
    if (VOIDP2INT(contents->data[i]) == offset) {
      // Already loaded.
      return NULL;
    }
  }

  fseek(ar->fp, offset, SEEK_SET);

  struct ar_hdr hdr;
  read_or_die(ar->fp, &hdr, sizeof(hdr), "hdr");
  if (memcmp(hdr.ar_fmag, ARFMAG, sizeof(hdr.ar_fmag)) != 0)
    error("Malformed archive");

  ArContent *content = malloc_or_die(sizeof(*content) + sizeof(hdr.ar_name));

  memcpy(content->name, hdr.ar_name, sizeof(hdr.ar_name));
  char *p = memchr(content->name, '/', sizeof(hdr.ar_name));
  if (p == NULL)
    p = &content->name[sizeof(hdr.ar_name)];
  *p = '\0';

  char sizestr[sizeof(hdr.ar_size) + 1];
  memcpy(sizestr, hdr.ar_size, sizeof(hdr.ar_size));
  sizestr[sizeof(hdr.ar_size)] = '\0';
  content->size = strtoul(sizestr, NULL, 10);

  ElfObj *elfobj = malloc_or_die(sizeof(*elfobj));
  content->elfobj = elfobj;
  elfobj_init(elfobj);
  if (!read_elf(elfobj, ar->fp, content->name)) {
    error("Failed to extract .o: %s", content->name);
  }

  vec_push(contents, INT2VOIDP(offset));
  vec_push(contents, content);

  return elfobj;
}

//

typedef struct {
  const char *filename;
  enum {
    FK_ELFOBJ,
    FK_ARCHIVE,
  } kind;
  union {
    ElfObj *elfobj;
    Archive *archive;
  };
} File;

typedef struct {
  File *files;
  int nfiles;
  uintptr_t offsets[SEC_BSS + 1];
  Vector *progbit_sections[SEC_BSS + 1];
} LinkEditor;

void ld_init(LinkEditor *ld, int nfiles) {
  ld->files = calloc_or_die(sizeof(*ld->files) * nfiles);
  ld->nfiles = nfiles;

  for (int secno = 0; secno < SEC_BSS + 1; ++secno) {
    ld->offsets[secno] = 0;
    ld->progbit_sections[secno] = new_vector();
  }
}

void ld_load(LinkEditor *ld, int i, const char *filename) {
  char *ext = get_ext(filename);
  File *file = &ld->files[i];
  file->filename = filename;
  if (strcasecmp(ext, "o") == 0) {
    ElfObj *elfobj = malloc_or_die(sizeof(*elfobj));
    elfobj_init(elfobj);
    if (!open_elf(filename, elfobj)) {
      exit(1);
    }
    file->kind = FK_ELFOBJ;
    file->elfobj = elfobj;
  } else if (strcasecmp(ext, "a") == 0) {
    Archive *archive = load_archive(filename);
    if (archive == NULL) {
      error("load failed: %s\n", filename);
    }
    file->kind = FK_ARCHIVE;
    file->archive = archive;
  } else {
    error("Unsupported file: %s", filename);
  }
}

static Elf64_Sym *ld_find_symbol(LinkEditor *ld, const Name *name, ElfObj **pelfobj) {
  for (int i = 0; i < ld->nfiles; ++i) {
    File *file = &ld->files[i];
    switch (file->kind) {
    case FK_ELFOBJ:
      {
        ElfObj *elfobj = file->elfobj;
        Elf64_Sym *sym = elfobj_find_symbol(elfobj, name);
        if (sym != NULL) {
          if (pelfobj != NULL)
            *pelfobj = elfobj;
          return sym;
        }
      }
      break;
    case FK_ARCHIVE:
      {
        Archive *ar = file->archive;
        for (int i = 0; i < ar->contents->len; i += 2) {
          ArContent *content = ar->contents->data[i + 1];
          ElfObj *elfobj = content->elfobj;
          Elf64_Sym *sym = elfobj_find_symbol(elfobj, name);
          if (sym != NULL) {
            if (pelfobj != NULL)
              *pelfobj = elfobj;
            return sym;
          }
        }
      }
      break;
    }
  }
  return NULL;
}

static void resolve_rela_elfobj(LinkEditor *ld, ElfObj *elfobj) {
  for (Elf64_Half sec = 0; sec < elfobj->ehdr.e_shnum; ++sec) {
    Elf64_Shdr *shdr = &elfobj->shdrs[sec];
    if (shdr->sh_type != SHT_RELA || shdr->sh_size <= 0)
      continue;
    const Elf64_Rela *relas = read_from(elfobj->fp, shdr->sh_offset + elfobj->start_offset,
                                        shdr->sh_size);
    if (relas == NULL) {
      perror("read error");
    }
    const Elf64_Shdr *symhdr = &elfobj->shdrs[shdr->sh_link];
    const ElfSectionInfo *symhdrinfo = &elfobj->section_infos[shdr->sh_link];
    const ElfSectionInfo *strinfo = &elfobj->section_infos[symhdr->sh_link];
    assert(elfobj->shdrs[shdr->sh_info].sh_type == SHT_PROGBITS);
    const ElfSectionInfo *dst_info = &elfobj->section_infos[shdr->sh_info];
    for (size_t j = 0, n = shdr->sh_size / sizeof(Elf64_Rela); j < n; ++j) {
      const Elf64_Rela *rela = &relas[j];
      const Elf64_Sym *sym = &symhdrinfo->symtab.syms[ELF64_R_SYM(rela->r_info)];

      uintptr_t address = 0;
      switch (ELF64_ST_BIND(sym->st_info)) {
      case STB_LOCAL:
        {
          assert(ELF64_R_SYM(rela->r_info) < elfobj->ehdr.e_shnum);
          const ElfSectionInfo *s = &elfobj->section_infos[ELF64_R_SYM(rela->r_info)];
          address = s->progbits.address;
        }
        break;
      case STB_GLOBAL:
        {
          const char *label = &strinfo->strtab.buf[sym->st_name];
          ElfObj *telfobj;
          const Elf64_Sym *tsym = ld_find_symbol(ld, alloc_name(label, NULL, false), &telfobj);
          assert(tsym != NULL && tsym->st_shndx != SHN_UNDEF);
#ifndef NDEBUG
          const Elf64_Shdr *tshdr = &telfobj->shdrs[tsym->st_shndx];
          assert(tshdr->sh_type == SHT_PROGBITS || tshdr->sh_type == SHT_NOBITS);
#endif
          address = telfobj->section_infos[tsym->st_shndx].progbits.address + tsym->st_value;
        }
        break;
      default: assert(false); break;
      }
      address += rela->r_addend;

      void *p = dst_info->progbits.content + rela->r_offset;
      uintptr_t pc = elfobj->section_infos[shdr->sh_info].progbits.address + rela->r_offset;
      switch (ELF64_R_TYPE(rela->r_info)) {
      case R_X86_64_64:
        *(uint64_t*)p = address;
        break;
      case R_X86_64_PC32:
      case R_X86_64_PLT32:
        *(uint32_t*)p = address - pc;
        break;
      default: assert(false); break;
      }
    }
  }
}

static void link_elfobj(LinkEditor *ld, ElfObj *elfobj, Table *unresolved) {
  for (Elf64_Half sec = 0; sec < elfobj->ehdr.e_shnum; ++sec) {
    Elf64_Shdr *shdr = &elfobj->shdrs[sec];
    switch (shdr->sh_type) {
    case SHT_PROGBITS:
      {
        Elf64_Xword size = shdr->sh_size;
        if (size <= 0)
          break;
        void *buf = read_from(elfobj->fp, shdr->sh_offset + elfobj->start_offset, size);
        if (buf == NULL) {
          perror("read error");
        }
        enum SectionType secno;
        if (shdr->sh_flags & SHF_EXECINSTR) {
          secno = SEC_CODE;
        } else if (shdr->sh_flags & SHF_WRITE) {
          secno = SEC_DATA;
        } else {
          secno = SEC_RODATA;
        }
        Elf64_Xword align = shdr->sh_addralign;
        align_section_size(secno, align);

        uintptr_t address = ALIGN(ld->offsets[secno], align);
        ElfSectionInfo *p = &elfobj->section_infos[sec];
        p->progbits.address = address;
        p->progbits.content = buf;
        vec_push(ld->progbit_sections[secno], p);
        ld->offsets[secno] = address + size;
      }
      break;
    case SHT_NOBITS:
      {
        Elf64_Xword size = shdr->sh_size;
        if (size <= 0)
          break;
        Elf64_Xword align = shdr->sh_addralign;
        align_section_size(SEC_BSS, align);
        add_bss(size);

        uintptr_t address = ALIGN(ld->offsets[SEC_BSS], align);
        ElfSectionInfo *p = &elfobj->section_infos[sec];
        p->progbits.address = address;
        p->progbits.content = NULL;
        vec_push(ld->progbit_sections[SEC_BSS], p);
        ld->offsets[SEC_BSS] = address + size;
      }
      break;
    case SHT_SYMTAB:
      {
        ElfSectionInfo *q = &elfobj->section_infos[shdr->sh_link];  // Strtab
        Table *symbol_table = elfobj->symbol_table;
        const char *str = q->strtab.buf;
        const Name *name;
        Elf64_Sym *sym;
        for (int it = 0; (it = table_iterate(symbol_table, it, &name, (void**)&sym)) != -1; ) {
          unsigned char type = ELF64_ST_TYPE(sym->st_info);
          if (type != STT_NOTYPE || str[sym->st_name] == '\0')
            continue;
          const Name *name = alloc_name(&str[sym->st_name], NULL, false);
          if (sym->st_shndx == SHN_UNDEF) {
            if (ld_find_symbol(ld, name, NULL) == NULL)
              table_put(unresolved, name, (void*)name);
          } else {
            table_delete(unresolved, name);
          }
        }
      }
      break;
    default: break;
    }
  }
}

static void link_archive(LinkEditor *ld, Archive *ar, Table *unresolved) {
  Table *table = &ar->symbol_table;
  for (;;) {
    bool retry = false;
    const Name *name;
    void *dummy;
    for (int it = 0; (it = table_iterate(unresolved, it, &name, &dummy)) != -1;) {
      ArSymbol *symbol;
      if (!table_try_get(table, name, (void**)&symbol))
        continue;
      table_delete(unresolved, name);

      ElfObj *elfobj = load_archive_elfobj(ar, symbol->offset);
      if (elfobj != NULL) {
        link_elfobj(ld, elfobj, unresolved);
        retry = true;
        break;
      }
    }
    if (!retry)
      break;
  }
}

static void resolve_rela_archive(LinkEditor *ld, Archive *ar) {
  for (int i = 0; i < ar->contents->len; i += 2) {
    ArContent *content = ar->contents->data[i + 1];
    resolve_rela_elfobj(ld, content->elfobj);
  }
}

static void resolve_relas(LinkEditor *ld) {
  for (int i = 0; i < ld->nfiles; ++i) {
    File *file = &ld->files[i];
    switch (file->kind) {
    case FK_ELFOBJ:
      resolve_rela_elfobj(ld, file->elfobj);
      break;
    case FK_ARCHIVE:
      resolve_rela_archive(ld, file->archive);
      break;
    }
  }
}

static void add_elfobj_sections(ElfObj *elfobj) {
  for (Elf64_Half sec = 0; sec < elfobj->ehdr.e_shnum; ++sec) {
    Elf64_Shdr *shdr = &elfobj->shdrs[sec];
    switch (shdr->sh_type) {
    case SHT_PROGBITS:
      {
        const ElfSectionInfo *p = &elfobj->section_infos[sec];
        void *content = p->progbits.content;
        if (content == NULL)
          continue;
        enum SectionType secno;
        if (shdr->sh_flags & SHF_EXECINSTR) {
          secno = SEC_CODE;
        } else if (shdr->sh_flags & SHF_WRITE) {
          secno = SEC_DATA;
        } else {
          secno = SEC_RODATA;
        }
        align_section_size(secno, shdr->sh_addralign);
        add_section_data(secno, content, shdr->sh_size);
      }
      break;
    default: break;
    }
  }
}

bool ld_link(LinkEditor *ld, Table *unresolved, uintptr_t start_address) {
  for (int i = 0; i < ld->nfiles; ++i) {
    File *file = &ld->files[i];
    switch (file->kind) {
    case FK_ELFOBJ:
      link_elfobj(ld, file->elfobj, unresolved);
      break;
    case FK_ARCHIVE:
      link_archive(ld, file->archive, unresolved);
      break;
    }
  }

  if (unresolved->count > 0) {
    fprintf(stderr, "Unresolved: #%d\n", unresolved->count);
    const Name *name;
    void *dummy;
    for (int it = 0; (it = table_iterate(unresolved, it, &name, &dummy)) != -1;) {
      fprintf(stderr, "  %.*s\n", NAMES(name));
    }
    return false;
  }

  // Calculate address.
  {
    uintptr_t address = start_address;
    for (int secno = 0; secno < SEC_BSS + 1; ++secno) {
      address = ALIGN(address, section_aligns[secno]);
      Vector *v = ld->progbit_sections[secno];
      if (v->len > 0) {
        for (int i = 0; i < v->len; ++i) {
          ElfSectionInfo *p = v->data[i];
          p->progbits.address += address;
        }
        ElfSectionInfo *last = v->data[v->len - 1];
        address = last->progbits.address + last->shdr->sh_size;
      }
    }
  }

  resolve_relas(ld);

  for (int i = 0; i < ld->nfiles; ++i) {
    File *file = &ld->files[i];
    switch (file->kind) {
    case FK_ELFOBJ:
      add_elfobj_sections(file->elfobj);
      break;
    case FK_ARCHIVE:
      {
        Archive *ar = file->archive;
        for (int i = 0; i < ar->contents->len; i += 2) {
          ArContent *content = ar->contents->data[i + 1];
          add_elfobj_sections(content->elfobj);
        }
      }
      break;
    }
  }

  return true;
}

static bool output_exe(const char *ofn, uintptr_t entry_address) {
  size_t codesz, rodatasz, datasz, bsssz;
  uintptr_t codeloadadr, dataloadadr;
  get_section_size(SEC_CODE, &codesz, &codeloadadr);
  get_section_size(SEC_RODATA, &rodatasz, NULL);
  get_section_size(SEC_DATA, &datasz, &dataloadadr);
  get_section_size(SEC_BSS, &bsssz, NULL);

  int phnum = datasz > 0 || bsssz > 0 ? 2 : 1;

  FILE *fp;
  if (ofn == NULL) {
    fp = stdout;
  } else {
    const int mod = S_IRUSR | S_IWUSR | S_IXUSR | S_IRGRP | S_IXGRP | S_IROTH | S_IXOTH;  // 0755
    const int flag = O_WRONLY | O_CREAT;
    int fd = open(ofn, flag, mod);
    if (fd < 0) {
      perror("open failed");
      return false;
    }
    fp = fdopen(fd, "wb");
    assert(fp != NULL);
  }

  size_t rodata_align = MAX(section_aligns[SEC_RODATA], 1);
  size_t code_rodata_sz = ALIGN(codesz, rodata_align) + rodatasz;
  out_elf_header(fp, entry_address, phnum, 0);
  out_program_header(fp, 0, PROG_START, codeloadadr, code_rodata_sz, code_rodata_sz);
  if (phnum > 1) {
    size_t bss_align = MAX(section_aligns[SEC_BSS], 1);
    size_t datamemsz = ALIGN(datasz, bss_align) + bsssz;
    out_program_header(fp, 1, ALIGN(PROG_START + code_rodata_sz, DATA_ALIGN), dataloadadr, datasz,
                       datamemsz);
  }

  uintptr_t addr = PROG_START;
  put_padding(fp, addr);
  output_section(fp, SEC_CODE);
  addr += codesz;
  if (rodatasz > 0) {
    size_t rodata_align = MAX(section_aligns[SEC_RODATA], 1);
    addr = ALIGN(addr, rodata_align);
    put_padding(fp, addr);
    output_section(fp, SEC_RODATA);
    addr += rodatasz;
  }
  if (datasz > 0) {
    addr = ALIGN(addr, DATA_ALIGN);
    put_padding(fp, addr);
    output_section(fp, SEC_DATA);
    addr += datasz;
  }
  fclose(fp);

  return true;
}

static void dump_map_elfobj(LinkEditor *ld, ElfObj *elfobj, File *file, ArContent *content, FILE *fp) {
  const Name *name;
  Elf64_Sym* sym;
  for (int it = 0; (it = table_iterate(elfobj->symbol_table, it, &name, (void**)&sym)) != -1; ) {
    if (sym->st_shndx == SHN_UNDEF)
      continue;

    uintptr_t address = 0;
    switch (ELF64_ST_BIND(sym->st_info)) {
    case STB_LOCAL:
      {
        const ElfSectionInfo *s = &elfobj->section_infos[sym->st_shndx];
        address = s->progbits.address;
      }
      break;
    case STB_GLOBAL:
      {
        ElfObj *telfobj;
        const Elf64_Sym *tsym = ld_find_symbol(ld, name, &telfobj);
        address = telfobj->section_infos[tsym->st_shndx].progbits.address + tsym->st_value;
      }
      break;
    default: assert(false); break;
    }
    fprintf(fp, "%9lx: %.*s  (%s", address, NAMES(name), file->filename);
    if (content != NULL)
      fprintf(fp, ", %s", content->name);
    fprintf(fp, ")\n");
  }
}

static void dump_map_file(LinkEditor *ld, FILE *fp) {
  for (int i = 0; i < ld->nfiles; ++i) {
    File *file = &ld->files[i];
    switch (file->kind) {
    case FK_ELFOBJ:
      dump_map_elfobj(ld, file->elfobj, file, NULL, fp);
      break;
    case FK_ARCHIVE:
      {
        Archive *ar = file->archive;
        for (int j = 0; j < ar->contents->len; j += 2) {
          ArContent *content = ar->contents->data[j + 1];
          dump_map_elfobj(ld, content->elfobj, file, content, fp);
        }
      }
      break;
    }
  }
}

int main(int argc, char *argv[]) {
  const char *ofn = NULL;
  const char *entry = kDefaultEntryName;
  const char *outmapfn = NULL;

  enum {
    OPT_HELP = 128,
    OPT_VERSION,
    OPT_OUTMAP,
  };

  static const struct option options[] = {
    {"o", required_argument},  // Specify output filename
    {"e", required_argument},  // Entry name
    {"Map", required_argument, OPT_OUTMAP},  // Output map file
    {"-version", no_argument, 'V'},
    {NULL},
  };
  int opt;
  while ((opt = optparse(argc, argv, options)) != -1) {
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
    case OPT_OUTMAP:
      outmapfn = optarg;
      break;
    default:
      fprintf(stderr, "Warning: unknown option: %s\n", argv[optind - 1]);
      break;
    }
  }

  int iarg = optind;
  if (iarg >= argc)
    error("no input");

  if (ofn == NULL)
    ofn = "a.out";

  section_aligns[SEC_DATA] = DATA_ALIGN;

  LinkEditor *ld = malloc_or_die(sizeof(*ld));
  ld_init(ld, argc - iarg);
  for (int i = iarg; i < argc; ++i) {
    char *src = argv[i];
    ld_load(ld, i - iarg, src);
  }

  const Name *entry_name = alloc_name(entry, NULL, false);
  Table unresolved;
  table_init(&unresolved);
  table_put(&unresolved, entry_name, (void*)entry_name);

  bool result = ld_link(ld, &unresolved, LOAD_ADDRESS);
  if (result) {
    fix_section_size(LOAD_ADDRESS);

    ElfObj *telfobj;
    const Elf64_Sym *tsym = ld_find_symbol(ld, entry_name, &telfobj);
    if (tsym == NULL)
      error("Cannot find label: `%.*s'", NAMES(entry_name));
    uintptr_t entry_address = telfobj->section_infos[tsym->st_shndx].progbits.address + tsym->st_value;

    result = output_exe(ofn, entry_address);

    if (outmapfn != NULL && result) {
      FILE *mapfp;
      if (strcmp(outmapfn, "-") == 0) {
        mapfp = stdout;
      } else {
        mapfp = fopen(outmapfn, "w");
        if (mapfp == NULL)
          perror("Failed to open map file");
      }

      fprintf(mapfp, "### Symbols\n");
      fprintf(mapfp, "%9lx:  (start address)\n", (long)LOAD_ADDRESS);
      dump_map_file(ld, mapfp);

      fprintf(mapfp, "\n### Entry point\n");
      fprintf(mapfp, "%9lx: %.*s\n", entry_address, NAMES(entry_name));

      if (mapfp != stdout)
        fclose(mapfp);
    }
  }
  return result ? 0 : 1;
}
