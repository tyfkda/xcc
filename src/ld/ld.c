#include <assert.h>
#include <fcntl.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <strings.h>

#include "archive.h"
#include "elfobj.h"
#include "elfutil.h"
#include "gen_section.h"
#include "table.h"
#include "util.h"

static const char kDefaultEntryName[] = "_start";

#define PROG_START   (0x100)

#if defined(__XV6)
// XV6
#include "../kernel/syscall.h"
#include "../kernel/traps.h"

#define START_ADDRESS    0x1000

#else
// *nix

#include <sys/stat.h>

#define START_ADDRESS    (0x01000000 + PROG_START)

#endif

#define LOAD_ADDRESS    START_ADDRESS
#define DATA_ALIGN      (0x1000)

//

typedef struct {
  enum {
    FK_ELFOBJ,
    FK_ARCHIVE,
  } kind;
  union {
    ElfObj *elfobj;
    Archive *archive;
  };
} File;

//

static Elf64_Sym *find_symbol_from_all(File *files, int nfiles, const Name *name,
                                       ElfObj **pelfobj) {
  for (int i = 0; i < nfiles; ++i) {
    File *file = &files[i];
    switch (file->kind) {
    case FK_ELFOBJ:
      {
        ElfObj *elfobj = file->elfobj;
        Elf64_Sym *sym = elfobj_find_symbol(elfobj, name);
        if (sym != NULL && sym->st_shndx != 0) {
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
          if (sym != NULL && sym->st_shndx != 0) {
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

static void resolve_rela_elfobj(ElfObj *elfobj, File *files, int nfiles) {
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
      const Elf64_Sym *sym = &symhdrinfo->symtab.symtabs[ELF64_R_SYM(rela->r_info)];

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
          const Elf64_Sym *tsym = find_symbol_from_all(files, nfiles,
                                                       alloc_name(label, NULL, false), &telfobj);
          assert(tsym != NULL && tsym->st_shndx > 0);
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

static void link_elfobj(ElfObj *elfobj, File *files, int nfiles, uintptr_t *offsets,
                        Vector **progbit_sections, Table *unresolved) {
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

        uintptr_t address = ALIGN(offsets[secno], align);
        ElfSectionInfo *p = &elfobj->section_infos[sec];
        p->progbits.address = address;
        p->progbits.content = buf;
        vec_push(progbit_sections[secno], p);
        offsets[secno] = address + size;
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

        uintptr_t address = ALIGN(offsets[SEC_BSS], align);
        ElfSectionInfo *p = &elfobj->section_infos[sec];
        p->progbits.address = address;
        p->progbits.content = NULL;
        vec_push(progbit_sections[SEC_BSS], p);
        offsets[SEC_BSS] = address + size;
      }
      break;
    case SHT_SYMTAB:
      {
        ElfSectionInfo *p = &elfobj->section_infos[sec];
        ElfSectionInfo *q = &elfobj->section_infos[shdr->sh_link];  // Strtab
        Table *names = p->symtab.names;
        const char *str = q->strtab.buf;
        const Name *name;
        Elf64_Sym *sym;
        for (int it = 0; (it = table_iterate(names, it, &name, (void**)&sym)) != -1; ) {
          unsigned char type = ELF64_ST_TYPE(sym->st_info);
          if (type != STT_NOTYPE || str[sym->st_name] == '\0')
            continue;
          const Name *name = alloc_name(&str[sym->st_name], NULL, false);
          if (sym->st_shndx == 0) {
            if (find_symbol_from_all(files, nfiles, name, NULL) == NULL)
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

static void link_archive(Archive *ar, File *files, int nfiles, uintptr_t *offsets,
                         Vector **progbit_sections, Table *unresolved) {
  Table *table = &ar->symbol_table;
  const Name *name;
  void *dummy;
  for (;;) {
    bool retry = false;
    for (int it = 0; (it = table_iterate(unresolved, it, &name, &dummy)) != -1;) {
      ArSymbol *symbol;
      if (!table_try_get(table, name, (void**)&symbol))
        continue;
      table_delete(unresolved, name);

      ElfObj *elfobj = load_archive_elfobj(ar, symbol->offset);
      if (elfobj != NULL) {
        link_elfobj(elfobj, files, nfiles, offsets, progbit_sections, unresolved);
        retry = true;
        break;
      }
    }
    if (!retry)
      break;
  }
}

static void resolve_rela_archive(Archive *ar, File *files, int nfiles) {
  for (int i = 0; i < ar->contents->len; i += 2) {
    ArContent *content = ar->contents->data[i + 1];
    resolve_rela_elfobj(content->elfobj, files, nfiles);
  }
}

static void resolve_relas(File *files, int nfiles) {
  for (int i = 0; i < nfiles; ++i) {
    File *file = &files[i];
    switch (file->kind) {
    case FK_ELFOBJ:
      resolve_rela_elfobj(file->elfobj, files, nfiles);
      break;
    case FK_ARCHIVE:
      resolve_rela_archive(file->archive, files, nfiles);
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

static bool link_files(File *files, int nfiles, const Name *entry, uintptr_t start_address) {
  Table unresolved;
  table_init(&unresolved);
  table_put(&unresolved, entry, (void*)entry);

  uintptr_t offsets[SEC_BSS + 1];
  Vector *progbit_sections[SEC_BSS + 1];
  for (int secno = 0; secno < SEC_BSS + 1; ++secno) {
    offsets[secno] = 0;
    progbit_sections[secno] = new_vector();
  }

  for (int i = 0; i < nfiles; ++i) {
    File *file = &files[i];
    switch (file->kind) {
    case FK_ELFOBJ:
      link_elfobj(file->elfobj, files, nfiles, offsets, progbit_sections, &unresolved);
      break;
    case FK_ARCHIVE:
      link_archive(file->archive, files, nfiles, offsets, progbit_sections, &unresolved);
      break;
    }
  }

  if (unresolved.count > 0) {
    fprintf(stderr, "Unresolved: #%d\n", unresolved.count);
    const Name *name;
    void *dummy;
    for (int it = 0; (it = table_iterate(&unresolved, it, &name, &dummy)) != -1;) {
      fprintf(stderr, "  %.*s\n", NAMES(name));
    }
    return false;
  }

  // Calculate address.
  {
    uintptr_t address = start_address;
    for (int secno = 0; secno < SEC_BSS + 1; ++secno) {
      address = ALIGN(address, section_aligns[secno]);
      Vector *v = progbit_sections[secno];
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

  resolve_relas(files, nfiles);

  for (int i = 0; i < nfiles; ++i) {
    File *file = &files[i];
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

static bool output_exe(const char *ofn, File *files, int nfiles, const Name *entry) {
  size_t codesz, rodatasz, datasz, bsssz;
  uintptr_t codeloadadr, dataloadadr;
  get_section_size(SEC_CODE, &codesz, &codeloadadr);
  get_section_size(SEC_RODATA, &rodatasz, NULL);
  get_section_size(SEC_DATA, &datasz, &dataloadadr);
  get_section_size(SEC_BSS, &bsssz, NULL);

  ElfObj *telfobj;
  const Elf64_Sym *tsym = find_symbol_from_all(files, nfiles, entry, &telfobj);
  if (tsym == NULL)
    error("Cannot find label: `%.*s'", NAMES(entry));
  uintptr_t entry_address = telfobj->section_infos[tsym->st_shndx].progbits.address +
                            tsym->st_value;

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

int main(int argc, char *argv[]) {
  const char *ofn = NULL;
  const char *entry = kDefaultEntryName;

  static const struct option options[] = {
    {"o", required_argument},  // Specify output filename
    {"e", required_argument},  // Entry name
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

  int nfiles = 0;
  File *files = malloc_or_die(sizeof(*files) * (argc - iarg));
  for (int i = iarg; i < argc; ++i) {
    char *src = argv[i];
    char *ext = get_ext(src);
    File *file = &files[nfiles];
    if (strcasecmp(ext, "o") == 0) {
      ElfObj *elfobj = malloc_or_die(sizeof(*elfobj));
      elfobj_init(elfobj);
      if (!open_elf(src, elfobj)) {
        close_elf(file->elfobj);
        free(elfobj);
        exit(1);
      }
      file->kind = FK_ELFOBJ;
      file->elfobj = elfobj;
    } else if (strcasecmp(ext, "a") == 0) {
      Archive *archive = load_archive(src);
      if (archive == NULL) {
        fprintf(stderr, "load failed: %s\n", src);
        exit(1);
      }
      file->kind = FK_ARCHIVE;
      file->archive = archive;
    } else {
      error("Unsupported file: %s", src);
    }
    ++nfiles;
  }

  const Name *entry_name = alloc_name(entry, NULL, false);
  bool result = link_files(files, nfiles, entry_name, LOAD_ADDRESS);
  if (result) {
    fix_section_size(LOAD_ADDRESS);
    result = output_exe(ofn, files, nfiles, entry_name);
  }

  for (int i = 0; i < nfiles; ++i) {
    File *file = &files[i];
    switch (file->kind) {
    case FK_ELFOBJ:
      close_elf(file->elfobj);
      break;
    case FK_ARCHIVE:
      break;
    }
  }
  return result ? 0 : 1;
}
