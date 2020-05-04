#include <assert.h>
#include <ctype.h>
#include <stdint.h>  // uintptr_t
#include <stdio.h>
#include <stdlib.h>  // malloc, calloc
#include <string.h>
#include <strings.h>  // strncasecmp
#include <unistd.h>

#include "asm_x86.h"
#include "elfutil.h"
#include "gen.h"
#include "ir_asm.h"
#include "parse_asm.h"
#include "table.h"
#include "util.h"

#define PROG_START   (0x100)

#if defined(__XV6)
// XV6
#include "../kernel/syscall.h"
#include "../kernel/traps.h"

#define START_ADDRESS    0x1000

#elif defined(__linux__)
// Linux

#include <sys/stat.h>

#define START_ADDRESS    (0x01000000 + PROG_START)

#else

#define UNSUPPORTED

#endif

#if defined(UNSUPPORTED)
int main() {
  fprintf(stderr, "AS: unsupported environment\n");
  return 1;
}

#else

#define LOAD_ADDRESS    START_ADDRESS
#define DATA_ALIGN      (0x1000)

void parse_file(FILE *fp, const char *filename, Vector **section_irs, Table *label_table) {
  ParseInfo info;
  info.filename = filename;
  info.lineno = 1;
  for (;; ++info.lineno) {
    char *rawline = NULL;
    size_t capa = 0;
    ssize_t len = getline_(&rawline, &capa, fp, 0);
    if (len == EOF)
      break;
    info.rawline = rawline;

    Vector *irs = section_irs[current_section];
    Line *line = parse_line(&info);
    if (line == NULL)
      continue;

    if (line->label != NULL) {
      vec_push(irs, new_ir_label(line->label));

      if (!add_label_table(label_table, line->label, current_section, true, false))
        err = true;
    }

    if (line->dir == NODIRECTIVE) {
      Code code;
      if (assemble_inst(&line->inst, &info, &code)) {
        if (code.len > 0)
          vec_push(irs, new_ir_code(&code));
      }
    } else {
      handle_directive(&info, line->dir, section_irs, label_table);
    }
  }
}

static void put_padding(FILE *fp, uintptr_t start) {
  long cur = ftell(fp);
  if (start > (size_t)cur) {
    size_t size = start - (uintptr_t)cur;
    char *buf = calloc(1, size);
    fwrite(buf, size, 1, fp);
    free(buf);
  }
}

static void putnum(FILE *fp, unsigned long num, int bytes) {
  for (int i = 0; i < bytes; ++i) {
    fputc(num, fp);
    num >>= 8;
  }
}

static void drop_all(FILE *fp) {
  for (;;) {
    char buf[4096];
    size_t size = fread(buf, 1, sizeof(buf), fp);
    if (size < sizeof(buf))
      break;
  }
}

static int output_obj(const char *ofn, Table *label_table, Vector *unresolved) {
  size_t codesz, rodatasz, datasz, bsssz;
  get_section_size(SEC_CODE, &codesz, NULL);
  get_section_size(SEC_RODATA, &rodatasz, NULL);
  get_section_size(SEC_DATA, &datasz, NULL);
  get_section_size(SEC_BSS, &bsssz, NULL);

  // Construct symtab and strtab.
  Symtab symtab;
  symtab_init(&symtab);
  {
    // UND
    Elf64_Sym *sym;
    sym = symtab_add(&symtab, alloc_name("", NULL, false));
    sym->st_info = ELF64_ST_INFO(STB_LOCAL, STT_NOTYPE);
    // SECTION
    for (int i = 0; i < 4; ++i) {
      sym = symtab_add(&symtab, alloc_name("", NULL, false));
      sym->st_info = ELF64_ST_INFO(STB_LOCAL, STT_SECTION);
      sym->st_shndx = i + 1;  // Section index.
    }

    // Label symbols
    const Name *name;
    LabelInfo *info;
    for (int it = 0; (it = table_iterate(label_table, it, &name, (void**)&info)) != -1; ) {
      if (!(info->flag & LF_GLOBAL) || !(info->flag & LF_DEFINED))
        continue;
      sym = symtab_add(&symtab, name);
      sym->st_info = ELF64_ST_INFO(STB_GLOBAL, STT_NOTYPE);
      sym->st_value = info->address - section_start_addresses[info->section];
      sym->st_shndx = info->section + 1;  // Symbol index for Local section.
    }
  }

  FILE *ofp;
  if (ofn == NULL) {
    ofp = stdout;
  } else {
    ofp = fopen(ofn, "wb");
    if (ofp == NULL) {
      fprintf(stderr, "Failed to open output file: %s\n", ofn);
      if (!isatty(STDIN_FILENO))
        drop_all(stdin);
      return 1;
    }
  }

  uintptr_t entry = 0;
  int phnum = 0;
  int shnum = 11;
  out_elf_header(ofp, entry, phnum, shnum);

  uintptr_t addr = sizeof(Elf64_Ehdr);
  uintptr_t code_ofs = addr;
  output_section(ofp, SEC_CODE);
  uintptr_t rodata_ofs = addr += codesz;
  if (rodatasz > 0) {
    rodata_ofs = ALIGN(rodata_ofs, 0x10);
    put_padding(ofp, rodata_ofs);
    output_section(ofp, SEC_RODATA);
    addr = rodata_ofs + rodatasz;
  }
  uintptr_t data_ofs = addr;
  if (datasz > 0) {
    data_ofs = ALIGN(data_ofs, 0x10);
    put_padding(ofp, data_ofs);
    output_section(ofp, SEC_DATA);
    addr = data_ofs + datasz;
  }
  uintptr_t bss_ofs = addr;
  if (bsssz > 0) {
    bss_ofs = ALIGN(bss_ofs, 0x10);
    put_padding(ofp, bss_ofs);
    addr = bss_ofs;
  }

  int rela_counts[SECTION_COUNT];
  memset(rela_counts, 0x00, sizeof(rela_counts));
  for (int i = 0; i < unresolved->len; ++i) {
    UnresolvedInfo *u = unresolved->data[i];
    assert(u->src_section >= 0 && u->src_section < SECTION_COUNT);
    ++rela_counts[u->src_section];
  }

  Elf64_Rela *rela_bufs[SECTION_COUNT];
  for (int i = 0; i < SECTION_COUNT; ++i) {
    int count = rela_counts[i];
    rela_bufs[i] = count <= 0 ? NULL : calloc(count, sizeof(*rela_bufs[0]));
  }
  memset(rela_counts, 0x00, sizeof(rela_counts));  // Reset count.

  for (int i = 0; i < unresolved->len; ++i) {
    UnresolvedInfo *u = unresolved->data[i];
    Elf64_Rela *rela = &rela_bufs[u->src_section][rela_counts[u->src_section]++];
    switch (u->kind) {
    case UNRES_EXTERN:
    case UNRES_EXTERN_PC32:
      {
        Elf64_Sym *sym = symtab_add(&symtab, u->label);
        sym->st_info = ELF64_ST_INFO(STB_GLOBAL, STT_NOTYPE);
        size_t index = sym - symtab.buf;

        rela->r_offset = u->offset;
        rela->r_info = ELF64_R_INFO(index, u->kind == UNRES_EXTERN_PC32 ? R_X86_64_PC32 : R_X86_64_PLT32);
        rela->r_addend = u->add;
      }
      break;
    case UNRES_OTHER_SECTION:
      {
        LabelInfo *label = table_get(label_table, u->label);
        assert(label != NULL);
        int rodata_index = label->section + 1;  // Symtab index for .rodata section = section number + 1
        rela->r_offset = u->offset;
        rela->r_info = ELF64_R_INFO(rodata_index, R_X86_64_PC32);
        rela->r_addend = u->add;
      }
      break;
    case UNRES_ABS64:
      {
        LabelInfo *label = table_get(label_table, u->label);
        assert(label != NULL);
        if (label->flag & LF_GLOBAL) {
          Elf64_Sym *sym = symtab_add(&symtab, u->label);
          sym->st_info = ELF64_ST_INFO(STB_GLOBAL, STT_NOTYPE);
          size_t index = sym - symtab.buf;

          rela->r_offset = u->offset;
          rela->r_info = ELF64_R_INFO(index, R_X86_64_64);
          rela->r_addend = u->add;
        } else {
          rela->r_offset = u->offset;
          rela->r_info = ELF64_R_INFO(label->section + 1, R_X86_64_64);
          rela->r_addend = u->add + (label->address - section_start_addresses[label->section]);
        }
      }
      break;
    default: assert(false); break;
    }
  }

  uintptr_t rela_ofss[SECTION_COUNT];
  for (int i = 0; i < SEC_BSS; ++i) {
    rela_ofss[i] = addr = ALIGN(addr, 0x10);
    put_padding(ofp, addr);
    if (rela_counts[i] > 0) {
      fwrite(rela_bufs[i], sizeof(*rela_bufs[i]), rela_counts[i], ofp);
      addr += sizeof(*rela_bufs[i]) * rela_counts[i];
    }
  }

  uintptr_t symtab_ofs = addr + unresolved->len * sizeof(Elf64_Rela);
  put_padding(ofp, symtab_ofs);
  fwrite(symtab.buf, sizeof(*symtab.buf), symtab.count, ofp);

  uintptr_t strtab_ofs = symtab_ofs + sizeof(*symtab.buf) * symtab.count;
  fwrite(strtab_dump(&symtab.strtab), symtab.strtab.size, 1, ofp);

  // Set up shstrtab.
  Strtab shstrtab;
  strtab_init(&shstrtab);

  // Output section headers.
  {
    Elf64_Shdr nulsec = {
      .sh_name = strtab_add(&shstrtab, alloc_name("", NULL, false)),
      .sh_type = SHT_NULL,
      .sh_addralign = 1,
    };
    Elf64_Shdr textsec = {
      .sh_name = strtab_add(&shstrtab, alloc_name(".text", NULL, false)),
      .sh_type = SHT_PROGBITS,
      .sh_flags = SHF_EXECINSTR | SHF_ALLOC,
      .sh_addr = 0,
      .sh_offset = code_ofs,
      .sh_size = codesz,
      .sh_link = 0,
      .sh_info = 0,
      .sh_addralign = MAX(section_aligns[SEC_CODE], 1),
      .sh_entsize = 0,
    };
    Elf64_Shdr rodatasec = {
      .sh_name = strtab_add(&shstrtab, alloc_name(".rodata", NULL, false)),
      .sh_type = SHT_PROGBITS,
      .sh_flags = SHF_ALLOC,
      .sh_addr = 0,
      .sh_offset = rodata_ofs,
      .sh_size = rodatasz,
      .sh_link = 0,
      .sh_info = 0,
      .sh_addralign = MAX(section_aligns[SEC_RODATA], 1),
      .sh_entsize = 0,
    };
    Elf64_Shdr datasec = {
      .sh_name = strtab_add(&shstrtab, alloc_name(".data", NULL, false)),
      .sh_type = SHT_PROGBITS,
      .sh_flags = SHF_WRITE | SHF_ALLOC,
      .sh_addr = 0,
      .sh_offset = data_ofs,
      .sh_size = datasz,
      .sh_link = 0,
      .sh_info = 0,
      .sh_addralign = MAX(section_aligns[SEC_DATA], 1),
      .sh_entsize = 0,
    };
    Elf64_Shdr bsssec = {
      .sh_name = strtab_add(&shstrtab, alloc_name(".bss", NULL, false)),
      .sh_type = SHT_NOBITS,
      .sh_flags = SHF_WRITE | SHF_ALLOC,
      .sh_addr = 0,
      .sh_offset = bss_ofs,
      .sh_size = bsssz,
      .sh_link = 0,
      .sh_info = 0,
      .sh_addralign = MAX(section_aligns[SEC_BSS], 1),
      .sh_entsize = 0,
    };
    Elf64_Shdr relatextsec = {
      .sh_name = strtab_add(&shstrtab, alloc_name(".rela.text", NULL, false)),
      .sh_type = SHT_RELA,
      .sh_flags = SHF_INFO_LINK,
      .sh_addr = 0,
      .sh_offset = rela_ofss[SEC_CODE],
      .sh_size = sizeof(Elf64_Rela) * rela_counts[SEC_CODE],
      .sh_link = 9,  // Index of symtab
      .sh_info = 1,  // Index of text
      .sh_addralign = 8,
      .sh_entsize = sizeof(Elf64_Rela),
    };
    Elf64_Shdr relarodatasec = {
      .sh_name = strtab_add(&shstrtab, alloc_name(".rela.rodata", NULL, false)),
      .sh_type = SHT_RELA,
      .sh_flags = SHF_INFO_LINK,
      .sh_addr = 0,
      .sh_offset = rela_ofss[SEC_RODATA],
      .sh_size = sizeof(Elf64_Rela) * rela_counts[SEC_RODATA],
      .sh_link = 9,  // Index of symtab
      .sh_info = 2,  // Index of rodata
      .sh_addralign = 8,
      .sh_entsize = sizeof(Elf64_Rela),
    };
    Elf64_Shdr reladatasec = {
      .sh_name = strtab_add(&shstrtab, alloc_name(".rela.data", NULL, false)),
      .sh_type = SHT_RELA,
      .sh_flags = SHF_INFO_LINK,
      .sh_addr = 0,
      .sh_offset = rela_ofss[SEC_DATA],
      .sh_size = sizeof(Elf64_Rela) * rela_counts[SEC_DATA],
      .sh_link = 9,  // Index of symtab
      .sh_info = 3,  // Index of data
      .sh_addralign = 8,
      .sh_entsize = sizeof(Elf64_Rela),
    };
    Elf64_Shdr strtabsec = {
      .sh_name = strtab_add(&shstrtab, alloc_name(".strtab", NULL, false)),
      .sh_type = SHT_STRTAB,
      .sh_flags = 0,
      .sh_addr = 0,
      .sh_offset = strtab_ofs,
      .sh_size = symtab.strtab.size,
      .sh_link = 0,
      .sh_info = 0,
      .sh_addralign = 1,
      .sh_entsize = 0,
    };
    Elf64_Shdr symtabsec = {
      .sh_name = strtab_add(&shstrtab, alloc_name(".symtab", NULL, false)),
      .sh_type = SHT_SYMTAB,
      .sh_flags = 0,
      .sh_addr = 0,
      .sh_offset = symtab_ofs,
      .sh_size = sizeof(*symtab.buf) * symtab.count,
      .sh_link = 8,  // Index of strtab
      .sh_info = 5,  // Number of local symbols
      .sh_addralign = 8,
      .sh_entsize = sizeof(Elf64_Sym),
    };
    Elf64_Shdr shstrtabsec = {
      .sh_name = strtab_add(&shstrtab, alloc_name(".shstrtab", NULL, false)),
      .sh_type = SHT_STRTAB,
      .sh_flags = 0,
      .sh_addr = 0,
      .sh_offset = 0,  // Dummy
      .sh_size = 0,    // Dummy
      .sh_link = 0,
      .sh_info = 0,
      .sh_addralign = 1,
      .sh_entsize = 0,
    };

    long shstrtab_ofs;
    {
      void *buf = strtab_dump(&shstrtab);
      long cur = ftell(ofp);
      shstrtab_ofs = ALIGN(cur, 0x10);
      put_padding(ofp, shstrtab_ofs);
      fwrite(buf, shstrtab.size, 1, ofp);
    }
    shstrtabsec.sh_offset = shstrtab_ofs;
    shstrtabsec.sh_size = shstrtab.size;

    long cur = ftell(ofp);
    long sh_ofs = ALIGN(cur, 0x10);
    put_padding(ofp, sh_ofs);

    fwrite(&nulsec, sizeof(nulsec), 1, ofp);
    fwrite(&textsec, sizeof(textsec), 1, ofp);
    fwrite(&rodatasec, sizeof(rodatasec), 1, ofp);
    fwrite(&datasec, sizeof(datasec), 1, ofp);
    fwrite(&bsssec, sizeof(bsssec), 1, ofp);
    fwrite(&relatextsec, sizeof(relatextsec), 1, ofp);
    fwrite(&relarodatasec, sizeof(relarodatasec), 1, ofp);
    fwrite(&reladatasec, sizeof(reladatasec), 1, ofp);
    fwrite(&strtabsec, sizeof(strtabsec), 1, ofp);
    fwrite(&symtabsec, sizeof(symtabsec), 1, ofp);
    fwrite(&shstrtabsec, sizeof(shstrtabsec), 1, ofp);

    // Write section table offset.
    fseek(ofp, 0x28, SEEK_SET);
    putnum(ofp, sh_ofs, 8);
  }

  return 0;
}

static int output_exe(const char *ofn, Table *label_table) {
  size_t codesz, rodatasz, datasz, bsssz;
  uintptr_t codeloadadr, dataloadadr;
  get_section_size(SEC_CODE, &codesz, &codeloadadr);
  get_section_size(SEC_RODATA, &rodatasz, NULL);
  get_section_size(SEC_DATA, &datasz, &dataloadadr);
  get_section_size(SEC_BSS, &bsssz, NULL);

  LabelInfo *entry = table_get(label_table, alloc_name("_start", NULL, false));
  if (entry == NULL)
    error("Cannot find label: `%s'", "_start");

  int phnum = datasz > 0 || bsssz > 0 ? 2 : 1;

  FILE *fp;
  if (ofn == NULL) {
    fp = stdout;
  } else {
    fp = fopen(ofn, "wb");
    if (fp == NULL) {
      fprintf(stderr, "Failed to open output file: %s\n", ofn);
      if (!isatty(STDIN_FILENO))
        drop_all(stdin);
      return 1;
    }
  }

  size_t rodata_align = MAX(section_aligns[SEC_RODATA], 1);
  size_t code_rodata_sz = ALIGN(codesz, rodata_align) + rodatasz;
  out_elf_header(fp, entry->address, phnum, 0);
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

#if !defined(__XV6) && defined(__linux__)
  if (chmod(ofn, 0755) == -1) {
    perror("chmod failed\n");
    return 1;
  }
#endif
  return 0;
}

// ================================================

int main(int argc, char *argv[]) {
  const char *ofn = NULL;
  bool out_obj = false;
  int iarg;

  for (iarg = 1; iarg < argc; ++iarg) {
    char *arg = argv[iarg];
    if (*arg != '-')
      break;

    if (strcmp(arg, "-c") == 0) {
      out_obj = true;
    } else if (starts_with(arg, "-o")) {
      ofn = strdup_(arg + 2);
    } else if (strcmp(arg, "--version") == 0) {
      show_version("as");
      return 0;
    } else {
      fprintf(stderr, "Unknown option: %s\n", arg);
      return 1;
    }
  }

  if (ofn == NULL) {
    if (out_obj) {
      if (iarg < argc)
        ofn = change_ext(argv[iarg], "o");
    } else {
      ofn = "a.out";
    }
  }

  // ================================================
  // Run own assembler

  Vector *section_irs[SECTION_COUNT];
  Table label_table;
  table_init(&label_table);
  for (int i = 0; i < SECTION_COUNT; ++i)
    section_irs[i] = new_vector();

  if (iarg < argc) {
    for (int i = iarg; i < argc; ++i) {
      FILE *fp = fopen(argv[i], "r");
      if (fp == NULL)
        error("Cannot open %s\n", argv[i]);
      parse_file(fp, argv[i], section_irs, &label_table);
      fclose(fp);
      if (err)
        break;
    }
  } else {
    parse_file(stdin, "*stdin*", section_irs, &label_table);
  }

  if (!out_obj)
    section_aligns[SEC_DATA] = DATA_ALIGN;

  Vector *unresolved = out_obj ? new_vector() : NULL;
  if (!err) {
    do {
      calc_label_address(LOAD_ADDRESS, section_irs, &label_table);
    } while (!resolve_relative_address(section_irs, &label_table, unresolved));
    emit_irs(section_irs, &label_table);
  }

  if (err) {
    return 1;
  }

  fix_section_size(LOAD_ADDRESS);

  return out_obj ? output_obj(ofn, &label_table, unresolved) : output_exe(ofn, &label_table);
}
#endif
