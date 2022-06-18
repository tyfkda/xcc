#include "archive.h"

#include <assert.h>
#include <stdlib.h>  // strtoul
#include <string.h>

// https://sourceware.org/git/?p=binutils-gdb.git;a=blob;f=include/aout/ar.h;h=471a859fc57c7d8207193718610980f6bf2f83b3;hb=2cb5c79dad39dd438fb0f7372ac04cf5aa2a7db7
#include <ar.h>

#include "elfobj.h"
#include "util.h"

static void read_or_die(FILE *fp, void *buf, size_t size, const char *msg) {
  size_t count = fread(buf, size, 1, fp);
  if (count != 1)
    error(msg);
}

// 4bytes big endian
static uint32_t read4be(FILE *fp) {
  unsigned char buf[4];
  read_or_die(fp, buf, sizeof(buf), "read4be");
  return (buf[0] << 24) | (buf[1] << 16) | (buf[2] << 8) | buf[3];
}

Archive *load_archive(const char *filename) {
  FILE *fp = fopen(filename, "r");
  if (fp == NULL)
    return NULL;

  Archive *ar = malloc_or_die(sizeof(*ar));
  ar->fp = fp;
  ar->symbol_count = 0;
  ar->symbols = NULL;
  table_init(&ar->symbol_table);
  ar->contents = new_vector();

  char mag[SARMAG];
  read_or_die(fp, mag, sizeof(mag), "Magic");
  if (memcmp(mag, ARMAG, sizeof(mag)) != 0)
    error("Magic expected");

  struct ar_hdr ghdr;
  read_or_die(fp, &ghdr, sizeof(ghdr), "Global header");
  if (memcmp(ghdr.ar_fmag, ARFMAG, sizeof(ghdr.ar_fmag)) != 0)
    error("FMagic expected");

  uint32_t symbol_count = read4be(fp);
  ar->symbol_count = symbol_count;
  if (symbol_count > 0) {
    ArSymbol *symbols = malloc_or_die(sizeof(*symbols) * symbol_count);
    ar->symbols = symbols;
    for (uint32_t i = 0; i < symbol_count; ++i) {
      symbols[i].offset = read4be(fp);
    }

    size_t pos = ftell(fp);
    assert(pos < symbols[0].offset);
    size_t strtablen = symbols[0].offset - pos;
    char *strtab = malloc_or_die(strtablen);  // Buffer pointer is not kept.
    read_or_die(fp, strtab, strtablen, "Strtab");
    char *p = strtab;
    for (uint32_t i = 0; i < symbol_count; ++i) {
      symbols[i].ident = p;
      char *q = memchr(p, '\0', &strtab[strtablen] - p);
      if (q == NULL) {
        error("Illegal strtab");
        return false;
      }
      p = q + 1;
    }

    for (uint32_t i = 0; i < symbol_count; ++i) {
      ArSymbol *symbol = &symbols[i];
      const Name *name = alloc_name(symbol->ident, NULL, false);
      table_put(&ar->symbol_table, name, symbol);
    }
  }
  return ar;
}

ElfObj *load_archive_elfobj(Archive *ar, uint32_t offset) {
  Vector *contents = ar->contents;
  for (int i = 0; i < contents->len; i += 2) {
    if ((intptr_t)contents->data[i] == offset) {
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
  if (p == NULL) p = &content->name[sizeof(hdr.ar_name)];
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
    return NULL;
  }

  vec_push(contents, (void*)(intptr_t)offset);
  vec_push(contents, content);

  return elfobj;
}
