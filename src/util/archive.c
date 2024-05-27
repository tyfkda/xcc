#include "archive.h"

#include <assert.h>
#include <stdlib.h>  // strtoul
#include <string.h>

// https://sourceware.org/git/?p=binutils-gdb.git;a=blob;f=include/aout/ar.h;h=471a859fc57c7d8207193718610980f6bf2f83b3;hb=2cb5c79dad39dd438fb0f7372ac04cf5aa2a7db7
#include "../ar/ar.h"

#include "util.h"

// 4bytes big endian
static uint32_t read4be(FILE *fp) {
  unsigned char buf[4];
  read_or_die(fp, buf, sizeof(buf), "read4be");
  return (buf[0] << 24) | (buf[1] << 16) | (buf[2] << 8) | buf[3];
}

Archive *load_archive(const char *filename) {
  FILE *fp;
  if (!is_file(filename) || (fp = fopen(filename, "rb")) == NULL)
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
