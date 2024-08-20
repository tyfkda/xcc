#include "archive.h"

#include <assert.h>
#include <stdlib.h>  // strtoul
#include <string.h>

// https://sourceware.org/git/?p=binutils-gdb.git;a=blob;f=include/aout/ar.h;h=471a859fc57c7d8207193718610980f6bf2f83b3;hb=2cb5c79dad39dd438fb0f7372ac04cf5aa2a7db7
#include <ar.h>

#include "util.h"

// 4bytes big endian
static uint32_t read4be(FILE *fp) {
  unsigned char buf[4];
  read_or_die(fp, buf, sizeof(buf), "read4be");
  return (buf[0] << 24) | (buf[1] << 16) | (buf[2] << 8) | buf[3];
}

static uint32_t *read_file_offsets(FILE *fp, uint32_t symbol_count) {
  uint32_t *file_offsets = malloc_or_die(sizeof(*file_offsets) * symbol_count);
  read_or_die(fp, file_offsets, sizeof(*file_offsets) * symbol_count, "Offsets");
  // Convert big endian to machine endian.
  uint32_t *p = file_offsets;
  for (uint32_t i = 0; i < symbol_count; ++i) {
    unsigned char *q = (unsigned char*)p;
    uint32_t offset = (q[0] << 24) | (q[1] << 16) | (q[2] << 8) | q[3];
    *p++ = offset;
  }
  return file_offsets;
}

static int compare_uint32(const void *a, const void *b) {
  uint32_t x = *(const uint32_t*)a;
  ArContent *content = (ArContent*)b;
  uint32_t y = content->file_offset;
  return x < y ? -1 : x > y ? 1 : 0;
}

static ArContent *allocate_contents_buffer(const uint32_t *file_offsets, uint32_t symbol_count,
                                           size_t *plen) {
  uint32_t *offsets = NULL;
  ssize_t len = 0, capa = 0;

  // Make file offsets unique.
  for (uint32_t i = 0; i < symbol_count; ++i) {
    uint32_t value = file_offsets[i];
    ssize_t lo = -1, hi = len;
    while (hi - lo > 1) {
      ssize_t m = lo + ((hi - lo) >> 1);
      if (offsets[m] < value)
        lo = m;
      else
        hi = m;
    }

    if (hi >= len || offsets[hi] != value) {
      if (capa <= len) {
        capa <<= 1;
        if (capa <= 0)
          capa = 8;
        offsets = realloc_or_die(offsets, sizeof(*offsets) * capa);
      }
      memmove(&offsets[hi + 1], &offsets[hi], (len - hi) * sizeof(*offsets));
      offsets[hi] = value;
      ++len;
    }
  }

  ArContent *contents = calloc_or_die(sizeof(*contents) * len);
  for (ssize_t i = 0; i < len; ++i) {
    ArContent *content = &contents[i];
    content->obj = NULL;
    content->file_offset = offsets[i];
  }
  free(offsets);

  *plen = len;
  return contents;
}

Archive *load_archive(const char *filename) {
  FILE *fp;
  if (!is_file(filename) || (fp = fopen(filename, "rb")) == NULL)
    return NULL;

  Archive *ar = calloc_or_die(sizeof(*ar));
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
    uint32_t *file_offsets = read_file_offsets(fp, symbol_count);
    size_t content_count;
    ArContent *contents = allocate_contents_buffer(file_offsets, symbol_count, &content_count);

    ArSymbol *symbols = malloc_or_die(sizeof(*symbols) * symbol_count);
    ar->symbols = symbols;
    for (uint32_t i = 0; i < symbol_count; ++i) {
      ArSymbol *symbol = &symbols[i];
      uint32_t value = file_offsets[i];
      ArContent *p = bsearch(&value, contents, content_count, sizeof(*contents), compare_uint32);
      assert(p != NULL);
      uint32_t index = p - contents;
      symbol->content = &contents[index];
    }

    size_t pos = ftell(fp);
    assert(pos < contents[0].file_offset);
    size_t strtablen = contents[0].file_offset - pos;
    char *strtab = malloc_or_die(strtablen);  // Buffer pointer is not kept.
    read_or_die(fp, strtab, strtablen, "Strtab");
    char *p = strtab;
    for (uint32_t i = 0; i < symbol_count; ++i) {
      char *q = memchr(p, '\0', &strtab[strtablen] - p);
      if (q == NULL)
        error("Illegal strtab");

      ArSymbol *symbol = &symbols[i];
      const Name *name = alloc_name(p, q, false);
      table_put(&ar->symbol_table, name, symbol);

      p = q + 1;
    }

    free(file_offsets);
  }
  return ar;
}

void *load_archive_content(Archive *ar, ArSymbol *symbol,
                           void *(*load)(FILE*, const char*, size_t)) {
  ArContent *content = symbol->content;
  if (content->obj != NULL)
    return content->obj;

  fseek(ar->fp, content->file_offset, SEEK_SET);

  struct ar_hdr hdr;
  read_or_die(ar->fp, &hdr, sizeof(hdr), "hdr");
  if (memcmp(hdr.ar_fmag, ARFMAG, sizeof(hdr.ar_fmag)) != 0)
    error("Malformed archive");

  memcpy(content->name, hdr.ar_name, sizeof(hdr.ar_name));
  char *p = memchr(content->name, '/', sizeof(hdr.ar_name));
  if (p != NULL)
    *p = '\0';

  char sizestr[sizeof(hdr.ar_size) + 1];
  memcpy(sizestr, hdr.ar_size, sizeof(hdr.ar_size));
  sizestr[sizeof(hdr.ar_size)] = '\0';
  content->size = strtoul(sizestr, NULL, 10);

  void *obj = (*load)(ar->fp, content->name, content->size);
  if (obj == NULL) {
    error("Failed to extract .o: %.*s", (int)sizeof(content->name), content->name);
  }
  content->obj = obj;
  vec_push(ar->contents, content);

  return obj;
}
