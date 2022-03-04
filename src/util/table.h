// Hash Table

#pragma once

#include <stdbool.h>
#include <stdint.h>  // uint32_t

// Name

typedef struct Name {
  const char *chars;
  int bytes;
  uint32_t hash;
} Name;

const Name *alloc_name(const char *begin, const char *end, bool make_copy);
bool equal_name(const Name *name1, const Name *name2);

// Hash Table

typedef struct TableEntry {
  const Name *key;
  void *value;
} TableEntry;

typedef struct Table {
  TableEntry *entries;
  int capacity;
  int count;
  int used;  // Include tombstone count.
} Table;

Table *alloc_table(void);
void table_init(Table *table);
void *table_get(Table *table, const Name *key);
bool table_try_get(Table *table, const Name *key, void **output);
bool table_put(Table *table, const Name *key, void *value);
bool table_delete(Table *table, const Name *key);
int table_iterate(Table *table, int iterator, const Name **name, void **value);  // -1 => end
