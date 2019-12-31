#include "table.h"

#include <stdlib.h>  // malloc
#include <string.h>

// Hash

static uint32_t hash_string(const char* key, int length) {
  // FNV1a
  uint32_t hash = 2166136261u;
  for (int i = 0; i < length; ++i)
    hash = (hash ^ key[i]) * 16777619u;
  return hash;
}

// Name

static Table name_table;

static const Name *find_name_table(const char *chars, int bytes, uint32_t hash) {
  const Table *table = &name_table;;
  if (table->count == 0)
    return NULL;

  for (uint32_t index = hash % table->capacity; ; index = (index + 1) % table->capacity) {
    TableEntry *entry = &table->entries[index];
    const Name *key = entry->key;
    if (key == NULL) {
      if (entry->value == NULL)
        return NULL;
    } else if (key->bytes == bytes &&
               key->hash == hash &&
               memcmp(key->chars, chars, bytes) == 0) {
      return key;
    }
  }
}

const Name *alloc_name(const char *begin, const char *end, bool make_copy) {
  int bytes = end != NULL ? (int)(end - begin) : (int)strlen(begin);
  uint32_t hash = hash_string(begin, bytes);
  const Name *name = find_name_table(begin, bytes, hash);
  if (name == NULL) {
    if (make_copy) {
      char *new_str = malloc(bytes);
      memcpy(new_str, begin, bytes);
      begin = new_str;
    }
    Name *new_name = malloc(sizeof(*new_name));
    new_name->chars = begin;
    new_name->bytes = bytes;
    new_name->hash = hash;
    table_put(&name_table, new_name, new_name);
    name = new_name;
  }
  return name;
}

bool equal_name(const Name *name1, const Name *name2) {
  return name1 == name2;  // All names are interned, so they can compare by pointers.
}

// Table

static TableEntry *find_entry(TableEntry *entries, int capacity, const Name *key) {
  TableEntry *tombstone = NULL;
  for (uint32_t index = key->hash % capacity; ; index = (index + 1) % capacity) {
    TableEntry *entry = &entries[index];
    if (entry->key == NULL) {
      if (entry->value == NULL) {
        return tombstone != NULL ? tombstone : entry;
      } else {  // Tombstone.
        if (tombstone == NULL)
          tombstone = entry;
      }
    } else if (entry->key == key) {
      return entry;
    }
  }
}

static void adjust_capacity(Table *table, int new_capacity) {
  TableEntry *new_entries = malloc(sizeof(TableEntry) * new_capacity);
  for (int i = 0; i < new_capacity; ++i) {
    TableEntry *entry = &new_entries[i];
    entry->key = NULL;
    entry->value = NULL;
  }

  TableEntry *old_entries = table->entries;
  int old_capacity = table->capacity;
  int new_count = 0;
  for (int i = 0; i < old_capacity; ++i) {
    TableEntry *entry = &old_entries[i];
    if (entry->key == NULL)
      continue;

    TableEntry *dest = find_entry(new_entries, new_capacity, entry->key);
    dest->key = entry->key;
    dest->value = entry->value;
    ++new_count;
  }

  free(old_entries);
  table->entries = new_entries;
  table->capacity = new_capacity;
  table->count = new_count;
}

void table_init(Table *table) {
  table->entries = NULL;
  table->count = table->capacity = 0;
}

void *table_get(Table *table, const Name *key) {
  if (table->count == 0)
    return NULL;

  TableEntry *entry = find_entry(table->entries, table->capacity, key);
  if (entry->key == NULL)
    return NULL;

  return entry->value;
}

bool table_try_get(Table *table, const Name *key, void **output) {
  if (table->count == 0)
    return false;

  TableEntry *entry = find_entry(table->entries, table->capacity, key);
  if (entry->key == NULL)
    return false;

  *output = entry->value;
  return true;
}

bool table_put(Table *table, const Name *key, void *value) {
  const int MIN_CAPACITY = 15;
  if (table->count >= table->capacity / 2) {
    int capacity = table->capacity * 2 - 1;  // Keep odd.
    if (capacity < MIN_CAPACITY)
      capacity = MIN_CAPACITY;
    adjust_capacity(table, capacity);
  }

  TableEntry *entry = find_entry(table->entries, table->capacity, key);
  bool is_new_key = entry->key == NULL;
  if (is_new_key && entry->value == NULL)
    ++table->count;

  entry->key = key;
  entry->value = value;
  return is_new_key;
}

bool table_delete(Table *table, const Name *key) {
  if (table->count == 0)
    return false;

  TableEntry *entry = find_entry(table->entries, table->capacity, key);
  if (entry->key == NULL)
    return false;

  // Put tombstone.
  entry->key = NULL;
  entry->value = entry;

  return true;
}

int table_iterate(Table *table, int iterator, const Name **pkey, void **pvalue) {
  int capacity = table->capacity;
  for (; iterator < capacity; ++iterator) {
    const TableEntry *entry = &table->entries[iterator];
    const Name *key = entry->key;
    if (key != NULL) {
      if (pkey != NULL)
        *pkey = key;
      if (pvalue != NULL)
        *pvalue = entry->value;
      return iterator + 1;
    }
  }
  return -1;
}
