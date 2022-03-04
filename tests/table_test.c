#include "table.h"

#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define EXPECT(result)  expect(__LINE__, result)

void expect(int line, bool result) {
  if (result)
    return;
  fprintf(stderr, "at line %d: failed\n", line);
  exit(1);
}

void test_table(void) {
  const Name *key = alloc_name("1", NULL, false);

  Table table;
  table_init(&table);
  EXPECT(table_get(&table, key) == NULL);
  EXPECT(!table_delete(&table, key));

  void *data = &table;
  table_put(&table, key, data);
  EXPECT(table_get(&table, key) == data);
  EXPECT(table_try_get(&table, key, NULL));
  EXPECT(table.count == 1);

  const Name *key_dup = alloc_name("1", NULL, false);
  table_put(&table, key_dup, data);
  EXPECT(table.count == 1);

  EXPECT(table_delete(&table, key));
  EXPECT(table_get(&table, key) == NULL);
  EXPECT(!table_try_get(&table, key, NULL));
  EXPECT(table.count == 0);
  EXPECT(table.used == 1);
}

void runtest(void) {
  test_table();

  printf("OK\n");
}

int main(void) {
  runtest();
  return 0;
}
