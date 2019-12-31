#include "table.h"

#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define EXPECT(result)  expect(__LINE__, result)

void expect(int line, bool result) {
  if (result)
    return;
  fprintf(stderr, "%d: failed\n", line);
  exit(1);
}

int count_table_elems(Table *table) {
  int count = 0;
  for (int i = 0;; ++count) {
    i = table_iterate(table, i, NULL, NULL);
    if (i < 0)
      break;
  }
  return count;
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
  void *dummy;
  EXPECT(table_try_get(&table, key, &dummy));
  EXPECT(count_table_elems(&table) == 1);

  EXPECT(table_delete(&table, key));
  EXPECT(table_get(&table, key) == NULL);
  EXPECT(!table_try_get(&table, key, &dummy));
  EXPECT(count_table_elems(&table) == 0);
}

void runtest(void) {
  test_table();

  printf("OK\n");
}

int main(void) {
  runtest();
  return 0;
}
