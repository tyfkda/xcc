#include "table.h"

#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "./xtest.h"

TEST(table) {
  const Name *key = alloc_name("1", NULL, false);

  Table table;
  table_init(&table);
  EXPECT_NULL(table_get(&table, key));
  EXPECT_FALSE(table_delete(&table, key));

  void *data = &table;
  table_put(&table, key, data);
  EXPECT_PTREQ(data, table_get(&table, key));
  EXPECT_TRUE(table_try_get(&table, key, NULL));
  EXPECT_EQ(1, table.count);

  const Name *key_dup = alloc_name("1", NULL, false);
  table_put(&table, key_dup, data);
  EXPECT_EQ(1, table.count);

  EXPECT_TRUE(table_delete(&table, key));
  EXPECT_NULL(table_get(&table, key));
  EXPECT_TRUE(!table_try_get(&table, key, NULL));
  EXPECT_EQ(0, table.count);
  EXPECT_EQ(1, table.used);
}

XTEST_MAIN();
