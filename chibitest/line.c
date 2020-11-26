#include "test.h"

int main() {
#line 501 "foo"
  ASSERT(501, __LINE__);
  ASSERT(0, strcmp(__FILE__, "foo"));

#line 801 "bar"
  ASSERT(801, __LINE__);
  ASSERT(0, strcmp(__FILE__, "bar"));

#line 2
  ASSERT(2, __LINE__);

#line 201 "xyz"
  ASSERT(201, __LINE__);
  ASSERT(0, strcmp(__FILE__, "xyz"));

  printf("OK\n");
  return 0;
}
