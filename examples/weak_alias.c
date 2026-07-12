#define weak_alias(old, new) \
  extern __typeof(old) new __attribute__((__weak__, alias(#old)))

int dummy(void) {
  return 99;
}

static int sub(void) {
  return 123;
}

weak_alias(sub, foobar);
// static int foobar(void) {
//   return 123;
// }

int main(void) {
  int (*p)(void) = foobar;
  return (*p)();
}
