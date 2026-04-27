#define weak_alias(old, new) \
  extern __typeof(old) new __attribute__((__weak__, alias(#old)))

int dummy(void) {
  return 99;
}

static int sub(void) {
  return 123;
}

weak_alias(sub, foobar);

int main(void) {
  return foobar();
}
