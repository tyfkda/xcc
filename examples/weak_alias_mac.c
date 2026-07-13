#define weak_alias(old, new) \
  extern __typeof(old) new __attribute__((__weak__, alias(#old)))

/*static*/ int value = 67;

int dummy(void) {
  return 99;
}

// weak_alias(value, ref);
#pragma weak ref = value
int ref;

/*static*/ int sub(void) {
  return ref;
}

// weak_alias(sub, foobar);
// #pragma weak foobar = sub
__attribute__((weak)) int foobar(void);

int main(void) {
  return foobar();
  // int (*p)(void) = foobar;
  // return (*p)();
}
