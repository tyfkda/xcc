struct HFA { float a; float b; };

__attribute__((noinline)) float hfa_sum(struct HFA s, float x, float y) {
  return s.a + s.b + x + y;
}
