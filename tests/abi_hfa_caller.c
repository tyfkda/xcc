struct HFA { float a; float b; };
float hfa_sum(struct HFA s, float x, float y);

int main(void) {
  struct HFA s = {1.5f, 2.25f};
  float r = hfa_sum(s, 3.75f, 4.5f);
  return r == 12.0f ? 0 : 1;
}
