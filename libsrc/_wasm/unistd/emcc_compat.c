#include <stdint.h>

extern uintptr_t __stack_pointer;

static uintptr_t stack_base;

__attribute__((constructor))
static void init() {
  stack_base = __stack_pointer;
}

uintptr_t emscripten_stack_get_base(void) {
  return stack_base;
}

uintptr_t emscripten_stack_get_end(void) {
  return 0;
}

uintptr_t emscripten_stack_get_current(void) {
  return __stack_pointer;
}

void _emscripten_stack_restore(void) {
  __stack_pointer = stack_base;
}

void emscripten_notify_memory_growth(int32_t index) {
  (void)index;
}
