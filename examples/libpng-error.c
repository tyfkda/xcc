#include <stdio.h>
#include <setjmp.h>

#define PNG_RESTRICT

struct png_struct_def
{
   jmp_buf jmp_buf_local;     /* New name in 1.6.0 for jmp_buf in png_struct */
   jmp_buf *jmp_buf_ptr;      /* passed to longjmp_fn */
};
typedef struct png_struct_def png_struct;

typedef png_struct * PNG_RESTRICT png_structrp;

void test(png_structrp png_ptr) {
  png_ptr->jmp_buf_ptr = &png_ptr->jmp_buf_local;
  if (png_ptr->jmp_buf_ptr != &png_ptr->jmp_buf_local) {
    printf("1\n");
  }
}

int main() {
  printf("size=%zu\n", sizeof(struct png_struct_def));
  return 0;
}
