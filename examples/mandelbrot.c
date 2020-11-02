// Mandelbrot

// Compile:
//   $ ./xcc -omandelbrot -Iinc examples/mandelbrot.c lib/sprintf.c lib/umalloc.c lib/lib.c lib/crt0.c
// Run:
//   $ time ./mandelbrot

#include <stdio.h>
#include <stdlib.h>

int main() {
  const unsigned int threshold = 3000;
  const int W = 1024, H = 1024;
  const double XMIN = -1.75;
  const double YMIN = -1.125;
  const double XS = 2.25, YS = XS * H / W;

  unsigned char *buf = malloc(W * H * 3);
  unsigned char *p = buf;

  for (int i = 0; i < H; ++i) {
    double cy = YS * i / H + YMIN;
    for (int j = 0; j < W; ++j) {
      double cx = XS * j / W + XMIN;
      double x = 0, y = 0;
      unsigned int n;
      for (n = 0; n < threshold; ++n) {
        if (x * x + y * y > 4.0)
          break;
        double nx = x * x - y * y + cx;
        double ny = 2 * x * y     + cy;
        x = nx;
        y = ny;
      }

      unsigned char r = 0, g = 0, b = 0;
      if (n < threshold) {
        ++n;
        r = n * 3;
        g = n * 2;
        b = n * 11;
      }
      *p++ = r;
      *p++ = g;
      *p++ = b;
    }
  }

  FILE *fp = fopen("mandelbrot.ppm", "wb");
  if (fp == NULL) {
    fprintf(stderr, "Cannot open output file\n");
    exit(1);
  }
  fprintf(fp, "P6\n%d %d\n255\n", W, H);
  fwrite(buf, W * H * 3, 1, fp);
  fclose(fp);

  return 0;
}
