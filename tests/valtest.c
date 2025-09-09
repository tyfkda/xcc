#include "alloca.h"
#include "limits.h"
#include "stdarg.h"
#include "stdbool.h"
#include "stddef.h"  // offsetof
#include "stdint.h"
#include "stdio.h"
#include "stdlib.h"  // exit
#include "string.h"  // memcmp

#include "./xtest.h"

#ifndef __NO_FLONUM
#include "math.h"
#endif

#if defined(__GNUC__)
#pragma GCC diagnostic ignored "-Wimplicit-int"
#endif

#define EXPECT(title, expected, actual)  expecti64(title, expected, actual)

int g_zero, g_work;
int g_init = (8 * 7 + 100 / 4 - 50 % 3) & 0x5a | 0x100;

void *null = (void*)0;

struct {int x; int *p;} g_struct = { 42, &g_zero };

static int s_val = 456;

extern int e_val;

short protodecl(short);

int foo(void) {
  return 123;
}
int (*foop)(void) = foo;

int sq(int x) {
  return x * x;
}

int sqsub(int x, int y) {
  int xx = x * x;
  int yy = y * y;
  return xx - yy;
}

int sub(int x, int y) {
  return x - y;
}

int apply(int (*f)(int, int), int x, int y) {
  return f(x, y);
}

int apply2(int f(int, int), int x, int y) {
  return f(x, y);
}

int array_from_ptr1(int a[]) {
  return a[0];
}
int array_from_ptr2(int a[][2]) {
  return a[1][1];
}
int array_from_ptr3(int a[][3]) {
  return a[1][1];
}
int ptr_from_array(int *p) {
  return *(p + 1);
}

int vaargs(int n, ...) {
  int acc = 0;
  va_list ap;
  va_start(ap, n);
  acc += va_arg(ap, int);
  if (n >= 2) {
    char v = va_arg(ap, int);  // char is promoted to int.
    acc += v;
  }
  if (n >= 3)
    acc += va_arg(ap, long);
  va_end(ap);
  return acc;
}

int vaargs_before_many(int a, int b, int c, int d, int e, int f, int g, int h, int i, ...) {
  va_list ap;
  va_start(ap, i);
  int x = va_arg(ap, int);
  va_end(ap);
  return a + b + c + d + e + f + g + h + i + x;
}

typedef struct {long x, y, z;} VaargStruct;
long vaargs_struct(int n, ...) {
  va_list ap;
  va_start(ap, n);
  int a = 0;
  for (int i = 0; i < n; ++i) {
    VaargStruct s = va_arg(ap, VaargStruct);
    int m = va_arg(ap, int);
    a += (s.x * 100 + s.y * 10 + s.z) * m;
  }
  va_end(ap);
  return a;
}

long struct_vaargs(VaargStruct s, int n, ...) {
  va_list ap;
  va_start(ap, n);
  int a = s.x * 100 + s.y * 10 + s.z;
  for (int i = 0; i < n; ++i) {
    int m = va_arg(ap, int);
    a += m;
  }
  va_end(ap);
  return a;
}

int static_local(void) {
  static int x = 42;
  return ++x;
}

#if !defined(__wasm)
int oldfuncptr(int (*f)(), int x) {
  return f(f(x));
}
#endif

_Bool bool_inc(_Bool x) { return x + 1; }

TEST(all) {
  int x, y;
#if defined(__LP64__)
  EXPECT("big literal", 4, sizeof(2147483647));  // (1<<31) - 1
  EXPECT("big literal", 8, sizeof(2147483648));  // 1<<31
  EXPECT("big literal", 4, sizeof(4294967295U));  // (1<<32) - 1
  EXPECT("big literal", 8, sizeof(4294967296U));  // 1<<32
#endif
  EXPECT("zero", 0, 0);
  EXPECT("decimal", 42, 42);
  EXPECT("hex", 18, 0x12);
  EXPECT("octal", 83, 0123);
  EXPECT("binary", 14953, 0b0011101001101001);
  EXPECT("negative", -42, (x=42, -x));
  EXPECT("negative unsigned literal is unsigned", ~0U, -1U);
  EXPECT_TRUE(-(unsigned short)1 < 0);
  EXPECT("0 - x", -42, (x=42, 0-x));
  EXPECT("long", 123, 123L);
  { long long x = 9876543LL; EXPECT("long long", 9876543, x); }
  {
    long long x = 0;
    EXPECT_FALSE(x < -32768LL);       // 0xFFFFFFFFFFFF8000
    EXPECT_FALSE(x < -65536LL);       // 0xFFFFFFFFFFFF0000
    EXPECT_FALSE(x < -2147483648LL);  // 0xFFFFFFFF80000000
    EXPECT_FALSE(2147483647LL < x);
  }
  EXPECT("escape sequence octal", 28, '\034');
  EXPECT("escape sequence hex", 27, '\x1b');
  EXPECT("escape char in str", 19, "\023"[0]);
  EXPECT("+-", 21, (x=5, x+20-4));
  EXPECT("*+", 47, (x=6, 5+x*7));
  EXPECT("()", 15, (x=9, 5*(x-6)));
  EXPECT("/", 4, (x=3, (x+5)/2));
  EXPECT("%", 3, (x=123, x%10));
  EXPECT("<<", 32, (x=4, 1 << x + 1));
  EXPECT(">>", 0x0a, (x=0xa5, x >> 4));
  EXPECT(">>", -6, (x=-0x5b, x >> 4));
  {
    int32_t x;
    // Signed left shift is undefined behavior if the value is negative or the result overflows.
    // EXPECT("signed + <<",  0x25a5a5a0,     (x= 0x5a5a5a5a,     x << 4));
    // EXPECT("signed - <<", -0x25a5a5a0 - 1, (x=-0x5a5a5a5a - 1, x << 4));
    EXPECT("signed + >>",  0x05a5a5a5,     (x= 0x5a5a5a5a,     x >> 4));
    EXPECT("signed - >>", -0x05a5a5a5 - 1, (x=-0x5a5a5a5a - 1, x >> 4));
  }
  {
    uint32_t x;
    EXPECT("unsigned + <<", 0xa5a5a5a0U, (x=0x5a5a5a5a, x << 4));
    EXPECT("unsigned - <<", 0x5a5a5a50U, (x=0xa5a5a5a5, x << 4));
    EXPECT("unsigned + >>", 0x05a5a5a5U, (x=0x5a5a5a5a, x >> 4));
    EXPECT("unsigned - >>", 0x0a5a5a5aU, (x=0xa5a5a5a5, x >> 4));
  }
  {
    // Check shift types, taken from tcc test #55.
#define PTYPE(M) ((M) < 0 || -(M) < 0 ? -1 : 1) * (ssize_t) sizeof((M)+0)
#define U(x)  ((ssize_t)sizeof(x))
#define S(x)  (-(ssize_t)sizeof(x))
    short s = 1;
    unsigned short us = 1;
    int i = 1;
    unsigned int ui = 1;
    long long ll = 1;
    EXPECT("short << unsigned int",        S(i), PTYPE( s << ui));
    EXPECT("short << long long",           S(i), PTYPE( s << ll));
    EXPECT("int << unsigned int",          S(i), PTYPE( i << ui));
    EXPECT("unsigned int << long long",   U(ui), PTYPE(ui << ll));
    EXPECT("long long << unsigned short", S(ll), PTYPE(ll << us));
#undef PTYPE
#undef U
#undef S
  }
  EXPECT("&", 0xa0, (x=0xa5, x & 0xf0));
  EXPECT("|", 0xbc, (x=0x88, x | 0x3c));
  EXPECT("^", 0x66, (x=0xc3, x ^ 0xa5));

  {
    unsigned char x = 0xff;
    unsigned char y = 0x13;
    unsigned char z;
    EXPECT("unsigned char +", 0x12, z = x + y);
    EXPECT("unsigned char -", 0xec, z = x - y);
    EXPECT("unsigned char -", 0x14, z = y - x);
    EXPECT("unsigned char *", 0xed, z = x * y);
    EXPECT("unsigned char /", 0x0d, z = x / y);
    EXPECT("unsigned char %", 0x08, z = x % y);
  }

  {
    short x = 3;
    EXPECT("short", 1, x == 3 && sizeof(x) == 2);
  }
  {
    long x = 3;
    EXPECT("long arithmetic", 3, 5L + 4L - x * 2L / 1L);
  }
  {
    unsigned char c = 255;
    EXPECT("unsigned char", 255, c);
  }
  {
    char c = -0x43;
    EXPECT("cast to unsigned", 0xbd, (unsigned char)c);
  }
  {
    unsigned char c = 0xbd;
    EXPECT("cast to signed", -0x43, (char)c);
  }
  {
    unsigned int x = 0x80000000U;
    EXPECT("unsigned division", 173, x / 12345678);
  }
  {
    unsigned int x = 0x80000000U;
    EXPECT("unsigned modulo", 80, x % 123);
  }
  {
    int a = 3;
    int b = 5 * 6 - 8;
    EXPECT("variable", 14, a + b / 2);
  }
  {
    int foo = 3;
    int bar = 5 * 6 - 8;
    EXPECT("variable2", 14, foo + bar / 2);
  }
  EXPECT("positive var", 42, (x=42, +x));
  EXPECT("negative var", -42, (x=42, -x));
  {
    int a, b, c;
    a = b = (c = 1) + 2;
    EXPECT("assign", a, b);
  }
  EXPECT("!=", 1, (x=123, x != 456));
  EXPECT("not true", 0, (x=1, !(x == 1)));
  EXPECT("not false", 1, (x=1, !(x == 0)));
  EXPECT("not str", 0, !"abc");
  EXPECT("bit not", 0x12345678, (x=0xedcba987, ~x));
  EXPECT("bit not (u32)", 0xffffffff, ~(uint32_t)0);
  {
    int x = 1;
    int y = ++x;
    EXPECT("preinc", 4, x + y);
  }
  {
    int x = 1;
    int y = --x;
    EXPECT("predec", 0, x + y);
  }
  {
    int x = 1;
    int y = x++;
    EXPECT("postinc", 3, x + y);
  }
  {
    int x = 1;
    int y = x--;
    EXPECT("postdec", 1, x + y);
  }
  {
    int x = 10;
    x += 3;
    EXPECT("+=", 13, x);
  }
  {
    char x = 0x25;
    x += (short)0x1234;
    EXPECT("char += short", 0x59, x);
  }
  {
    int x = 10;
    x -= 3;
    EXPECT("-=", 7, x);
  }
  {
    int x = 10;
    x *= 3;
    EXPECT("*=", 30, x);
  }
  {
    int x = 10;
    x /= 3;
    EXPECT("/=", 3, x);
  }
  {
    int x = 10;
    x %= 3;
    EXPECT("%=", 1, x);
  }
  {
    int x = 0x12;
    x <<= 4;
    EXPECT("<<=", 0x120, x);
  }
  {
    int x = 0x123;
    x >>= 8;
    EXPECT(">>=", 0x1, x);
  }
  {
    int x = 0x123;
    x &= 0xa5;
    EXPECT("&=", 0x21, x);
  }
  {
    int x = 0x123;
    x |= 0xa5;
    EXPECT("|=", 0x1a7, x);
  }
  {
    int x = 0x123;
    x ^= 0xa5;
    EXPECT("^=", 0x186, x);
  }

  {
    int32_t a = -1;
    EXPECT("cast from small signed to large unsigned", 0xffffffffffffffffULL, (uint64_t)a);
  }
  {
    int32_t a = 0xF3E141B6L;
    int32_t b = 0x3DE467B4L;
    EXPECT("cast and assign-op", 0, (a ^= 0x2D3D478254981C58LL) > b);
  }

  {
    uint8_t b = 0xff;
    EXPECT("uint8_t pre-inc overlapping", 0, ++b);
    uint16_t s = 0x0;
    EXPECT("uint16_t post-dec overlapping", 0xffff, (s--, s));
    uint32_t i = 0x0;
    EXPECT("uint32_t pre-dec overlapping", 0xffffffff, --i);
    uint64_t l = 0xffffffffffffffff;
    EXPECT("uint64_t post-inc overlapping", 0, (l++, l));
  }

  EXPECT("var < num", 0, (x=1, x < 0));
  EXPECT("var <= num", 0, (x=1, x <= 0));
  EXPECT("var > num", 1, (x=1, x > 0));
  EXPECT("var >= num", 1, (x=1, x >= 0));

  EXPECT("num > var", 0, (x=1, 0 > x));
  EXPECT("num >= var", 0, (x=1, 0 >= x));
  EXPECT("num < var", 1, (x=1, 0 < x));
  EXPECT("num <= var", 1, (x=1, 0 <= x));

  {
    unsigned char x = 255;
    EXPECT("unsigned cmp", 1, x > (unsigned char)0);
    EXPECT("unsigned cmp2", 1, x > 0);
    if (x <= (unsigned char)0)
      fail("unsigned cmp jmp");
    if (x <= 0)
      fail("unsigned cmp jmp2");
  }
  {
    int8_t x = 0xe7;
    uint16_t y = 1;
    EXPECT("cmp signed with unsigned under int", 0, x >= y);
  }
  {
    // C implicitly cast to unsigned type if it handles mixed signed values.
    int minus = -1;
    unsigned int uone = 1U;
    EXPECT("compare with different sign1", 1, minus > uone);  // !!!
    EXPECT("compare with different sign2", 1, uone < minus);  // !!!
    EXPECT("compare with hex literal", 1, minus < 0xf6);  // Hex literal is signed.

    int32_t x = -20;
    uint32_t y = -10;
    EXPECT_TRUE(y > x);
    EXPECT("compare with different sign3", 111, y > x ? 111 : -111);
  }
  EXPECT("condition is true", true, (x = 3, (x != 0) == true));
  EXPECT("condition is not true", false, (x = 4, (x > 0) != true));
  EXPECT("condition is false", true, (x = 3, (x && !x) == false));
  EXPECT("condition is not false", false, (x = 4, (x <= -5 || x >= 5) != false));

  EXPECT("t && t", 1, (x=1, y=1, x && y));
  EXPECT("f && t", 0, (x=0, y=1, x && y));
  EXPECT("t && f", 0, (x=1, y=0, x && y));
  EXPECT("f && f", 0, (x=0, y=0, x && y));
  EXPECT("t || t", 1, (x=1, y=1, x || y));
  EXPECT("f || t", 1, (x=0, y=1, x || y));
  EXPECT("t || f", 1, (x=1, y=0, x || y));
  EXPECT("f || f", 0, (x=0, y=0, x || y));

  EXPECT("funcall", 23, foo() - 100);
  EXPECT("func var", 9, sqsub(5, 4));
  {
    int x = 0;
    if (1)
      x = 2;
    EXPECT("if", 2, x);
  }
  {
    int x = 0;
    if (0)
      x = 2;
    EXPECT("if-false", 0, x);
  }
  {
    int x = 0;
    if (1 == 1)
      x = 2;
    else
      x = 3;
    EXPECT("if else", 2, x);
  }
  {
    int x = 0;
    if (1 == 0)
      x = 2;
    else
      x = 3;
    EXPECT("if else-false", 3, x);
  }
  {
    int a = 0, b = 0;
    if (1) {
      a = 1;
      b = 2;
    }
    EXPECT("block statement", 3, a + b);
  }
  {
    int x = 0;
    if (1)
      ;
    else
      x = 1;
    EXPECT("empty statement", 0, x);
  }
  {
    int i = 0, acc = 0;
    while (i <= 10) {
      acc += i;
      ++i;
    }
    EXPECT("while", 55, acc);
  }
  {
    int i, acc;
    for (i = acc = 0; i <= 10; i++)
      acc += i;
    EXPECT("for", 55, acc);
  }
  {
    int x = 0;
    int y = 1;
    switch (y) {
    case 1:
      x = 11;
      break;
    default:
      x = 22;
      break;
    }
    EXPECT("switch", 11, x);
  }
  {
    int x = 0;
    int y = 2;
    switch (y) {
    case 1:
      x = 11;
      break;
    default:
      x = 22;
      break;
    }
    EXPECT("switch default", 22, x);
  }
  {
    int x = 0;
    int y = 3;
    switch (y) {
    case 1:
      x = 11;
      break;
    }
    EXPECT("switch no-default", 0, x);
  }
  {
    int x = 0;
    int y = 1;
    switch (y) {
    case 1:
      x += 1;
      // Fallthrough
    default:
      x += 10;
    }
    EXPECT("switch fallthrough", 11, x);
  }
  {
    int x = 44;
    switch (x)
      case 1:
        x = 55;
    EXPECT("switch w/o bracket", 44, x);
  }
  {
    int x = 10, *p = &x;
    ++(*p);
    EXPECT("inc pointee", 11, x);
  }
  {
    int a[3], *p = a;
    ++p;
    *p = 123;
    EXPECT("array", 123, *(a + 1));
  }
  {
    int a[2];
    *a = 1;
    a[1] = 10;
    EXPECT("array access", 11, a[0] + 1[a]);
  }
  {
    int a[2], *p;
    a[0] = 10;
    a[1] = 20;
    p = a;
    EXPECT("preinc pointer", 20, *(++p));
  }
  {
    int a[2], *p;
    a[0] = 10;
    a[1] = 20;
    p = a;
    EXPECT("postinc pointer", 10, *p++);
  }
  {
    int a[2], *p;
    a[0] = 98;
    a[1] = 76;
    p = a;
    p += 1;
    EXPECT("pointer +=", 76, *p);
  }
  {
    int a[2], *p;
    a[0] = 54;
    a[1] = 32;
    p = &a[1];
    p -= 1;
    EXPECT("pointer -=", 54, *p);
  }
  {
    int a[2], *p;
    a[0] = 11;
    a[1] = 22;
    p = a;
    *(p++) += 100;
    EXPECT("postinc +=, 1", 111, a[0]);
    EXPECT("postinc +=, 2", 22, a[1]);
  }
  {
    int x = 0;
    EXPECT("cast pointer", 0, *(int*)&x);
  }
  {
    char x, *p = &x;
    EXPECT("cast pointer", 1, (void*)&x == (void(*)())p);
  }
  {
    void *p = (void*)1234;
    EXPECT("cast pointer", 1234L, (long)p);
  }
  EXPECT("global cleared", 0, g_zero);
  EXPECT("global initializer", 330, g_init);
  EXPECT("global struct initializer: int", 42, g_struct.x);
  EXPECT("global struct initializer: ptr", (intptr_t)&g_zero, (intptr_t)g_struct.p);
  {
    g_work = 1;
    EXPECT("global access", 11, g_work + 10);
  }
  {
    struct {char x; int y;} foo;
    foo.x = 1;
    foo.y = 2;
    EXPECT("struct", 3, foo.x + foo.y);
  }
  {
    struct {char x; int y;} foo, *p = &foo;
    p->x = 1;
    p->y = 2;
    EXPECT("struct pointer", 3, foo.x + foo.y);
  }
  {
    union {char x; int y;} foo;
    EXPECT("union", 1, sizeof(foo) == sizeof(int) && (void*)&foo.x == (void*)&foo.y);
  }
  {
    struct{
      union{
        int x;
      };
    } a;
    a.x = 596;
    EXPECT("anonymous", 596, a.x);
    EXPECT("anonymous adr", (intptr_t)&a, (intptr_t)&a.x);
  }
  EXPECT("func pointer", 9, apply(&sub, 15, 6));
  EXPECT("func pointer w/o &", 9, apply(sub, 15, 6));
  EXPECT("func", 2469, apply2(sub, 12345, 9876));
  EXPECT("block comment", 123, /* comment */ 123);
  EXPECT("line comment", 123, // comment
         123);
  EXPECT("proto decl", 12321, protodecl(111));
  {
    long proto_in_func(short);
    EXPECT("proto in func", 78, proto_in_func(77));
  }

  {
    int i, acc;
    for (i = acc = 0; i <= 10; i++) {
      if (i == 5)
        break;
      acc += i;
    }
    EXPECT("for-break", 10, acc);
  }
  {
    int i, acc;
    for (i = acc = 0; i <= 10; i++) {
      if (i == 5)
        continue;
      acc += i;
    }
    EXPECT("for-continue", 50, acc);
  }
  {
    int i = 0, acc = 0;
    while (++i <= 10) {
      if (i == 5)
        break;
      acc += i;
    }
    EXPECT("while-break", 10, acc);
  }
  {
    int i = 0, acc = 0;
    while (++i <= 10) {
      if (i == 5)
        continue;
      acc += i;
    }
    EXPECT("while-continue", 50, acc);
  }
  {
    int i = 0, acc = 0;
    do {
      if (i == 5)
        break;
      acc += i;
    } while (++i <= 10);
    EXPECT("do-while-break", 10, acc);
  }
  {
    int i = 0, acc = 0;
    do {
      if (i == 5)
        continue;
      acc += i;
    } while (++i <= 10);
    EXPECT("do-while-continue", 50, acc);
  }
  EXPECT("t && t", 1, 1 && 2);
  {
    int x = 1;
    0 && (x = 0);
    EXPECT("&& shortcut", 1, x);
  }
  EXPECT("f || t", 1, 0 || 2);
  {
    int x = 1;
    1 || (x = 0);
    EXPECT("|| shortcut", 1, x);
  }
  {
    int x = 0;
    int a = 1, b = 0;
    if (!(a && b))
      x = 1;
    EXPECT("conditional !(t && f)", 1, x);
  }
  {
    int x = 0;
    int a = 0, b = 1;
    if (a || b)
      x = 1;
    EXPECT("conditional (f || t)", 1, x);
  }
  {
    int x = 1;
    { int x = 2; (void)x; }
    EXPECT("block scope", 1, x);
  }
  {
    char a[2][3];
    a[1][0] = 1;
    EXPECT("nested-array", 1, ((char*)a)[3]);
  }
  {
    int a[2];
    a[1] = 45;
    EXPECT("array <- ptr", 45, array_from_ptr1(&a[1]));
  }
  {
    int a[3][2];
    a[1][1] = 39;
    EXPECT("array <- ptr:2", 39, array_from_ptr2(a));
  }
  {
    int a[3][2];
    a[2][0] = 987;
    EXPECT("array <- ptr:3", 987, array_from_ptr3((int(*)[3])a));
  }
  {
    int a[6] = {11, 22, 33, 44, 55, 66};
    int (*b)[3] = (int(*)[3])a;
    EXPECT("array ptr", 55, b[1][1]);
  }
  {
    int a[2];
    a[1] = 55;
    EXPECT("ptr <- array", 55, ptr_from_array(a));
  }
  {
    int a[2] = {1, 2};
    int p = 1;
    a[p = 0] = 3;
    EXPECT("array index w/ side effect 1", 3, a[0]);
    EXPECT("array index w/ side effect 2", 0, p);
  }

  EXPECT("sizeof(int)", __SIZEOF_INT__, sizeof(int));
  EXPECT("sizeof(long)", __SIZEOF_LONG__, sizeof(long));
  EXPECT("int*", __SIZEOF_POINTER__, sizeof(int*));
  EXPECT("int8_t",  1, sizeof(int8_t));
  EXPECT("int16_t", 2, sizeof(int16_t));
  EXPECT("int32_t", 4, sizeof(int32_t));
  EXPECT("int64_t", 8, sizeof(int64_t));
  EXPECT("intptr_t", __SIZEOF_POINTER__, sizeof(intptr_t));
  EXPECT("sizeof(void)", 1, sizeof(void));
  EXPECT("sizeof(array)", 3, sizeof(char[3]));
  {
    int x;
    EXPECT("sizeof var", 4, sizeof(x));
  }
  EXPECT("sizeof(expr)", 4, sizeof(5 * 9));
  {
    int a[5];
    EXPECT("sizeof(array len)", 5, sizeof(a) / sizeof(*a));
  }
  EXPECT("sizeof(struct)", 8, sizeof(struct {int a; char b;}));
  EXPECT("sizeof(empty struct)", 0, sizeof(struct {}));
  {
    struct {} a, b;
    EXPECT("empty struct occupy", 1, &a != &b);
  }
  EXPECT("sizeof(str) include nul", 12, sizeof("hello\0world"));
  EXPECT("sizeof(struct ptr)", sizeof(void*), sizeof(struct Undefined*));
  EXPECT("sizeof(func ptr)", sizeof(void*), sizeof(int (*)()));
  {
    struct S { char a[77]; };
    EXPECT("sizeof cast expression", 77, sizeof ((struct S*)0)->a);
  }

  {
    int a[3] = {1, 2, 3};
    EXPECT("array initializer", 1, a[0] == 1 && a[1] == 2 && a[2] == 3);
  }
  {
    int a[] = {1, 2};
    EXPECT("array without size", 1, sizeof(a) == 2 * sizeof(int) && a[0] == 1 && a[1] == 2);
  }
  {
    int a[] = {1, 2, 3,};
    EXPECT("array with last comma", 3 * sizeof(int), sizeof(a));
  }
  {
    char buf[3] = {};
    EXPECT("char array with empty initializer", 0, buf[0]);

#if !defined(__GNUC__)
    int z = {};
    EXPECT("primitive with empty initializer", 0, z);
#endif
  }
  {
    struct {int x; int y;} s = {3};
    EXPECT("struct initializer", 3, s.x + s.y);
  }
  {
    struct {int x; int y;} s = {3, 4,};
    EXPECT("struct initializer with last comma", 7, s.x + s.y);
  }
  {
    struct {int x; int y;} s = {.y = 9};
    EXPECT("struct initializer with member", 9, s.x + s.y);
  }
  {
    union {char x; int y;} u = {0x1234};
    EXPECT("union initializer", 0x34, u.x);
  }
  {
    union {int y; char x;} u = {0x5678};
    EXPECT("union initializer2", 0x5678, u.y);
  }
  {
    union {char x; int y;} u = {.y=0xabcd};
    EXPECT("union initializer with member", 0xabcd, u.y);
  }
  {
    const int x = 123;
    EXPECT("const", 123, x);
  }
  EXPECT("file static", 456, s_val);
  EXPECT("extern", 789, e_val);

  {
    static static_only = 11;
    EXPECT("static_only", 11, static_only);
    EXPECT("sizeof static_only", sizeof(int), sizeof(static_only));
  }
  {
    extern extern_only;
    EXPECT("extern_only", 22, extern_only);
    EXPECT("sizeof extern_only", sizeof(int), sizeof(extern_only));
  }
  {
    const const_only = 33;
    EXPECT("const_only", 33, const_only);
    EXPECT("sizeof const_only", sizeof(int), sizeof(const_only));
  }
  {
    volatile volatile_only = 44;
    EXPECT("volatile_only", 44, volatile_only);
    EXPECT("sizeof volatile_only", sizeof(int), sizeof(volatile_only));
  }
  {
    static const struct {
      int x;
    } sc_struct = {
      55
    };
    EXPECT("sc_struct", 55, sc_struct.x);
  }
  {
    static const struct {
      int a;
      struct {
        int b;
        struct {
          int z;
        } y;
      } x;
    } t = {
      1, { 2, { 3 } }
    };
    static const int *p = &t.x.y.z;
    EXPECT("&member initializer", 3, *p);
  }
  {
    static const struct {
      int a;
      struct {
        int b[2];
      } x[2];
    } t = {
      11, { {{22, 23}}, {{33, 34}} }
    };
    static const int *p = t.x[1].b;
    EXPECT("member[].member initializer", 33, *p);
  }
  {
    struct S {
      int a;
      int b;
    };
    struct S x = {11, 22};
    struct S a[] = {x};
    EXPECT("struct initializer with variable", 33, a[0].a + a[0].b);
  }

  {
    int desig[] = {[2] = 100, [1] 200};
    EXPECT("desig[0]",   0, desig[0]);
    EXPECT("desig[1]", 200, desig[1]);
    EXPECT("desig[2]", 100, desig[2]);
    EXPECT("sizeof(desig)", 3, sizeof(desig) / sizeof(*desig));
  }
  {
    static int desig[] = {[2] = 1000, 2000};
    EXPECT("static desig[0]",    0, desig[0]);
    EXPECT("static desig[1]",    0, desig[1]);
    EXPECT("static desig[2]", 1000, desig[2]);
    EXPECT("static desig[3]", 2000, desig[3]);
    EXPECT("sizeof(static desig)", 4, sizeof(desig) / sizeof(*desig));
  }
  EXPECT("?:", 2, 1 ? 2 : 3);
  EXPECT("comma", 3333, (11, 222, 3333));
  EXPECT("vaargs 1", 1, vaargs(1, (int)1, (char)20, 300L));
  EXPECT("vaargs 2", 21, vaargs(2, (int)1, (char)20, 300L));
  EXPECT("vaargs 3", 321, vaargs(3, (int)1, (char)20, 300L));
  {
    char c = 'A';
    EXPECT("vaargs char", 65, vaargs(1, c));
  }
  EXPECT("vaargs before many", 145, vaargs_before_many(1, 2, 3, 4, 5, 6, 7, 8, 9, 100));
  {
    VaargStruct s1 = {1, 2, 3};
    VaargStruct s2 = {4, 5, 6};
    EXPECT("vaargs struct", 456369, vaargs_struct(2, s1, 3, s2, 1000));
    EXPECT("vaargs struct2", 123 + 55, struct_vaargs(s1, 10, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10));
  }

  EXPECT("static local var", 44, (static_local(), static_local()));
  EXPECT("null initializer", 0L, (long)null);
  {
    int buf[16], *p = buf + 13;
    EXPECT("diff ptr and array", 13, p - buf);
    EXPECT("diff ptr and array2", -13, buf - p);
  }
  {
    int buf[16];
    const int *p = &buf[9];
    EXPECT("diff ptr and const ptr", 9, p - buf);
  }
  {
    int array1[] = {1}, array2[] = {2};
    EXPECT("compare arrays", 0, array1 == array2);
    EXPECT("compare arrays2", 1, array1 == array1);
  }

  EXPECT("block expr", 169, ({int x = 13; x * x;}));
  EXPECT("block expr simple", 13, ({int zzz = 13; zzz;}));

  {
    int64_t x = 0x123456789abcdef0LL;
    EXPECT("64bit literal 1", 0xdef0,  x        & 0xffff);
    EXPECT("64bit literal 2", 0x9abc, (x >> 16) & 0xffff);
    EXPECT("64bit literal 3", 0x5678, (x >> 32) & 0xffff);
    EXPECT("64bit literal 4", 0x1234, (x >> 48) & 0xffff);
  }
  {
    int64_t x = 0;
    EXPECT("literal auto unsigned", 1, 0x89abcdef01234567LL > x);
  }

  {
    EXPECT("_Bool from int", true, ({int x = -123; (_Bool)x;}));
    EXPECT("_Bool from ulong", true, ({unsigned long x = 9876UL; (_Bool)x;}));
#ifndef __NO_FLONUM
    EXPECT("_Bool from double", true, ({double x = 12.34; (_Bool)x;}));
#endif
    EXPECT("_Bool function", true, bool_inc(3));

    size_t m = 3;
    size_t n = 8;
    EXPECT("ternary _Bool 1", -1, m < n ? -1: m != n);
    EXPECT("ternary _Bool 2",  1, n < m ? -1: n != m);

    _Bool t1 = true, t2 = true;
    EXPECT("_Bool + _Bool",  2, t1 + t2);
  }

#ifndef __NO_WCHAR
  EXPECT("wide character", 0x1f600, L'😀');
  {
    static wchar_t ws[] = L"aあ";
    EXPECT("wide string 1", 3 * 4, sizeof(ws));
    EXPECT("wide string 1", L'a', ws[0]);
    EXPECT("wide string 2", L'あ', ws[1]);
    EXPECT("wide string 3", L'\0', ws[2]);
  }
#endif
}

int e_val = 789;

int extern_only = 22;

short protodecl(short x) { return x * x; }
long proto_in_func(short x) { return x + 1; }

//

typedef int SameTypedefAllowed;
typedef int SameTypedefAllowed;

int f27(void){return 27;}
int f53(void){return 53;}
void mul2p(int *p) {*p *= 2;}
const char *retstr(void){ return "foo"; }

TEST(basic) {
  {
    int array[0];
    EXPECT("zero sized array", 0, sizeof(array));
  }

  {
    static char s[4] = "abcd";
    EXPECT("non nul-terminated str static", 4, sizeof(s));
    char l[6] = "efghij";
    EXPECT("non nul-terminated str local", 6, sizeof(l));

    struct S {char str[7];} static ss = {"klmnopq"};
    EXPECT("non nul-terminated str in struct", 7, sizeof(ss));
  }

  {
    char *l = (char*)"x";
    EXPECT("cast string", 120, l[0]);

    static char *s = (char*)"x";
    EXPECT("cast string static", 120, s[0]);
  }

  {
    static uintptr_t x = (uintptr_t)"str";
    char *p = (char*)x;
    EXPECT("cast str to int", 116, p[1]);
  }

  {
    int num = 4;
    EXPECT("cast str to int", 52, *("012345" + num));
    EXPECT("cast str to int", 52, "012345"[num]);
  }

  {
    int x;
    (x) = 98;
    EXPECT("paren =", 98, x);
  }

  {
    enum Num { Zero, One, Two };
    EXPECT("enum", 11, One + 10);

    enum Num2 { Ten = 10, Eleven, };
    EXPECT("enum with assign and trailing comma", 11, Eleven);

    int y = 1;
    switch (y) {
    case One: EXPECT_TRUE("enum can use in case"); break;
    default: fail("enum can use in case"); break;
    }

    enum Num num = Zero;
    EXPECT_FALSE(num == One);

    static enum Num num2 = Two;
    EXPECT_TRUE(num2 == Two);
  }

  // empty block
  ; {} {;;;}

  {
    typedef char T1, T2[29];
    EXPECT("multi typedef", 29, sizeof(T2));
  }

  {
    typedef void VOID;
    extern VOID voidfunc(VOID);
    EXPECT_TRUE("typedef void");

    typedef char T[];
    T t1 = {1, 2};
    T t2 = {3, 4, 5, 6, 7};
    EXPECT("typedef[]", 2, sizeof(t1));
    EXPECT("typedef[]", 5, sizeof(t2));
  }

#if !defined(__wasm)
  {
    int oldstylefunc();
    EXPECT("old-style func", 93, oldstylefunc(31));

    EXPECT("old-func-ptr", 81, oldfuncptr(sq, 3));
  }
#endif

  {
    EXPECT("global-func-var", 123, foop());

    static int (*f)(void) = foo;
    EXPECT("static-func-var", 123, f());
  }

  {
    int acc;
    acc = 0;
    for (int i = 1, len = 10; i <= len; ++i)
      acc += i;
    EXPECT("for-var", 55, acc);

    const char *p = "abc";
    int len = 0;
    for (char c; (c = *p) != '\0'; ++p)
      ++len;
    EXPECT("for-no-initial-val", 3, len);

    acc = 0;
    for (int i = 1; i <= 10; ++i) {
      if (i > 5)
        break;
      acc += i;
    }
    EXPECT("break", 15, acc);

    acc = 0;
    for (int i = 1; i <= 10; ++i) {
      if (i <= 5)
        continue;
      acc += i;
    }
    EXPECT("continue", 40, acc);
  }

  {
    int x = 123;
    (void)x;
  }

  EXPECT_STREQ("strings", "hello world", "hello " "world");
  {
    const char *const ccc = "foobar";
    EXPECT_STREQ("const char* const", "foobar", ccc);
  }
  {
    static const char nulchr_contained[12] = "foo\0bar\0\0";
    const char *expected = "foo\0bar\0\0\0\0";
    EXPECT("nulchr contained string", 0, memcmp(expected, nulchr_contained, sizeof(nulchr_contained)));
  }

#if !defined(__wasm)
  {
#define SUPPRESS_LABEL(label)  do { if (false) goto label; } while (0)
    int x = 1;
    SUPPRESS_LABEL(dummy1);
    goto label;
dummy1:
    x = 2;
label:
    EXPECT("goto", 1, x);

j3:
    goto j1;
dummy2:
    goto j2;
j2:
    goto dummy2;
j1:;
    SUPPRESS_LABEL(j3);

    typedef int TTT;
    goto TTT;
TTT:;
    TTT ttt = 369;
    EXPECT("typedef and label", 369, ttt);
  }
#endif

  {
    int x;
    x = 0;
    int z = 0;
    switch (z) {
    default: x = 1; break;
    }
    EXPECT("switch w/o case", 1, x);

    x = 0;
    switch (z) {
      x = 1;
    }
    EXPECT("switch w/o case & default", 0, x);

#if !defined(__wasm)
    x = 94;
    switch (z) {
      if (0) {
        default: x = 49;
      }
    }
    EXPECT("switch-if-default", 49, x);
#endif

    x = 2;
    switch (x) {
    case 0: x = 0; break;
    case 1: x = 11; break;
    case 2: x = 22; break;
    case 3: x = 33; break;
    }
    EXPECT("switch table", 22, x);

    short y = 1;
    switch (y) {
    case 2: y = 22; break;
    case 3: y = 33; break;
    case 4: y = 44; break;
    case 5: y = 55; break;
    default: y = 99; break;
    }
    EXPECT("switch table less", 99, y);
  }

  {  // "post inc pointer"
    char *p = (char*)(-1L);
    p++;
    EXPECT_NULL(p);
  }

  {
    int x = 1;
    {
      x = 10;
      int x = 100; (void)x;
    }
    EXPECT("shadow var", 10, x);
  }

  {
    typedef int Foo;
    {
      int Foo = 61;
      EXPECT("typedef name can use in local", 61, Foo);
    }
  }

  {
    unsigned x = 92;
    EXPECT("implicit int", 92, x);
  }

  {
    int x = 1;
    const char *p = x ? "true" : "false";
    EXPECT("ternary string", 114, p[1]);

    const char *q = "abc";
    q = q != 0 ? q + 1 : 0;
    EXPECT("ternary ptr:0", 98, *q);

    int selector = 0;
    EXPECT("ternary w/ func", 53, (selector ? f27 : f53)());

    char buf[16] = "";
    selector ? (void)strcpy(buf, "true") : (void)strcpy(buf, "false");
    EXPECT_STREQ("ternary void", "false", buf);
  }
  {
    typedef struct {
      int x;
    } Foo;
    Foo foo = {54321};
    const void *p = &foo;
    Foo *q = &foo;
    int x = 1;
    const Foo *r = x ? p : q;
    EXPECT("ternary between const void* and struct*", 54321, r->x);
  }

  {
    int x;
    x = 43;
    mul2p(&(x));
    EXPECT("&()", 86, x);
    x = 33;
    EXPECT("pre-inc ()", 34, ++(x));
    x = 44;
    EXPECT("post-dec ()", 44, (x)--);
  }

  {
    const char *p = "foo";
    p = "bar";
    EXPECT("can assign const ptr", 97, p[1]);

    "use strict";

    p = (1, "use strict", "dummy");
    EXPECT("str in comma", 117, p[1]);

    int x = 1;
    x != 0 && (x = 0, 1);
    EXPECT("condition with comma expr", 0, x);

    EXPECT("return str", 111, retstr()[2]);
    EXPECT("deref str", 48, *"0");
  }
}

int oldstylefunc(int x) {
  return x * 3;
}

//

typedef struct {int x, y;} FooStruct;

int struct_arg(FooStruct foo, int k) { return foo.x * k + foo.y; }
FooStruct return_struct(void) { FooStruct s = {.x = 12, .y = 34}; return s; }

typedef struct {long x; long y;} LongStruct;
LongStruct return_lstruct(void) { LongStruct s = {111, 222}; return s; }

typedef struct { short x, y, z; } SVec3;

SVec3 vadd(SVec3 va, SVec3 vb) {
  return (SVec3){va.x + vb.x, va.y + vb.y, va.z + vb.z};
}

SVec3 refvadd(SVec3 va, SVec3 vb) {
  SVec3 *pa = &va, *pb = &vb;
  return (SVec3){pa->x + pb->x, pa->y + pb->y, pa->z + pb->z};
}

SVec3 modify_struct_param(SVec3 v) {
  v.x += 1;
  v.y += 2;
  v.z += 3;
  return v;
}

TEST(struct) {
  FooStruct foo;
  foo.x = 123;
  EXPECT("typedef", 123, foo.x);

  {
    typedef struct FILE FILE;
    EXPECT("Undeclared struct typedef", sizeof(void*), sizeof(FILE*));
  }

  {
    struct Foo *p;
    struct Foo {int x;};
    struct Foo foo;
    p = &foo;
    p->x = 42;
    EXPECT("late declare struct", 42, p->x);
  }

  {
    int size;
    struct S {int x;};
    {
      struct S {char y;};
      size = sizeof(struct S);
    }
    EXPECT("scoped struct", 5, size + sizeof(struct S));
  }

  {
    int size;
    typedef struct {int x;} S;
    {
      typedef struct {char y;} S;
      size = sizeof(S);
    }
    EXPECT("scoped typedef", 5, size + sizeof(S));
  }

  {
    struct S {struct S *p;} *s = 0;
    EXPECT("self referential struct", sizeof(void*), sizeof(s->p[0]));
  }

  {
    struct Foo { int x; };
    struct Foo foo, bar;
    foo.x = 33;
    bar = foo;
    EXPECT("struct assign", 33, bar.x);
  }

  {
    struct Foo { int x; };
    struct Foo foo = {55}, bar = foo;
    EXPECT("struct initial assign", 55, bar.x);
  }

  {
    struct Foo { long x; };
    struct Foo foo, bar, *baz = &bar;
    baz->x = 44;
    foo = *baz;
    EXPECT("struct deref", 44, foo.x);
  }

  {
    typedef struct {int x;} S;
    S s = {51}, x;
    S *e1 = &x;
    S *e2 = &s;
    *e1 = *e2;
    EXPECT("struct copy", 51, x.x);
  }

  {
    struct empty {};
    EXPECT("empty struct size", 0, sizeof(struct empty));

    struct empty a = {}, b;
    b = a;
    EXPECT("empty struct copy", 0, sizeof(b));
  }

  {
    struct S {int x;};
    const struct S s = {123};
    struct S a;
    a = s;
    EXPECT("copy struct from const", 123, a.x);

    struct S b = s;
    EXPECT("init struct with variable", 123, b.x);
  }

  {
    FooStruct foo = {12, 34};
    EXPECT("struct args", 82, struct_arg(foo, 4));

    const FooStruct bar = {56, 78};
    EXPECT("implicit cast to non-const", 246, struct_arg(bar, 3));
  }

  {
    typedef struct { int x, y, z; } S;
    S a = {1, 2, 3}, b = {10, 20, 30};
    int x = 1;
    S c = x == 0 ? a : b;
    EXPECT("ternary struct", 60, c.x + c.y + c.z);
  }

  {
#undef NULL
#define NULL  ((void*)0)
#undef offsetof
#define offsetof(S, m)  ((long)(&((S*)NULL)->m))
    struct X {char x[5]; char z; struct {char b;} s; struct {char c[3];} t[4]; };
    char a[offsetof(struct X, z)];
    EXPECT("offsetof is const", 5, sizeof(a));
    char b[offsetof(struct X, s.b)];
    EXPECT("offsetof is const", 6, sizeof(b));
    char c[offsetof(struct X, t[3].c)];
    EXPECT("offsetof is const", 16, sizeof(c));
    char d[offsetof(struct X, t[3].c[2])];
    EXPECT("offsetof is const", 18, sizeof(d));
  }

  {
    FooStruct s = return_struct();
    EXPECT("return struct", 46, s.x + s.y);

    EXPECT("return struct member", 12, return_struct().x);
    int y = return_struct().y;
    EXPECT("return struct member", 34, y);

    return_struct();  // Call only.
  }
  {
    int dummy[1]; (void)dummy;
    LongStruct s;
    s = return_lstruct();
    EXPECT("return struct not broken", 222, s.y);
  }

  {
    static const SVec3 v1 = {11, 22, 33}, v2 = {99, 88, 77};
    SVec3 v = vadd(v1, v2);
    EXPECT_TRUE(v.x == 110 && v.y == 110 && v.z == 110);
    SVec3 pv = refvadd(v1, v2);
    EXPECT_TRUE(pv.x == 110 && pv.y == 110 && pv.z == 110);

    SVec3 mod = modify_struct_param(v1);
    EXPECT_TRUE(mod.x == 12 && mod.y == 24 && mod.z == 36);
    EXPECT_TRUE(v1.x == 11 && v1.y == 22 && v1.z == 33);  // Argument entity is not modified.
  }

  {
    struct FlexibleArrayMember {
      int a;
      int b[];
    };
    EXPECT("flexible array member", sizeof(int), sizeof(struct FlexibleArrayMember));

    // Can declare local variable for FAM.
    struct FlexibleArrayMember fam; (void)fam;

    int buf[4];
    memset(buf, -1, sizeof(buf));
    struct FlexibleArrayMember *p = (struct FlexibleArrayMember*)&buf;
    p->a = 12;
    p->b[0] = 23;
    p->b[1] = 34;
    p->b[2] = 45;
    EXPECT_TRUE(buf[0] == 12 && buf[1] == 23 && buf[2] == 34 && buf[3] == 45);

    struct RequirePadding {
      short a;
      long b[];
    };
    EXPECT("flexible array member", sizeof(long), sizeof(struct RequirePadding));

#if defined(__XCC)  // gcc does not allow initializer for local variable.
    struct FlexibleArrayMember local = {111, {222, 333, 444, 555}};
    EXPECT("FAM local 1", 111, local.a);
    EXPECT("FAM local 2", 222, local.b[0]);
    EXPECT("FAM local 3", 333, local.b[1]);
    EXPECT("FAM local 4", 444, local.b[2]);
    EXPECT("FAM local 5", 555, local.b[3]);
    EXPECT("sizeof(x) with flexible is not actual",
           sizeof(struct FlexibleArrayMember), sizeof(local));
#endif
  }
}

//

#ifndef __NO_BITFIELD
struct {
  char x;
  int b1 : 4;
  unsigned int b2 : 5;
  int b3 : 3;
  int z;
} g_bfwinit = {
  111,
  0x05,
  0x0a,
  0x03,
  222,
};

static int return_bitfield_value(void) {
  return g_bfwinit.b1;
}

TEST(bitfield) {
  {
    union {
      int16_t _;
      struct {
        int16_t x : 5;
        int16_t y : 5;
        int16_t z : 6;
      };
    } u;

    // EXPECT("sizeof", sizeof(int16_t), sizeof(u));

    u.x = 5;
    u.y = 23;
    u.z = 27;
    EXPECT("value 1", 5, u.x);
    EXPECT("value 2", -9, u.y);
    EXPECT("value 3", 27, u.z);
    EXPECT("total", (27 << 10) | (23 << 5) | 5, u._);
  }

  {
    union {
      uint16_t _;
      struct {
        uint16_t x : 5;
        uint16_t y : 5;
        uint16_t z : 6;
      };
    } u;

    u.x = -1;
    u.y = -2;
    u.z = -3;
    EXPECT("unsigned 1", 31, u.x);
    EXPECT("unsigned 2", 30, u.y);
    EXPECT("unsigned 3", 61, u.z);
    EXPECT("total2", (61 << 10) | (30 << 5) | 31, u._);
  }

  {
    union BitFieldInUnion {
      int x;
      unsigned int b: 1;
    };
    static union BitFieldInUnion bfu = {123};
    EXPECT("bitfield in union", 1, bfu.b);
  }

  // {
  //   typedef struct {
  //     char a;
  //     long x : 5;
  //     long y : 4;
  //     long z : 3;
  //     short b;
  //   } M;
  //   EXPECT("mix size", sizeof(long) * 3, sizeof(M));
  //   EXPECT("mix layout", sizeof(long) * 2, offsetof(M, b));
  // }

  {
    struct {
      int _1 : 5;
      int x  : 5;
      int _2 : 6;
    } s = {};

    s.x = 5;
    EXPECT("assign-op +=", 12, s.x += 7);
    EXPECT("assign-op /=", 4, s.x /= 3);

    EXPECT("assign-op overflow", -13, s.x += 15);
    EXPECT("assign-op underflow", 7, s.x -= 12);
    EXPECT("assign-op overflow 2", -11, s.x *= 3);

    EXPECT("assign-op pad 1", 0, s._1);
    EXPECT("assign-op pad 2", 0, s._2);
  }

  {
    struct {
      int x : 3;
      int y : 4;
      int z : 5;
      int w : 6;
    } s;

    s.x = 0;
    s.y = 5;
    s.z = 10;
    s.w = 15;

    EXPECT("pre-dec", -1, --s.x);
    EXPECT("pre-dec'", -1, s.x);
    EXPECT("pre-inc", 6, ++s.y);
    EXPECT("pre-inc'", 6, s.y);
    EXPECT("post-inc", 10, s.z++);
    EXPECT("post-inc'", 11, s.z);
    EXPECT("post-dec", 15, s.w--);
    EXPECT("post-dec'", 14, s.w);

    s.y = (1 << 3) - 1;
    EXPECT("pre-overflow", -(1 << 3), ++s.y);
    EXPECT("pre-overflow'", -(1 << 3), s.y);
    s.z = -(1 << 4);
    EXPECT("post-underflow", -(1 << 4), s.z--);
    EXPECT("post-underflow'", (1 << 4) - 1, s.z);
  }

  {
    struct {  // |....xx|x___..|
      int _ : __SIZEOF_INT__ * CHAR_BIT - 1;
      int x : 3;
    } s;
    s.x = 0;
    EXPECT("extend bits pre-dec", -1, --s.x);
    EXPECT("extend bits +=", 3, s.x += 4);
    EXPECT("extend bits post-inc", 3, s.x++);
    EXPECT("extend bits post-inc after", -4, s.x);
  }

  {
    union {
      int _;
      int x: 5;
      int y: 9;
    } s;

    s._ = 0xa5a5;
    EXPECT("union 1", 0x5, s.x);
    EXPECT("union 2", -0x5b, s.y);
  }

  {
    struct S {
      int x : 4;
      int y : 6;
      int z : 10;
    };
    struct S s = {
      1,
      22,
      333,
    };

    struct S s2 = s;
    EXPECT("copy 1'", 1, s2.x);
    EXPECT("copy 2'", 22, s2.y);
    EXPECT("copy 3'", 333, s2.z);
  }

  {
    struct {
      int _: 1;
      long long ll : 33;
      unsigned long long ull : 33;
    } s = {
      0,
      0x123456789LL,
      0x123456789LL,
    };

    EXPECT("long long 1", -0xDCBA9877LL, s.ll);
    EXPECT("long long 2", 0x123456789LL, s.ull);
  }

  {
    enum Num {
      Zero,
      One,
      Two,
      Three,
    };

    struct {
      enum Num x : 2;
      enum Num y : 2;
      enum Num z : 4;
    } s = {
      One,
      Two,
      Three,
    };

    EXPECT("enum 1", One, s.x);
#if !defined(__GNUC__)
    EXPECT("enum 2", -Two, s.y);  // enum is treated as signed on XCC.
#endif
    EXPECT("enum 3", Three, s.z);
  }

  {
    EXPECT("global w/init x", 111, g_bfwinit.x);
    EXPECT("global w/init b1", 0x05, g_bfwinit.b1);
    EXPECT("global w/init b2", 0x0a, g_bfwinit.b2);
    EXPECT("global w/init b3", 0x03, g_bfwinit.b3);
    EXPECT("global w/init z", 222, g_bfwinit.z);

    EXPECT("return bitfield value", 0x05, return_bitfield_value());
  }

  {
    struct S1 {
      char x : 3;
      char y : 4;
    };
    struct S2 {
      char x : 3;
      char : 0;
      char y : 4;
    };
    // Bit width zero breaks bit packing.
    EXPECT_TRUE(sizeof(struct S1) < sizeof(struct S2));

    struct S2 s2 = {1, 2};
    EXPECT("zero bitfield not consume initializer", 2, s2.y);
  }

  {
    struct {
      unsigned int x : 8;
    } s = {5};
    EXPECT_FALSE(-1 > s.x);
  }

  {
    struct {
      signed int x : 2;
    } s;
    EXPECT("signed bitfield assigned overflow", -2, s.x = 2);
  }

  {
    struct {
      _Bool f : 1;
      _Bool t : 1;
      _Bool t2 : 1;
    } s = {
      false, true, true,
    };
    EXPECT("bool bitfield: false", false, s.f);
    EXPECT("bool bitfield: true", true, s.t);
    s.t2 = true;
    EXPECT("bool bitfield 2: true", true, s.t2);
  }
}
#endif

//

char g_strarray[] = "StrArray";
char *g_strptr = "StrPtr";
char *g_strptrarray[] = {"StrPtrArray"};
struct {char *s;} g_str_struct_array[] = {{"StrStructArray"}};
struct {char s[4];} g_chararray_struct[] = {{"abc"}};
char nums[] = "0123456789";
char *g_ptr_ref1 = nums + 4;
char *g_ptr_ref2 = &nums[2];
int g_array[] = {10,20,30};
FooStruct g_comp_deficit = (FooStruct){};
int g_comp_array[] = (int[]){11, 22, 33, 0};
int *g_comp_ptr = (int[]){45, 56, 67, 78, 0};
union { int x; struct { char a; short b; } y; } g_union = {.y={.b=77}};
struct {union {int x;};} g_anonymous = {.x = 99};
FooStruct *g_comp_p = &(FooStruct){88};

TEST(initializer) {
  {
    char s[] = "abc";
    s[1] = 'B';
    EXPECT_STREQ("string initializer", "aBc", s);
  }

  {
    enum Num { Zero } num = Zero;
    EXPECT("enum initializer", 0, num);

    enum Num num2 = 67;
    EXPECT("enum initializer2", 67, num2);
  }

  { int x = {34}; EXPECT("brace initializer", 34, x); }

  EXPECT_STREQ("global str-array init", "StrArray", g_strarray);
  EXPECT_STREQ("global str-ptr init", "StrPtr", g_strptr);
  EXPECT_STREQ("global str-ptr-array init", "StrPtrArray", g_strptrarray[0]);
  EXPECT_STREQ("global str-in-struct init", "StrStructArray", g_str_struct_array[0].s);
  EXPECT_STREQ("global char-array-in-struct init", "abc", g_chararray_struct[0].s);
  EXPECT_STREQ("global ptr-ref1", "456789", g_ptr_ref1);
  EXPECT_STREQ("global ptr-ref2", "23456789", g_ptr_ref2);
  EXPECT("global array", 42, sizeof(g_array) + g_array[2]);

  EXPECT("global compound literal init (deficit)", 0, g_comp_deficit.x);
  {
    FooStruct l_comp_deficit = (FooStruct){};
    EXPECT("global compound literal init (deficit)", 0, l_comp_deficit.x);
  }

  {
    int *p, sum;

    p = g_comp_array;
    sum = 0;
    while (*p != 0)
      sum += *p++;
    EXPECT("global compound literal array", 66, sum);

    p = g_comp_ptr;
    sum = 0;
    while (*p != 0)
      sum += *p++;
    EXPECT("global compound literal ptr", 246, sum);

#if !defined(__GNUC__)
    int l_comp_array[] = (int[]){111, 222, 333, 0};
    p = l_comp_array;
    sum = 0;
    while (*p != 0)
      sum += *p++;
    EXPECT("local compound literal array", 666, sum);
#endif

    int *l_comp_ptr = (int[]){1, 11, 111, 1111, 0};
    p = l_comp_ptr;
    sum = 0;
    while (*p != 0)
      sum += *p++;
    EXPECT("local compound literal ptr", 1234, sum);
  }

  {
    static const int array[] = {11, 22, 33};
    static const int *p = array;
    EXPECT("static ref", 22, p[1]);
  }
  {
    static int array[] = {10, 20, 30};
    EXPECT("local static array", 42, sizeof(array) + array[2]);
  }
  {
    int static const a = 34;
    EXPECT("int static const", 34, a);
  }
  {
    struct {int x;} static const a[] = {{67}};
    EXPECT("struct static const", 67, a[0].x);
  }
  {
    struct { union { long a; char b; } x; int y; } static s = {.x={.b=88}, .y=99};
    EXPECT("init struct contain union", 99, s.y);
  }
  {
    int x = sizeof(x);
    EXPECT("sizeof(self) in initializer", 4, x);
  }
  EXPECT("init union", 77, g_union.y.b);
  EXPECT("anonymous union init", 99, g_anonymous.x);

  {
    int *foo = (int[]){1, 2, 3};
    EXPECT("compound literal:array", 2, foo[1]);
  }
  {
    struct Foo {int x;};
    struct Foo foo = (struct Foo){66};
    EXPECT("compound literal:struct", 66, foo.x);

    struct Foo *foop = &(struct Foo){77};
    EXPECT("compound literal:struct ptr", 77, foop->x);
  }
  {
    int i = ++(int){55};
    EXPECT("inc compound literal", 56, i);
  }
  EXPECT("compound literal in global", 88, g_comp_p->x);
  {
    struct S {int x;};
    struct S s = (struct S){44};
    EXPECT("Initializer with compound literal", 44, s.x);

    struct T {char *str;};
    struct T *t = &(struct T){"xyz"};
    EXPECT_STREQ("String in initializer with compound literal", "xyz", t->str);
  }
}

//

void empty_function(void){}
int more_params(int a, int b, int c, int d, int e, int f, char g, long h, short i, unsigned char j) { return a + b + c + d + e + f + g + h + i + j; }
typedef struct {int x; int y;} MoreParamsReturnsStruct;
MoreParamsReturnsStruct more_params_returns_struct(int a, int b, int c, int d, int e, int f, char g, long h, short i, unsigned char j) { return (MoreParamsReturnsStruct){a + b + c + d + e + f + g + h + i + j}; }
int array_arg_wo_size(int arg[]) { return arg[1]; }
long long long_immediate(unsigned long long x) { return x / 11; }

static inline int inline_square(int x) { return x * x; }
static int g_shadow = 55;
static inline void inline_shadow(int x) { g_shadow = x; }
static inline int inline_conflict_varname(int x, int y) { return x * g_shadow + y; }
static inline int inline_share_static_var(int add) { static int sum; if (add == 0) sum = 0; return sum += add; }
static inline int inline_modifiy_param(int x) { int *p = &x; *p += 1; return x + x; }
static inline int inline_factorial(int x) { return x <= 1 ? 1 : x * inline_factorial(x - 1); }
static inline bool inline_even(int x);
static inline bool inline_odd(int x)  { return x == 0 ? false : inline_even(x - 1); }
static inline bool inline_even(int x)  { return x == 0 ? true : inline_odd(x - 1); }
static inline MoreParamsReturnsStruct inline_returns_struct(int x, int y) { return (MoreParamsReturnsStruct){-x, ~y}; }

int mul2(int x) {return x * 2;}
int div2(int x) {return x / 2;}
int (*func_ptr_array[])(int) = {mul2, div2};

typedef unsigned char u8;
u8 const_typedefed(const u8 x);
u8 const_typedefed(const unsigned char x) { return x - 1;}

int vaarg_and_array(int n, ...) {
  int a[14 * 2];
  for (int i = 0; i < 14 * 2; ++i)
    a[i] = 100 + i;
  va_list ap;
  va_start(ap, n);
  int sum = 0;
  for (int i = 0; i < n; ++i)
    sum += va_arg(ap, int);
  va_end(ap);
  return sum;
}

int (*fnptr(int (*fn)(int n, ...)))(int, ...) {
  return fn;
}

int use_alloca(int index) {
  int *a = alloca(10 * sizeof(*a));
  for (int i = 0; i < 10; ++i)
    a[i] = i;
  return a[index];
}

bool add_n_false(int *dst, int add) {
  *dst += add;
  return false;
}

bool add_n_true(int *dst, int add) {
  *dst += add;
  return true;
}

int identity(int x) { return x; }

int 漢字(int χ) { return χ * χ; }

const char *get_FUNCTION(void) { return __FUNCTION__; }
const char *get_func(void) { return __func__; }

TEST(function) {
  empty_function();
  EXPECT("more params", 55, more_params(1, 2, 3, 4, 5, 6, 7, 8, 9, 10));
  {
    MoreParamsReturnsStruct s = more_params_returns_struct(11, 22, 33, 44, 55, 66, 77, 88, 99, 110);
    EXPECT("more params w/ struct", 605, s.x);
  }

  // EXPECT("proto in func", 78, 'int main(){ int sub(int); return sub(77); } int sub(int x) { return x + 1; }')
  {
    extern long extern_in_func;
    extern_in_func = 45;
    EXPECT("extern in func", 45, extern_in_func);
  }
  {
    extern int extern_array_wo_size[];
    EXPECT("array arg w/o size", 22, array_arg_wo_size(extern_array_wo_size));
  }
  EXPECT_PTREQ(empty_function, &empty_function);  // "func ref"
  EXPECT_PTREQ((void*)empty_function, (void*)*empty_function);  // "func deref"
  {
    int acc = 0;
    for (int i = 0; i < 2; ++i)
      acc += func_ptr_array[i](12);
    EXPECT("func-ptr-array", 30, acc);

    int (*funcs[])(int) = {div2, sq, mul2};
    int value = 18;
    for (int i = 0; i < 3; ++i)
      value = funcs[i](value);
    EXPECT("func-ptr-array in local", 162, value);
  }
  {
    int w = 0, x = 2, y = 5;
    int z = sub(++x, y += 10);
    EXPECT("modify arg", 6, x + y + z + w);
  }

  EXPECT("long immediate", 119251678860344574LL, long_immediate(0x123456789abcdef0));
  EXPECT("const typedef-ed type", 65, const_typedefed(66));

  EXPECT("inline", 1522756, inline_square(1234));
  EXPECT("inline nest", 65536, inline_square(inline_square(16)));
  {
    int (*f)(int) = inline_square;
    EXPECT("inline fnptr", 321489, f(567));
  }
  {
    {
      int g_shadow = 66;
      inline_shadow(77);
      EXPECT("inline not overwrite local", 66, g_shadow);
    }
    EXPECT("inline overwrite global", 77, g_shadow);
  }
  {
    int x = 3, y = 4;
    g_shadow = 100;
    EXPECT("inline conflict varname", 809, inline_conflict_varname(y * 2, x * 3));
  }
  {
    int result;
    for (int i = 0; i <= 10; ++i)
      result = inline_share_static_var(i);
    EXPECT("static variable in inline func", 55, result);
  }
  {
    int val = 111;
    EXPECT("inline func modifies parameter", 224, inline_modifiy_param(val));
    EXPECT("modifying parameter has no effect", 111, val);
  }
  EXPECT("inline recursion", 5040, inline_factorial(7));
  EXPECT_FALSE(inline_even(9));  // Inline mutual recursion
  EXPECT_TRUE(inline_odd(9));
  EXPECT_TRUE(inline_even(8));
  EXPECT_FALSE(inline_odd(8));
  {
    MoreParamsReturnsStruct r = inline_returns_struct(1234, 5678);
    EXPECT("inline return struct 1", -1234, r.x);
    EXPECT("inline return struct 2", ~5678, r.y);
  }

  EXPECT("stdarg", 55, vaarg_and_array(10, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10));
  EXPECT("vaarg fnptr", 15, fnptr(vaarg_and_array)(5, 1, 2, 3, 4, 5));

  {
    int x = 77;
    int y = use_alloca(5);
    EXPECT("alloca", 82, x + y);
  }

  {
    EXPECT("funcall and shortcut 1", 1,  ({ int x=0; identity(add_n_false(&x, 1) && add_n_true(&x, 10)), x;}));
    EXPECT("funcall and shortcut 2", 11, ({ int x=0; identity(add_n_true(&x, 1)  && add_n_true(&x, 10)), x;}));
    EXPECT("funcall and shortcut 3", 11, ({ int x=0; identity(add_n_false(&x, 1) || add_n_true(&x, 10)), x;}));
    EXPECT("funcall and shortcut 4", 1,  ({ int x=0; identity(add_n_true(&x, 1)  || add_n_true(&x, 10)), x;}));
  }

  EXPECT("unicode", 121, 漢字(11));

  EXPECT_STREQ("__FUNCTION__", "get_FUNCTION", get_FUNCTION());
  EXPECT_STREQ("__func__", "get_func", get_func());
}

long extern_in_func;
int extern_array_wo_size[] = {11, 22, 33};

//

#if !defined(__NO_VLA) && !defined(__STDC_NO_VLA__)
int vla_funparam(int n, int a[n][n], int (*p)[n]) {
  int acc = 0;
  for (int i = 0; i < n; ++i)
    for (int j = 0; j < n; ++j)
      acc += a[i][j];
  for (int i = 0; i < n; ++i)
    for (int j = 0; j < n; ++j)
      acc += p[i][j];
  return acc;
}

TEST(vla) {
  {
    int n = 3;
    int arr[n];
    EXPECT("vla size", n * sizeof(int), sizeof(arr));
    for (int i = 0; i < n; ++i)
      arr[i] = (i + 1) * (i + 1);
    int total = 0;
    for (int i = 0; i < n; ++i)
      total += arr[i];
    EXPECT("vla", 14, total);
  }

  {
    int n = 3;
    typedef long ARR[n];
    EXPECT("vla size", n * sizeof(long), sizeof(ARR));
  }

  {
    int n2 = 3;
    int arr2[++n2];
    EXPECT("vla side-effect", 4 * sizeof(int), sizeof(arr2));

    int n3 = 2;
    int arr31[n3][n3];
    int arr32[3][n3];
    n3 = -1;
    EXPECT("vla size var modified", 4 * sizeof(int), sizeof(arr31));
    EXPECT("vla size var modified 2", 6 * sizeof(int), sizeof(arr32));

    int n4 = 3;
    typedef char ARR4[n4++];
    ARR4 arr4;
    EXPECT("typedef vla side-effect", 3 * sizeof(char), sizeof(arr4));
  }

  {
    int n = 4;
    typedef short ARR[n][n];
    n = 0;
    ARR arr;
    EXPECT("vla[][] size", 16 * sizeof(short), sizeof(arr));
    EXPECT("vla[0][] size", 4 * sizeof(short), sizeof(arr[0]));
  }

  {
    int n = 3;
    const char s[] = "AbcDefGhi";
    char (*p)[n] = (void*)s;  // p is a pointer to char[n].
    EXPECT("vla ptr", 'G', *p[2]);
    EXPECT("vla sizeof(ptr)", sizeof(char*), sizeof(p));
    EXPECT("vla sizeof(*ptr)", 3 * sizeof(char), sizeof(*p));

    typedef char (*PPP)[n];
    EXPECT("typedef vla sizeof(*ptr)", 3 * sizeof(char), sizeof(*(PPP)NULL));
  }

  {
    int a[2][2] = {{1000, 200}, {30, 4}};
    int (*p)[2] = a;
    EXPECT("vla size", 2468, vla_funparam(2, a, p));
  }
}
#endif

//

TEST(extension) {
  {
#define GENERIC_FUNC(x) _Generic((x), int: 1, long: 2, uint64_t: 3, char*: 10, const char*: 11, void*: 12, default: 99)
    EXPECT("generic int",         1, GENERIC_FUNC(123));
    EXPECT("generic const ignored", 1, _Generic((const int)123, const int: 2, int: 1, default: 3));
    EXPECT("generic long",        2, GENERIC_FUNC(456L));
    EXPECT("generic uint64_t",    3, GENERIC_FUNC((uint64_t)789));
    EXPECT("generic str literal", 10, GENERIC_FUNC("str"));
    const char str[] = "str";
    EXPECT("generic char array",  11, GENERIC_FUNC(str));
    EXPECT("generic char array2", 10, _Generic((char*)str, const char*: 11, char*: 10));
    int a[] = {1, 2, 3};
    EXPECT("generic pointer",     99, GENERIC_FUNC(a));
    EXPECT("generic pointer",     99, GENERIC_FUNC((char)'x'));

#ifndef __NO_FLONUM
#define GENERIC_FLONUM(x) _Generic((x), int: 1, float: 2, double: 3, long double: 4, default: 99)
    EXPECT("generic float",       2, GENERIC_FLONUM(1.0f));
    EXPECT("generic double",      3, GENERIC_FLONUM(2.3));
    EXPECT("generic long double", 4, GENERIC_FLONUM(4.5L));
#endif

    {
      typedef int (*fptr)(int);
      extern int dummy_func_for_generic(int i);
      EXPECT("generic function", 3, _Generic(dummy_func_for_generic, fptr: 3, int: 4));
    }

#undef GENERIC_FUNC
#undef GENERIC_FLONUM
  }

  {
    __auto_type x = 1 + 2 * 3;
    EXPECT("auto type size", sizeof(int), sizeof(x));
    EXPECT("auto type", 7, x);

    __auto_type f = &foo;  // int (*f)(void)
    EXPECT("auto type fnptr", 123, f());
  }
}

TEST(builtin) {
  EXPECT("builtin clz", sizeof(0U) * CHAR_BIT - 20, __builtin_clz(0x000f0f00U));
  EXPECT("builtin clzl", sizeof(0UL) * CHAR_BIT - 20, __builtin_clzl(0x000f0f00UL));
  EXPECT("builtin clzll", sizeof(0ULL) * CHAR_BIT - 20, __builtin_clzll(0x000f0f00ULL));
  EXPECT("builtin ctz", 8, __builtin_ctz(0x000f0f00U));
  EXPECT("builtin ctzl", 8, __builtin_ctzl(0x000f0f00UL));
  EXPECT("builtin ctzll", 8, __builtin_ctzll(0x000f0f00ULL));
  EXPECT("builtin popcount", 8, __builtin_popcount(0x000f0f00U));
  EXPECT("builtin popcountl", 8, __builtin_popcountl(0x000f0f00UL));
  EXPECT("builtin popcountll", 8, __builtin_popcountll(0x000f0f00ULL));

#ifndef __NO_FLONUM
  {
    union { double nan; uint64_t x; } u;
    u.nan = __builtin_nan("0xdeadbeefcafebabe");
    EXPECT_TRUE(isnan(u.nan));
    EXPECT("builtin nan", 0x7ffdbeefcafebabe, u.x);
  }
#endif
}

//

XTEST_MAIN();
