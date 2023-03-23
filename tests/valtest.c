#include "stdarg.h"
#include "stddef.h"  // offsetof
#include "stdint.h"
#include "stdio.h"
#include "stdlib.h"  // exit

#include "./xtest.h"

#if defined(__LP64__)
#define LONG_SIZE  (8)
#elif defined(__ILP32__)
#define LONG_SIZE  (4)
#else
#error Environment unknonwn
#endif

#define EXPECT(title, expected, actual)  expecti64(title, expected, actual)

int g_zero, g_work;
int g_init = (8 * 7 + 100 / 4 - 50 % 3) & 0x5a | 0x100;

void *null = (void*)0;

struct {int x; int *p;} g_struct = { 42, &g_zero };

static int s_val = 456;

extern int e_val;

short protodecl(short);

int foo() {
  return 123;
}

int sqsub(int x, int y) {
  int xx = x * x; int yy = y * y;
  return xx - yy;
}

int sub(int x, int y) {
  return x - y;
}

int apply(int (*f)(int, int), int x, int y) {
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

int static_local(void) {
  static int x = 42;
  return ++x;
}

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
  EXPECT("negative", -42, (x=42, -x));
  EXPECT("long", 123, 123L);
  { long long x = 9876543LL; EXPECT("long long", 9876543, x); }
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
    EXPECT("assign", 1, a == b);
  }
  EXPECT("!=", 1, (x=123, x != 456));
  EXPECT("not true", 0, (x=1, !(x == 1)));
  EXPECT("not false", 1, (x=1, !(x == 0)));
  EXPECT("not str", 0, !"abc");
  EXPECT("bit not", 0x12345678, (x=0xedcba987, ~x));
  {
    int x = 1;
    int y = ++x;
    EXPECT("preinc", 4, x + y);
  }
  {
    int x = 1;
    int y = --x;
    EXPECT("preinc", 0, x + y);
  }
  {
    int x = 1;
    int y = x++;
    EXPECT("postinc", 3, x + y);
  }
  {
    int x = 1;
    int y = x--;
    EXPECT("postinc", 1, x + y);
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
    EXPECT("unsigned cmp", 1, (x > (unsigned char)0));
    if (x <= (unsigned char)0)
      fail("unsigned cmp jmp");
  }
  {
    // C implicitly cast to unsigned type if it handles mixed signed values.
    int minus = -1;
    unsigned int uone = 1U;
    EXPECT("compare with different sign1", 1, minus > uone);  // !!!
    EXPECT("compare with different sign2", 1, uone < minus);  // !!!
  }

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
    switch (1) {
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
    switch (2) {
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
    switch (3) {
    case 1:
      x = 11;
      break;
    }
    EXPECT("switch no-default", 0, x);
  }
  {
    int x = 0;
    switch (1) {
    case 1:
      x += 1;
      // Fallthrough
    default:
      x += 10;
    }
    EXPECT("switch fallthrough", 11, x);
  }
  {
    int x = 10, *p = &x;
    ++(*p);
    EXPECT("pointer", 11, x);
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
  EXPECT("global struct initializer: ptr", (long)&g_zero, (long)g_struct.p);
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
    EXPECT("anonymous adr", (long)&a, (long)&a.x);
  }
  EXPECT("func pointer", 9, apply(&sub, 15, 6));
  EXPECT("func pointer w/o &", 9, apply(sub, 15, 6));
  EXPECT("block comment", 123, /* comment */ 123);
  EXPECT("line comment", 123, // comment
         123);
  EXPECT("proto decl", 12321, protodecl(111));
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
    if (!(1 && 0))
      x = 1;
    EXPECT("conditional !(t && t)", 1, x);
  }
  {
    int x = 0;
    if (0 || 1)
      x = 1;
    EXPECT("conditional (f || t)", 1, x);
  }
  {
    int x = 1;
    { int x = 2; }
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

  EXPECT("sizeof(int)", 4, sizeof(int));
  EXPECT("sizeof(long)", LONG_SIZE, sizeof(long));
  EXPECT("int8_t",  1, sizeof(int8_t));
  EXPECT("int16_t", 2, sizeof(int16_t));
  EXPECT("int32_t", 4, sizeof(int32_t));
  EXPECT("int64_t", 8, sizeof(int64_t));
#if defined(__LP64__)
  EXPECT("intptr_t", 8, sizeof(intptr_t));
#elif defined(__ILP32__)
  EXPECT("intptr_t", 4, sizeof(intptr_t));
#endif
  EXPECT("sizeof(void)", 1, sizeof(void));
  EXPECT("sizeof(array)", 3, sizeof(char [3]));
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

  {
    int64_t x = 0x123456789abcdef0LL;
    EXPECT("64bit literal 1", 0xdef0,  x        & 0xffff);
    EXPECT("64bit literal 2", 0x9abc, (x >> 16) & 0xffff);
    EXPECT("64bit literal 3", 0x5678, (x >> 32) & 0xffff);
    EXPECT("64bit literal 4", 0x1234, (x >> 48) & 0xffff);
  }
} END_TEST()

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

    EXPECT("value 1", 1, s.x);
    EXPECT("value 2", 22, s.y);
    EXPECT("value 3", 333, s.z);

    struct S s2 = s;
    EXPECT("value 1'", 1, s2.x);
    EXPECT("value 2'", 22, s2.y);
    EXPECT("value 3'", 333, s2.z);
  }
} END_TEST()

int main(void) {
  return RUN_ALL_TESTS(
    test_all,
    test_bitfield,
  );
}

int e_val = 789;

int extern_only = 22;

short protodecl(short x) {
  return x * x;
}
