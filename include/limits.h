#pragma once

#define CHAR_BIT   (8)  // Number of bits in one char

#define CHAR_MIN   (-128)  // -(2^7)
#define CHAR_MAX   (127)   //  (2^7)-1
#define UCHAR_MAX  (255)   // (2^8)-1

#define SHRT_MIN   (-32768)  // -(2^15)
#define SHRT_MAX   ( 32767)  //  (2^15)-1
#define USHRT_MAX  (65535)  // (2^16)-1

#define INT_MIN    (-2147483648)  // -(2^31)
#define INT_MAX    ( 2147483647)  //  (2^31)-1
#define UINT_MAX   (4294967295)  // (2^32)-1

#if defined(__ILP32__)

#define LONG_MIN   (-2147483648)  // -(2^31)
#define LONG_MAX   ( 2147483647)  //  (2^31)-1
#define ULONG_MAX  (4294967295)  // (2^32)-1

#define LLONG_MIN  (-9223372036854775808LL)  // -(2^63)
#define LLONG_MAX  ( 9223372036854775807LL)  //  (2^63)-1
#define ULLONG_MAX (18446744073709551615ULL)  // (2^64)-1

#elif defined(__LP64__)

#define LONG_MIN   (-9223372036854775808L)  // -(2^63)
#define LONG_MAX   ( 9223372036854775807L)  //  (2^63)-1
#define ULONG_MAX  (18446744073709551615UL)  // (2^64)-1

#define LLONG_MIN  (-9223372036854775808LL)  // -(2^63)
#define LLONG_MAX  ( 9223372036854775807LL)  //  (2^63)-1
#define ULLONG_MAX (18446744073709551615ULL)  // (2^64)-1

#endif

#define PATH_MAX  (512)
