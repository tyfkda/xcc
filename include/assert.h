#pragma once

#if defined(NDEBUG)
#define assert(x)  ((void)0)

#elif defined(__APPLE__)

void __assert_rtn(const char *, const char *, int, const char *); // __dead2 __cold __disable_tail_calls;
#define assert(x)  ((x) ? ((void)0) : __assert_rtn(0, __FILE__, __LINE__, #x))

#else

extern void __assert_fail(const char *assertion, const char *fn, int lineno, const char *func);
#define assert(x)  ((x) ? ((void)0) : __assert_fail(#x, __FILE__, __LINE__, 0))

#endif
