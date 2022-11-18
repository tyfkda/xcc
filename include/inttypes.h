#pragma once

#define PRIdPTR  "ld"
#define PRIxPTR  "lx"

#if defined(__ILP32__)

#define PRId64  "lld"

#elif defined(__LP64__)

#define PRId64  "ld"

#else
// ?
#endif
