#pragma once

#define PRIdPTR  "ld"
#define PRIxPTR  "lx"

#if defined(__ILP32__)

#define PRId64  "lld"
#define PRIu64  "llu"

#elif defined(__LP64__)

#define PRId64  "ld"
#define PRIu64  "lu"

#else
// ?
#endif

#define PRIdMAX  "lld"
#define PRIuMAX  "llu"
#define PRIoMAX  "llo"
