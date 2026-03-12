#pragma once

#define PRId32   "d"
#define PRIu32   "u"
#define PRIo32   "o"
#define PRIx32   "x"

#define PRIdPTR  "ld"
#define PRIuPTR  "lu"
#define PRIoPTR  "lo"
#define PRIxPTR  "lx"

#define PRIdMAX  "lld"
#define PRIuMAX  "llu"
#define PRIoMAX  "llo"
#define PRIxMAX  "llx"

#if defined(__ILP32__)

#define PRId64   "lld"
#define PRIu64   "llu"
#define PRIo64   "llo"
#define PRIx64   "llx"

#elif defined(__LP64__)

#define PRId64   "ld"
#define PRIu64   "lu"
#define PRIo64   "lo"
#define PRIx64   "lx"

#else
// ?
#endif

#define PRId8    PRId32
#define PRIu8    PRIu32
#define PRIo8    PRIo32
#define PRIx8    PRIx32

#define PRId16   PRId32
#define PRIu16   PRIu32
#define PRIo16   PRIo32
#define PRIx16   PRIx32
