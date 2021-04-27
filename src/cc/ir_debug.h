#pragma once

#if !defined(SELF_HOSTING) && !defined(__XV6)
typedef struct Vector Vector;

void do_dump_ir(Vector *decls);
#endif
