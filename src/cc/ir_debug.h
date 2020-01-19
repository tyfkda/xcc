#pragma once

#if !defined(SELF_HOSTING)
typedef struct Vector Vector;

void do_dump_ir(Vector *toplevel);
#endif
