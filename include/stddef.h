#pragma once

#ifndef NULL
#define NULL  ((void*)0)
#endif

#define offsetof(S, mem)  ((size_t)&(((S *)0)->mem))

typedef unsigned long size_t;
typedef long ptrdiff_t;

//

#ifndef _WCHAR_T_DEFINED
#define _WCHAR_T_DEFINED
typedef unsigned int wchar_t;
#endif
