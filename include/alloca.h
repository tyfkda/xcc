#pragma once

#ifdef __WASM
#error Cannot use alloca in wcc
#else

#include "stddef.h"  // size_t

void *alloca(size_t size);

#endif
