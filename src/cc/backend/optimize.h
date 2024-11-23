#pragma once

typedef struct Vector BBContainer;
typedef struct RegAlloc RegAlloc;

void optimize(RegAlloc *ra, BBContainer *bbcon);
