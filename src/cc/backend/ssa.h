#pragma once

// Single Static Assignment

typedef struct Vector BBContainer;
typedef struct RegAlloc RegAlloc;

void make_ssa(RegAlloc *ra, BBContainer *bbcon);
