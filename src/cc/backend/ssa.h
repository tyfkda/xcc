// Static Single Assignment

#pragma once

typedef struct Vector BBContainer;
typedef struct RegAlloc RegAlloc;

void make_ssa(RegAlloc *ra, BBContainer *bbcon);
void resolve_phis(RegAlloc *ra, BBContainer *bbcon);
