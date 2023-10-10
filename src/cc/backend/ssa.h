#pragma once

// Single Static Assignment

typedef struct BBContainer BBContainer;
typedef struct RegAlloc RegAlloc;

void make_ssa(RegAlloc *ra, BBContainer *bbcon);
void resolve_phis(BBContainer *bbcon);
