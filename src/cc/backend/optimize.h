#pragma once

typedef struct BBContainer BBContainer;
typedef struct RegAlloc RegAlloc;

void optimize(RegAlloc *ra, BBContainer *bbcon);
