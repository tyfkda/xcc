#pragma once

// Architecture dependent.

typedef struct Vector BBContainer;  // <BB*>
typedef struct Function Function;
typedef struct FuncBackend FuncBackend;
typedef struct IR IR;
typedef struct RegAllocSettings RegAllocSettings;

enum RegSet {
  GPREG,  // General-purpose
  FPREG,  // Floating-point
};

typedef struct {
  int max_reg_args[2];  // [0]=gp-reg, [1]=fp-reg
} ArchSetting;

extern const ArchSetting kArchSetting;
extern const RegAllocSettings kArchRegAllocSettings;

void tweak_irs(FuncBackend *fnbe);
int calculate_func_param_bottom(Function *func);

void emit_bb_irs(BBContainer *bbcon);
void emit_defun_body(Function *func);

typedef void (*EmitIrFunc)(IR*);
extern const EmitIrFunc kEmitIrFuncTable[];
