#pragma once

#include <inttypes.h>  // PRIdPTR

#ifndef ADD_LABEL
#define ADD_LABEL(label)  add_asm_label(label)
#endif
#ifndef ADD_ASM
#define ADD_ASM(...)  add_asm(__VA_ARGS__)
#endif

#define MOV_IM_AL(x)     ADD_ASM("mov $%d, %%al", (int)(x))
#define MOV_IM_AX(x)     ADD_ASM("mov $%d, %%ax", (int)(x))
#define MOV_IM_EAX(x)    ADD_ASM("mov $%d, %%eax", (int)(x))
#define MOV_IM_EDI(x)    ADD_ASM("mov $%d, %%edi", (int)(x))
#define MOV_IM_RAX(x)    ADD_ASM("mov $%" PRIdPTR ", %%rax", (intptr_t)(x))
#define MOV_IM_RDI(x)    ADD_ASM("mov $%" PRIdPTR ", %%rdi", (intptr_t)(x))
#define MOV_IM_RDX(x)    ADD_ASM("mov $%" PTRIdPTR ", %%rdx", (intptr_t)(x))
#define MOVSX_AL_AX()    ADD_ASM("movsx %%al, %%ax")
#define MOVSX_AL_EAX()   ADD_ASM("movsx %%al, %%eax")
#define MOVSX_AL_RAX()   ADD_ASM("movsx %%al, %%rax")
#define MOVSX_AX_EAX()   ADD_ASM("movsx %%ax, %%eax")
#define MOVSX_AX_RAX()   ADD_ASM("movsx %%ax, %%rax")
#define MOVSX_EAX_RAX()  ADD_ASM("movsx %%eax, %%rax")
#define MOVSX_EAX_RDI()  ADD_ASM("movsx %%eax, %%rdi")
#define MOV_EAX_EDI()    ADD_ASM("mov %%eax, %%edi")
#define MOV_RAX_RSI()    ADD_ASM("mov %%rax, %%rsi")
#define MOV_RAX_RDI()    ADD_ASM("mov %%rax, %%rdi")
#define MOV_DL_AL()      ADD_ASM("mov %%dl, %%al")
#define MOV_DX_AX()      ADD_ASM("mov %%dx, %%ax")
#define MOV_EDX_EAX()    ADD_ASM("mov %%edx, %%eax")
#define MOV_RDX_RAX()    ADD_ASM("mov %%rdx, %%rax")
#define MOV_RDI_RAX()    ADD_ASM("mov %%rdi, %%rax")
#define MOV_RDI_RCX()    ADD_ASM("mov %%rdi, %%rcx")
#define MOV_RSP_RBP()    ADD_ASM("mov %%rsp, %%rbp")
#define MOV_RBP_RSP()    ADD_ASM("mov %%rbp, %%rsp")
#define MOV_RBP_RAX()    ADD_ASM("mov %%rbp, %%rax")
#define MOV_AL_IND_RSI()   ADD_ASM("mov %%al, (%%rsi)")
#define MOV_AL_IND_RDI()   ADD_ASM("mov %%al, (%%rdi)")
#define MOV_AX_IND_RSI()   ADD_ASM("mov %%ax, (%%rsi)")
#define MOV_AX_IND_RDI()   ADD_ASM("mov %%ax, (%%rdi)")
#define MOV_IND_RAX_AL()   ADD_ASM("mov (%%rax), %%al")
#define MOV_IND_RAX_DIL()  ADD_ASM("mov (%%rax), %%dil")
#define MOV_IND_RAX_AX()   ADD_ASM("mov (%%rax), %%ax")
#define MOV_IND_RAX_DI()   ADD_ASM("mov (%%rax), %%di")
#define MOV_IND_RAX_EAX()  ADD_ASM("mov (%%rax), %%eax")
#define MOV_IND_RAX_EDI()  ADD_ASM("mov (%%rax), %%edi")
#define MOV_IND_RAX_RAX()  ADD_ASM("mov (%%rax), %%rax")
#define MOV_IND_RAX_RDI()  ADD_ASM("mov (%%rax), %%rdi")
#define MOV_RAX_IND_RAX()  ADD_ASM("mov %%rax, (%%rax)")
#define MOV_EAX_IND_RSI()  ADD_ASM("mov %%eax, (%%rsi)")
#define MOV_EAX_IND_RDI()  ADD_ASM("mov %%eax, (%%rdi)")
#define MOV_RAX_IND_RSI()  ADD_ASM("mov %%rax, (%%rsi)")
#define MOV_RAX_IND_RDI()  ADD_ASM("mov %%rax, (%%rdi)")
#define MOV_RDI_IND_RAX()  ADD_ASM("mov %%rdi, (%%rax)")
#define MOV_DIL_AL()           ADD_ASM("mov %%dil, %%al")
#define MOV_DIL_CL()           ADD_ASM("mov %%dil, %%cl")
#define MOV_DI_AX()            ADD_ASM("mov %%di, %%ax")
#define MOV_DI_CX()            ADD_ASM("mov %%di, %%cx")
#define MOV_EDI_EAX()          ADD_ASM("mov %%edi, %%eax")
#define MOV_EDI_ECX()          ADD_ASM("mov %%edi, %%ecx")
#define MOV_DIL_IND8_RBP(ofs)  ADD_ASM("mov %%dil, %d(%%rbp)", (int)(ofs))
#define MOV_EDI_IND8_RBP(ofs)  ADD_ASM("mov %%edi, %d(%%rbp)", (int)(ofs))
#define MOV_RDI_IND8_RBP(ofs)  ADD_ASM("mov %%rdi, %d(%%rbp)", (int)(ofs))
#define MOV_SIL_IND8_RBP(ofs)  ADD_ASM("mov %%sil, %d(%%rbp)", (int)(ofs))
#define MOV_ESI_IND8_RBP(ofs)  ADD_ASM("mov %%esi, %d(%%rbp)", (int)(ofs))
#define MOV_RSI_IND8_RBP(ofs)  ADD_ASM("mov %%rsi, %d(%%rbp)", (int)(ofs))
#define MOV_DL_IND8_RBP(ofs)   ADD_ASM("mov %%dl, %d(%%rbp)", (int)(ofs))
#define MOV_EDX_IND8_RBP(ofs)  ADD_ASM("mov %%edx, %d(%%rbp)", (int)(ofs))
#define MOV_RDX_IND8_RBP(ofs)  ADD_ASM("mov %%rdx, %d(%%rbp)", (int)(ofs))
#define MOV_CL_IND8_RBP(ofs)   ADD_ASM("mov %%cl, %d(%%rbp)", (int)(ofs))
#define MOV_ECX_IND8_RBP(ofs)  ADD_ASM("mov %%ecx, %d(%rbp)", (int)(ofs))
#define MOV_RCX_IND8_RBP(ofs)  ADD_ASM("mov %%rcx, %d(%%rbp)", (int)(ofs))
#define MOV_R8B_IND8_RBP(ofs)  ADD_ASM("mov %%r8b, %d(%%rbp)", (int)(ofs))
#define MOV_R8D_IND8_RBP(ofs)  ADD_ASM("mov %%r8d, %d(%%rbp)", (int)(ofs))
#define MOV_R8_IND8_RBP(ofs)   ADD_ASM("mov %%r8,%d(%%rbp)", (int)(ofs))
#define MOV_R9B_IND8_RBP(ofs)  ADD_ASM("mov %%r9b,%d(%%rbp)", (int)(ofs))
#define MOV_R9D_IND8_RBP(ofs)  ADD_ASM("mov %%r9d, %d(%%rbp)", (int)(ofs))
#define MOV_R9_IND8_RBP(ofs)   ADD_ASM("mov %%r9, %d(%%rbp)", (int)(ofs))
#define MOV_IND8_RSP_RDI(ofs)  ADD_ASM("mov %d(%%rsp), %%rdi", (int)(ofs))
#define MOV_IND8_RSP_RSI(ofs)  ADD_ASM("mov %d(%%rsp), %%rsi", (int)(ofs))
#define MOV_IND8_RSP_RDX(ofs)  ADD_ASM("mov %d(%%rsp), %%rdx", (int)(ofs))
#define LEA_OFS8_RSP_RSI(ofs)       ADD_ASM("lea %d(%rsp),%rsi", (int)(ofs))
#define LEA_OFS32_RBP_RAX(ofs)      ADD_ASM("lea %d(%rbp),%rax", (int)ofs)
#define LEA_OFS32_RBP_RSI(ofs)      ADD_ASM("lea %d(%rbp),%rsi", (int)ofs)
#define LEA_LABEL32_RAX(label)      ADD_ASM("lea %s,%rax", label)
#define LEA_LABEL32_RIP_RAX(label)  ADD_ASM("lea %s(%rip),%rax", label)
#define ADD_DIL_AL()         ADD_ASM("add %%dil, %%al")
#define ADD_DI_AX()          ADD_ASM("add %%di, %%ax")
#define ADD_EDI_EAX()        ADD_ASM("add %%edi, %%eax")
#define ADD_RDI_RAX()        ADD_ASM("add %%rdi, %%rax")
#define ADD_IM_RSP(x)        ADD_ASM("add $%" PRIdPTR ", %%rsp", (intptr_t)(x))
#define ADD_IM_RAX(x)        ADD_ASM("add $%" PRIdPTR ", %%rax", (intptr_t)(x))
#define ADDQ_IM_IND_RAX(x)   ADD_ASM("addq $%" PRIdPTR ", (%%rax)", (intptr_t)(x))
#define ADD_IND_RDI_RAX()    ADD_ASM("add (%%rdi), %%rax")
#define SUB_DIL_AL()         ADD_ASM("sub %%dil, %%al")
#define SUB_DI_AX()          ADD_ASM("sub %%di, %%ax")
#define SUB_EDI_EAX()        ADD_ASM("sub %%edi, %%eax")
#define SUB_RDI_RAX()        ADD_ASM("sub %%rdi, %%rax")
#define SUB_IM_RSP(x)        ADD_ASM("sub $%" PRIdPTR ", %%rsp", (intptr_t)(x))
#define SUB_IM_RAX(x)        ADD_ASM("sub $%" PRIdPTR ", %%rax", (intptr_t)(x))
#define SUB_IM_RSP(x)        ADD_ASM("sub $%" PRIdPTR ", %%rsp", (intptr_t)(x))
#define SUBQ_IM_IND_RAX(x)   ADD_ASM("subq $%" PRIdPTR ", (%%rax)", (intptr_t)(x))
#define MUL_DIL()        ADD_ASM("mul %%dil")
#define MUL_DI()         ADD_ASM("mul %%di")
#define MUL_EDI()        ADD_ASM("mul %%edi")
#define MUL_RDI()        ADD_ASM("mul %%rdi")
#define DIV_DIL()        ADD_ASM("div %%dil")
#define DIV_DI()         ADD_ASM("div %%di")
#define DIV_EDI()        ADD_ASM("div %%edi")
#define DIV_RDI()        ADD_ASM("div %%rdi")
#define CMP_AL_DIL()     ADD_ASM("cmp %%al, %%dil")
#define CMP_AX_DI()      ADD_ASM("cmp %%ax, %%dx")
#define CMP_EAX_EDI()    ADD_ASM("cmp %%eax, %%edi")
#define CMP_RAX_RDI()    ADD_ASM("cmp %%rax, %%rdi")
#define CMP_RDI_RAX()    ADD_ASM("cmp %%rdi, %%rax")
#define CMP_IM_AL(x)     ADD_ASM("cmp $%d, %%al", (int)(x))
#define CMP_IM_DIL(x)    ADD_ASM("cmp $%d, %%dil", (int)(x))
#define CMP_IM_AX(x)     ADD_ASM("cmp $%d, %%ax", (int)(x))
#define CMP_IM_EAX(x)    ADD_ASM("cmp $%d, %%eax", (int)(x))
#define CMP_IM_EDI(x)    ADD_ASM("cmp $%d, %%edi", (int)(x))
#define CMP_IM_RAX(x)    ADD_ASM("cmp $%" PRIdPTR ", %%rax", (intptr_t)(x))
#define CMP_IM_RDI(x)    ADD_ASM("cmp $%" PRIdPTR ", %%rdi", (intptr_t)(x))
#define TEST_AL_AL()     ADD_ASM("test %%al, %%al")
#define TEST_AX_AX()     ADD_ASM("test %%ax, %%ax")
#define TEST_EAX_EAX()   ADD_ASM("test %%eax, %%eax")
#define TEST_RAX_RAX()   ADD_ASM("test %%rax, %%rax")
#define INC_RSI()        ADD_ASM("inc %%rsi")
#define INCB_IND_RAX()   ADD_ASM("incb (%%rax)")
#define INCW_IND_RAX()   ADD_ASM("incw (%%rax)")
#define INCL_IND_RAX()   ADD_ASM("incl (%%rax)")
#define INCQ_IND_RAX()   ADD_ASM("incq (%%rax)")
#define DEC_EDI()        ADD_ASM("dec %%edi")
#define DECB_IND_RAX()   ADD_ASM("decb (%%rax)")
#define DECW_IND_RAX()   ADD_ASM("decb (%%rax)")
#define DECL_IND_RAX()   ADD_ASM("decl (%%rax)")
#define DECQ_IND_RAX()   ADD_ASM("decq (%%rax)")
#define AND_DIL_AL()     ADD_ASM("and %%dil, %%al")
#define AND_DI_AX()      ADD_ASM("and %%di, %%ax")
#define AND_EDI_EAX()    ADD_ASM("and %%edi, %%eax")
#define AND_RDI_RAX()    ADD_ASM("and %%rdi, %%rax")
#define OR_DIL_AL()      ADD_ASM("or %%dil, %%al")
#define OR_DI_AX()       ADD_ASM("or %%di, %%ax")
#define OR_EDI_EAX()     ADD_ASM("or %%edi, %%eax")
#define OR_RDI_RAX()     ADD_ASM("or %%rdi, %%rax")
#define XOR_DIL_AL()     ADD_ASM("xor %%dil, %%al")
#define XOR_DI_AX()      ADD_ASM("xor %%di, %%ax")
#define XOR_EDI_EAX()    ADD_ASM("xor %%edi, %%eax")
#define XOR_RDI_RAX()    ADD_ASM("xor %%rdi, %%rax")
#define XOR_AL_AL()      ADD_ASM("xor %%al, %%al")
#define XOR_AX_AX()      ADD_ASM("xor %%ax, %%ax")
#define XOR_EAX_EAX()    ADD_ASM("xor %%eax, %%eax")
#define XOR_EDX_EDX()    ADD_ASM("xor %%edx, %%edx")
#define SHL_CL_AL()      ADD_ASM("shl %%cl, %%al")
#define SHL_CL_AX()      ADD_ASM("shl %%cl, %%ax")
#define SHL_CL_EAX()     ADD_ASM("shl %%cl, %%eax")
#define SHL_CL_RAX()     ADD_ASM("shl %%cl, %%rax")
#define SHR_CL_AL()      ADD_ASM("shr %%cl, %%al")
#define SHR_CL_AX()      ADD_ASM("shr %%cl, %%ax")
#define SHR_CL_EAX()     ADD_ASM("shr %%cl, %%eax")
#define SHR_CL_RAX()     ADD_ASM("shr %%cl, %%rax")
#define NEG_AL()         ADD_ASM("neg %%al")
#define NEG_AX()         ADD_ASM("neg %%ax")
#define NEG_EAX()        ADD_ASM("neg %%eax")
#define NEG_RAX()        ADD_ASM("neg %%rax")
#define SETE_AL()        ADD_ASM("sete %%al")
#define SETNE_AL()       ADD_ASM("setne %%al")
#define SETS_AL()        ADD_ASM("sets %%al")
#define SETNS_AL()       ADD_ASM("setns %%al")
#define SETL_AL()        ADD_ASM("setl %%al")
#define SETG_AL()        ADD_ASM("setg %%al")
#define SETLE_AL()       ADD_ASM("setle %%al")
#define SETGE_AL()       ADD_ASM("setge %%al")
#define SAR_RAX(x)       ADD_ASM("sar %%rax")
#define SAR_IM_RAX(x)    ADD_ASM("sar $%d, %%rax", (int)(x))
#define PUSH_RAX()       ADD_ASM("push %%rax")
#define PUSH_RBP()       ADD_ASM("push %%rbp")
#define PUSH_RDI()       ADD_ASM("push %%rdi")
#define POP_RAX()        ADD_ASM("pop %%rax")
#define POP_RCX()        ADD_ASM("pop %%rcx")
#define POP_RDX()        ADD_ASM("pop %%rdx")
#define POP_RBP()        ADD_ASM("pop %%rbp")
#define POP_RSI()        ADD_ASM("pop %%rsi")
#define POP_RDI()        ADD_ASM("pop %%rdi")
#define POP_R8()         ADD_ASM("pop %%r8")
#define POP_R9()         ADD_ASM("pop %%r9")
#define JE32(label)      ADD_ASM("je %s", label)
#define JNE32(label)     ADD_ASM("jne %s", label)
#define JL32(label)      ADD_ASM("jl %s", label)
#define JG32(label)      ADD_ASM("jg %s", label)
#define JLE32(label)     ADD_ASM("jle %s", label)
#define JGE32(label)     ADD_ASM("jge %s", label)
#define JNE8(label)      ADD_ASM("jne %s", label)
#define JA8(label)       ADD_ASM("ja %s", label)
#define JMP8(label)      ADD_ASM("jmp %s", label)
#define JMP32(label)     ADD_ASM("jmp %s", label)
#define CALL(label)      ADD_ASM("call %s", label)
#define CALL_IND_RAX()   ADD_ASM("call *%%rax")
#define RET()            ADD_ASM("ret")
#define INT(x)           ADD_ASM("int $%d", x)
#define SYSCALL()        ADD_ASM("syscall")
