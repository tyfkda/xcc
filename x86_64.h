
#ifndef ADD_CODE
#define ADD_CODE(...)  do { unsigned char buf[] = {__VA_ARGS__}; add_code(buf, sizeof(buf)); } while (0)
#endif
#ifndef ADD_LOC_REL8
#define ADD_LOC_REL8(label, ofs, base)  add_loc_rel8(label, ofs, base)
#endif
#ifndef ADD_LOC_REL32
#define ADD_LOC_REL32(label, ofs, base)  add_loc_rel32(label, ofs, base)
#endif

#define IM8(x)   (x)
#define IM32(x)  (x), ((x) >> 8), ((x) >> 16), ((x) >> 24)
#define IM64(x)  (x), ((x) >> 8), ((x) >> 16), ((x) >> 24), ((x) >> 32), ((x) >> 40), ((x) >> 48), ((x) >> 56)

#define MOV_IM8_AL(x)    ADD_CODE(0xb0, x)  // mov $x,%al
#define MOV_IM32_EAX(x)  ADD_CODE(0xb8, IM32(x))  // mov $0xNN,%eax
#define MOV_IM32_RAX(x)  ADD_CODE(0x48, 0xc7, 0xc0, IM32(x))  // mov $0x12345678,%rax
#define MOV_IM64_RAX(x)  ADD_CODE(0x48, 0xb8, IM64(x))  // mov $0x123456789abcdef0,%rax
#define MOV_IM32_EDI(x)  ADD_CODE(0xbf, IM32(x))  // mov $0x12345678,%edi
#define MOV_IM64_RDI(x)  ADD_CODE(0x48, 0xbf, IM64(x))  // mov $0x123456789abcdef0,%rdi
#define MOV_IM32_RDX(x)  ADD_CODE(0x48, 0xc7, 0xc2, IM32(x)) // mov $0x0,%rdx
#define MOVSX_AL_AX()    ADD_CODE(0x66, 0x0f, 0xbe, 0xc0)  // movsx al,%ax
#define MOVSX_AL_EAX()   ADD_CODE(0x0f, 0xbe, 0xc0)  // movsbl %al,%eax
#define MOVSX_AL_RAX()   ADD_CODE(0x48, 0x0f, 0xbe, 0xc0)  // movsbq %al,%rax
#define MOVSX_AX_EAX()   ADD_CODE(0x0f, 0xbf, 0xc0)  // movsx %ax,%eax
#define MOVSX_AX_RAX()   ADD_CODE(0x48, 0x0f, 0xbf, 0xc0)  // movsx %ax,%rax
#define MOVZX_AL_EAX()   ADD_CODE(0x0f, 0xb6, 0xc0)  // movzbl %al,%eax
#define MOVZX_AL_RAX()   ADD_CODE(0x48, 0x0f, 0xb6, 0xc0)  // movzbq %al,%rax
#define MOVSX_EAX_RAX()  ADD_CODE(0x48, 0x63, 0xc0)  // movsx %eax,%rax
#define MOVSX_EAX_RDI()  ADD_CODE(0x48, 0x63, 0xf8)  // movsx %eax, %rdi
#define MOV_EAX_EDI()    ADD_CODE(0x89, 0xc7)  // mov %eax,%edi
#define MOV_RAX_RSI()    ADD_CODE(0x48, 0x89, 0xc6)  // mov %rax,%rsi
#define MOV_RAX_RDI()    ADD_CODE(0x48, 0x89, 0xc7)  // mov %rax,%rdi
#define MOV_DL_AL()      ADD_CODE(0x88, 0xd0)  // mov %dl,%al
#define MOV_EDX_EAX()    ADD_CODE(0x89, 0xd0)  // mov %edx,%eax
#define MOV_RDX_RAX()    ADD_CODE(0x48, 0x89, 0xd0)  // mov %rdx,%rax
#define MOV_RDI_RAX()    ADD_CODE(0x48, 0x89, 0xf8)  // mov %rdi,%rax
#define MOV_RDI_RCX()    ADD_CODE(0x48, 0x89, 0xf9)  // mov %rdi,%rcx
#define MOV_RSP_RBP()    ADD_CODE(0x48, 0x89, 0xe5)  // mov %rsp,%rbp
#define MOV_RBP_RSP()    ADD_CODE(0x48, 0x89, 0xec)  // mov %rbp,%rsp
#define MOV_RBP_RAX()    ADD_CODE(0x48, 0x89, 0xe8)  // mov %rbp,%rax
#define MOV_AL_IND_RSI()   ADD_CODE(0x88, 0x06)  // mov %al,(%rsi)
#define MOV_AL_IND_RDI()   ADD_CODE(0x88, 0x07)  // mov %al,(%rdi)
#define MOV_IND_RAX_AL()   ADD_CODE(0x8a, 0x00)  // mov (%rax),%al
#define MOV_IND_RAX_AX()   ADD_CODE(0x66, 0x8b, 0x00)  // mov (%rax),%al
#define MOV_IND_RAX_EAX()  ADD_CODE(0x8b, 0x00)  // mov (%rax),%eax
#define MOV_IND_RAX_RAX()  ADD_CODE(0x48, 0x8b, 0x00)  // mov (%rax),%rax
#define MOV_IND_RAX_RDI()  ADD_CODE(0x48, 0x8b, 0x38)  // mov (%rax),%rdi
#define MOV_RAX_IND_RAX()  ADD_CODE(0x48, 0x89, 0x00)  // mov %rax,(%rax)
#define MOV_EAX_IND_RSI()  ADD_CODE(0x89, 0x06)  // mov %eax,(%rsi)
#define MOV_EAX_IND_RDI()  ADD_CODE(0x89, 0x07)  // mov %eax,(%rdi)
#define MOV_RAX_IND_RSI()  ADD_CODE(0x48, 0x89, 0x06)  // mov %rax,(%rsi)
#define MOV_RAX_IND_RDI()  ADD_CODE(0x48, 0x89, 0x07)  // mov %rax,(%rdi)
#define MOV_RDI_IND_RAX()  ADD_CODE(0x48, 0x89, 0x38)  // mov %rdi,(%rax)
#define MOV_EDI_ECX()    ADD_CODE(0x89, 0xf9)  // mov %edi,%ecx
#define MOV_DIL_CL()     ADD_CODE(0x40, 0x88, 0xf9)  // mov %dil,%cl
#define MOV_DIL_IND8_RBP(ofs)  ADD_CODE(0x40, 0x88, 0x7d, ofs)  // mov %dil,ofs(%rbp)
#define MOV_EDI_IND8_RBP(ofs)  ADD_CODE(0x89, 0x7d, ofs)  // mov %edi,ofs(%rbp)
#define MOV_RDI_IND8_RBP(ofs)  ADD_CODE(0x48, 0x89, 0x7d, ofs)  // mov %rdi,ofs(%rbp)
#define MOV_SIL_IND8_RBP(ofs)  ADD_CODE(0x40, 0x88, 0x75, ofs)  // mov %sil,ofs(%rbp)
#define MOV_ESI_IND8_RBP(ofs)  ADD_CODE(0x89, 0x75, ofs)  // mov %esi,ofs(%rbp)
#define MOV_RSI_IND8_RBP(ofs)  ADD_CODE(0x48, 0x89, 0x75, ofs)  // mov %rsi,ofs(%rbp)
#define MOV_DL_IND8_RBP(ofs)   ADD_CODE(0x88, 0x55, ofs)  // mov %dl,ofs(%rbp)
#define MOV_EDX_IND8_RBP(ofs)  ADD_CODE(0x89, 0x55, ofs)  // mov %edx,ofs(%rbp)
#define MOV_RDX_IND8_RBP(ofs)  ADD_CODE(0x48, 0x89, 0x55, ofs)  // mov %rdx,ofs(%rbp)
#define MOV_CL_IND8_RBP(ofs)   ADD_CODE(0x88, 0x4d, ofs)  // mov %cl,ofs(%rbp)
#define MOV_ECX_IND8_RBP(ofs)  ADD_CODE(0x89, 0x4d, ofs)  // mov %ecx,ofs(%rbp)
#define MOV_RCX_IND8_RBP(ofs)  ADD_CODE(0x48, 0x89, 0x4d, ofs)  // mov %rcx,ofs(%rbp)
#define MOV_R8B_IND8_RBP(ofs)  ADD_CODE(0x44, 0x88, 0x45, ofs)  // mov %r8b,ofs(%rbp)
#define MOV_R8D_IND8_RBP(ofs)  ADD_CODE(0x44, 0x89, 0x45, ofs)  // mov %r8d,ofs(%rbp)
#define MOV_R8_IND8_RBP(ofs)   ADD_CODE(0x4c, 0x89, 0x45, ofs)  // mov %r8,ofs(%rbp)
#define MOV_R9B_IND8_RBP(ofs)  ADD_CODE(0x44, 0x88, 0x4d, ofs)  // mov %r9b,ofs(%rbp)
#define MOV_R9D_IND8_RBP(ofs)  ADD_CODE(0x44, 0x89, 0x4d, ofs)  // mov %r9d,ofs(%rbp)
#define MOV_R9_IND8_RBP(ofs)   ADD_CODE(0x4c, 0x89, 0x4d, ofs)  // mov %r9,ofs(%rbp)
#define MOV_IND8_RSP_RDI(ofs)  ADD_CODE(0x48, 0x8b, 0x7c, 0x24, ofs)  // mov ofs(%rsp),%rdi
#define MOV_IND8_RSP_RSI(ofs)  ADD_CODE(0x48, 0x8b, 0x74, 0x24, ofs)  // mov ofs(%rsp),%rsi
#define MOV_IND8_RSP_RDX(ofs)  ADD_CODE(0x48, 0x8b, 0x54, 0x24, ofs)  // mov ofs(%rsp),%rdx
#define LEA_OFS8_RSP_RSI(ofs)  ADD_CODE(0x48, 0x8d, 0x74, 0x24, ofs)  // lea ofs(%rsp),%rsi
#define LEA_OFS32_RAX(label)      do { ADD_LOC_REL32(label, 4, 8); ADD_CODE(0x48, 0x8d, 0x04, 0x25, IM32(0)); } while(0)  // lea 0x0,%rax
#define LEA_OFS32_RIP_RAX(label)  do { ADD_LOC_REL32(label, 3, 7); ADD_CODE(0x48, 0x8d, 0x05, IM32(0)); } while(0)  // lea 0x0(%rip),%rax
#define ADD_DIL_AL()     ADD_CODE(0x40, 0x00, 0xf8)  // add %dil,%al
#define ADD_EDI_EAX()    ADD_CODE(0x01, 0xf8)  // add %edi,%eax
#define ADD_RDI_RAX()    ADD_CODE(0x48, 0x01, 0xf8)  // add %rdi,%rax
#define ADD_IM32_RAX(x)  ADD_CODE(0x48, 0x05, IM32(x))  // add $12345678,%rax
#define ADD_IM32_RSP(x)  ADD_CODE(0x48, 0x81, 0xc4, IM32(x))  // add $IM32,%rsp
#define ADD_IND_RDI_RAX()  ADD_CODE(0x48, 0x03, 0x07)  // add (%rdi),%rax
#define SUB_DIL_AL()     ADD_CODE(0x40, 0x28, 0xf8)  // sub %dil,%al
#define SUB_EDI_EAX()    ADD_CODE(0x29, 0xf8)  // sub %edi,%eax
#define SUB_RDI_RAX()    ADD_CODE(0x48, 0x29, 0xf8)  // sub %rdi,%rax
#define SUB_IM32_RAX(x)  ADD_CODE(0x48, 0x2d, IM32(x))  // sub $12345678,%rax
#define SUB_IM32_RSP(x)  ADD_CODE(0x48, 0x81, 0xec, IM32(x))  // sub $IM32,%rsp
#define MUL_DIL()        ADD_CODE(0x40, 0xf6, 0xe7)  // mul %dil
#define MUL_EDI()        ADD_CODE(0xf7, 0xe7)  // mul %edi
#define MUL_RDI()        ADD_CODE(0x48, 0xf7, 0xe7)  // mul %rdi
#define DIV_DIL()        ADD_CODE(0x40, 0xf6, 0xf7)  // div %dil
#define DIV_EDI()        ADD_CODE(0xf7, 0xf7)  // div %edi
#define DIV_RDI()        ADD_CODE(0x48, 0xf7, 0xf7)  // div %rdi
#define CMP_AL_DIL()     ADD_CODE(0x40, 0x38, 0xc7)  // cmp %al,%dil
#define CMP_EAX_EDI()    ADD_CODE(0x39, 0xc7)  // cmp %eax,%edi
#define CMP_RAX_RDI()    ADD_CODE(0x48, 0x39, 0xc7)  // cmp %rax,%rdi
#define CMP_RDI_RAX()    ADD_CODE(0x48, 0x39, 0xf8)  // cmp %rdi,%rax
#define CMP_IM8_AL(x)    ADD_CODE(0x3c, x)  // cmp $x,%al
#define CMP_IM8_DIL(x)   ADD_CODE(0x40, 0x80, 0xff, x)  // cmp $x,%dil
#define CMP_IM8_EAX(x)   ADD_CODE(0x83, 0xf8, x)  // cmp $x,%eax
#define CMP_IM8_EDI(x)   ADD_CODE(0x83, 0xff, x)  // cmp $x,%edi
#define CMP_IM8_RAX(x)   ADD_CODE(0x48, 0x83, 0xf8, x)  // cmp $x,%rax
#define CMP_IM8_RDI(x)   ADD_CODE(0x48, 0x83, 0xff, x)  // cmp $x,%rdi
#define CMP_IM32_EAX(x)  ADD_CODE(0x3d, IM32(x))  // cmp $im32,%eax
#define INCB_IND_RAX()   ADD_CODE(0xfe, 0x00)  // incb (%rax)
#define INCL_IND_RAX()   ADD_CODE(0xff, 0x00)  // incl (%rax)
#define DECB_IND_RAX()   ADD_CODE(0xfe, 0x08)  // decb (%rax)
#define DECL_IND_RAX()   ADD_CODE(0xff, 0x08)  // decl (%rax)
#define AND_DIL_AL()     ADD_CODE(0x40, 0x20, 0xf8)  // and %dil,%al
#define AND_EDI_EAX()    ADD_CODE(0x21, 0xf8)  // and %edi,%eax
#define AND_RDI_RAX()    ADD_CODE(0x48, 0x21, 0xf8)  // and %rdi,%rax
#define OR_DIL_AL()      ADD_CODE(0x40, 0x08, 0xf8)  // or %dil,%al
#define OR_EDI_EAX()     ADD_CODE(0x09, 0xf8)  // or %edi,%eax
#define OR_RDI_RAX()     ADD_CODE(0x48, 0x09, 0xf8)  // or %rdi,%rax
#define XOR_DIL_AL()     ADD_CODE(0x40, 0x30, 0xf8)  // xor %dil,%al
#define XOR_EDI_EAX()    ADD_CODE(0x31, 0xf8)  // xor %edi,%eax
#define XOR_RDI_RAX()    ADD_CODE(0x48, 0x31, 0xf8)  // xor %rdi,%rax
#define SHL_CL_AL()      ADD_CODE(0xd2, 0xe0)  // shl %cl,%al
#define SHL_CL_EAX()     ADD_CODE(0xd3, 0xe0)  // shl %cl,%eax
#define SHL_CL_RAX()     ADD_CODE(0x48, 0xd3, 0xe0)  // shl %cl,%rax
#define SHR_CL_AL()      ADD_CODE(0xd2, 0xe8)  // shr %cl,%al
#define SHR_CL_EAX()     ADD_CODE(0xd3, 0xe8)  // shr %cl,%eax
#define SHR_CL_RAX()     ADD_CODE(0x48, 0xd3, 0xe8)  // shr %cl,%rax
#define NEG_AL()         ADD_CODE(0xf6, 0xd8)  // neg %al
#define NEG_EAX()        ADD_CODE(0xf7, 0xd8)  // neg %eax
#define NEG_RAX()        ADD_CODE(0x48, 0xf7, 0xd8)  // neg %rax
#define SETE_AL()        ADD_CODE(0x0f, 0x94, 0xc0)  // sete %al
#define SETNE_AL()       ADD_CODE(0x0f, 0x95, 0xc0)  // setne %al
#define SETS_AL()        ADD_CODE(0x0f, 0x98, 0xc0)  // sets %al
#define SETNS_AL()       ADD_CODE(0x0f, 0x99, 0xc0)  // setns %al
#define SAR_RAX(x)       ADD_CODE(0x48, 0xd1, 0xf8)  // sar %rax
#define SAR_IM8_RAX(x)   ADD_CODE(0x48, 0xc1, 0xf8, x)  // sar $x,%rax
#define PUSH_RAX()       ADD_CODE(0x50)  // push %rax
#define PUSH_RBP()       ADD_CODE(0x55)  // push %rbp
#define PUSH_RDI()       ADD_CODE(0x57)  // push %rdi
#define POP_RAX()        ADD_CODE(0x58)  // pop %rax
#define POP_RCX()        ADD_CODE(0x59)  // pop %rcx
#define POP_RDX()        ADD_CODE(0x5a)  // pop %rdx
#define POP_RBP()        ADD_CODE(0x5d)  // pop %rbp
#define POP_RSI()        ADD_CODE(0x5e)  // pop %rsi
#define POP_RDI()        ADD_CODE(0x5f)  // pop %rdi
#define POP_R8()         ADD_CODE(0x41, 0x58)  // pop %r8
#define POP_R9()         ADD_CODE(0x41, 0x59)  // pop %r9
#define JE32(label)      do { ADD_LOC_REL32(label, 2, 6); ADD_CODE(0x0f, 0x84, IM32(0)); } while(0)  // je
#define JNE32(label)     do { ADD_LOC_REL32(label, 2, 6); ADD_CODE(0x0f, 0x85, IM32(0)); } while(0)  // jne
#define JA8(label)       do { ADD_LOC_REL8(label, 1, 2); ADD_CODE(0x77, IM8(0)); } while(0)  // jmp
#define JMP8(label)      do { ADD_LOC_REL8(label, 1, 2); ADD_CODE(0xeb, IM8(0)); } while(0)  // jmp
#define JMP32(label)     do { ADD_LOC_REL32(label, 1, 5); ADD_CODE(0xe9, IM32(0)); } while(0)  // jmp
#define CALL(label)      do { ADD_LOC_REL32(label, 1, 5); ADD_CODE(0xe8, IM32(0)); } while(0)  // call
#define CALL_IND_RAX()   ADD_CODE(0xff, 0xd0)  // call *%rax
#define RET()            ADD_CODE(0xc3)  // retq
#define INT(x)           ADD_CODE(0xcd, x)  // int $x
#define SYSCALL()        ADD_CODE(0x0f, 0x05)  // syscall
