
#ifndef ADD_CODE
#define ADD_CODE(...)  do { unsigned char buf[] = {__VA_ARGS__}; add_code(buf, sizeof(buf)); } while (0)
#endif
#ifndef CURIP
#define CURIP(ofs)  (start_address + codesize + ofs)
#endif

#define IM32(x)  (x), ((x) >> 8), ((x) >> 16), ((x) >> 24)
#define IM64(x)  (x), ((x) >> 8), ((x) >> 16), ((x) >> 24), ((x) >> 32), ((x) >> 40), ((x) >> 48), ((x) >> 56)

#define MOV_I8_AL(x)     ADD_CODE(0xb0, x)  // mov $x,%al
#define MOV_I32_EAX(x)   ADD_CODE(0xb8, IM32(x))  // mov $0xNN,%eax
#define MOV_I64_RAX(x)   ADD_CODE(0x48, 0xb8, IM64(x))  // mov $0x123456789abcdef0,%rax
#define MOV_I32_EDI(x)   ADD_CODE(0xbf, IM32(x))  // mov $0x12345678,%edi
#define MOV_I64_RDI(x)   ADD_CODE(0x48, 0xbf, IM64(x))  // mov $0x123456789abcdef0,%rdi
#define MOV_I32_RDX(x)   ADD_CODE(0x48, 0xc7, 0xc2, IM32(x)) // mov $0x0,%rdx
#define MOVSX_AL_EAX()   ADD_CODE(0x0f, 0xbe, 0xc0)  // movsbl %al,%eax
#define MOVSX_AL_RAX()   ADD_CODE(0x48, 0x0f, 0xbe, 0xc0)  // movsbq %al,%rax
#define MOVZX_AL_EAX()   ADD_CODE(0x48, 0x0f, 0xb6, 0xc0)  // movzbl %al,%eax
#define MOVZX_AL_RAX()   ADD_CODE(0x48, 0x0f, 0xb6, 0xc0)  // movzbq %al,%rax
#define MOVSX_EAX_RDI()  ADD_CODE(0x48, 0x63, 0xf8)  // movsx %eax, %rdi
#define MOV_RAX_RDI()    ADD_CODE(0x48, 0x89, 0xc7)  // mov %rax,%rdi
#define MOV_EDX_EAX()    ADD_CODE(0x89, 0xd0)  // mov %edx,%eax
#define MOV_RDX_RAX()    ADD_CODE(0x48, 0x89, 0xd0)  // mov %rdx,%rax
#define MOV_RSP_RBP()    ADD_CODE(0x48, 0x89, 0xe5)  // mov %rsp,%rbp
#define MOV_RBP_RSP()    ADD_CODE(0x48, 0x89, 0xec)  // mov %rbp,%rsp
#define MOV_RBP_RAX()    ADD_CODE(0x48, 0x89, 0xe8)  // mov %rbp,%rax
#define MOV_AL_IND_RDI()   ADD_CODE(0x88, 0x07)  // mov %al,(%rdi)
#define MOV_IND_RAX_RAX()  ADD_CODE(0x48, 0x8b, 0x00)  // mov (%rax),%rax
#define MOV_RAX_IND_RAX()  ADD_CODE(0x48, 0x89, 0x00)  // mov %rax,(%rax)
#define MOV_EAX_IND_RDI()  ADD_CODE(0x89, 0x07)  // mov %eax,(%rdi)
#define MOV_RAX_IND_RDI()  ADD_CODE(0x48, 0x89, 0x07)  // mov %rax,(%rdi)
#define MOV_RDI_IND_RAX()  ADD_CODE(0x48, 0x89, 0x38)  // mov %rdi,(%rax)
#define MOV_EDI_IND8_RBP(ofs)  ADD_CODE(0x89, 0x7d, ofs)  // mov %edi,ofs(%rbp)
#define MOV_RDI_IND8_RBP(ofs)  ADD_CODE(0x48, 0x89, 0x7d, ofs)  // mov %rdi,ofs(%rbp)
#define MOV_ESI_IND8_RBP(ofs)  ADD_CODE(0x89, 0x75, ofs)  // mov %esi,ofs(%rbp)
#define MOV_RSI_IND8_RBP(ofs)  ADD_CODE(0x48, 0x89, 0x75, ofs)  // mov %rsi,ofs(%rbp)
#define MOV_EDX_IND8_RBP(ofs)  ADD_CODE(0x89, 0x55, ofs)  // mov %edx,ofs(%rbp)
#define MOV_RDX_IND8_RBP(ofs)  ADD_CODE(0x48, 0x89, 0x55, ofs)  // mov %rdx,ofs(%rbp)
#define MOV_ECX_IND8_RBP(ofs)  ADD_CODE(0x89, 0x4d, ofs)  // mov %ecx,ofs(%rbp)
#define MOV_RCX_IND8_RBP(ofs)  ADD_CODE(0x48, 0x89, 0x4d, ofs)  // mov %rcx,ofs(%rbp)
#define MOV_R8D_IND8_RBP(ofs)  ADD_CODE(0x44, 0x89, 0x45, ofs)  // mov %r8d,ofs(%rbp)
#define MOV_R8_IND8_RBP(ofs)   ADD_CODE(0x4c, 0x89, 0x45, ofs)  // mov %r8,ofs(%rbp)
#define MOV_R9D_IND8_RBP(ofs)  ADD_CODE(0x44, 0x89, 0x4d, ofs)  // mov %r9d,ofs(%rbp)
#define MOV_R9_IND8_RBP(ofs)   ADD_CODE(0x4c, 0x89, 0x4d, ofs)  // mov %r9,ofs(%rbp)
#define LEA_OFS32_RAX(label)      do { add_loc_rel32(codesize + 4, label, CURIP(8)); ADD_CODE(0x48, 0x8d, 0x04, 0x25, IM32(0)); } while(0)  // lea    0x0,%rax
#define LEA_OFS32_RIP_RAX(label)  do { add_loc_rel32(codesize + 3, label, CURIP(7)); ADD_CODE(0x48, 0x8d, 0x05, IM32(0)); } while(0)  // lea    0x0(%rip),%rax
#define ADD_EDI_EAX()    ADD_CODE(0x01, 0xf8)  // add %edi,%eax
#define ADD_RDI_RAX()    ADD_CODE(0x48, 0x01, 0xf8)  // add %rdi,%rax
#define ADD_IM32_RAX(x)  ADD_CODE(0x48, 0x05, IM32(x))  // add $12345678,%rax
#define ADD_IM32_RSP(x)  ADD_CODE(0x48, 0x81, 0xc4, IM32(x))  // add $IM32,%rsp
#define SUB_EDI_EAX()    ADD_CODE(0x29, 0xf8)  // sub %edi,%eax
#define SUB_RDI_RAX()    ADD_CODE(0x48, 0x29, 0xf8)  // sub %rdi,%rax
#define SUB_IM32_RAX(x)  ADD_CODE(0x48, 0x2d, IM32(x))  // sub $12345678,%rax
#define SUB_IM32_RSP(x)  ADD_CODE(0x48, 0x81, 0xec, IM32(x))  // sub $IM32,%rsp
#define MUL_EDI()        ADD_CODE(0xf7, 0xe7)  // mul %edi
#define MUL_RDI()        ADD_CODE(0x48, 0xf7, 0xe7)  // mul %rdi
#define DIV_EDI()        ADD_CODE(0xf7, 0xf7)  // div %edi
#define DIV_RDI()        ADD_CODE(0x48, 0xf7, 0xf7)  // div %rdi
#define CMP_EAX_EDI()    ADD_CODE(0x39, 0xc7)  // cmp %eax,%edi
#define CMP_RAX_RDI()    ADD_CODE(0x48, 0x39, 0xc7)  // cmp %rax,%rdi
#define CMP_I8_EAX(x)    ADD_CODE(0x83, 0xf8, x)  // cmp $x,%eax
#define CMP_I8_RAX(x)    ADD_CODE(0x48, 0x83, 0xf8, x)  // cmp $x,%rax
#define SETE_AL()        ADD_CODE(0x0f, 0x94, 0xc0)  // sete %al
#define SETNE_AL()       ADD_CODE(0x0f, 0x95, 0xc0)  // setne %al
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
#define JE32(label)      do { add_loc_rel32(codesize + 2, label, CURIP(6)); ADD_CODE(0x0f, 0x84, IM32(0)); } while(0)  // je
#define JNE32(label)     do { add_loc_rel32(codesize + 2, label, CURIP(6)); ADD_CODE(0x0f, 0x85, IM32(0)); } while(0)  // jne
#define JMP32(label)     do { add_loc_rel32(codesize + 1, label, CURIP(5)); ADD_CODE(0xe9, IM32(0)); } while(0)  // jmp
#define CALL(label)      do { add_loc_rel32(codesize + 1, label, CURIP(5)); ADD_CODE(0xe8, IM32(0)); } while(0)  // call
#define RET()            ADD_CODE(0xc3)  // retq
#define INT(x)           ADD_CODE(0xcd, x)  // int $x
#define SYSCALL()        ADD_CODE(0x0f, 0x05)  // syscall
