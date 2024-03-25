// $ ./xcc -nostdlib ,foo/foo.c ../musl/lib/crt0.a ../musl/lib/libc.a
// Unresolved: #8
//   __init_array_start
//   __init_array_end
//   exit
//   _init
//   __syscall_cp
//   __init_tls
//   __libc_start_init
//   _fini

// $ gcc -g -nostdlib ,foo/foo.c ../musl/lib/crt0.a ../musl/lib/libc.a
// /usr/bin/ld: ../musl/lib/crt0.a(Scrt1.o): warning: relocation against `_init' in read-only section `.text'
// /usr/bin/ld: ../musl/lib/crt0.a(Scrt1.o): in function `_start_c':
// (.text+0x16): undefined reference to `_fini'
// /usr/bin/ld: (.text+0x20): undefined reference to `_init'
// /usr/bin/ld: ../musl/lib/libc.a(__libc_start_main.o):(.text+0x9d): undefined reference to `__syscall_cp'
// /usr/bin/ld: ../musl/lib/libc.a(__libc_start_main.o): in function `__init_libc':
// (.text+0x933): undefined reference to `__init_tls'
// /usr/bin/ld: ../musl/lib/libc.a(__libc_start_main.o): in function `__libc_start_main':
// (.text+0xb78): undefined reference to `__libc_start_init'
// /usr/bin/ld: (.text+0xb8a): undefined reference to `exit'
// /usr/bin/ld: ../musl/lib/libc.a(__stack_chk_fail.o):(.text+0x9d): undefined reference to `__syscall_cp'
// /usr/bin/ld: warning: creating DT_TEXTREL in a PIE
// collect2: error: ld returned 1 exit status

.globl __init_array_start
.globl __init_array_end
.globl __libc_start_init
.globl __init_tls
.globl __syscall_cp
.globl _init

    .text
__init_array_start:
__init_array_end:
__libc_start_init:
__init_tls:
__syscall_cp:
_init:
    ret

// crti.s
//.section .fini
.globl _fini
_fini:
    push %rax

//// crtn.s
//.section .init
//	pop %rax
//	ret
//
//.section .fini
//	pop %rax
//	ret

.globl exit
exit:
    mov $60, %eax
    syscall

//
