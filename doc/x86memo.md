
## x86-64 calling convention

### Function parameters

On register:

  * `rdi`
  * `rsi`
  * `rdx`
  * `rcx` (for syscall, `r10`)
  * `r8`
  * `r9`

### Callee save

  * `rbx`
  * `rbp`
  * `r12`~`r15`
