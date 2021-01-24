x86-64 memo
===========

## Registers

||0|1|2|3|4|5|6|7|
|-:|:-:|:-:|:-:|:-:|:-:|:-:|:-:|:-:|
|+0|`rax`|`rcx`|`rdx`|`rbx`|`rsp`|`rbp`|`rsi`|`rdi`|
|+8|`r8`|`r9`|`r10`|`r11`|`r12`|`r13`|`r14`|`r15`|

#### Special purpose registers

| Reg   | Purpose |
|:-----:|:--------|
| `rip` | Instruction Pointer (Program Counter) |
| `rsp` | Stack Pointer |
| `rbp` | Base Pointer |

## Instructions

### JMP (conditional)

|opcode|signed?|condition|flags|
|:----|:-:|:-------------------|:-|
| JO  |   | Overflow           | OF=1           |
| JNO |   | Not Overflow       | OF=0           |
| JB  |❌ | Below              | CF=1           |
| JAE |❌ | Above or Equal     | CF=0           |
| JE  | - | Equal              | ZF=1           |
| JNE | - | Not Equal          | ZF=0           |
| JBE |❌ | Below or Equal     | CF=1 or ZF=1   |
| JA  |❌ | Above              | CF=0 and ZF=0  |
| JS  |   | Sign               | SF=1           |
| JNS |   | Not Sign           | SF=0           |
| JP  |   | Parity even        | PF=1           |
| JNP |   | Not Parity         | PF=0           |
| JL  |✅ | Less than          | SF<>OF         |
| JGE |✅ | Greater or Equal   | SF=OF          |
| JLE |✅ | Less than or Equal | ZF=1 or SF<>OF |
| JG  |✅ | Greater            | ZF=0 or SF=OF  |

### Convert

| Opcode | Description           | Behavior          |
|:----:|:------------------------|:------------------|
| CWTL | Convert Word To Long    | %ax  -> %dx:%ax   |
| CLTD | Convert Long To Double  | %eax -> %edx:%eax |
| CQTO | Convert Quad to Octuple | %rax -> %rdx:%rax |

### DIV, MOD

Signed: `idiv`, Unsigned: `div`

  * 32bit: `idiv %ecx` => `%edx:%eax` / `%ecx`  =>  Quotient: `%eax`, Remainder: `%edx`
  * 64bit: `idiv %rcx` => `%rdx:%rax` / `%rcx`  =>  Quotient: `%rax`, Remainder: `%rdx`


## Addressing mode

#### Indirect : Base plus Scaled Index with offset

  * `offset(%base, %index, scaling)`
    * `offset + %base + %index * scaling`
    * `scaling` = 1, 2, 4 or 8
    * ex. `39(%rbp, %rax, 4)`


## Calling conventions

### Function parameters

On register:

  1. `rdi`
  2. `rsi`
  3. `rdx`
  4. `rcx` (for syscall, `r10`)
  5. `r8`
  6. `r9`

### Callee save

  * `rbx`
  * `rbp`
  * `r12`~`r15`
