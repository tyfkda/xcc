#pragma once

#define CPU_TYPE_ARM         ((cpu_type_t)12)
#define CPU_TYPE_X86         ((cpu_type_t)7)

#define CPU_ARCH_MASK        0xff000000
#define CPU_ARCH_ABI64       0x01000000

#define CPU_TYPE_X86_64      (CPU_TYPE_X86 | CPU_ARCH_ABI64)

#define CPU_SUBTYPE_X86_ALL  ((cpu_subtype_t)3)

#define CPU_TYPE_ARM64       (CPU_TYPE_ARM | CPU_ARCH_ABI64)

#define CPU_SUBTYPE_ARM_ALL  ((cpu_subtype_t)0)

typedef int integer_t;
typedef integer_t cpu_type_t;
typedef integer_t cpu_subtype_t;
