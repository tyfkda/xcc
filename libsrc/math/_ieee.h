#pragma once

#include "stdint.h"  // int64_t

// double: Sign(1):Exponent(11):Fraction(52)

#define FRAC_BIT   (52)  // Fraction (significand in other words)
#define EXPO_POS   FRAC_BIT
#define EXPO_BIT   (11)
#define EXPO_BIAS  (1022)
#define SIGN_POS   (FRAC_BIT + EXPO_BIT)

#define FRAC_MASK  (((int64_t)1 << FRAC_BIT) - 1)
#define EXPO_MASK  ((((int64_t)1 << EXPO_BIT) - 1) << EXPO_POS)
#define SIGN_MASK  ((int64_t)1 << SIGN_POS)

#define NAN_MASK   ((((int64_t)1 << (EXPO_BIT + 1)) - 1) << (EXPO_POS - 1))

#define GET_EXPO(hex)         (GET_BIASED_EXPO(hex) - EXPO_BIAS)
#define GET_BIASED_EXPO(hex)  (((int)((hex) >> EXPO_POS)) & ((1 << EXPO_BIT) - 1))
