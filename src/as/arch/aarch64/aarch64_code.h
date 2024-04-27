#pragma once

#define W_MOVK(sz, rd, imm, sft)                   MAKE_CODE32(inst, code, 0x72800000U | ((sz) << 31) | ((sft) << 21) | (((imm) & ((1U << 16) - 1)) << 5) | (rd))
#define W_MOVZ(sz, rd, imm, sft)                   MAKE_CODE32(inst, code, 0x52800000U | ((sz) << 31) | ((sft) << 21) | (((imm) & ((1U << 16) - 1)) << 5) | (rd))
#define W_MOVN(sz, rd, imm, sft)                   MAKE_CODE32(inst, code, 0x12800000U | ((sz) << 31) | ((sft) << 21) | (((imm) & ((1U << 16) - 1)) << 5) | (rd))

#define W_LDP(sz, rs1, rs2, ofs, base, prepost)    MAKE_CODE32(inst, code, 0x28400000U | ((sz) << 31) | ((prepost) << 23) | (((((ofs) >> 3) & ((1U << 7) - 1))) << 15) | ((rs2) << 10) | ((base) << 5) | (rs1))
#define W_STP(sz, rs1, rs2, ofs, base, prepost)    MAKE_CODE32(inst, code, 0x28000000U | ((sz) << 31) | ((prepost) << 23) | (((((ofs) >> 3) & ((1U << 7) - 1))) << 15) | ((rs2) << 10) | ((base) << 5) | (rs1))

#define W_BL(offset)                               MAKE_CODE32(inst, code, 0x94000000U | ((offset) & ((1U << 26) - 1)))
#define W_BLR(rn)                                  MAKE_CODE32(inst, code, 0xd63f0000U | ((rn) << 5))
#define W_RET(rn)                                  MAKE_CODE32(inst, code, 0xd65f0000U | ((rn) << 5))



