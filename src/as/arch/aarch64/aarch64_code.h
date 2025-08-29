#pragma once

#define IMM(imm, t, b)  (((imm) >> (b)) & ((1 << (t - b + 1)) - 1))

#define W_MOVK(sz, rd, imm, sft)                   MAKE_CODE32(inst, code, 0x72800000U | ((sz) << 31) | ((sft) << 21) | (((imm) & ((1U << 16) - 1)) << 5) | (rd))
#define W_MOVZ(sz, rd, imm, sft)                   MAKE_CODE32(inst, code, 0x52800000U | ((sz) << 31) | ((sft) << 21) | (((imm) & ((1U << 16) - 1)) << 5) | (rd))
#define W_MOVN(sz, rd, imm, sft)                   MAKE_CODE32(inst, code, 0x12800000U | ((sz) << 31) | ((sft) << 21) | (((imm) & ((1U << 16) - 1)) << 5) | (rd))

#define W_ADD_I(sz, rd, rn, imm)                   MAKE_CODE32(inst, code, 0x11000000U | ((sz) << 31) | (((imm) & ((1U << 12) - 1)) << 10) | ((rn) << 5) | (rd))
#define W_ADD_S(sz, rd, rn, rm, sft, imm)          MAKE_CODE32(inst, code, 0x0b000000U | ((sz) << 31) | ((sft) << 22) | ((rm) << 16) | (((imm) & ((1U << 6) - 1)) << 10) | ((rn) << 5) | (rd))
#define W_ADD_E(sz, rd, rn, rm, option, ov)        MAKE_CODE32(inst, code, 0x0b200000U | ((sz) << 31) | ((rm) << 16) | ((option) << 13) | ((ov) << 10) | ((rn) << 5) | (rd))
#define W_ADDS_I(sz, rd, rn, imm)                  MAKE_CODE32(inst, code, 0x31000000U | ((sz) << 31) | (((imm) & ((1U << 12) - 1)) << 10) | ((rn) << 5) | (rd))
#define W_ADDS_S(sz, rd, rn, rm, imm)              MAKE_CODE32(inst, code, 0x2b000000U | ((sz) << 31) | ((rm) << 16) | (((imm) & ((1U << 6) - 1)) << 10) | ((rn) << 5) | (rd))
#define W_SUB_I(sz, rd, rn, imm)                   MAKE_CODE32(inst, code, 0x51000000U | ((sz) << 31) | (((imm) & ((1U << 12) - 1)) << 10) | ((rn) << 5) | (rd))
#define W_SUB_S(sz, rd, rn, rm, sft, imm)          MAKE_CODE32(inst, code, 0x4b000000U | ((sz) << 31) | ((sft) << 22) | ((rm) << 16) | (((imm) & ((1U << 6) - 1)) << 10) | ((rn) << 5) | (rd))
#define W_SUBS_I(sz, rd, rn, imm)                  MAKE_CODE32(inst, code, 0x71000000U | ((sz) << 31) | (((imm) & ((1U << 12) - 1)) << 10) | ((rn) << 5) | (rd))
#define W_SUBS_S(sz, rd, rn, rm, imm)              MAKE_CODE32(inst, code, 0x6b000000U | ((sz) << 31) | ((rm) << 16) | (((imm) & ((1U << 6) - 1)) << 10) | ((rn) << 5) | (rd))
#define W_SUB_E(sz, rd, rn, rm, option, ov)        MAKE_CODE32(inst, code, 0x4b200000U | ((sz) << 31) | ((rm) << 16) | ((option) << 13) | ((ov) << 10) | ((rn) << 5) | (rd))
#define W_MADD(sz, rd, rn, rm, ra)                 MAKE_CODE32(inst, code, 0x1b000000U | ((sz) << 31) | ((rm) << 16) | ((ra) << 10) | ((rn) << 5) | (rd))
#define W_MSUB(sz, rd, rn, rm, ra)                 MAKE_CODE32(inst, code, 0x1b008000U | ((sz) << 31) | ((rm) << 16) | ((ra) << 10) | ((rn) << 5) | (rd))
#define W_SDIV(sz, rd, rn, rm)                     MAKE_CODE32(inst, code, 0x1ac00c00U | ((sz) << 31) | ((rm) << 16) | ((rn) << 5) | (rd))
#define W_UDIV(sz, rd, rn, rm)                     MAKE_CODE32(inst, code, 0x1ac00800U | ((sz) << 31) | ((rm) << 16) | ((rn) << 5) | (rd))
#define W_AND_S(sz, rd, rn, rm, imm)               MAKE_CODE32(inst, code, 0x0a000000U | ((sz) << 31) | ((rm) << 16) | (((imm) & ((1U << 6) - 1)) << 10) | ((rn) << 5) | (rd))
#define W_ORR_S(sz, rd, rn, rm, imm)               MAKE_CODE32(inst, code, 0x2a000000U | ((sz) << 31) | ((rm) << 16) | (((imm) & ((1U << 6) - 1)) << 10) | ((rn) << 5) | (rd))
#define W_EOR_S(sz, rd, rn, rm, imm)               MAKE_CODE32(inst, code, 0x4a000000U | ((sz) << 31) | ((rm) << 16) | (((imm) & ((1U << 6) - 1)) << 10) | ((rn) << 5) | (rd))
#define W_EON_S(sz, rd, rn, rm, imm)               MAKE_CODE32(inst, code, 0x4a200000U | ((sz) << 31) | ((rm) << 16) | (((imm) & ((1U << 6) - 1)) << 10) | ((rn) << 5) | (rd))

#define W_LSLV(sz, rd, rn, rm)                     MAKE_CODE32(inst, code, 0x1ac02000U | ((sz) << 31) | ((rm) << 16) | ((rn) << 5) | (rd))
#define W_LSRV(sz, rd, rn, rm)                     MAKE_CODE32(inst, code, 0x1ac02400U | ((sz) << 31) | ((rm) << 16) | ((rn) << 5) | (rd))
#define W_ASRV(sz, rd, rn, rm)                     MAKE_CODE32(inst, code, 0x1ac02800U | ((sz) << 31) | ((rm) << 16) | ((rn) << 5) | (rd))

#define W_SBFM(sz, rd, sn, rn, immr, imms)         MAKE_CODE32(inst, code, 0x13000000U | ((sz) << 31) | ((sn) << 22) | ((immr) << 16) | ((imms) << 10) | ((rn) << 5) | (rd))
#define W_UBFM(sz, rd, sn, rn, immr, imms)         MAKE_CODE32(inst, code, 0x53000000U | ((sz) << 31) | ((sn) << 22) | ((immr) << 16) | ((imms) << 10) | ((rn) << 5) | (rd))

#define W_LDUR(b, s, rt, ofs, base)                MAKE_CODE32(inst, code, 0x38400000U | ((b) << 30) | ((s) << 23) | ((((ofs) & ((1U << 9) - 1))) << 12) | ((base) << 5) | (rt))
#define W_LDR_UIMM(b, s, rt, ofs, base)            MAKE_CODE32(inst, code, 0x39400000U | ((b) << 30) | ((s) << 23) | ((((ofs) & ((1U << 12) - 1))) << 10) | ((base) << 5) | (rt))
#define W_LDR(b, s, rt, ofs, base, prepost)        MAKE_CODE32(inst, code, 0x38400000U | ((b) << 30) | ((s) << 23) | ((((ofs) & ((1U << 9) - 1))) << 12) | ((prepost) << 10) | ((base) << 5) | (rt))
#define W_STUR(b, rt, ofs, base)                   MAKE_CODE32(inst, code, 0x38000000U | ((b) << 30) | ((((ofs) & ((1U << 9) - 1))) << 12) | ((base) << 5) | (rt))
#define W_STR_UIMM(b, rt, ofs, base)               MAKE_CODE32(inst, code, 0x39000000U | ((b) << 30) | ((((ofs) & ((1U << 12) - 1))) << 10) | ((base) << 5) | (rt))
#define W_STR(b, rt, ofs, base, prepost)           MAKE_CODE32(inst, code, 0x38000000U | ((b) << 30) | ((((ofs) & ((1U << 9) - 1))) << 12) | ((prepost) << 10) | ((base) << 5) | (rt))
#define W_LDP(sz, rs1, rs2, ofs, base, prepost)    MAKE_CODE32(inst, code, 0x28400000U | ((sz) << 31) | ((prepost) << 23) | (((((ofs) >> 3) & ((1U << 7) - 1))) << 15) | ((rs2) << 10) | ((base) << 5) | (rs1))
#define W_STP(sz, rs1, rs2, ofs, base, prepost)    MAKE_CODE32(inst, code, 0x28000000U | ((sz) << 31) | ((prepost) << 23) | (((((ofs) >> 3) & ((1U << 7) - 1))) << 15) | ((rs2) << 10) | ((base) << 5) | (rs1))

#define W_LDR_R(sz, rt, base, rm, s, s2, option)   MAKE_CODE32(inst, code, 0x38600800U | ((sz) << 30) | ((s) << 23) | ((rm) << 16) | ((option) << 13) | ((s2) << 12) | ((base) << 5) | (rt))
#define W_STR_R(sz, rt, base, rm, s2, option)      MAKE_CODE32(inst, code, 0x38200800U | ((sz) << 30) | ((rm) << 16) | ((option) << 13) | ((s2) << 12) | ((base) << 5) | (rt))

#define W_ADRP(rd, imm)                            MAKE_CODE32(inst, code, 0x90000000U | (IMM(imm, 31, 30) << 29) | (IMM(imm, 29, 12) << 5) | (rd))

#define W_CSINC(sz, rd, rn, rm, cond)              MAKE_CODE32(inst, code, 0x1a800400U | ((sz) << 31) | ((rm) << 16) | ((cond) << 12) | ((rn) << 5) | (rd))

#define W_B()                                      MAKE_CODE32(inst, code, 0x14000000U)
#define W_BR(rn)                                   MAKE_CODE32(inst, code, 0xd61f0000U | ((rn) << 5))
#define W_BCC(cond)                                MAKE_CODE32(inst, code, 0x54000000U | (cond))
#define W_CBZ(sz, rt)                              MAKE_CODE32(inst, code, 0x34000000U | ((sz) << 31) | (rt))
#define W_CBNZ(sz, rt)                             MAKE_CODE32(inst, code, 0x35000000U | ((sz) << 31) | (rt))

#define W_CLZ(sz, rd, rn)                          MAKE_CODE32(inst, code, 0x5ac01000U | ((sz) << 31) | ((rn) << 5) | (rd))
#define W_RBIT(sz, rd, rn)                         MAKE_CODE32(inst, code, 0x5ac00000U | ((sz) << 31) | ((rn) << 5) | (rd))

#define W_BL(offset)                               MAKE_CODE32(inst, code, 0x94000000U | ((offset) & ((1U << 26) - 1)))
#define W_BLR(rn)                                  MAKE_CODE32(inst, code, 0xd63f0000U | ((rn) << 5))
#define W_RET(rn)                                  MAKE_CODE32(inst, code, 0xd65f0000U | ((rn) << 5))
#define W_SVC(imm)                                 MAKE_CODE32(inst, code, 0xd4000001U | ((imm) << 5))

#define P_MOV(sz, rd, rs)                          W_ORR_S(sz, rd, ZERO, rs, 0)
#define P_MOV_SP(sz, rd, rs)                       W_ADD_I(sz, rd, rs, 0)
#define P_MUL(sz, rd, rn, rm)                      W_MADD(sz, rd, rn, rm, ZERO)
#define P_CMP(sz, rm, rn)                          W_SUBS_S(sz, ZERO, rm, rn, 0)
#define P_CMP_I(sz, rd, imm)                       W_SUBS_I(sz, ZERO, rd, imm)
#define P_CMN(sz, rn, rm)                          W_ADDS_S(sz, ZERO, rn, rm, 0)
#define P_CMN_I(sz, rd, imm)                       W_ADDS_I(sz, ZERO, rd, imm)
#define P_LSL_I(sz, rd, rn, imm)                   W_UBFM(sz, opr1->reg.no, sz, opr2->reg.no, -(imm) & (63>>(1-(sz))), (63>>(1-(sz))) - (imm))
#define P_LSR_I(sz, rd, rn, imm)                   W_UBFM(sz, opr1->reg.no, sz, opr2->reg.no, imm & (63>>(1-(sz))), 63>>(1-(sz)))
#define P_ASR_I(sz, rd, rn, imm)                   W_SBFM(sz, opr1->reg.no, sz, opr2->reg.no, imm & (63>>(1-(sz))), 63>>(1-(sz)))
#define P_CSET(sz, rd, cond)                       W_CSINC(sz, rd, ZERO, ZERO, (cond) ^ 1)

// FP instructions.

#define F_LDUR(b, s, rt, ofs, base)                MAKE_CODE32(inst, code, 0xbc400000U | ((b) << 30) | ((s) << 23) | ((((ofs) & ((1U << 9) - 1))) << 12) | ((base) << 5) | (rt))
#define F_LDR_UIMM(b, s, rt, ofs, base)            MAKE_CODE32(inst, code, 0xbd400000U | ((b) << 30) | ((s) << 23) | ((((ofs) & ((1U << 12) - 1))) << 10) | ((base) << 5) | (rt))
#define F_LDR(b, s, rt, ofs, base, prepost)        MAKE_CODE32(inst, code, 0xbc400000U | ((b) << 30) | ((s) << 23) | ((((ofs) & ((1U << 9) - 1))) << 12) | ((prepost) << 10) | ((base) << 5) | (rt))
#define F_STUR(b, rt, ofs, base)                   MAKE_CODE32(inst, code, 0xbc000000U | ((b) << 30) | ((((ofs) & ((1U << 9) - 1))) << 12) | ((base) << 5) | (rt))
#define F_STR_UIMM(b, rt, ofs, base)               MAKE_CODE32(inst, code, 0xbd000000U | ((b) << 30) | ((((ofs) & ((1U << 12) - 1))) << 10) | ((base) << 5) | (rt))
#define F_STR(b, rt, ofs, base, prepost)           MAKE_CODE32(inst, code, 0xbc000000U | ((b) << 30) | ((((ofs) & ((1U << 9) - 1))) << 12) | ((prepost) << 10) | ((base) << 5) | (rt))
#define F_LDP(b, rt, ru, base, ofs, prepost)       MAKE_CODE32(inst, code, 0x2c400000U | ((b) << 30) | ((prepost) << 23) | ((((ofs) & ((1U << 10) - (1 << 3)))) << (15 - 3)) | ((ru) << 10) | ((base) << 5) | (rt))
#define F_STP(b, rt, ru, base, ofs, prepost)       MAKE_CODE32(inst, code, 0x2c000000U | ((b) << 30) | ((prepost) << 23) | ((((ofs) & ((1U << 10) - (1 << 3)))) << (15 - 3)) | ((ru) << 10) | ((base) << 5) | (rt))

#define F_LDR_R(sz, rt, base, rm, s, s2, option)   MAKE_CODE32(inst, code, 0xbc700800U | ((sz) << 30) | ((s) << 23) | ((rm) << 16) | ((option) << 13) | ((s2) << 12) | ((base) << 5) | (rt))
#define F_STR_R(sz, rt, base, rm, s2, option)      MAKE_CODE32(inst, code, 0xbc300800U | ((sz) << 30) | ((rm) << 16) | ((option) << 13) | ((s2) << 12) | ((base) << 5) | (rt))

#define FMOV(sz, rd, rn)                           MAKE_CODE32(inst, code, 0x1e204000U | ((sz) << 22) | ((rn) << 5) | (rd))
#define FADD(sz, rd, rn, rm)                       MAKE_CODE32(inst, code, 0x1e202800U | ((sz) << 22) | ((rm) << 16) | ((rn) << 5) | (rd))
#define FSUB(sz, rd, rn, rm)                       MAKE_CODE32(inst, code, 0x1e203800U | ((sz) << 22) | ((rm) << 16) | ((rn) << 5) | (rd))
#define FMUL(sz, rd, rn, rm)                       MAKE_CODE32(inst, code, 0x1e200800U | ((sz) << 22) | ((rm) << 16) | ((rn) << 5) | (rd))
#define FDIV(sz, rd, rn, rm)                       MAKE_CODE32(inst, code, 0x1e201800U | ((sz) << 22) | ((rm) << 16) | ((rn) << 5) | (rd))
#define FCMP(sz, rd, rn)                           MAKE_CODE32(inst, code, 0x1e202000U | ((sz) << 22) | ((rn) << 16) | ((rd) << 5))
#define FNEG(sz, rd, rn)                           MAKE_CODE32(inst, code, 0x1e214000U | ((sz) << 22) | ((rn) << 5) | (rd))
#define FSQRT(sz, rd, rn)                          MAKE_CODE32(inst, code, 0x1e21c000U | ((sz) << 22) | ((rn) << 5) | (rd))

#define SCVTF(dsz, rt, ssz, rn)                    MAKE_CODE32(inst, code, 0x1e220000 | ((dsz) << 31) | ((ssz) << 22) | ((rn) << 5) | (rt))
#define UCVTF(dsz, rt, ssz, rn)                    MAKE_CODE32(inst, code, 0x1e230000 | ((dsz) << 31) | ((ssz) << 22) | ((rn) << 5) | (rt))
#define FCVT(dsz, rt, rn)                          MAKE_CODE32(inst, code, 0x1e224000 | ((1 - (dsz)) << 22) | ((dsz) << 15) | ((rn) << 5) | (rt))
#define FCVTZS(dsz, rt, ssz, rn)                   MAKE_CODE32(inst, code, 0x1e380000 | ((dsz) << 31) | ((ssz) << 22) | ((rn) << 5) | (rt))
#define FCVTZU(dsz, rt, ssz, rn)                   MAKE_CODE32(inst, code, 0x1e390000 | ((dsz) << 31) | ((ssz) << 22) | ((rn) << 5) | (rt))
