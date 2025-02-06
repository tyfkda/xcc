// riscv64 Code macros

#pragma once

#define ZERO  0
#define RA    1
#define SP    2

#define IMM(imm, t, b)  (((imm) >> (b)) & ((1 << (t - b + 1)) - 1))

// Instruction formats: 32bit=[7|5|5|3|5|7]
#define RTYPE(funct7, rs2, rs1, funct3, rd, opcode)  (((funct7) << 25) | ((rs2) << 20) | ((rs1) << 15) | ((funct3) << 12) | ((rd) << 7) | (opcode))
#define ITYPE(imm, rs1, funct3, rd, opcode)          ((IMM(imm, 11, 0) << 20) | ((rs1) << 15) | ((funct3) << 12) | ((rd) << 7) | (opcode))
#define STYPE(imm, rs2, rs1, funct3, opcode)         ((IMM(imm, 11, 5) << 25) | ((rs2) << 20) | ((rs1) << 15) | ((funct3) << 12) | (IMM(imm, 4, 0) << 7) | (opcode))
#define BTYPE(imm, rs2, rs1, funct3, opcode)         ((IMM(imm, 12, 12) << 31) | (IMM(imm, 10, 5) << 25) | ((rs2) << 20) | ((rs1) << 15) | ((funct3) << 12) | (IMM(imm, 4, 0) << 7) | (opcode))
#define UTYPE(imm, rd, opcode)                       ((IMM(imm, 31, 12) << 12) | ((rd) << 7) | (opcode))
#define JTYPE(imm, rd, opcode)                       ((IMM(imm, 20, 20) << 31) | (IMM(imm, 10, 1) << 20) | (IMM(imm, 11, 11) << 19) | (IMM(imm, 19, 12) << 12) | ((rd) << 7) | (opcode))

// 32-bit instructions
#define W_ADD(rd, rs1, rs2)       MAKE_CODE32(inst, code, RTYPE(0x00, rs2, rs1, 0x00, rd, 0x33))
#define W_ADDW(rd, rs1, rs2)      MAKE_CODE32(inst, code, RTYPE(0x00, rs2, rs1, 0x00, rd, 0x3b))
#define W_ADDI(rd, rs, imm)       MAKE_CODE32(inst, code, ITYPE(imm, rs, 0x00, rd, 0x13))
#define W_ADDIW(rd, rs, imm)      MAKE_CODE32(inst, code, ITYPE(imm, rs, 0x00, rd, 0x1b))
#define W_SUB(rd, rs1, rs2)       MAKE_CODE32(inst, code, RTYPE(0x20, rs2, rs1, 0x00, rd, 0x33))
#define W_SUBW(rd, rs1, rs2)      MAKE_CODE32(inst, code, RTYPE(0x20, rs2, rs1, 0x00, rd, 0x3b))
#define W_MUL(rd, rs1, rs2)       MAKE_CODE32(inst, code, RTYPE(0x01, rs2, rs1, 0x00, rd, 0x33))
#define W_MULW(rd, rs1, rs2)      MAKE_CODE32(inst, code, RTYPE(0x01, rs2, rs1, 0x00, rd, 0x3b))
#define W_DIV(rd, rs1, rs2)       MAKE_CODE32(inst, code, RTYPE(0x01, rs2, rs1, 0x04, rd, 0x33))
#define W_DIVU(rd, rs1, rs2)      MAKE_CODE32(inst, code, RTYPE(0x01, rs2, rs1, 0x05, rd, 0x33))
#define W_DIVW(rd, rs1, rs2)      MAKE_CODE32(inst, code, RTYPE(0x01, rs2, rs1, 0x04, rd, 0x3b))
#define W_DIVUW(rd, rs1, rs2)     MAKE_CODE32(inst, code, RTYPE(0x01, rs2, rs1, 0x05, rd, 0x3b))
#define W_REM(rd, rs1, rs2)       MAKE_CODE32(inst, code, RTYPE(0x01, rs2, rs1, 0x06, rd, 0x33))
#define W_REMU(rd, rs1, rs2)      MAKE_CODE32(inst, code, RTYPE(0x01, rs2, rs1, 0x07, rd, 0x33))
#define W_REMW(rd, rs1, rs2)      MAKE_CODE32(inst, code, RTYPE(0x01, rs2, rs1, 0x06, rd, 0x3b))
#define W_REMUW(rd, rs1, rs2)     MAKE_CODE32(inst, code, RTYPE(0x01, rs2, rs1, 0x07, rd, 0x3b))
#define W_AND(rd, rs1, rs2)       MAKE_CODE32(inst, code, RTYPE(0x00, rs2, rs1, 0x07, rd, 0x33))
#define W_ANDI(rd, rs, imm)       MAKE_CODE32(inst, code, ITYPE(imm, rs, 0x07, rd, 0x13))
#define W_OR(rd, rs1, rs2)        MAKE_CODE32(inst, code, RTYPE(0x00, rs2, rs1, 0x06, rd, 0x33))
#define W_ORI(rd, rs, imm)        MAKE_CODE32(inst, code, ITYPE(imm, rs, 0x06, rd, 0x13))
#define W_XOR(rd, rs1, rs2)       MAKE_CODE32(inst, code, RTYPE(0x00, rs2, rs1, 0x04, rd, 0x33))
#define W_XORI(rd, rs, imm)       MAKE_CODE32(inst, code, ITYPE(imm, rs, 0x04, rd, 0x13))
#define W_SLL(rd, rs1, rs2)       MAKE_CODE32(inst, code, RTYPE(0x00, rs2, rs1, 0x01, rd, 0x33))
#define W_SLLW(rd, rs1, rs2)      MAKE_CODE32(inst, code, RTYPE(0x00, rs2, rs1, 0x01, rd, 0x3b))
#define W_SLLI(rd, rs, imm)       MAKE_CODE32(inst, code, ITYPE((imm) & 63, rs, 0x01, rd, 0x13))
#define W_SLLIW(rd, rs, imm)      MAKE_CODE32(inst, code, ITYPE((imm) & 31, rs, 0x01, rd, 0x1b))
#define W_SRL(rd, rs1, rs2)       MAKE_CODE32(inst, code, RTYPE(0x00, rs2, rs1, 0x05, rd, 0x33))
#define W_SRLW(rd, rs1, rs2)      MAKE_CODE32(inst, code, RTYPE(0x00, rs2, rs1, 0x05, rd, 0x3b))
#define W_SRLI(rd, rs, imm)       MAKE_CODE32(inst, code, ITYPE((imm) & 63, rs, 0x05, rd, 0x13))
#define W_SRLIW(rd, rs, imm)      MAKE_CODE32(inst, code, ITYPE((imm) & 63, rs, 0x05, rd, 0x1b))
#define W_SRA(rd, rs1, rs2)       MAKE_CODE32(inst, code, RTYPE(0x20, rs2, rs1, 0x05, rd, 0x33))
#define W_SRAW(rd, rs1, rs2)      MAKE_CODE32(inst, code, RTYPE(0x20, rs2, rs1, 0x05, rd, 0x3b))
#define W_SRAI(rd, rs, imm)       MAKE_CODE32(inst, code, ITYPE(0x400 | ((imm) & 63), rs, 0x05, rd, 0x13))
#define W_SRAIW(rd, rs, imm)      MAKE_CODE32(inst, code, ITYPE(0x400 | ((imm) & 63), rs, 0x05, rd, 0x1b))
#define W_SLT(rd, rs1, rs2)       MAKE_CODE32(inst, code, RTYPE(0x00, rs2, rs1, 0x02, rd, 0x33))
#define W_SLTU(rd, rs1, rs2)      MAKE_CODE32(inst, code, RTYPE(0x00, rs2, rs1, 0x03, rd, 0x33))
#define W_SLTI(rd, rs, imm)       MAKE_CODE32(inst, code, ITYPE(imm, rs, 0x02, rd, 0x13))
#define W_SLTIU(rd, rs, imm)      MAKE_CODE32(inst, code, ITYPE(imm, rs, 0x03, rd, 0x13))
#define W_LB(rd, ofs, rs)         MAKE_CODE32(inst, code, ITYPE(ofs, rs, 0x00, rd, 0x03))
#define W_LH(rd, ofs, rs)         MAKE_CODE32(inst, code, ITYPE(ofs, rs, 0x01, rd, 0x03))
#define W_LW(rd, ofs, rs)         MAKE_CODE32(inst, code, ITYPE(ofs, rs, 0x02, rd, 0x03))
#define W_LD(rd, ofs, rs)         MAKE_CODE32(inst, code, ITYPE(ofs, rs, 0x03, rd, 0x03))
#define W_LBU(rd, ofs, rs)        MAKE_CODE32(inst, code, ITYPE(ofs, rs, 0x04, rd, 0x03))
#define W_LHU(rd, ofs, rs)        MAKE_CODE32(inst, code, ITYPE(ofs, rs, 0x05, rd, 0x03))
#define W_LWU(rd, ofs, rs)        MAKE_CODE32(inst, code, ITYPE(ofs, rs, 0x06, rd, 0x03))
#define W_SB(rs2, ofs, rs1)       MAKE_CODE32(inst, code, STYPE(ofs, rs2, rs1, 0x00, 0x23))
#define W_SH(rs2, ofs, rs1)       MAKE_CODE32(inst, code, STYPE(ofs, rs2, rs1, 0x01, 0x23))
#define W_SW(rs2, ofs, rs1)       MAKE_CODE32(inst, code, STYPE(ofs, rs2, rs1, 0x02, 0x23))
#define W_SD(rs2, ofs, rs1)       MAKE_CODE32(inst, code, STYPE(ofs, rs2, rs1, 0x03, 0x23))
#define W_LUI(rd, imm)            MAKE_CODE32(inst, code, UTYPE(imm, rd, 0x37))
#define W_AUIPC(rd, imm)          MAKE_CODE32(inst, code, UTYPE(imm, rd, 0x17))
#define W_JAL(rd, imm)            MAKE_CODE32(inst, code, UTYPE(SWIZZLE_JAL(imm), rd, 0x6f))
#define W_JALR(rd, rs, imm)       MAKE_CODE32(inst, code, ITYPE(imm, rs, 0x00, rd, 0x67))
#define W_BXX(xx, rs1, rs2, ofs)  MAKE_CODE32(inst, code, STYPE(0, rs2, rs1, xx, 0x63) | SWIZZLE_BXX(ofs))
#define W_ECALL()                 MAKE_CODE32(inst, code, UTYPE(0, 0, 0x73))

#define W_FADD_D(rd, rs1, rs2)    MAKE_CODE32(inst, code, RTYPE(0x01, rs2, rs1, 0x07, rd, 0x53))
#define W_FSUB_D(rd, rs1, rs2)    MAKE_CODE32(inst, code, RTYPE(0x05, rs2, rs1, 0x07, rd, 0x53))
#define W_FMUL_D(rd, rs1, rs2)    MAKE_CODE32(inst, code, RTYPE(0x09, rs2, rs1, 0x07, rd, 0x53))
#define W_FDIV_D(rd, rs1, rs2)    MAKE_CODE32(inst, code, RTYPE(0x0d, rs2, rs1, 0x07, rd, 0x53))
#define W_FADD_S(rd, rs1, rs2)    MAKE_CODE32(inst, code, RTYPE(0x00, rs2, rs1, 0x07, rd, 0x53))
#define W_FSUB_S(rd, rs1, rs2)    MAKE_CODE32(inst, code, RTYPE(0x04, rs2, rs1, 0x07, rd, 0x53))
#define W_FMUL_S(rd, rs1, rs2)    MAKE_CODE32(inst, code, RTYPE(0x08, rs2, rs1, 0x07, rd, 0x53))
#define W_FDIV_S(rd, rs1, rs2)    MAKE_CODE32(inst, code, RTYPE(0x0c, rs2, rs1, 0x07, rd, 0x53))
#define W_FSQRT_D(rd, rs)         MAKE_CODE32(inst, code, RTYPE(0x2d, 0, rs, 0x07, rd, 0x53))
#define W_FSQRT_S(rd, rs)         MAKE_CODE32(inst, code, RTYPE(0x2c, 0, rs, 0x07, rd, 0x53))
#define W_FEQ_D(rd, rs1, rs2)     MAKE_CODE32(inst, code, RTYPE(0x51, rs2, rs1, 0x02, rd, 0x53))
#define W_FLT_D(rd, rs1, rs2)     MAKE_CODE32(inst, code, RTYPE(0x51, rs2, rs1, 0x01, rd, 0x53))
#define W_FLE_D(rd, rs1, rs2)     MAKE_CODE32(inst, code, RTYPE(0x51, rs2, rs1, 0x00, rd, 0x53))
#define W_FEQ_S(rd, rs1, rs2)     MAKE_CODE32(inst, code, RTYPE(0x50, rs2, rs1, 0x02, rd, 0x53))
#define W_FLT_S(rd, rs1, rs2)     MAKE_CODE32(inst, code, RTYPE(0x50, rs2, rs1, 0x01, rd, 0x53))
#define W_FLE_S(rd, rs1, rs2)     MAKE_CODE32(inst, code, RTYPE(0x50, rs2, rs1, 0x00, rd, 0x53))
#define W_FLD(rd, ofs, rs)        MAKE_CODE32(inst, code, ITYPE(ofs, rs, 0x03, rd, 0x07))
#define W_FLW(rd, ofs, rs)        MAKE_CODE32(inst, code, ITYPE(ofs, rs, 0x02, rd, 0x07))
#define W_FSD(rs2, ofs, rs1)      MAKE_CODE32(inst, code, STYPE(ofs, rs2, rs1, 0x03, 0x27))
#define W_FSW(rs2, ofs, rs1)      MAKE_CODE32(inst, code, STYPE(ofs, rs2, rs1, 0x02, 0x27))
#define W_FSGNJ_D(rd, rs1, rs2)   MAKE_CODE32(inst, code, RTYPE(0x11, rs2, rs1, 0x00, rd, 0x53))
#define W_FSGNJ_S(rd, rs1, rs2)   MAKE_CODE32(inst, code, RTYPE(0x10, rs2, rs1, 0x00, rd, 0x53))
#define W_FSGNJN_D(rd, rs1, rs2)  MAKE_CODE32(inst, code, RTYPE(0x11, rs2, rs1, 0x01, rd, 0x53))
#define W_FSGNJN_S(rd, rs1, rs2)  MAKE_CODE32(inst, code, RTYPE(0x10, rs2, rs1, 0x01, rd, 0x53))
#define W_FSGNJX_D(rd, rs1, rs2)  MAKE_CODE32(inst, code, RTYPE(0x11, rs2, rs1, 0x02, rd, 0x53))
#define W_FSGNJX_S(rd, rs1, rs2)  MAKE_CODE32(inst, code, RTYPE(0x10, rs2, rs1, 0x02, rd, 0x53))

#define W_FCVT_D_W(rd, rs)        MAKE_CODE32(inst, code, RTYPE(0x69, 0, rs, 0x00, rd, 0x53))
#define W_FCVT_D_WU(rd, rs)       MAKE_CODE32(inst, code, RTYPE(0x69, 1, rs, 0x00, rd, 0x53))
#define W_FCVT_D_L(rd, rs)        MAKE_CODE32(inst, code, RTYPE(0x69, 2, rs, 0x07, rd, 0x53))
#define W_FCVT_D_LU(rd, rs)       MAKE_CODE32(inst, code, RTYPE(0x69, 3, rs, 0x07, rd, 0x53))
#define W_FCVT_S_W(rd, rs)        MAKE_CODE32(inst, code, RTYPE(0x68, 0, rs, 0x07, rd, 0x53))
#define W_FCVT_S_WU(rd, rs)       MAKE_CODE32(inst, code, RTYPE(0x68, 1, rs, 0x07, rd, 0x53))
#define W_FCVT_S_L(rd, rs)        MAKE_CODE32(inst, code, RTYPE(0x68, 2, rs, 0x07, rd, 0x53))
#define W_FCVT_S_LU(rd, rs)       MAKE_CODE32(inst, code, RTYPE(0x68, 3, rs, 0x07, rd, 0x53))

#define W_FCVT_W_D(rd, rs, rm)    MAKE_CODE32(inst, code, RTYPE(0x61, 0, rs, rm, rd, 0x53))
#define W_FCVT_WU_D(rd, rs, rm)   MAKE_CODE32(inst, code, RTYPE(0x61, 1, rs, rm, rd, 0x53))
#define W_FCVT_L_D(rd, rs, rm)    MAKE_CODE32(inst, code, RTYPE(0x61, 2, rs, rm, rd, 0x53))
#define W_FCVT_LU_D(rd, rs, rm)   MAKE_CODE32(inst, code, RTYPE(0x61, 3, rs, rm, rd, 0x53))
#define W_FCVT_W_S(rd, rs, rm)    MAKE_CODE32(inst, code, RTYPE(0x60, 0, rs, rm, rd, 0x53))
#define W_FCVT_WU_S(rd, rs, rm)   MAKE_CODE32(inst, code, RTYPE(0x60, 1, rs, rm, rd, 0x53))
#define W_FCVT_L_S(rd, rs, rm)    MAKE_CODE32(inst, code, RTYPE(0x60, 2, rs, rm, rd, 0x53))
#define W_FCVT_LU_S(rd, rs, rm)   MAKE_CODE32(inst, code, RTYPE(0x60, 3, rs, rm, rd, 0x53))

#define W_FCVT_D_S(rd, rs)        MAKE_CODE32(inst, code, RTYPE(0x21, 0, rs, 0x00, rd, 0x53))
#define W_FCVT_S_D(rd, rs)        MAKE_CODE32(inst, code, RTYPE(0x20, 1, rs, 0x07, rd, 0x53))

#define W_FMV_X_D(rd, rs)         MAKE_CODE32(inst, code, RTYPE(0x71, 0, rs, 0x00, rd, 0x53))
#define W_FMV_X_W(rd, rs)         MAKE_CODE32(inst, code, RTYPE(0x70, 0, rs, 0x00, rd, 0x53))

// Compressed instructions
#define C_MV(rd, rs)              MAKE_CODE16(inst, code, 0x8002 | ((rd) << 7) | ((rs) << 2))
#define C_LI(rd, imm)             MAKE_CODE16(inst, code, 0x4001 | (IMM(imm, 5, 5) << 12) | ((rd) << 7) | (IMM(imm, 4, 0) << 2))
#define C_LUI(rd, imm)            MAKE_CODE16(inst, code, 0x6001 | (IMM(imm, 17, 17) << 12) | ((rd) << 7) | (IMM(imm, 16, 12) << 2))
#define C_ADD(rd, rs)             MAKE_CODE16(inst, code, 0x9002 | ((rd) << 7) | ((rs) << 2))
#define C_ADDW(rd, rs)            MAKE_CODE16(inst, code, 0x9c21 | (to_rvc_reg(rd) << 7) | (to_rvc_reg(rs) << 2))
#define C_ADDI(rd, imm)           MAKE_CODE16(inst, code, 0x0001 | (IMM(imm, 5, 5) << 12) | ((rd) << 7) | (IMM(imm, 4, 0) << 2))
#define C_ADDIW(rd, imm)          MAKE_CODE16(inst, code, 0x2001 | (IMM(imm, 5, 5) << 12) | ((rd) << 7) | (IMM(imm, 4, 0) << 2))
#define C_ADDI16SP(imm)           MAKE_CODE16(inst, code, 0x6101 | (IMM(imm, 9, 9) << 12) | (IMM(imm, 4, 4) << 6) | (IMM(imm, 6, 6) << 5) | (IMM(imm, 8, 7) << 3) | (IMM(imm, 5, 5) << 2))
#define C_SUB(rd, rs)             MAKE_CODE16(inst, code, 0x8c01 | (to_rvc_reg(rd) << 7) | (to_rvc_reg(rs) << 2))
#define C_SUBW(rd, rs)            MAKE_CODE16(inst, code, 0x9c01 | (to_rvc_reg(rd) << 7) | (to_rvc_reg(rs) << 2))
#define C_AND(rd, rs)             MAKE_CODE16(inst, code, 0x8c61 | (to_rvc_reg(rd) << 7) | (to_rvc_reg(rs) << 2))
#define C_ANDI(rd, imm)           MAKE_CODE16(inst, code, 0x8801 | (IMM(imm, 5, 5) << 12) | (to_rvc_reg(rd) << 7) | (IMM(imm, 4, 0) << 2))
#define C_OR(rd, rs)              MAKE_CODE16(inst, code, 0x8c41 | (to_rvc_reg(rd) << 7) | (to_rvc_reg(rs) << 2))
#define C_XOR(rd, rs)             MAKE_CODE16(inst, code, 0x8c21 | (to_rvc_reg(rd) << 7) | (to_rvc_reg(rs) << 2))
#define C_SLLI(rd, imm)           MAKE_CODE16(inst, code, 0x0002 | (IMM(imm, 5, 5) << 12) | ((rd) << 7) | (IMM(imm, 4, 0) << 2))
#define C_SRLI(rd, imm)           MAKE_CODE16(inst, code, 0x8001 | (IMM(imm, 5, 5) << 12) | (to_rvc_reg(rd) << 7) | (IMM(imm, 4, 0) << 2))
#define C_SRAI(rd, imm)           MAKE_CODE16(inst, code, 0x8401 | (IMM(imm, 5, 5) << 12) | (to_rvc_reg(rd) << 7) | (IMM(imm, 4, 0) << 2))
#define C_LW(rd, imm, rs)         MAKE_CODE16(inst, code, 0x4000 | (IMM(imm, 5, 3) << 10) | (to_rvc_reg(rs) << 7) | (IMM(imm, 2, 2) << 6) | (IMM(imm, 6, 6) << 5) | (to_rvc_reg(rd) << 2))
#define C_LD(rd, imm, rs)         MAKE_CODE16(inst, code, 0x6000 | (IMM(imm, 5, 3) << 10) | (to_rvc_reg(rs) << 7) | (IMM(imm, 7, 6) << 5) | (to_rvc_reg(rd) << 2))
#define C_SW(rs2, imm, rs1)       MAKE_CODE16(inst, code, 0xc000 | (IMM(imm, 5, 3) << 10) | (to_rvc_reg(rs1) << 7) | (IMM(imm, 7, 6) << 5) | (to_rvc_reg(rs2) << 2))
#define C_SD(rs2, imm, rs1)       MAKE_CODE16(inst, code, 0xe000 | (IMM(imm, 5, 3) << 10) | (to_rvc_reg(rs1) << 7) | (IMM(imm, 7, 6) << 5) | (to_rvc_reg(rs2) << 2))
#define C_LDSP(rd, imm)           MAKE_CODE16(inst, code, 0x6002 | (IMM(imm, 5, 5) << 12) | ((rd) << 7) | (IMM(imm, 4, 3) << 5) | (IMM(imm, 8, 6) << 2))
#define C_SDSP(rs, imm)           MAKE_CODE16(inst, code, 0xe002 | (IMM(imm, 5, 3) << 10) | (IMM(imm, 8, 6) << 7) | ((rs) << 2))
#define C_J()                     MAKE_CODE16(inst, code, 0xa001)
#define C_JR(rs)                  MAKE_CODE16(inst, code, 0x8002 | ((rs) << 7))
#define C_JALR(rs)                MAKE_CODE16(inst, code, 0x9002 | ((rs) << 7))
#define C_BEQZ(rs)                MAKE_CODE16(inst, code, 0xc001 | (to_rvc_reg(rs) << 7))
#define C_BNEZ(rs)                MAKE_CODE16(inst, code, 0xe001 | (to_rvc_reg(rs) << 7))

#define C_FLD(rd, imm, rs)        MAKE_CODE16(inst, code, 0x2000 | (IMM(imm, 5, 3) << 10) | (to_rvc_freg(rs) << 7) | (IMM(imm, 7, 6) << 5) | (to_rvc_freg(rd) << 2))
#define C_FSD(rs2, imm, rs1)      MAKE_CODE16(inst, code, 0xa000 | (IMM(imm, 5, 3) << 10) | (to_rvc_freg(rs1) << 7) | (IMM(imm, 7, 6) << 5) | (to_rvc_freg(rs2) << 2))
#define C_FLDSP(rd, imm)          MAKE_CODE16(inst, code, 0x2002 | (IMM(imm, 5, 5) << 12) | ((rd) << 7) | (IMM(imm, 4, 3) << 5) | (IMM(imm, 8, 6) << 2))
#define C_FSDSP(rs, imm)          MAKE_CODE16(inst, code, 0xa002 | (IMM(imm, 5, 3) << 10) | (IMM(imm, 8, 6) << 7) | ((rs) << 2))

// Pseudo instructions
#define P_RET()                   C_JR(RA)
#define P_LI(rd, imm)             W_ADDI(rd, ZERO, imm)
#define P_NEG(rd, rs)             W_SUB(rd, ZERO, rs)
#define P_NOT(rd, rs)             W_XORI(rd, rs, -1)
#define P_SEXT_B(rd, rs)          do { if ((rd) == (rs)) C_SLLI(rd, 56); else W_SLLI(rd, rs, 56); if (is_rvc_reg(rd)) C_SRAI(rd, 56); else W_SRAI(rd, rd, 56); } while (0)
#define P_SEXT_H(rd, rs)          do { if ((rd) == (rs)) C_SLLI(rd, 48); else W_SLLI(rd, rs, 48); if (is_rvc_reg(rd)) C_SRAI(rd, 48); else W_SRAI(rd, rd, 48); } while (0)
#define P_SEXT_W(rd, rs)          do { if ((rd) == (rs)) C_ADDIW(rd, 0); else W_ADDIW(rd, rs, 0); } while (0)
#define P_ZEXT_B(rd, rs)          W_ANDI(rd, rs, 0xff)
#define P_ZEXT_H(rd, rs)          do { if ((rd) == (rs)) C_SLLI(rd, 48); else W_SLLI(rd, rs, 48); if (is_rvc_reg(rd)) C_SRLI(rd, 48); else W_SRLI(rd, rd, 48); } while (0)
#define P_ZEXT_W(rd, rs)          do { if ((rd) == (rs)) C_SLLI(rd, 32); else W_SLLI(rd, rs, 32); if (is_rvc_reg(rd)) C_SRLI(rd, 32); else W_SRLI(rd, rd, 32); } while (0)
#define P_SEQZ(rd, rs)            W_SLTIU(rd, rs, 1)
#define P_SNEZ(rd, rs)            W_SLTU(rd, ZERO, rs)
#define P_SLTZ(rd, rs)            W_SLT(rd, rs, ZERO)
#define P_SGTZ(rd, rs)            W_SLT(rd, ZERO, rs)

#define P_FMV_D(rd, rs)           W_FSGNJ_D(rd, rs, rs)
#define P_FMV_S(rd, rs)           W_FSGNJ_S(rd, rs, rs)
#define P_FNEG_D(rd, rs)          W_FSGNJN_D(rd, rs, rs)
#define P_FNEG_S(rd, rs)          W_FSGNJN_S(rd, rs, rs)

#define SWIZZLE_C_J(offset) \
    ((IMM(offset, 11, 11) << 12) | (IMM(offset, 4, 4) << 11) | \
     (IMM(offset, 9, 8) << 9) | (IMM(offset, 10, 10) << 8) | (IMM(offset, 6, 6) << 7) | \
     (IMM(offset, 7, 7) << 6) | (IMM(offset, 3, 1) << 3) | (IMM(offset, 5, 5) << 2))
#define SWIZZLE_C_BXX(offset) \
    ((IMM(offset, 8, 8) << 12) | (IMM(offset, 4, 3) << 10) | \
     (IMM(offset, 7, 6) << 5) | (IMM(offset, 2, 1) << 3) | (IMM(offset, 5, 5) << 2))
#define SWIZZLE_BXX(offset) \
    ((IMM(offset, 12, 12) << 31) | (IMM(offset, 10, 5) << 25) | \
     (IMM(offset, 4, 1) << 8) | (IMM(offset, 11, 11) << 7))
#define SWIZZLE_JAL(ofs)  ((IMM(ofs, 20, 20) << 31) | (IMM(ofs, 10, 1) << 21) | (IMM(ofs, 11, 11) << 20) | (IMM(ofs, 19, 12) << 12))

static inline bool is_rvc_reg(int reg)  { return reg >= 8 && reg <= 15; }  // X8~X15
static inline int to_rvc_reg(int reg)  { return reg - 8; }
#define is_rvc_freg  is_rvc_reg
#define to_rvc_freg  to_rvc_reg
