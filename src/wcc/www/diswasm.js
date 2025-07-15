#! /usr/bin/env ts-node
'use strict';
var __spreadArray = (this && this.__spreadArray) || function (to, from, pack) {
    if (pack || arguments.length === 2) for (var i = 0, l = from.length, ar; i < l; i++) {
        if (ar || !(i in from)) {
            if (!ar) ar = Array.prototype.slice.call(from, 0, i);
            ar[i] = from[i];
        }
    }
    return to.concat(ar || Array.prototype.slice.call(from));
};
Object.defineProperty(exports, "__esModule", { value: true });
exports.DisWasm = void 0;
var Custom;
(function (Custom) {
    Custom["LINKING"] = "linking";
    Custom["RELOC_CODE"] = "reloc.CODE";
    Custom["RELOC_DATA"] = "reloc.DATA";
    Custom["NAME"] = "name";
})(Custom || (Custom = {}));
var LINKING_VERSION = 2;
var InstTable = new Map([
    [0 /* Opcode.UNREACHABLE */, { op: 'unreachable' }],
    [1 /* Opcode.NOP */, { op: 'nop' }],
    [2 /* Opcode.BLOCK */, { op: 'block', operands: [0 /* OperandKind.TYPE */], opKind: 1 /* OpKind.BLOCK */ }],
    [3 /* Opcode.LOOP */, { op: 'loop', operands: [0 /* OperandKind.TYPE */], opKind: 1 /* OpKind.BLOCK */ }],
    [4 /* Opcode.IF */, { op: 'if', operands: [0 /* OperandKind.TYPE */], opKind: 1 /* OpKind.BLOCK */ }],
    [5 /* Opcode.ELSE */, { op: 'else', opKind: 2 /* OpKind.ELSE */ }],
    [6 /* Opcode.TRY */, { op: 'try', operands: [0 /* OperandKind.TYPE */], opKind: 1 /* OpKind.BLOCK */ }],
    [7 /* Opcode.CATCH */, { op: 'catch', operands: [1 /* OperandKind.ULEB128 */], opKind: 2 /* OpKind.ELSE */ }],
    [8 /* Opcode.THROW */, { op: 'throw', operands: [1 /* OperandKind.ULEB128 */] }],
    [9 /* Opcode.RETHROW */, { op: 'rethrow', operands: [1 /* OperandKind.ULEB128 */] }],
    [11 /* Opcode.END */, { op: 'end' }],
    [12 /* Opcode.BR */, { op: 'br', operands: [1 /* OperandKind.ULEB128 */] }],
    [13 /* Opcode.BR_IF */, { op: 'br_if', operands: [1 /* OperandKind.ULEB128 */] }],
    [14 /* Opcode.BR_TABLE */, { op: 'br_table', operands: [2 /* OperandKind.ULEB128ARRAY */, 1 /* OperandKind.ULEB128 */], opKind: 5 /* OpKind.BR_TABLE */ }],
    [15 /* Opcode.RETURN */, { op: 'return' }],
    [16 /* Opcode.CALL */, { op: 'call', operands: [1 /* OperandKind.ULEB128 */], opKind: 7 /* OpKind.CALL */ }],
    [17 /* Opcode.CALL_INDIRECT */, { op: 'call_indirect', operands: [1 /* OperandKind.ULEB128 */, 1 /* OperandKind.ULEB128 */], opKind: 8 /* OpKind.CALL_INDIRECT */ }],
    [24 /* Opcode.DELEGATE */, { op: 'delegate', operands: [3 /* OperandKind.I32CONST */] }],
    [25 /* Opcode.CATCH_ALL */, { op: 'catch_all', opKind: 2 /* OpKind.ELSE */ }],
    [26 /* Opcode.DROP */, { op: 'drop' }],
    [27 /* Opcode.SELECT */, { op: 'select' }],
    [32 /* Opcode.LOCAL_GET */, { op: 'local.get', operands: [1 /* OperandKind.ULEB128 */] }],
    [33 /* Opcode.LOCAL_SET */, { op: 'local.set', operands: [1 /* OperandKind.ULEB128 */] }],
    [34 /* Opcode.LOCAL_TEE */, { op: 'local.tee', operands: [1 /* OperandKind.ULEB128 */] }],
    [35 /* Opcode.GLOBAL_GET */, { op: 'global.get', operands: [1 /* OperandKind.ULEB128 */], opKind: 6 /* OpKind.GLOBAL */ }],
    [36 /* Opcode.GLOBAL_SET */, { op: 'global.set', operands: [1 /* OperandKind.ULEB128 */], opKind: 6 /* OpKind.GLOBAL */ }],
    [40 /* Opcode.I32_LOAD */, { op: 'i32.load', operands: [1 /* OperandKind.ULEB128 */, 1 /* OperandKind.ULEB128 */], opKind: 3 /* OpKind.LOAD */ }],
    [41 /* Opcode.I64_LOAD */, { op: 'i64.load', operands: [1 /* OperandKind.ULEB128 */, 1 /* OperandKind.ULEB128 */], opKind: 3 /* OpKind.LOAD */ }],
    [42 /* Opcode.F32_LOAD */, { op: 'f32.load', operands: [1 /* OperandKind.ULEB128 */, 1 /* OperandKind.ULEB128 */], opKind: 3 /* OpKind.LOAD */ }],
    [43 /* Opcode.F64_LOAD */, { op: 'f64.load', operands: [1 /* OperandKind.ULEB128 */, 1 /* OperandKind.ULEB128 */], opKind: 3 /* OpKind.LOAD */ }],
    [54 /* Opcode.I32_STORE */, { op: 'i32.store', operands: [1 /* OperandKind.ULEB128 */, 1 /* OperandKind.ULEB128 */], opKind: 4 /* OpKind.STORE */ }],
    [55 /* Opcode.I64_STORE */, { op: 'i64.store', operands: [1 /* OperandKind.ULEB128 */, 1 /* OperandKind.ULEB128 */], opKind: 4 /* OpKind.STORE */ }],
    [56 /* Opcode.F32_STORE */, { op: 'f32.store', operands: [1 /* OperandKind.ULEB128 */, 1 /* OperandKind.ULEB128 */], opKind: 4 /* OpKind.STORE */ }],
    [57 /* Opcode.F64_STORE */, { op: 'f64.store', operands: [1 /* OperandKind.ULEB128 */, 1 /* OperandKind.ULEB128 */], opKind: 4 /* OpKind.STORE */ }],
    [44 /* Opcode.I32_LOAD8_S */, { op: 'i32.load8_s', operands: [1 /* OperandKind.ULEB128 */, 1 /* OperandKind.ULEB128 */], opKind: 3 /* OpKind.LOAD */ }],
    [45 /* Opcode.I32_LOAD8_U */, { op: 'i32.load8_u', operands: [1 /* OperandKind.ULEB128 */, 1 /* OperandKind.ULEB128 */], opKind: 3 /* OpKind.LOAD */ }],
    [46 /* Opcode.I32_LOAD16_S */, { op: 'i32.load16_s', operands: [1 /* OperandKind.ULEB128 */, 1 /* OperandKind.ULEB128 */], opKind: 3 /* OpKind.LOAD */ }],
    [47 /* Opcode.I32_LOAD16_U */, { op: 'i32.load16_u', operands: [1 /* OperandKind.ULEB128 */, 1 /* OperandKind.ULEB128 */], opKind: 3 /* OpKind.LOAD */ }],
    [48 /* Opcode.I64_LOAD8_S */, { op: 'i64.load8_s', operands: [1 /* OperandKind.ULEB128 */, 1 /* OperandKind.ULEB128 */], opKind: 3 /* OpKind.LOAD */ }],
    [49 /* Opcode.I64_LOAD8_U */, { op: 'i64.load8_u', operands: [1 /* OperandKind.ULEB128 */, 1 /* OperandKind.ULEB128 */], opKind: 3 /* OpKind.LOAD */ }],
    [50 /* Opcode.I64_LOAD16_S */, { op: 'i64.load16_s', operands: [1 /* OperandKind.ULEB128 */, 1 /* OperandKind.ULEB128 */], opKind: 3 /* OpKind.LOAD */ }],
    [51 /* Opcode.I64_LOAD16_U */, { op: 'i64.load16_u', operands: [1 /* OperandKind.ULEB128 */, 1 /* OperandKind.ULEB128 */], opKind: 3 /* OpKind.LOAD */ }],
    [52 /* Opcode.I64_LOAD32_S */, { op: 'i64.load32_s', operands: [1 /* OperandKind.ULEB128 */, 1 /* OperandKind.ULEB128 */], opKind: 3 /* OpKind.LOAD */ }],
    [53 /* Opcode.I64_LOAD32_U */, { op: 'i64.load32_u', operands: [1 /* OperandKind.ULEB128 */, 1 /* OperandKind.ULEB128 */], opKind: 3 /* OpKind.LOAD */ }],
    [58 /* Opcode.I32_STORE8 */, { op: 'i32.store8', operands: [1 /* OperandKind.ULEB128 */, 1 /* OperandKind.ULEB128 */], opKind: 4 /* OpKind.STORE */ }],
    [59 /* Opcode.I32_STORE16 */, { op: 'i32.store16', operands: [1 /* OperandKind.ULEB128 */, 1 /* OperandKind.ULEB128 */], opKind: 4 /* OpKind.STORE */ }],
    [60 /* Opcode.I64_STORE8 */, { op: 'i64.store8', operands: [1 /* OperandKind.ULEB128 */, 1 /* OperandKind.ULEB128 */], opKind: 4 /* OpKind.STORE */ }],
    [61 /* Opcode.I64_STORE16 */, { op: 'i64.store16', operands: [1 /* OperandKind.ULEB128 */, 1 /* OperandKind.ULEB128 */], opKind: 4 /* OpKind.STORE */ }],
    [62 /* Opcode.I64_STORE32 */, { op: 'i64.store32', operands: [1 /* OperandKind.ULEB128 */, 1 /* OperandKind.ULEB128 */], opKind: 4 /* OpKind.STORE */ }],
    [63 /* Opcode.MEMORY_SIZE */, { op: 'memory.size', operands: [1 /* OperandKind.ULEB128 */], opKind: 9 /* OpKind.OMIT_OPERANDS */ }],
    [64 /* Opcode.MEMORY_GROW */, { op: 'memory.grow', operands: [1 /* OperandKind.ULEB128 */], opKind: 9 /* OpKind.OMIT_OPERANDS */ }],
    [65 /* Opcode.I32_CONST */, { op: 'i32.const', operands: [3 /* OperandKind.I32CONST */] }],
    [66 /* Opcode.I64_CONST */, { op: 'i64.const', operands: [4 /* OperandKind.I64CONST */] }],
    [67 /* Opcode.F32_CONST */, { op: 'f32.const', operands: [5 /* OperandKind.F32CONST */] }],
    [68 /* Opcode.F64_CONST */, { op: 'f64.const', operands: [6 /* OperandKind.F64CONST */] }],
    [69 /* Opcode.I32_EQZ */, { op: 'i32.eqz' }],
    [70 /* Opcode.I32_EQ */, { op: 'i32.eq' }],
    [71 /* Opcode.I32_NE */, { op: 'i32.ne' }],
    [72 /* Opcode.I32_LT_S */, { op: 'i32.lt_s' }],
    [73 /* Opcode.I32_LT_U */, { op: 'i32.lt_u' }],
    [74 /* Opcode.I32_GT_S */, { op: 'i32.gt_s' }],
    [75 /* Opcode.I32_GT_U */, { op: 'i32.gt_u' }],
    [76 /* Opcode.I32_LE_S */, { op: 'i32.le_s' }],
    [77 /* Opcode.I32_LE_U */, { op: 'i32.le_u' }],
    [78 /* Opcode.I32_GE_S */, { op: 'i32.ge_s' }],
    [79 /* Opcode.I32_GE_U */, { op: 'i32.ge_u' }],
    [80 /* Opcode.I64_EQZ */, { op: 'i64.eqz' }],
    [81 /* Opcode.I64_EQ */, { op: 'i64.eq' }],
    [82 /* Opcode.I64_NE */, { op: 'i64.ne' }],
    [83 /* Opcode.I64_LT_S */, { op: 'i64.lt_s' }],
    [84 /* Opcode.I64_LT_U */, { op: 'i64.lt_u' }],
    [85 /* Opcode.I64_GT_S */, { op: 'i64.gt_s' }],
    [86 /* Opcode.I64_GT_U */, { op: 'i64.gt_u' }],
    [87 /* Opcode.I64_LE_S */, { op: 'i64.le_s' }],
    [88 /* Opcode.I64_LE_U */, { op: 'i64.le_u' }],
    [89 /* Opcode.I64_GE_S */, { op: 'i64.ge_s' }],
    [90 /* Opcode.I64_GE_U */, { op: 'i64.ge_u' }],
    [91 /* Opcode.F32_EQ */, { op: 'f32.eq' }],
    [92 /* Opcode.F32_NE */, { op: 'f32.ne' }],
    [93 /* Opcode.F32_LT */, { op: 'f32.lt' }],
    [94 /* Opcode.F32_GT */, { op: 'f32.gt' }],
    [95 /* Opcode.F32_LE */, { op: 'f32.le' }],
    [96 /* Opcode.F32_GE */, { op: 'f32.ge' }],
    [97 /* Opcode.F64_EQ */, { op: 'f64.eq' }],
    [98 /* Opcode.F64_NE */, { op: 'f64.ne' }],
    [99 /* Opcode.F64_LT */, { op: 'f64.lt' }],
    [100 /* Opcode.F64_GT */, { op: 'f64.gt' }],
    [101 /* Opcode.F64_LE */, { op: 'f64.le' }],
    [102 /* Opcode.F64_GE */, { op: 'f64.ge' }],
    [103 /* Opcode.I32_CLZ */, { op: 'i32.clz' }],
    [104 /* Opcode.I32_CTZ */, { op: 'i32.ctz' }],
    [105 /* Opcode.I32_POPCNT */, { op: 'i32.popcnt' }],
    [106 /* Opcode.I32_ADD */, { op: 'i32.add' }],
    [107 /* Opcode.I32_SUB */, { op: 'i32.sub' }],
    [108 /* Opcode.I32_MUL */, { op: 'i32.mul' }],
    [109 /* Opcode.I32_DIV_S */, { op: 'i32.div_s' }],
    [110 /* Opcode.I32_DIV_U */, { op: 'i32.div_u' }],
    [111 /* Opcode.I32_REM_S */, { op: 'i32.rem_s' }],
    [112 /* Opcode.I32_REM_U */, { op: 'i32.rem_u' }],
    [113 /* Opcode.I32_AND */, { op: 'i32.and' }],
    [114 /* Opcode.I32_OR */, { op: 'i32.or' }],
    [115 /* Opcode.I32_XOR */, { op: 'i32.xor' }],
    [116 /* Opcode.I32_SHL */, { op: 'i32.shl' }],
    [117 /* Opcode.I32_SHR_S */, { op: 'i32.shr_s' }],
    [118 /* Opcode.I32_SHR_U */, { op: 'i32.shr_u' }],
    [119 /* Opcode.I32_ROTL */, { op: 'i32.rotl' }],
    [120 /* Opcode.I32_ROTR */, { op: 'i32.rotr' }],
    [121 /* Opcode.I64_CLZ */, { op: 'i64.clz' }],
    [122 /* Opcode.I64_CTZ */, { op: 'i64.ctz' }],
    [123 /* Opcode.I64_POPCNT */, { op: 'i64.popcnt' }],
    [124 /* Opcode.I64_ADD */, { op: 'i64.add' }],
    [125 /* Opcode.I64_SUB */, { op: 'i64.sub' }],
    [126 /* Opcode.I64_MUL */, { op: 'i64.mul' }],
    [127 /* Opcode.I64_DIV_S */, { op: 'i64.div_s' }],
    [128 /* Opcode.I64_DIV_U */, { op: 'i64.div_u' }],
    [129 /* Opcode.I64_REM_S */, { op: 'i64.rem_s' }],
    [130 /* Opcode.I64_REM_U */, { op: 'i64.rem_u' }],
    [131 /* Opcode.I64_AND */, { op: 'i64.and' }],
    [132 /* Opcode.I64_OR */, { op: 'i64.or' }],
    [133 /* Opcode.I64_XOR */, { op: 'i64.xor' }],
    [134 /* Opcode.I64_SHL */, { op: 'i64.shl' }],
    [135 /* Opcode.I64_SHR_S */, { op: 'i64.shr_s' }],
    [136 /* Opcode.I64_SHR_U */, { op: 'i64.shr_u' }],
    [137 /* Opcode.I64_ROTL */, { op: 'i64.rotl' }],
    [138 /* Opcode.I64_ROTR */, { op: 'i64.rotr' }],
    [139 /* Opcode.F32_ABS */, { op: 'f32.abs' }],
    [140 /* Opcode.F32_NEG */, { op: 'f32.neg' }],
    [141 /* Opcode.F32_CEIL */, { op: 'f32.ceil' }],
    [142 /* Opcode.F32_FLOOR */, { op: 'f32.floor' }],
    [143 /* Opcode.F32_TRUNC */, { op: 'f32.trunc' }],
    [144 /* Opcode.F32_NEAREST */, { op: 'f32.nearest' }],
    [145 /* Opcode.F32_SQRT */, { op: 'f32.sqrt' }],
    [146 /* Opcode.F32_ADD */, { op: 'f32.add' }],
    [147 /* Opcode.F32_SUB */, { op: 'f32.sub' }],
    [148 /* Opcode.F32_MUL */, { op: 'f32.mul' }],
    [149 /* Opcode.F32_DIV */, { op: 'f32.div' }],
    [150 /* Opcode.F32_MIN */, { op: 'f32.min' }],
    [151 /* Opcode.F32_MAX */, { op: 'f32.max' }],
    [152 /* Opcode.F32_COPYSIGN */, { op: 'f32.copysign' }],
    [153 /* Opcode.F64_ABS */, { op: 'f64.abs' }],
    [154 /* Opcode.F64_NEG */, { op: 'f64.neg' }],
    [155 /* Opcode.F64_CEIL */, { op: 'f64.ceil' }],
    [156 /* Opcode.F64_FLOOR */, { op: 'f64.floor' }],
    [157 /* Opcode.F64_TRUNC */, { op: 'f64.trunc' }],
    [158 /* Opcode.F64_NEAREST */, { op: 'f64.nearest' }],
    [159 /* Opcode.F64_SQRT */, { op: 'f64.sqrt' }],
    [160 /* Opcode.F64_ADD */, { op: 'f64.add' }],
    [161 /* Opcode.F64_SUB */, { op: 'f64.sub' }],
    [162 /* Opcode.F64_MUL */, { op: 'f64.mul' }],
    [163 /* Opcode.F64_DIV */, { op: 'f64.div' }],
    [164 /* Opcode.F64_MIN */, { op: 'f64.min' }],
    [165 /* Opcode.F64_MAX */, { op: 'f64.max' }],
    [166 /* Opcode.F64_COPYSIGN */, { op: 'f64.copysign' }],
    [167 /* Opcode.I32_WRAP_I64 */, { op: 'i32.wrap_i64' }],
    [168 /* Opcode.I32_TRUNC_F32_S */, { op: 'i32.trunc_f32_s' }],
    [169 /* Opcode.I32_TRUNC_F32_U */, { op: 'i32.trunc_f32_u' }],
    [170 /* Opcode.I32_TRUNC_F64_S */, { op: 'i32.trunc_f64_s' }],
    [171 /* Opcode.I32_TRUNC_F64_U */, { op: 'i32.trunc_f64_u' }],
    [172 /* Opcode.I64_EXTEND_I32_S */, { op: 'i64.extend_i32_s' }],
    [173 /* Opcode.I64_EXTEND_I32_U */, { op: 'i64.extend_i32_u' }],
    [174 /* Opcode.I64_TRUNC_F32_S */, { op: 'i64.trunc_f32_s' }],
    [175 /* Opcode.I64_TRUNC_F32_U */, { op: 'i64.trunc_f32_u' }],
    [176 /* Opcode.I64_TRUNC_F64_S */, { op: 'i64.trunc_f64_s' }],
    [177 /* Opcode.I64_TRUNC_F64_U */, { op: 'i64.trunc_f64_u' }],
    [178 /* Opcode.F32_CONVERT_I32_S */, { op: 'f32.convert_i32_s' }],
    [179 /* Opcode.F32_CONVERT_I32_U */, { op: 'f32.convert_i32_u' }],
    [182 /* Opcode.F32_DEMOTE_F64 */, { op: 'f32.demote_f64' }],
    [180 /* Opcode.F32_CONVERT_I64_S */, { op: 'f32.convert_i64_s' }],
    [181 /* Opcode.F32_CONVERT_I64_U */, { op: 'f32.convert_i64_u' }],
    [183 /* Opcode.F64_CONVERT_I32_S */, { op: 'f64.convert_i32_s' }],
    [184 /* Opcode.F64_CONVERT_I32_U */, { op: 'f64.convert_i32_u' }],
    [185 /* Opcode.F64_CONVERT_I64_S */, { op: 'f64.convert_i64_s' }],
    [186 /* Opcode.F64_CONVERT_I64_U */, { op: 'f64.convert_i64_u' }],
    [187 /* Opcode.F64_PROMOTE_F32 */, { op: 'f64.promote_f32' }],
    [188 /* Opcode.I32_REINTERPRET_F32 */, { op: 'i32.reinterpret_f32' }],
    [189 /* Opcode.I64_REINTERPRET_F64 */, { op: 'i64.reinterpret_f64' }],
    [190 /* Opcode.F32_REINTERPRET_I32 */, { op: 'f32.reinterpret_i32' }],
    [191 /* Opcode.F64_REINTERPRET_I64 */, { op: 'f64.reinterpret_i64' }],
    [192 /* Opcode.I32_EXTEND8_S */, { op: 'i32.extend8_s' }],
    [193 /* Opcode.I32_EXTEND16_S */, { op: 'i32.extend16_s' }],
    [194 /* Opcode.I64_EXTEND8_S */, { op: 'i64.extend8_s' }],
    [195 /* Opcode.I64_EXTEND16_S */, { op: 'i64.extend16_s' }],
    [196 /* Opcode.I64_EXTEND32_S */, { op: 'i64.extend32_s' }],
]);
var InstTableFc = new Map([
    [0 /* OpcodeEx.I32_TRUNC_SAT_F32_S */, { op: 'i32.trunc_sat_f32_s' }],
    [1 /* OpcodeEx.I32_TRUNC_SAT_F32_U */, { op: 'i32.trunc_sat_f32_u' }],
    [2 /* OpcodeEx.I32_TRUNC_SAT_F64_S */, { op: 'i32.trunc_sat_f64_s' }],
    [3 /* OpcodeEx.I32_TRUNC_SAT_F64_U */, { op: 'i32.trunc_sat_f32_u' }],
    [4 /* OpcodeEx.I64_TRUNC_SAT_F32_S */, { op: 'i64.trunc_sat_f32_s' }],
    [5 /* OpcodeEx.I64_TRUNC_SAT_F32_U */, { op: 'i64.trunc_sat_f32_u' }],
    [6 /* OpcodeEx.I64_TRUNC_SAT_F64_S */, { op: 'i64.trunc_sat_f64_s' }],
    [7 /* OpcodeEx.I64_TRUNC_SAT_F64_U */, { op: 'i64.trunc_sat_f32_u' }],
    [10 /* OpcodeEx.MEMORY_COPY */, { op: 'memory.copy', operands: [1 /* OperandKind.ULEB128 */, 1 /* OperandKind.ULEB128 */], opKind: 9 /* OpKind.OMIT_OPERANDS */ }], // src, dst
    [11 /* OpcodeEx.MEMORY_FILL */, { op: 'memory.fill', operands: [1 /* OperandKind.ULEB128 */], opKind: 9 /* OpKind.OMIT_OPERANDS */ }], // dst
]);
var BufferReader = /** @class */ (function () {
    function BufferReader(buffer) {
        this.offset = 0;
        this.byteArray = new Uint8Array(buffer);
    }
    BufferReader.prototype.getOffset = function () { return this.offset; };
    BufferReader.prototype.setOffset = function (offset) { this.offset = offset; };
    BufferReader.prototype.isEof = function () { return this.offset >= this.byteArray.byteLength; };
    BufferReader.prototype.readu8 = function () {
        return this.byteArray[this.offset++];
    };
    BufferReader.prototype.readi32 = function () {
        var value = new Int32Array(this.byteArray.buffer, this.offset, 1)[0];
        this.offset += 4;
        return value;
    };
    BufferReader.prototype.readiconst = function () {
        var x = 0;
        var bits = 0;
        var ofs = this.offset;
        while (ofs < this.byteArray.byteLength) {
            if (bits >= 32 - 7)
                return this.readiconstBig(BigInt(x), BigInt(bits), ofs);
            var c = this.byteArray[ofs++];
            x |= (c & 0x7f) << bits;
            bits += 7;
            if ((c & 0x80) === 0) {
                if ((c & 0x40) !== 0)
                    x -= 1 << bits;
                break;
            }
        }
        this.offset = ofs;
        return x;
    };
    BufferReader.prototype.readiconstBig = function (x, bits, ofs) {
        while (ofs < this.byteArray.byteLength) {
            var c = this.byteArray[ofs++];
            x += BigInt(c & 0x7f) << bits;
            bits += BigInt(7);
            if ((c & 0x80) === 0) {
                if ((c & 0x40) !== 0)
                    x -= BigInt(1) << bits;
                break;
            }
        }
        this.offset = ofs;
        return x;
    };
    BufferReader.prototype.readf32 = function () {
        var buffer = this.byteArray.buffer;
        var offset = this.offset;
        if ((offset & 3) !== 0) {
            buffer = this.byteArray.slice(offset, offset + 4).buffer;
            offset = 0;
        }
        var value = new Float32Array(buffer, offset, 1)[0];
        this.offset += 4;
        return value;
    };
    BufferReader.prototype.readf64 = function () {
        var buffer = this.byteArray.buffer;
        var offset = this.offset;
        if ((offset & 7) !== 0) {
            buffer = this.byteArray.slice(offset, offset + 8).buffer;
            offset = 0;
        }
        var value = new Float64Array(buffer, offset, 1)[0];
        this.offset += 8;
        return value;
    };
    BufferReader.prototype.readLeb128 = function () {
        var x = 0;
        var bits = 0;
        var ofs = this.offset;
        while (ofs < this.byteArray.byteLength) {
            var c = this.byteArray[ofs++];
            x |= (c & 0x7f) << bits;
            bits += 7;
            if ((c & 0x80) === 0) {
                if ((c & 0x40) !== 0)
                    x -= 1 << bits;
                break;
            }
        }
        this.offset = ofs;
        return x;
    };
    BufferReader.prototype.readUleb128 = function () {
        var x = 0;
        var bits = 0;
        var ofs = this.offset;
        while (ofs < this.byteArray.byteLength) {
            var c = this.byteArray[ofs++];
            x |= (c & 0x7f) << bits;
            bits += 7;
            if ((c & 0x80) === 0)
                break;
        }
        this.offset = ofs;
        return x;
    };
    BufferReader.prototype.readString = function () {
        var len = this.readUleb128();
        var u8array = this.byteArray.slice(this.offset, this.offset + len);
        this.offset += len;
        return new TextDecoder('utf-8').decode(u8array);
    };
    BufferReader.prototype.u8array = function (length) {
        var u8array = this.byteArray.slice(this.offset, length);
        this.offset += length;
        return u8array;
    };
    return BufferReader;
}());
var Type = /** @class */ (function () {
    function Type(type) {
        this.type = type;
    }
    Type.prototype.getType = function () { return this.type; };
    Type.prototype.toString = function () {
        if (typeof (this.type) === 'object') {
            switch (this.type.type) {
                case 'func':
                    {
                        var t = this.type;
                        var params = t.params.length === 0 ? '' : " (param ".concat(t.params.map(function (param) { return "".concat(param); }).join(' '), ")");
                        var results = t.results.length === 0 ? '' : " (result ".concat(t.results.map(function (param) { return "".concat(param); }).join(' '), ")");
                        return "(".concat(t.type).concat(params).concat(results, ")");
                    }
                case 'funcref':
                    {
                        var t = this.type;
                        return "".concat(t.initial, " funcref"); // TODO: t.flag
                    }
                default:
                    throw "Unhandled: ".concat(this.type);
            }
        }
        else {
            switch (this.type) {
                case 64 /* WasmType.VOID */: return 'void';
                case 127 /* WasmType.I32 */: return 'i32';
                case 126 /* WasmType.I64 */: return 'i64';
                case 125 /* WasmType.F32 */: return 'f32';
                case 124 /* WasmType.F64 */: return 'f64';
                default: throw "Unhandled: ".concat(this.type);
            }
        }
    };
    return Type;
}());
function readType(bufferReader) {
    var t = bufferReader.readu8();
    switch (t) {
        case 64 /* WasmType.VOID */:
        case 127 /* WasmType.I32 */:
        case 126 /* WasmType.I64 */:
        case 125 /* WasmType.F32 */:
        case 124 /* WasmType.F64 */:
            return new Type(t);
        case 96 /* WasmType.FUNC */:
            {
                var numParams = bufferReader.readUleb128();
                var params = __spreadArray([], Array(numParams), true).map(function () { return readType(bufferReader); });
                var numResults = bufferReader.readUleb128();
                var results = __spreadArray([], Array(numResults), true).map(function () { return readType(bufferReader); });
                return new Type({ type: 'func', params: params, results: results });
            }
        case 112 /* WasmType.FUNCREF */:
            {
                var flag = bufferReader.readu8();
                var initial = bufferReader.readLeb128();
                return new Type({ type: 'funcref', flag: flag, initial: initial });
            }
        default:
            throw "Unhnadled type: at 0x".concat((bufferReader.getOffset() - 1).toString(16));
    }
}
function readGlobalValue(bufferReader) {
    var op = bufferReader.readu8();
    switch (op) {
        case 65 /* Opcode.I32_CONST */:
        case 66 /* Opcode.I64_CONST */:
            return bufferReader.readiconst();
        case 67 /* Opcode.F32_CONST */:
            return bufferReader.readf32();
        case 68 /* Opcode.F64_CONST */:
            return bufferReader.readf64();
        default:
            throw "Unhnadled type: ".concat(op, " at ").concat((bufferReader.getOffset() - 1).toString(16));
    }
}
function readOperand(bufferReader, kind) {
    switch (kind) {
        case 0 /* OperandKind.TYPE */:
            return readType(bufferReader);
        case 1 /* OperandKind.ULEB128 */:
            return bufferReader.readUleb128();
        case 2 /* OperandKind.ULEB128ARRAY */:
            {
                var count = bufferReader.readUleb128();
                return __spreadArray([], Array(count), true).map(function (_) { return bufferReader.readUleb128(); });
            }
        case 3 /* OperandKind.I32CONST */:
        case 4 /* OperandKind.I64CONST */:
            return bufferReader.readiconst();
        case 5 /* OperandKind.F32CONST */:
            return bufferReader.readf32();
        case 6 /* OperandKind.F64CONST */:
            return bufferReader.readf64();
        default:
            throw "Unhandled operand: ".concat(kind, " at 0x").concat(bufferReader.getOffset().toString(16));
    }
}
var Inst = /** @class */ (function () {
    function Inst() {
    }
    return Inst;
}());
function readInst(bufferReader) {
    var op = bufferReader.readu8();
    if (op === 252 /* Opcode._0xFC */) {
        var opex = bufferReader.readu8();
        var table_1 = InstTableFc.get(opex);
        if (table_1 == null) {
            throw "Unhandled opex: 0x".concat(opex.toString(16).padStart(2, '0'), " at 0x").concat((bufferReader.getOffset() - 1).toString(16));
        }
        var inst_1 = { opcode: op, opcodeex: opex, opKind: table_1.opKind || 0 /* OpKind.MISC */, opstr: table_1.op };
        if (table_1.operands != null) {
            inst_1.operandKinds = table_1.operands;
            inst_1.operands = table_1.operands.map(function (operand) { return readOperand(bufferReader, operand); });
        }
        return inst_1;
    }
    var table = InstTable.get(op);
    if (table == null) {
        throw "Unhandled op: 0x".concat(op.toString(16).padStart(2, '0'), " at 0x").concat((bufferReader.getOffset() - 1).toString(16));
    }
    var inst = { opcode: op, opKind: table.opKind || 0 /* OpKind.MISC */, opstr: table.op };
    if (table.operands != null) {
        inst.operandKinds = table.operands;
        inst.operands = table.operands.map(function (operand) { return readOperand(bufferReader, operand); });
    }
    return inst;
}
var SPACES = '    ';
function makeIndent(indent) {
    var len = indent * 2;
    while (len > SPACES.length)
        SPACES += SPACES;
    return SPACES.slice(0, len);
}
var DisWasm = /** @class */ (function () {
    function DisWasm(buffer, opts) {
        if (opts === void 0) { opts = {}; }
        this.opts = opts;
        this.version = -1;
        this.types = new Array();
        this.functions = new Array();
        this.codes = new Array();
        this.importFuncCount = 0;
        this.funcs = new Map();
        this.importGlobalCount = 0;
        this.globals = new Map();
        this.importTableCount = 0;
        this.tables = new Map();
        this.names = new Map(); // CustomNameType + index * 100
        this.log = console.log;
        this.bufferReader = new BufferReader(buffer);
    }
    DisWasm.prototype.setLogFunc = function (logFunc) {
        this.log = logFunc;
    };
    DisWasm.prototype.dump = function () {
        if (!this.checkHeader())
            throw Error('No wasm header');
        this.log('(module');
        this.log(";; WASM version: ".concat(this.version));
        this.findNameInfo();
        this.loadSections();
        this.log(')');
    };
    DisWasm.prototype.checkHeader = function () {
        var magic = this.bufferReader.u8array(4);
        if (new TextDecoder('utf-8').decode(magic) !== '\x00asm')
            return false;
        this.version = this.bufferReader.readi32();
        return true;
    };
    DisWasm.prototype.findNameInfo = function () {
        var offsetSaved = this.bufferReader.getOffset();
        var len = 0;
        var offset = 0;
        var importFuncCount = 0;
        var importGlobalCount = 0;
        var importTableCount = 0;
        for (; !this.bufferReader.isEof(); this.bufferReader.setOffset(offset + len)) {
            var sec = this.bufferReader.readu8();
            len = this.bufferReader.readUleb128();
            offset = this.bufferReader.getOffset();
            if (sec === 2 /* Section.IMPORT */) {
                var num = this.bufferReader.readUleb128();
                for (var i = 0; i < num; ++i) {
                    /*const modName =*/ this.bufferReader.readString();
                    /*const name =*/ this.bufferReader.readString();
                    var kind = this.bufferReader.readu8();
                    switch (kind) {
                        case 0 /* ImportKind.FUNC */:
                            {
                                /*const typeIndex =*/ this.bufferReader.readUleb128();
                                /*const index =*/ this.funcs.size;
                                ++importFuncCount;
                            }
                            break;
                        case 1 /* ImportKind.TABLE */:
                            {
                                /*const tt =*/ readType(this.bufferReader);
                                // this.log(`${this.addr(offset)}(import "${modName}" "${name}" (table ${tt}))`)
                                ++importTableCount;
                            }
                            break;
                        case 2 /* ImportKind.MEMORY */:
                            {
                                // TODO: Confirm
                                /*const index =*/ this.bufferReader.readUleb128();
                                /*const size =*/ this.bufferReader.readUleb128();
                                // this.log(`${this.addr(offset)}(import "${modName}" "${name}" (memory ${size} (;index=${index};)))`)
                            }
                            break;
                        case 3 /* ImportKind.GLOBAL */:
                            {
                                // TODO: Confirm
                                /*const type =*/ readType(this.bufferReader);
                                /*const mutable =*/ this.bufferReader.readu8();
                                // const index = this.globals.size
                                // const typename = mutable !== 0 ? `(mut ${type})` : `${type}`
                                // this.log(`${this.addr(offset)}(import "${modName}" "${name}" (global ${typename}))  ;; ${index}`)
                                // this.globals.set(index, [modName, name])
                                ++importGlobalCount;
                            }
                            break;
                        default:
                            throw ("Illegal import kind: ".concat(kind));
                    }
                }
            }
            else if (sec === 0 /* Section.CUSTOM */) {
                var customSectionOffset = this.bufferReader.getOffset();
                var name_1 = this.bufferReader.readString();
                if (name_1 === Custom.LINKING) {
                    var version = this.bufferReader.readUleb128();
                    if (version !== LINKING_VERSION)
                        continue;
                    while (this.bufferReader.getOffset() < customSectionOffset + len) {
                        var subsectype = this.bufferReader.readu8();
                        var payloadLen = this.bufferReader.readUleb128();
                        var subsecOffset = this.bufferReader.getOffset();
                        if (subsectype === 8 /* LinkingType.WASM_SYMBOL_TABLE */) {
                            var count = this.bufferReader.readUleb128();
                            for (var i = 0; i < count; ++i) {
                                var kind = this.bufferReader.readu8();
                                var flags = this.bufferReader.readUleb128();
                                switch (kind) {
                                    case 0 /* SymInfoKind.SYMTAB_FUNCTION */:
                                    case 2 /* SymInfoKind.SYMTAB_GLOBAL */:
                                    case 5 /* SymInfoKind.SYMTAB_TABLE */:
                                        {
                                            var index = this.bufferReader.readUleb128();
                                            switch (kind) {
                                                case 0 /* SymInfoKind.SYMTAB_FUNCTION */:
                                                    if (index >= importFuncCount || (flags & 64 /* SymFlags.WASM_SYM_EXPLICIT_NAME */)) {
                                                        var symname = this.bufferReader.readString();
                                                        this.setCustomName(1 /* CustomNameType.FUNCTION */, index, symname);
                                                    }
                                                    break;
                                                case 2 /* SymInfoKind.SYMTAB_GLOBAL */:
                                                    if (index >= importGlobalCount || (flags & 64 /* SymFlags.WASM_SYM_EXPLICIT_NAME */)) {
                                                        var symname = this.bufferReader.readString();
                                                        this.setCustomName(7 /* CustomNameType.GLOBAL */, index, symname);
                                                    }
                                                    break;
                                                case 5 /* SymInfoKind.SYMTAB_TABLE */:
                                                    if (index >= importTableCount || (flags & 64 /* SymFlags.WASM_SYM_EXPLICIT_NAME */)) {
                                                        var symname = this.bufferReader.readString();
                                                        this.setCustomName(5 /* CustomNameType.TABLE */, index, symname);
                                                    }
                                                    break;
                                                default: break;
                                            }
                                        }
                                        break;
                                    case 1 /* SymInfoKind.SYMTAB_DATA */:
                                        {
                                            /*const symname =*/ this.bufferReader.readString();
                                            if (!(flags & 16 /* SymFlags.WASM_SYM_UNDEFINED */)) {
                                                /*const index =*/ this.bufferReader.readUleb128();
                                                /*const suboffset =*/ this.bufferReader.readUleb128();
                                                /*const size =*/ this.bufferReader.readUleb128();
                                                // this.setCustomName(CustomNameType.DATASEG, index, symname)
                                            }
                                        }
                                        break;
                                    case 4 /* SymInfoKind.SYMTAB_EVENT */:
                                        {
                                            /*const typeindex =*/ this.bufferReader.readUleb128();
                                            /*const symname =*/ this.bufferReader.readString();
                                        }
                                        break;
                                    default: break;
                                }
                            }
                        }
                        this.bufferReader.setOffset(subsecOffset + payloadLen);
                    }
                    break;
                }
                if (name_1 === Custom.NAME) {
                    while (this.bufferReader.getOffset() < customSectionOffset + len) {
                        var nametype = this.bufferReader.readu8();
                        var payloadLen = this.bufferReader.readUleb128();
                        var subsecOffset2 = this.bufferReader.getOffset();
                        switch (nametype) {
                            case 0 /* CustomNameType.MODULE */:
                            case 1 /* CustomNameType.FUNCTION */:
                            case 2 /* CustomNameType.LOCAL */:
                            case 3 /* CustomNameType.LABEL */:
                            case 4 /* CustomNameType.TYPE */:
                            case 5 /* CustomNameType.TABLE */:
                            case 6 /* CustomNameType.MEMORY */:
                            case 7 /* CustomNameType.GLOBAL */:
                            case 8 /* CustomNameType.ELEMENT */:
                            case 9 /* CustomNameType.DATASEG */:
                                {
                                    var count = this.bufferReader.readUleb128();
                                    for (var i = 0; i < count; ++i) {
                                        var index = this.bufferReader.readUleb128();
                                        var name_2 = this.bufferReader.readString();
                                        this.setCustomName(nametype, index, name_2);
                                    }
                                }
                                break;
                            default:
                                console.assert("Illegal name type: ".concat(nametype));
                                break;
                        }
                        this.bufferReader.setOffset(subsecOffset2 + payloadLen);
                    }
                    break;
                }
            }
        }
        this.bufferReader.setOffset(offsetSaved);
    };
    DisWasm.prototype.loadSections = function () {
        var SectionNames = [
            'CUSTOM',
            'TYPE',
            'IMPORT',
            'FUNC',
            'TABLE',
            'MEMORY',
            'GLOBAL',
            'EXPORT',
            'START',
            'ELEM',
            'CODE',
            'DATA',
            'DATA_COUNT',
            'TAG',
        ];
        while (!this.bufferReader.isEof()) {
            var offset = this.bufferReader.getOffset();
            var sec = this.bufferReader.readu8();
            var len = this.bufferReader.readUleb128();
            var sectionStartOffset = this.bufferReader.getOffset();
            this.log("\n;;=== 0x".concat(offset.toString(16), ": ").concat(SectionNames[sec] || "(section ".concat(sec, ")"), ", len=").concat(len));
            switch (sec) {
                case 0 /* Section.CUSTOM */:
                    this.readCustomSection(len);
                    break;
                case 1 /* Section.TYPE */:
                    this.readTypeSection();
                    break;
                case 2 /* Section.IMPORT */:
                    this.readImportSection();
                    break;
                case 3 /* Section.FUNC */:
                    this.readFuncSection();
                    break;
                case 4 /* Section.TABLE */:
                    this.readTableSection();
                    break;
                case 5 /* Section.MEMORY */:
                    this.readMemorySection();
                    break;
                case 6 /* Section.GLOBAL */:
                    this.readGlobalSection();
                    break;
                case 7 /* Section.EXPORT */:
                    this.readExportSection();
                    break;
                case 9 /* Section.ELEM */:
                    this.readElemSection();
                    break;
                case 10 /* Section.CODE */:
                    this.readCodeSection();
                    break;
                case 11 /* Section.DATA */:
                    this.readDataSection();
                    break;
                case 12 /* Section.DATA_COUNT */:
                    this.readDataCountSection();
                    break;
                case 13 /* Section.TAG */:
                    this.readTagSection();
                    break;
                default:
                    throw "Unhandled section: ".concat(sec, ", offset=0x").concat(offset.toString(16), ", len=").concat(len);
            }
            this.bufferReader.setOffset(sectionStartOffset + len);
        }
    };
    DisWasm.prototype.readCustomSection = function (len) {
        var _a, _b;
        var kSymInfoKindNames = ['function', 'data', 'global', 'section', 'event', 'table'];
        var kRelocTypeNames = (_a = {},
            _a[0 /* RelocType.R_WASM_FUNCTION_INDEX_LEB */] = 'FUNCTION_INDEX_LEB',
            _a[1 /* RelocType.R_WASM_TABLE_INDEX_SLEB */] = 'TABLE_INDEX_SLEB',
            _a[2 /* RelocType.R_WASM_TABLE_INDEX_I32 */] = 'TABLE_INDEX_I32',
            _a[3 /* RelocType.R_WASM_MEMORY_ADDR_LEB */] = 'MEMORY_ADDR_LEB',
            _a[4 /* RelocType.R_WASM_MEMORY_ADDR_SLEB */] = 'MEMORY_ADDR_SLEB',
            _a[5 /* RelocType.R_WASM_MEMORY_ADDR_I32 */] = 'MEMORY_ADDR_I32',
            _a[6 /* RelocType.R_WASM_TYPE_INDEX_LEB */] = 'TYPE_INDEX_LEB',
            _a[7 /* RelocType.R_WASM_GLOBAL_INDEX_LEB */] = 'GLOBAL_INDEX_LEB',
            _a[8 /* RelocType.R_WASM_FUNCTION_OFFSET_I32 */] = 'FUNCTION_OFFSET_I32',
            _a[9 /* RelocType.R_WASM_SECTION_OFFSET_I32 */] = 'SECTION_OFFSET_I32',
            _a[10 /* RelocType.R_WASM_TAG_INDEX_LEB */] = 'TAG_INDEX_LEB',
            _a[13 /* RelocType.R_WASM_GLOBAL_INDEX_I32 */] = 'GLOBAL_INDEX_I32',
            _a[14 /* RelocType.R_WASM_MEMORY_ADDR_LEB64 */] = 'MEMORY_ADDR_LEB64',
            _a[15 /* RelocType.R_WASM_MEMORY_ADDR_SLEB64 */] = 'MEMORY_ADDR_SLEB64',
            _a[16 /* RelocType.R_WASM_MEMORY_ADDR_I64 */] = 'MEMORY_ADDR_I64',
            _a[18 /* RelocType.R_WASM_TABLE_INDEX_SLEB64 */] = 'TABLE_INDEX_SLEB64',
            _a[19 /* RelocType.R_WASM_TABLE_INDEX_I64 */] = 'TABLE_INDEX_I64',
            _a[20 /* RelocType.R_WASM_TABLE_NUMBER_LEB */] = 'TABLE_NUMBER_LEB',
            _a);
        var kSymFlagNames = new Map([
            [1 /* SymFlags.WASM_SYM_BINDING_WEAK */, 'BINDING_WEAK'],
            [2 /* SymFlags.WASM_SYM_BINDING_LOCAL */, 'BINDING_LOCAL'],
            [4 /* SymFlags.WASM_SYM_VISIBILITY_HIDDEN */, 'VISIBILITY_HIDDEN'],
            [16 /* SymFlags.WASM_SYM_UNDEFINED */, 'UNDEFINED'],
            [32 /* SymFlags.WASM_SYM_EXPORTED */, 'EXPORTED'],
            [64 /* SymFlags.WASM_SYM_EXPLICIT_NAME */, 'EXPLICIT_NAME'],
            [128 /* SymFlags.WASM_SYM_NO_STRIP */, 'NO_STRIP'],
            [256 /* SymFlags.WASM_SYM_TLS */, 'TLS'],
            [512 /* SymFlags.WASM_SYM_ABSOLUTE */, 'ABSOLUTE'],
        ]);
        var kNameTypeNames = (_b = {},
            _b[0 /* CustomNameType.MODULE */] = 'module',
            _b[1 /* CustomNameType.FUNCTION */] = 'func',
            _b[2 /* CustomNameType.LOCAL */] = 'local',
            _b[3 /* CustomNameType.LABEL */] = 'label',
            _b[4 /* CustomNameType.TYPE */] = 'type',
            _b[5 /* CustomNameType.TABLE */] = 'table',
            _b[6 /* CustomNameType.MEMORY */] = 'memory',
            _b[7 /* CustomNameType.GLOBAL */] = 'global',
            _b[8 /* CustomNameType.ELEMENT */] = 'element',
            _b[9 /* CustomNameType.DATASEG */] = 'dataseg',
            _b);
        var kSegmentFlagNames = new Map([
            [0 /* SegmentFlags.WASM_SEG_FLAG_STRINGS */, 'STRINGS'],
            [1 /* SegmentFlags.WASM_SEG_FLAG_TLS */, 'TLS'],
            [2 /* SegmentFlags.WASM_SEG_FLAG_RETAIN */, 'RETAIN'],
        ]);
        var customSectionOffset = this.bufferReader.getOffset();
        var name = this.bufferReader.readString();
        var symbols = new Array();
        // Special handling for wasm object file.
        switch (name) {
            case Custom.LINKING:
                {
                    var version = this.bufferReader.readUleb128();
                    if (version !== LINKING_VERSION)
                        throw new Error("Unsupported linking version: ".concat(version));
                    this.log("".concat(this.addr(customSectionOffset), ";; (custom \"").concat(name, "\""));
                    while (this.bufferReader.getOffset() < customSectionOffset + len) {
                        var subsecOffset0 = this.bufferReader.getOffset();
                        var subsectype = this.bufferReader.readu8();
                        var payloadLen = this.bufferReader.readUleb128();
                        var subsecOffset = this.bufferReader.getOffset();
                        switch (subsectype) {
                            case 5 /* LinkingType.WASM_SEGMENT_INFO */:
                                {
                                    this.log("".concat(this.addr(subsecOffset0), ";;   (segment-info"));
                                    var count = this.bufferReader.readUleb128();
                                    for (var i = 0; i < count; ++i) {
                                        var offset = this.bufferReader.getOffset();
                                        var name_3 = this.bufferReader.readString();
                                        var p2align = this.bufferReader.readUleb128();
                                        var flags = this.bufferReader.readUleb128();
                                        var flagElems = [];
                                        for (var _i = 0, kSegmentFlagNames_1 = kSegmentFlagNames; _i < kSegmentFlagNames_1.length; _i++) {
                                            var _c = kSegmentFlagNames_1[_i], bit = _c[0], str = _c[1];
                                            if (flags & (1 << bit))
                                                flagElems.push(str);
                                        }
                                        this.log("".concat(this.addr(offset), ";;     (data-seg (name ").concat(name_3, ") (p2align ").concat(p2align, ") (flags ").concat(flagElems.join(','), "))"));
                                    }
                                    this.log(';;     )');
                                }
                                break;
                            case 6 /* LinkingType.WASM_INIT_FUNCS */:
                                {
                                    this.log("".concat(this.addr(subsecOffset0), ";;   (init-funcs"));
                                    var count = this.bufferReader.readUleb128();
                                    for (var i = 0; i < count; ++i) {
                                        var offset = this.bufferReader.getOffset();
                                        var priority = this.bufferReader.readUleb128();
                                        var symbolIndex = this.bufferReader.readUleb128();
                                        var name_4 = symbols[symbolIndex];
                                        this.log("".concat(this.addr(offset), ";;     (func (name \"").concat(name_4, "\") (priority ").concat(priority, "))"));
                                    }
                                    this.log(';;     )');
                                }
                                break;
                            case 8 /* LinkingType.WASM_SYMBOL_TABLE */:
                                {
                                    this.log("".concat(this.addr(subsecOffset0), ";;   (symtab"));
                                    var countTable = new Map([
                                        [0 /* SymInfoKind.SYMTAB_FUNCTION */, this.importFuncCount],
                                        [2 /* SymInfoKind.SYMTAB_GLOBAL */, this.importGlobalCount],
                                        [5 /* SymInfoKind.SYMTAB_TABLE */, this.importTableCount],
                                    ]);
                                    var count = this.bufferReader.readUleb128();
                                    var _loop_1 = function (i) {
                                        var offset = this_1.bufferReader.getOffset();
                                        var kind = this_1.bufferReader.readu8();
                                        var flags = this_1.bufferReader.readUleb128();
                                        var flagStr = Array.from(kSymFlagNames.keys())
                                            .filter(function (key) { return (flags & key) !== 0; })
                                            .map(function (key) { return kSymFlagNames.get(key); })
                                            .join(' ');
                                        switch (kind) {
                                            case 0 /* SymInfoKind.SYMTAB_FUNCTION */:
                                            case 2 /* SymInfoKind.SYMTAB_GLOBAL */:
                                            case 5 /* SymInfoKind.SYMTAB_TABLE */:
                                                {
                                                    var index = this_1.bufferReader.readUleb128();
                                                    var symname = '';
                                                    if (index < countTable.get(kind) && !(flags & 64 /* SymFlags.WASM_SYM_EXPLICIT_NAME */)) {
                                                        switch (kind) {
                                                            case 0 /* SymInfoKind.SYMTAB_FUNCTION */:
                                                                symname = this_1.funcs.get(index).join('.');
                                                                break;
                                                            case 2 /* SymInfoKind.SYMTAB_GLOBAL */:
                                                                symname = this_1.globals.get(index).join('.');
                                                                break;
                                                            case 5 /* SymInfoKind.SYMTAB_TABLE */:
                                                                symname = this_1.tables.get(index).join('.');
                                                                break;
                                                        }
                                                    }
                                                    else {
                                                        symname = this_1.bufferReader.readString();
                                                    }
                                                    this_1.log("".concat(this_1.addr(offset), ";;     (").concat(kSymInfoKindNames[kind], " (index ").concat(index, ") (name \"").concat(symname, "\") (flags ").concat(flagStr, "))"));
                                                    symbols.push(symname);
                                                }
                                                break;
                                            case 1 /* SymInfoKind.SYMTAB_DATA */:
                                                {
                                                    var symname = this_1.bufferReader.readString();
                                                    if (flags & 16 /* SymFlags.WASM_SYM_UNDEFINED */) {
                                                        this_1.log("".concat(this_1.addr(offset), ";;     (").concat(kSymInfoKindNames[kind], " (name \"").concat(symname, "\") (flags ").concat(flagStr, "))"));
                                                    }
                                                    else {
                                                        var index = this_1.bufferReader.readUleb128();
                                                        var suboffset = this_1.bufferReader.readUleb128();
                                                        var size = this_1.bufferReader.readUleb128();
                                                        this_1.log("".concat(this_1.addr(offset), ";;     (").concat(kSymInfoKindNames[kind], " (name \"").concat(symname, "\") (index ").concat(index, ") (offset ").concat(suboffset, ") (size ").concat(size, ") (flags ").concat(flagStr, "))"));
                                                    }
                                                    symbols.push(symname);
                                                }
                                                break;
                                            case 4 /* SymInfoKind.SYMTAB_EVENT */:
                                                {
                                                    var index = this_1.bufferReader.readUleb128();
                                                    var symname = this_1.bufferReader.readString();
                                                    this_1.log("".concat(this_1.addr(offset), ";;     (").concat(kSymInfoKindNames[kind], " (name \"").concat(symname, "\") (index ").concat(index, ") (flags ").concat(flagStr, "))"));
                                                    symbols.push(symname);
                                                }
                                                break;
                                            default:
                                                throw "".concat(kind, " is not supported");
                                        }
                                    };
                                    var this_1 = this;
                                    for (var i = 0; i < count; ++i) {
                                        _loop_1(i);
                                    }
                                    this.log(';;     )');
                                }
                                break;
                            default:
                                console.log("Unhandled subsectype: ".concat(subsectype, " at 0x").concat(subsecOffset.toString(16)));
                                break;
                        }
                        this.bufferReader.setOffset(subsecOffset + payloadLen);
                    }
                    this.log(';;   )');
                }
                break;
            case Custom.RELOC_CODE:
            case Custom.RELOC_DATA:
                {
                    var sectionIndex = this.bufferReader.readUleb128();
                    var count = this.bufferReader.readUleb128();
                    this.log("".concat(this.addr(customSectionOffset), ";; (custom \"").concat(name, "\" (section-index ").concat(sectionIndex, ")"));
                    for (var i = 0; i < count; ++i) {
                        var ofs = this.bufferReader.getOffset();
                        var type = this.bufferReader.readu8();
                        var offset = this.bufferReader.readUleb128();
                        var index = this.bufferReader.readUleb128();
                        var addend = 0;
                        switch (type) {
                            case 3 /* RelocType.R_WASM_MEMORY_ADDR_LEB */:
                            case 4 /* RelocType.R_WASM_MEMORY_ADDR_SLEB */:
                            case 5 /* RelocType.R_WASM_MEMORY_ADDR_I32 */:
                            case 14 /* RelocType.R_WASM_MEMORY_ADDR_LEB64 */:
                            case 15 /* RelocType.R_WASM_MEMORY_ADDR_SLEB64 */:
                            case 16 /* RelocType.R_WASM_MEMORY_ADDR_I64 */:
                            case 8 /* RelocType.R_WASM_FUNCTION_OFFSET_I32 */:
                            case 9 /* RelocType.R_WASM_SECTION_OFFSET_I32 */:
                                addend = this.bufferReader.readUleb128();
                                break;
                            default: break;
                        }
                        this.log("".concat(this.addr(ofs), ";;   (").concat(kRelocTypeNames[type], " (offset ").concat(offset, ") (index ").concat(index, ") (addend ").concat(addend, "))"));
                    }
                    this.log(';;   )');
                }
                break;
            case Custom.NAME:
                {
                    this.log("".concat(this.addr(customSectionOffset), ";; (custom \"").concat(name, "\""));
                    while (this.bufferReader.getOffset() < customSectionOffset + len) {
                        var subsecOffset1 = this.bufferReader.getOffset();
                        var nametype = this.bufferReader.readu8();
                        var payloadLen = this.bufferReader.readUleb128();
                        var subsecOffset2 = this.bufferReader.getOffset();
                        switch (nametype) {
                            case 0 /* CustomNameType.MODULE */:
                            case 1 /* CustomNameType.FUNCTION */:
                            case 2 /* CustomNameType.LOCAL */:
                            case 3 /* CustomNameType.LABEL */:
                            case 4 /* CustomNameType.TYPE */:
                            case 5 /* CustomNameType.TABLE */:
                            case 6 /* CustomNameType.MEMORY */:
                            case 7 /* CustomNameType.GLOBAL */:
                            case 8 /* CustomNameType.ELEMENT */:
                            case 9 /* CustomNameType.DATASEG */:
                                {
                                    var count = this.bufferReader.readUleb128();
                                    for (var i = 0; i < count; ++i) {
                                        var offset = this.bufferReader.getOffset();
                                        var index = this.bufferReader.readUleb128();
                                        var name_5 = this.bufferReader.readString();
                                        var tname = kNameTypeNames[nametype];
                                        this.log("".concat(this.addr(offset), ";;   (").concat(tname, ":").concat(index, " \"").concat(name_5, "\")"));
                                        this.names.set(nametype + index * 100, name_5);
                                    }
                                }
                                break;
                            default:
                                this.log("".concat(this.addr(subsecOffset1), ";;   (nametype=").concat(nametype, ")"));
                                break;
                        }
                        this.bufferReader.setOffset(subsecOffset2 + payloadLen);
                    }
                    this.log(';;   )');
                }
                break;
            default:
                this.log("".concat(this.addr(customSectionOffset), ";; (custom \"").concat(name, "\")"));
                break;
        }
    };
    DisWasm.prototype.readTypeSection = function () {
        var num = this.bufferReader.readUleb128();
        for (var i = 0; i < num; ++i) {
            var offset = this.bufferReader.getOffset();
            var type = readType(this.bufferReader);
            this.types.push(type);
            this.log("".concat(this.addr(offset), "(type ").concat(type.toString(), ")  ;; ").concat(i));
        }
    };
    DisWasm.prototype.readImportSection = function () {
        var num = this.bufferReader.readUleb128();
        for (var i = 0; i < num; ++i) {
            var offset = this.bufferReader.getOffset();
            var modName = this.bufferReader.readString();
            var name_6 = this.bufferReader.readString();
            var kind = this.bufferReader.readu8();
            switch (kind) {
                case 0 /* ImportKind.FUNC */:
                    {
                        var typeIndex = this.bufferReader.readUleb128();
                        var index = this.funcs.size;
                        this.log("".concat(this.addr(offset), "(import \"").concat(modName, "\" \"").concat(name_6, "\" (func $").concat(name_6, " (type ").concat(typeIndex, ")))  ;; ").concat(index));
                        this.funcs.set(index, [modName, name_6]);
                    }
                    break;
                case 1 /* ImportKind.TABLE */:
                    {
                        var tt = readType(this.bufferReader);
                        var index = this.tables.size;
                        this.log("".concat(this.addr(offset), "(import \"").concat(modName, "\" \"").concat(name_6, "\" (table ").concat(tt, "))"));
                        this.tables.set(index, [modName, name_6]);
                    }
                    break;
                case 2 /* ImportKind.MEMORY */:
                    {
                        // TODO: Confirm
                        var index = this.bufferReader.readUleb128();
                        var size = this.bufferReader.readUleb128();
                        this.log("".concat(this.addr(offset), "(import \"").concat(modName, "\" \"").concat(name_6, "\" (memory ").concat(size, " (;index=").concat(index, ";)))"));
                    }
                    break;
                case 3 /* ImportKind.GLOBAL */:
                    {
                        // TODO: Confirm
                        var type = readType(this.bufferReader);
                        var mutable = this.bufferReader.readu8();
                        var index = this.globals.size;
                        var typename = mutable !== 0 ? "(mut ".concat(type, ")") : "".concat(type);
                        this.log("".concat(this.addr(offset), "(import \"").concat(modName, "\" \"").concat(name_6, "\" (global ").concat(typename, "))  ;; ").concat(index));
                        this.globals.set(index, [modName, name_6]);
                    }
                    break;
                default:
                    throw ("Illegal import kind: ".concat(kind));
            }
        }
        this.importFuncCount = this.funcs.size;
        this.importGlobalCount = this.globals.size;
        this.importTableCount = this.tables.size;
    };
    DisWasm.prototype.readFuncSection = function () {
        var num = this.bufferReader.readUleb128();
        this.log(";; func: #".concat(num));
        for (var i = 0; i < num; ++i) {
            var typeIndex = this.bufferReader.readUleb128();
            this.functions.push(typeIndex);
            this.log(";;   func ".concat(i + this.importFuncCount, ": type=#").concat(typeIndex));
        }
    };
    DisWasm.prototype.readTableSection = function () {
        var num = this.bufferReader.readUleb128();
        for (var i = 0; i < num; ++i) {
            var tt = this.bufferReader.readUleb128();
            var limits = this.bufferReader.readUleb128();
            var initial = this.bufferReader.readUleb128();
            if ((limits & 1) === 0) {
                this.log("(table ".concat(initial, " ").concat(tt == 112 /* WasmType.FUNCREF */ ? 'funcref' : '?', ")  ;; ").concat(i));
            }
            else {
                var max = this.bufferReader.readUleb128();
                this.log("(table ".concat(initial, " ").concat(max, " ").concat(tt == 112 /* WasmType.FUNCREF */ ? 'funcref' : '?', ")  ;; ").concat(i));
            }
        }
    };
    DisWasm.prototype.readMemorySection = function () {
        var num = this.bufferReader.readUleb128();
        for (var i = 0; i < num; ++i) {
            var offset = this.bufferReader.getOffset();
            var limits = this.bufferReader.readUleb128();
            var pageCount = this.bufferReader.readUleb128();
            if ((limits & 1) === 0) {
                this.log("".concat(this.addr(offset), "(memory ").concat(pageCount, ")"));
            }
            else {
                var maxPageCount = this.bufferReader.readUleb128();
                this.log("".concat(this.addr(offset), "(memory ").concat(pageCount, " ").concat(maxPageCount, ")"));
            }
        }
    };
    DisWasm.prototype.getCustomName = function (t, index) {
        var nameIndex = t + index * 100;
        var name = this.names.get(nameIndex);
        return name == null ? name : "$".concat(name); // '$' is prepended.
    };
    DisWasm.prototype.setCustomName = function (t, index, name) {
        var nameIndex = t + index * 100;
        this.names.set(nameIndex, name);
    };
    DisWasm.prototype.readGlobalSection = function () {
        var _a;
        var num = this.bufferReader.readUleb128();
        for (var i = 0; i < num; ++i) {
            var offset = this.bufferReader.getOffset();
            var type = readType(this.bufferReader);
            var mut = this.bufferReader.readu8();
            var value = readGlobalValue(this.bufferReader);
            var name_7 = (_a = this.getCustomName(7 /* CustomNameType.GLOBAL */, i)) !== null && _a !== void 0 ? _a : "(;".concat(i, ";)");
            this.log("".concat(this.addr(offset), "(global ").concat(name_7, " ").concat(mut !== 0 ? "(mut ".concat(type, ")") : "".concat(type), " (").concat(type, ".const ").concat(value, "))"));
            this.bufferReader.readu8(); // Skip OP_END
        }
    };
    DisWasm.prototype.readExportSection = function () {
        var KindNames = ['func', 'table', 'memory', 'global', 'tag'];
        var FUNC = 0;
        var num = this.bufferReader.readUleb128();
        for (var i = 0; i < num; ++i) {
            var offset = this.bufferReader.getOffset();
            var name_8 = this.bufferReader.readString();
            var kind = this.bufferReader.readu8();
            var index = this.bufferReader.readUleb128();
            this.log("".concat(this.addr(offset), "(export \"").concat(name_8, "\" (").concat(KindNames[kind] || "kind=".concat(kind), " ").concat(index, "))"));
            if (kind === FUNC) {
                this.funcs.set(index, ['', name_8]);
            }
        }
    };
    DisWasm.prototype.readElemSection = function () {
        var _this = this;
        var segnum = this.bufferReader.readUleb128();
        for (var i = 0; i < segnum; ++i) {
            /*const flag =*/ this.bufferReader.readUleb128();
            var start = 0;
            if (this.bufferReader.readu8() !== 65 /* Opcode.I32_CONST */ ||
                (start = this.bufferReader.readUleb128(),
                    this.bufferReader.readu8() !== 11 /* Opcode.END */))
                throw 'Unsupported elem section';
            var count = this.bufferReader.readUleb128();
            var elements = __spreadArray([], Array(count), true).map(function (_) {
                var _a;
                var index = _this.bufferReader.readUleb128();
                return (_a = _this.getCustomName(1 /* CustomNameType.FUNCTION */, index)) !== null && _a !== void 0 ? _a : "".concat(index);
            });
            this.log("(elem (i32.const ".concat(start, ") func ").concat(elements.join(' '), ")  ;; ").concat(i));
        }
    };
    DisWasm.prototype.readCodeSection = function () {
        var num = this.bufferReader.readUleb128();
        for (var i = 0; i < num; ++i) {
            var offset = this.bufferReader.getOffset();
            var typeIndex = this.functions[i];
            var funcNo = i + this.importFuncCount;
            var funcComment = "(;".concat(funcNo, ";)");
            var customName = this.getCustomName(1 /* CustomNameType.FUNCTION */, funcNo);
            if (customName != null) {
                funcComment = "".concat(customName, " ").concat(funcComment);
            }
            else if (this.funcs.has(funcNo)) {
                var _a = this.funcs.get(funcNo), _mod = _a[0], name_9 = _a[1];
                funcComment = "$".concat(name_9, " ").concat(funcComment);
            }
            this.log("".concat(this.addr(offset), "(func ").concat(funcComment, " (type ").concat(typeIndex, ")"));
            var code = this.readCode();
            this.codes.push(code);
        }
    };
    DisWasm.prototype.readCode = function () {
        var _this = this;
        var toStringOperand = function (x) {
            if (typeof x !== 'bigint') {
                if (x === Number.POSITIVE_INFINITY)
                    return 'inf';
                if (x === Number.NEGATIVE_INFINITY)
                    return '-inf';
                if (isNaN(x))
                    return 'nan';
            }
            return x.toString();
        };
        var bodySize = this.bufferReader.readUleb128();
        var endOfs = this.bufferReader.getOffset() + bodySize;
        var localDeclCount = this.bufferReader.readUleb128();
        if (localDeclCount > 0) {
            var offset = this.bufferReader.getOffset();
            var types = __spreadArray([], Array(localDeclCount), true).map(function (_) {
                var num = _this.bufferReader.readUleb128();
                var t = readType(_this.bufferReader);
                return __spreadArray([], Array(num), true).map(function (_) { return t; });
            }).flat().join(' ');
            this.log("".concat(this.addr(offset), "  (local ").concat(types, ")"));
        }
        var code = new Array();
        var indent = 1;
        while (this.bufferReader.getOffset() < endOfs) {
            var offset = this.bufferReader.getOffset();
            var inst = readInst(this.bufferReader);
            code.push(inst);
            switch (inst.opcode) {
                case 5 /* Opcode.ELSE */:
                case 11 /* Opcode.END */:
                case 7 /* Opcode.CATCH */:
                    --indent;
                    if (indent === 0 && inst.opcode === 11 /* Opcode.END */) {
                        this.log("".concat(this.addr(offset), ")"));
                        continue;
                    }
                    break;
            }
            var spaces = makeIndent(indent);
            var operands = '';
            if (inst.operands != null) {
                switch (inst.opKind) {
                    case 1 /* OpKind.BLOCK */:
                        {
                            var t = inst.operands[0];
                            if (t.getType() !== 64 /* WasmType.VOID */)
                                operands = "(result ".concat(t.toString(), ")");
                        }
                        break;
                    case 3 /* OpKind.LOAD */:
                    case 4 /* OpKind.STORE */:
                        {
                            var align = inst.operands[0];
                            var offset_1 = inst.operands[1];
                            var attrs = [];
                            if (offset_1 !== 0)
                                attrs.push("offset=".concat(offset_1));
                            if (!((inst.opstr.match(/(load8|store8)/) && align === 0) ||
                                (inst.opstr.match(/(load16|store16)/) && align === 1) ||
                                (inst.opstr.match(/(^i32|^f32|load32|store32)/) && align === 2) ||
                                (inst.opstr.match(/(^i64|^f64)/) && align === 3)))
                                attrs.push("align=".concat(1 << align));
                            if (attrs.length > 0)
                                operands = attrs.join(' ');
                        }
                        break;
                    case 5 /* OpKind.BR_TABLE */:
                        operands = "".concat(inst.operands[0].join(' '), " ").concat(inst.operands[1]);
                        break;
                    case 6 /* OpKind.GLOBAL */:
                        {
                            var no = inst.operands[0];
                            var customName = this.getCustomName(7 /* CustomNameType.GLOBAL */, no);
                            if (customName != null) {
                                operands = customName;
                            }
                            else if (this.globals.has(no)) {
                                var _a = this.globals.get(no), _mod = _a[0], name_10 = _a[1];
                                operands = "$".concat(name_10);
                            }
                            else {
                                operands = "".concat(no);
                            }
                        }
                        break;
                    case 7 /* OpKind.CALL */:
                        {
                            var funcNo = inst.operands[0];
                            var customName = this.getCustomName(1 /* CustomNameType.FUNCTION */, funcNo);
                            if (customName != null) {
                                operands = customName;
                            }
                            else if (this.funcs.has(funcNo)) {
                                var _b = this.funcs.get(funcNo), _mod = _b[0], name_11 = _b[1];
                                operands = "$".concat(name_11);
                            }
                            else {
                                operands = "".concat(funcNo);
                            }
                        }
                        break;
                    case 8 /* OpKind.CALL_INDIRECT */:
                        {
                            var tableIndex = inst.operands[1];
                            var tableIndexStr = tableIndex === 0 ? '' : " (tableIndex ".concat(tableIndex, ")");
                            operands = "(type ".concat(inst.operands[0], ")").concat(tableIndexStr);
                        }
                        break;
                    case 9 /* OpKind.OMIT_OPERANDS */:
                        // Omit operands.
                        break;
                    default:
                        operands = inst.operands.map(toStringOperand).join(' ');
                        break;
                }
            }
            this.log("".concat(this.addr(offset)).concat(spaces).concat(inst.opstr, " ").concat(operands).trimEnd());
            switch (inst.opKind) {
                case 1 /* OpKind.BLOCK */:
                case 2 /* OpKind.ELSE */:
                    ++indent;
                    break;
            }
        }
        return code;
    };
    DisWasm.prototype.readDataSection = function () {
        var _a;
        var escapeChar = function (c) {
            switch (c) {
                case 34: return '\\"';
                case 92: return '\\\\';
                default:
                    if (c < 0x20 || c > 0x7e)
                        return "\\".concat(c.toString(16).padStart(2, '0'));
                    return String.fromCharCode(c);
            }
        };
        var num = this.bufferReader.readUleb128();
        for (var i = 0; i < num; ++i) {
            var offset = this.bufferReader.getOffset();
            /*const flag =*/ this.bufferReader.readUleb128();
            var start = 0;
            if (this.bufferReader.readu8() !== 65 /* Opcode.I32_CONST */ ||
                (start = this.bufferReader.readUleb128(),
                    this.bufferReader.readu8() !== 11 /* Opcode.END */))
                throw 'Unsupported data section';
            var datasize = this.bufferReader.readUleb128();
            var data = new Array(datasize);
            for (var j = 0; j < datasize; ++j) {
                var c = this.bufferReader.readu8();
                data[j] = escapeChar(c);
            }
            var name_12 = (_a = this.getCustomName(9 /* CustomNameType.DATASEG */, i)) !== null && _a !== void 0 ? _a : "(;".concat(i, ";)");
            this.log("".concat(this.addr(offset), "(data ").concat(name_12, " (i32.const ").concat(start, ") \"").concat(data.join(''), "\")"));
        }
    };
    DisWasm.prototype.readDataCountSection = function () {
        var offset = this.bufferReader.getOffset();
        var count = this.bufferReader.readUleb128();
        this.log(";;".concat(this.addr(offset), "(data-count ").concat(count, ")"));
    };
    DisWasm.prototype.readTagSection = function () {
        var num = this.bufferReader.readUleb128();
        for (var i = 0; i < num; ++i) {
            var offset = this.bufferReader.getOffset();
            var attribute = this.bufferReader.readUleb128();
            var typeIndex = this.bufferReader.readUleb128();
            this.log(";;".concat(this.addr(offset), "(tag ").concat(typeIndex, " ").concat(attribute, ")"));
        }
    };
    DisWasm.prototype.addr = function (adr) {
        return this.opts['dumpAddr'] ? "(;".concat(adr.toString(16).padStart(5, '0'), ";) ") : '';
    };
    return DisWasm;
}());
exports.DisWasm = DisWasm;
