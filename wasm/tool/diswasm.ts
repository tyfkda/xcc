#! /usr/bin/env ts-node

'use strict'

import * as fs from 'fs'

const enum Section {
  TYPE      = 1,
  IMPORT    = 2,
  FUNC      = 3,
  TABLE     = 4,
  MEMORY    = 5,
  GLOBAL    = 6,
  EXPORT    = 7,
  ELEM      = 9,
  CODE      = 10,
  DATA      = 11,
}

// Wasm opcode
const enum Opcode {
  UNREACHABLE   = 0x00,
  NOP           = 0x01,
  BLOCK         = 0x02,
  LOOP          = 0x03,
  IF            = 0x04,
  ELSE          = 0x05,
  END           = 0x0b,
  BR            = 0x0c,
  BR_IF         = 0x0d,
  BR_TABLE      = 0x0e,
  RETURN        = 0x0f,
  CALL          = 0x10,
  CALL_INDIRECT = 0x11,
  DROP          = 0x1a,
  SELECT        = 0x1b,
  LOCAL_GET     = 0x20,
  LOCAL_SET     = 0x21,
  LOCAL_TEE     = 0x22,
  GLOBAL_GET    = 0x23,
  GLOBAL_SET    = 0x24,
  I32_LOAD      = 0x28,
  I64_LOAD      = 0x29,
  F32_LOAD      = 0x2a,
  F64_LOAD      = 0x2b,
  I32_LOAD8_S   = 0x2c,
  I32_LOAD8_U   = 0x2d,
  I32_LOAD16_S  = 0x2e,
  I32_LOAD16_U  = 0x2f,
  I32_STORE     = 0x36,
  I64_STORE     = 0x37,
  F32_STORE     = 0x38,
  F64_STORE     = 0x39,
  I32_STORE8    = 0x3a,
  I32_STORE16   = 0x3b,
  I64_STORE8    = 0x3c,
  I64_STORE16   = 0x3d,
  I64_STORE32   = 0x3e,
  MEMORY_SIZE   = 0x3f,
  MEMORY_GROW   = 0x40,
  I32_CONST     = 0x41,
  I64_CONST     = 0x42,
  F32_CONST     = 0x43,
  F64_CONST     = 0x44,
  I32_EQZ       = 0x45,
  I32_EQ        = 0x46,
  I32_NE        = 0x47,
  I32_LT_S      = 0x48,
  I32_LT_U      = 0x49,
  I32_GT_S      = 0x4a,
  I32_GT_U      = 0x4b,
  I32_LE_S      = 0x4c,
  I32_LE_U      = 0x4d,
  I32_GE_S      = 0x4e,
  I32_GE_U      = 0x4f,
  I64_EQZ       = 0x50,
  I64_EQ        = 0x51,
  I64_NE        = 0x52,
  I64_LT_S      = 0x53,
  I64_LT_U      = 0x54,
  I64_GT_S      = 0x55,
  I64_GT_U      = 0x56,
  I64_LE_S      = 0x57,
  I64_LE_U      = 0x58,
  I64_GE_S      = 0x59,
  I64_GE_U      = 0x5a,
  F32_EQ        = 0x5b,
  F32_NE        = 0x5c,
  F32_LT        = 0x5d,
  F32_GT        = 0x5e,
  F32_LE        = 0x5f,
  F32_GE        = 0x60,
  F64_EQ        = 0x61,
  F64_NE        = 0x62,
  F64_LT        = 0x63,
  F64_GT        = 0x64,
  F64_LE        = 0x65,
  F64_GE        = 0x66,
  I32_ADD       = 0x6a,
  I32_SUB       = 0x6b,
  I32_MUL       = 0x6c,
  I32_DIV_S     = 0x6d,
  I32_DIV_U     = 0x6e,
  I32_REM_S     = 0x6f,
  I32_REM_U     = 0x70,
  I32_AND       = 0x71,
  I32_OR        = 0x72,
  I32_XOR       = 0x73,
  I32_SHL       = 0x74,
  I32_SHR_S     = 0x75,
  I32_SHR_U     = 0x76,
  I32_ROTL      = 0x77,
  I32_ROTR      = 0x78,
  I64_ADD       = 0x7c,
  I64_SUB       = 0x7d,
  I64_MUL       = 0x7e,
  I64_DIV_S     = 0x7f,
  I64_DIV_U     = 0x80,
  I64_REM_S     = 0x81,
  I64_REM_U     = 0x82,
  I64_AND       = 0x83,
  I64_OR        = 0x84,
  I64_XOR       = 0x85,
  I64_SHL       = 0x86,
  I64_SHR_S     = 0x87,
  I64_SHR_U     = 0x88,
  I64_ROTL      = 0x89,
  I64_ROTR      = 0x8a,
  F32_ABS       = 0x8b,
  F32_NEG       = 0x8c,
  F32_ADD       = 0x92,
  F32_SUB       = 0x93,
  F32_MUL       = 0x94,
  F32_DIV       = 0x95,
  F64_ABS       = 0x99,
  F64_NEG       = 0x9a,
  F64_ADD       = 0xa0,
  F64_SUB       = 0xa1,
  F64_MUL       = 0xa2,
  F64_DIV       = 0xa3,
  I32_WRAP_I64        = 0xa7,  // i32 <- i64
//   I32_TRUNC_F32_S     = 0xa8,  // i32 <- f32
  I32_TRUNC_F64_S     = 0xaa,  // i32 <- f64
  I64_EXTEND_I32_S    = 0xac,  // i64 <- i32
//   I64_TRUNC_F32_S     = 0xae,  // i64 <- f32
//   I64_TRUNC_F64_S     = 0xb0,  // i64 <- f64
//   F32_CONVERT_I32_S   = 0xb2,  // f32 <- i32
  F32_DEMOTE_F64      = 0xb6,  // f32 <- f64
//   F32_CONVERT_I64_S   = 0xb4,  // f32 <- i64
  F64_CONVERT_I32_S   = 0xb7,  // f64 <- i32
//   F64_CONVERT_I64_S   = 0xb9,  // f64 <- i64
  F64_PROMOTE_F32     = 0xbb,  // f64 <- f32
  I32_REINTERPRET_F32 = 0xbc,  // i32 <- f32
  I64_REINTERPRET_F64 = 0xbd,  // i64 <- f64
}

const enum WasmType {
  VOID       = 0x40,
  FUNC       = 0x60,
  F64        = 0x7c,
  F32        = 0x7d,
  I64        = 0x7e,
  I32        = 0x7f,
}

const InstTable = new Map([
  [Opcode.UNREACHABLE, {op: 'unreachable'}],
  [Opcode.NOP, {op: 'nop'}],
  [Opcode.BLOCK, {op: 'block', operands: ['type']}],
  [Opcode.LOOP, {op: 'loop', operands: ['type']}],
  [Opcode.IF, {op: 'if', operands: ['type']}],
  [Opcode.ELSE, {op: 'else'}],
  [Opcode.END, {op: 'end'}],
  [Opcode.BR, {op: 'br', operands: ['uleb128']}],
  [Opcode.BR_IF, {op: 'br_if', operands: ['uleb128']}],
  [Opcode.BR_TABLE, {op: 'br_table', operands: ['uleb128array', 'uleb128']}],
  [Opcode.RETURN, {op: 'return'}],
  [Opcode.CALL, {op: 'call', operands: ['uleb128']}],
  [Opcode.CALL_INDIRECT, {op: 'call_indirect', operands: ['uleb128', 'uleb128']}],
  [Opcode.DROP, {op: 'drop'}],
  [Opcode.SELECT, {op: 'select'}],
  [Opcode.LOCAL_GET, {op: 'local.get', operands: ['uleb128']}],
  [Opcode.LOCAL_SET, {op: 'local.set', operands: ['uleb128']}],
  [Opcode.LOCAL_TEE, {op: 'local.tee', operands: ['uleb128']}],
  [Opcode.GLOBAL_GET, {op: 'global.get', operands: ['uleb128']}],
  [Opcode.GLOBAL_SET, {op: 'global.set', operands: ['uleb128']}],
  [Opcode.I32_LOAD, {op: 'i32.load', operands: ['uleb128', 'uleb128']}],
  [Opcode.I64_LOAD, {op: 'i64.load', operands: ['uleb128', 'uleb128']}],
  [Opcode.F32_LOAD, {op: 'f32.load', operands: ['uleb128', 'uleb128']}],
  [Opcode.F64_LOAD, {op: 'f64.load', operands: ['uleb128', 'uleb128']}],
  [Opcode.I32_STORE, {op: 'i32.store', operands: ['uleb128', 'uleb128']}],
  [Opcode.I64_STORE, {op: 'i64.store', operands: ['uleb128', 'uleb128']}],
  [Opcode.F32_STORE, {op: 'f32.store', operands: ['uleb128', 'uleb128']}],
  [Opcode.F64_STORE, {op: 'f64.store', operands: ['uleb128', 'uleb128']}],
  [Opcode.I32_LOAD8_S, {op: 'i32.load8_s', operands: ['uleb128', 'uleb128']}],
  [Opcode.I32_LOAD8_U, {op: 'i32.load8_u', operands: ['uleb128', 'uleb128']}],
  [Opcode.I32_LOAD16_S, {op: 'i32.load16_s', operands: ['uleb128', 'uleb128']}],
  [Opcode.I32_LOAD16_U, {op: 'i32.load16_u', operands: ['uleb128', 'uleb128']}],
  [Opcode.I32_STORE8, {op: 'i32.store8', operands: ['uleb128', 'uleb128']}],
  [Opcode.I32_STORE16, {op: 'i32.store16', operands: ['uleb128', 'uleb128']}],
  [Opcode.I64_STORE8, {op: 'i64.store8', operands: ['uleb128', 'uleb128']}],
  [Opcode.I64_STORE16, {op: 'i64.store16', operands: ['uleb128', 'uleb128']}],
  [Opcode.I64_STORE32, {op: 'i64.store32', operands: ['uleb128', 'uleb128']}],
  [Opcode.MEMORY_SIZE, {op: 'memory.size', operands: ['uleb128']}],
  [Opcode.MEMORY_GROW, {op: 'memory.grow', operands: ['uleb128']}],
  [Opcode.I32_CONST, {op: 'i32.const', operands: ['leb128']}],
  [Opcode.I64_CONST, {op: 'i64.const', operands: ['leb128']}],
  [Opcode.F32_CONST, {op: 'f32.const', operands: ['f32']}],
  [Opcode.F64_CONST, {op: 'f64.const', operands: ['f64']}],
  [Opcode.I32_EQZ, {op: 'i32.eqz'}],
  [Opcode.I32_EQ, {op: 'i32.eq'}],
  [Opcode.I32_NE, {op: 'i32.ne'}],
  [Opcode.I32_LT_S, {op: 'i32.lt_s'}],
  [Opcode.I32_LT_U, {op: 'i32.lt_u'}],
  [Opcode.I32_GT_S, {op: 'i32.gt_s'}],
  [Opcode.I32_GT_U, {op: 'i32.gt_u'}],
  [Opcode.I32_LE_S, {op: 'i32.le_s'}],
  [Opcode.I32_LE_U, {op: 'i32.le_u'}],
  [Opcode.I32_GE_S, {op: 'i32.ge_s'}],
  [Opcode.I32_GE_U, {op: 'i32.ge_u'}],
  [Opcode.I64_EQZ, {op: 'i64.eqz'}],
  [Opcode.I64_EQ, {op: 'i64.eq'}],
  [Opcode.I64_NE, {op: 'i64.ne'}],
  [Opcode.I64_LT_S, {op: 'i64.lt_s'}],
  [Opcode.I64_LT_U, {op: 'i64.lt_u'}],
  [Opcode.I64_GT_S, {op: 'i64.gt_s'}],
  [Opcode.I64_GT_U, {op: 'i64.gt_u'}],
  [Opcode.I64_LE_S, {op: 'i64.le_s'}],
  [Opcode.I64_LE_U, {op: 'i64.le_u'}],
  [Opcode.I64_GE_S, {op: 'i64.ge_s'}],
  [Opcode.I64_GE_U, {op: 'i64.ge_u'}],
  [Opcode.F32_EQ, {op: 'f32.eq'}],
  [Opcode.F32_NE, {op: 'f32.ne'}],
  [Opcode.F32_LT, {op: 'f32.lt'}],
  [Opcode.F32_GT, {op: 'f32.gt'}],
  [Opcode.F32_LE, {op: 'f32.le'}],
  [Opcode.F32_GE, {op: 'f32.ge'}],
  [Opcode.F64_EQ, {op: 'f64.eq'}],
  [Opcode.F64_NE, {op: 'f64.ne'}],
  [Opcode.F64_LT, {op: 'f64.lt'}],
  [Opcode.F64_GT, {op: 'f64.gt'}],
  [Opcode.F64_LE, {op: 'f64.le'}],
  [Opcode.F64_GE, {op: 'f64.ge'}],
  [Opcode.I32_ADD, {op: 'i32.add'}],
  [Opcode.I32_SUB, {op: 'i32.sub'}],
  [Opcode.I32_MUL, {op: 'i32.mul'}],
  [Opcode.I32_DIV_S, {op: 'i32.div_s'}],
  [Opcode.I32_DIV_U, {op: 'i32.div_u'}],
  [Opcode.I32_REM_S, {op: 'i32.rem_s'}],
  [Opcode.I32_REM_U, {op: 'i32.rem_u'}],
  [Opcode.I32_AND, {op: 'i32.and'}],
  [Opcode.I32_OR, {op: 'i32.or'}],
  [Opcode.I32_XOR, {op: 'i32.xor'}],
  [Opcode.I32_SHL, {op: 'i32.shl'}],
  [Opcode.I32_SHR_S, {op: 'i32.shr_s'}],
  [Opcode.I32_SHR_U, {op: 'i32.shr_u'}],
  [Opcode.I32_ROTL, {op: 'i32.rotl'}],
  [Opcode.I32_ROTR, {op: 'i32.rotr'}],
  [Opcode.I64_ADD, {op: 'i64.add'}],
  [Opcode.I64_SUB, {op: 'i64.sub'}],
  [Opcode.I64_MUL, {op: 'i64.mul'}],
  [Opcode.I64_DIV_S, {op: 'i64.div_s'}],
  [Opcode.I64_DIV_U, {op: 'i64.div_u'}],
  [Opcode.I64_REM_S, {op: 'i64.rem_s'}],
  [Opcode.I64_REM_U, {op: 'i64.rem_u'}],
  [Opcode.I64_AND, {op: 'i64.and'}],
  [Opcode.I64_OR, {op: 'i64.or'}],
  [Opcode.I64_XOR, {op: 'i64.xor'}],
  [Opcode.I64_SHL, {op: 'i64.shl'}],
  [Opcode.I64_SHR_S, {op: 'i64.shr_s'}],
  [Opcode.I64_SHR_U, {op: 'i64.shr_u'}],
  [Opcode.I64_ROTL, {op: 'i64.rotl'}],
  [Opcode.I64_ROTR, {op: 'i64.rotr'}],
  [Opcode.F32_ABS, {op: 'f32.abs'}],
  [Opcode.F32_NEG, {op: 'f32.neg'}],
  [Opcode.F32_ADD, {op: 'f32.add'}],
  [Opcode.F32_SUB, {op: 'f32.sub'}],
  [Opcode.F32_MUL, {op: 'f32.mul'}],
  [Opcode.F32_DIV, {op: 'f32.div'}],
  [Opcode.F64_ABS, {op: 'f64.abs'}],
  [Opcode.F64_NEG, {op: 'f64.neg'}],
  [Opcode.F64_ADD, {op: 'f64.add'}],
  [Opcode.F64_SUB, {op: 'f64.sub'}],
  [Opcode.F64_MUL, {op: 'f64.mul'}],
  [Opcode.F64_DIV, {op: 'f64.div'}],

  [Opcode.I32_WRAP_I64, {op: 'i32.wrap_i64'}],
  [Opcode.I32_TRUNC_F64_S, {op: 'i32.trunc_f64_s'}],
  [Opcode.I64_EXTEND_I32_S, {op: 'i64.extend_i32_s'}],
  [Opcode.F32_DEMOTE_F64, {op: 'f32.demote_f64'}],
  [Opcode.F64_CONVERT_I32_S, {op: 'f64.convert_i32_s'}],
  [Opcode.F64_PROMOTE_F32, {op: 'f64.promote_f32'}],
  [Opcode.I32_REINTERPRET_F32, {op: 'i32.reinterpret_f32'}],
  [Opcode.I64_REINTERPRET_F64, {op: 'i64.reinterpret_f64'}],
])

function readu8(buffer: ArrayBuffer, offset: number): number {
  return new Uint8Array(buffer, offset, 1)[0]
}

function readi32(buffer: ArrayBuffer, offset: number): number {
  return new Int32Array(buffer, offset, 1)[0]
}

function readf32(buffer: ArrayBuffer, offset: number): number {
  if ((offset & 3) !== 0) {
    buffer = new Uint8Array(buffer).slice(offset, offset + 4)
    offset = 0
  }
  return new Float32Array(buffer, offset, 1)[0]
}

function readf64(buffer: ArrayBuffer, offset: number): number {
  if ((offset & 7) !== 0) {
    buffer = new Uint8Array(buffer).slice(offset, offset + 8)
    offset = 0
  }
  return new Float64Array(buffer, offset, 1)[0]
}

function readLeb128(buffer: ArrayBuffer, offset: number): [number, number] {
  const u8array = new Uint8Array(buffer, offset)
  let x = 0
  let bits = 0
  let i
  for (i = 0; i < u8array.byteLength; ) {
    const c = u8array[i++]
    x |= (c & 0x7f) << bits
    bits += 7
    if ((c & 0x80) === 0) {
      if ((c & 0x40) !== 0)
        x -= 1 << bits
      break
    }
  }
  return [x, offset + i]
}

function readUleb128(buffer: ArrayBuffer, offset: number): [number, number] {
  const u8array = new Uint8Array(buffer, offset)
  let x = 0
  let bits = 0
  let i
  for (i = 0; i < u8array.byteLength; ) {
    const c = u8array[i++]
    x |= (c & 0x7f) << bits
    bits += 7
    if ((c & 0x80) === 0)
      break
  }
  return [x, offset + i]
}

class Type {
  private type: number | {type: string, params: Array<Type>, results: Array<Type>}

  constructor(type: number | {type: string, params: Array<Type>, results: Array<Type>}) {
    this.type = type
  }

  toString(): string {
    if (typeof(this.type) === 'object') {
      return `Func{params: ${this.type.params}, result:${this.type.results}}`
    } else {
      switch (this.type) {
      case WasmType.VOID:  return 'void'
      case WasmType.I32:   return 'i32'
      case WasmType.I64:   return 'i64'
      case WasmType.F32:   return 'f32'
      case WasmType.F64:   return 'f64'
      default:  throw `Unhandled: ${this.type}`
      }
    }
  }
}

function readType(buffer: ArrayBuffer, offset: number): [Type, number] {
  const t = readu8(buffer, offset)
  switch (t) {
  case WasmType.VOID:
  case WasmType.I32:
  case WasmType.I64:
  case WasmType.F32:
  case WasmType.F64:
    return [new Type(t), offset + 1]
  case WasmType.FUNC:
    {
      const [numParams, ofs] = readUleb128(buffer, offset + 1)
      offset = ofs
      const params = [...Array(numParams)].map(() => {
        const [t, ofs2] = readType(buffer, offset)
        offset = ofs2
        return t
      })
      const [num_results, ofs3] = readUleb128(buffer, offset)
      offset = ofs3
      const results = [...Array(num_results)].map(() => {
        const [t, ofs4] = readType(buffer, offset)
        offset = ofs4
        return t
      })
      return [new Type({type: 'func', params: params, results: results}), offset]
    }
  default:
    throw `Unhnadled type: at 0x${offset.toString(16)}`
  }
}

type Operand = number | Array<number>

function readOperand(buffer: ArrayBuffer, operand: string, offset: number): [Operand|Type, number] {
  switch (operand) {
  case 'type':
    return readType(buffer, offset)
  case 'leb128':
    return readLeb128(buffer, offset)
  case 'uleb128':
    return readUleb128(buffer, offset)
  case 'uleb128array':
    {
      const [count, ofs1] = readUleb128(buffer, offset)
      offset = ofs1
      const nums = [...Array(count)].map(_ => {
        const [x, ofs2] = readUleb128(buffer, offset)
        offset = ofs2
        return x
      })
      return [nums, offset]
    }
  case 'f32':
    return [readf32(buffer, offset), offset + 4]
  case 'f64':
    return [readf64(buffer, offset), offset + 8]
  default:
    throw `Unhandled operand: ${operand} at 0x${offset.toString(16)}`
  }
}

class Inst {
  public op: string
  public operands?: Array<Operand|Type>
}

function readInst(buffer: ArrayBuffer, offset: number): [Inst, number] {
  const op = readu8(buffer, offset)
  ++offset

  const table = InstTable.get(op)
  if (table == null) {
    throw `Unhandled op: 0x${op.toString(16).padStart(2, '0')} at 0x${offset.toString(16)}`
  }

  let inst: Inst = {op: table.op}
  if (table.operands != null) {
    inst.operands = table.operands.map(operand => {
      const [opr, next] = readOperand(buffer, operand, offset)
      offset = next
      return opr
    })
  }
  return [inst, offset]
}

let SPACES = '    '

class DisWasm {
  private buffer: ArrayBufferLike
  private version = -1
  private codes = new Array<Array<Inst>>()

  constructor(private filename: string) {
    const content = fs.readFileSync(this.filename)
    this.buffer = new Uint8Array(content).buffer
  }

  dump(): void {
    if (!this.checkHeader())
      throw Error('No wasm header')
    console.log(`WASM version: ${this.version}`)
    this.loadSections()
  }

  checkHeader(): boolean {
    const magic = new Uint8Array(this.buffer, 0, 4)
    if (new TextDecoder('utf-8').decode(magic) !== '\x00asm')
      return false
    this.version = readi32(this.buffer, 4)
    return true
  }

  loadSections(): void {
    let offset = 8
    while (offset < this.buffer.byteLength) {
      const sec = readu8(this.buffer, offset) as Section
      const [len, nextOfs] = readUleb128(this.buffer, offset + 1)

console.log(`0x${offset.toString(16)}: sec=${sec}, len=${len}`)
      switch (sec) {
      case Section.TYPE:
      case Section.IMPORT:
      case Section.FUNC:
      case Section.TABLE:
      case Section.MEMORY:
      case Section.GLOBAL:
      case Section.EXPORT:
      case Section.ELEM:
      case Section.DATA:
        // TODO
        break

      case Section.CODE:
        this.readCodeSection(nextOfs, len)
        break

      default:
        throw `Unhandled section: ${sec}, offset=0x${offset.toString(16)}, len=${len}`
      }

      offset = nextOfs + len
    }
  }

  readCodeSection(offset: number, _len: number): void {
    let [num, start] = readUleb128(this.buffer, offset)
    offset = start
    for (let i = 0; i < num; ++i) {
      const [code, nextOfs] = this.readCode(offset, i)
      this.codes.push(code)
      offset = nextOfs
    }
  }

  readCode(offset: number, funcIndex: number): [Inst[], number] {
    console.log(`\n=== Function ${funcIndex}`)
    const [bodySize, ofs] = readUleb128(this.buffer, offset)
    const endOfs = (offset = ofs) + bodySize
    const [localDeclCount, ofs2] = readUleb128(this.buffer, offset)
    offset = ofs2
    const localTypes = [...Array(localDeclCount)].map(() => {
      const [num, ofs3] = readUleb128(this.buffer, offset)
      const [t, ofs4] = readType(this.buffer, ofs3)
      offset = ofs4
      return [num, t]
    })
    console.log(`localTypes: ${localTypes.map(([num, t]) => `(${num}, ${t})`).join(', ')}`)

    const code = new Array<Inst>()
    let indent = 1
    while (offset < endOfs) {
      const [inst, nextOfs] = readInst(this.buffer, offset)
      code.push(inst)

      switch (inst.op) {
      case 'else': case 'end':
        --indent
        break
      }

      while (indent * 2 > SPACES.length)
        SPACES += SPACES
      const spaces = SPACES.slice(0, indent * 2)
      const operands = inst.operands != null ? inst.operands.map((x) => x.toString()).join(', ') : ''
      console.log(`${offset.toString(16).padStart(5, '0')}: ${spaces}${inst.op} ${operands != null ? operands.toString() : ''}`)
      offset = nextOfs

      switch (inst.op) {
      case 'if': case 'block': case 'loop': case 'else':
        ++indent
        break
      }
    }
    return [code, endOfs]
  }
}

function main(argv: string[]) {
  if (argv.length < 3) {
    console.error('Usage: [wasm file]')
    process.exit(1)
  }

  const diswasm = new DisWasm(argv[2])
  try {
    diswasm.dump()
  } catch (e) {
    console.error(e)
    process.exit(1)
  }
}

main(process.argv)
