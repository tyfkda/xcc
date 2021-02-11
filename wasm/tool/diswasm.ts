#! /usr/bin/env ts-node

'use strict'

import * as fs from 'fs'

const SEC_TYPE      = 1
const SEC_IMPORT    = 2
const SEC_FUNC      = 3
const SEC_TABLE     = 4
const SEC_MEMORY    = 5
const SEC_GLOBAL    = 6
const SEC_EXPORT    = 7
const SEC_ELEM      = 9
const SEC_CODE      = 10
const SEC_DATA      = 11

// Wasm opcode
const OP_UNREACHABLE   = 0x00
const OP_NOP           = 0x01
const OP_BLOCK         = 0x02
const OP_LOOP          = 0x03
const OP_IF            = 0x04
const OP_ELSE          = 0x05
const OP_END           = 0x0b
const OP_BR            = 0x0c
const OP_BR_IF         = 0x0d
const OP_BR_TABLE      = 0x0e
const OP_RETURN        = 0x0f
const OP_CALL          = 0x10
const OP_CALL_INDIRECT = 0x11
const OP_DROP          = 0x1a
const OP_SELECT        = 0x1b
const OP_LOCAL_GET     = 0x20
const OP_LOCAL_SET     = 0x21
const OP_LOCAL_TEE     = 0x22
const OP_GLOBAL_GET    = 0x23
const OP_GLOBAL_SET    = 0x24
const OP_I32_LOAD      = 0x28
const OP_I64_LOAD      = 0x29
const OP_F32_LOAD      = 0x2a
const OP_F64_LOAD      = 0x2b
const OP_I32_LOAD8_S   = 0x2c
const OP_I32_LOAD8_U   = 0x2d
const OP_I32_LOAD16_S  = 0x2e
const OP_I32_LOAD16_U  = 0x2f
const OP_I32_STORE     = 0x36
const OP_I64_STORE     = 0x37
const OP_F32_STORE     = 0x38
const OP_F64_STORE     = 0x39
const OP_I32_STORE8    = 0x3a
const OP_I32_STORE16   = 0x3b
const OP_I64_STORE8    = 0x3c
const OP_I64_STORE16   = 0x3d
const OP_I64_STORE32   = 0x3e
const OP_MEMORY_SIZE   = 0x3f
const OP_MEMORY_GROW   = 0x40
const OP_I32_CONST     = 0x41
const OP_I64_CONST     = 0x42
const OP_F32_CONST     = 0x43
const OP_F64_CONST     = 0x44
const OP_I32_EQZ       = 0x45
const OP_I32_EQ        = 0x46
const OP_I32_NE        = 0x47
const OP_I32_LT_S      = 0x48
const OP_I32_LT_U      = 0x49
const OP_I32_GT_S      = 0x4a
const OP_I32_GT_U      = 0x4b
const OP_I32_LE_S      = 0x4c
const OP_I32_LE_U      = 0x4d
const OP_I32_GE_S      = 0x4e
const OP_I32_GE_U      = 0x4f
const OP_I64_EQZ       = 0x50
const OP_I64_EQ        = 0x51
const OP_I64_NE        = 0x52
const OP_I64_LT_S      = 0x53
const OP_I64_LT_U      = 0x54
const OP_I64_GT_S      = 0x55
const OP_I64_GT_U      = 0x56
const OP_I64_LE_S      = 0x57
const OP_I64_LE_U      = 0x58
const OP_I64_GE_S      = 0x59
const OP_I64_GE_U      = 0x5a
const OP_F32_EQ        = 0x5b
const OP_F32_NE        = 0x5c
const OP_F32_LT        = 0x5d
const OP_F32_GT        = 0x5e
const OP_F32_LE        = 0x5f
const OP_F32_GE        = 0x60
const OP_F64_EQ        = 0x61
const OP_F64_NE        = 0x62
const OP_F64_LT        = 0x63
const OP_F64_GT        = 0x64
const OP_F64_LE        = 0x65
const OP_F64_GE        = 0x66
const OP_I32_ADD       = 0x6a
const OP_I32_SUB       = 0x6b
const OP_I32_MUL       = 0x6c
const OP_I32_DIV_S     = 0x6d
const OP_I32_DIV_U     = 0x6e
const OP_I32_REM_S     = 0x6f
const OP_I32_REM_U     = 0x70
const OP_I32_AND       = 0x71
const OP_I32_OR        = 0x72
const OP_I32_XOR       = 0x73
const OP_I32_SHL       = 0x74
const OP_I32_SHR_S     = 0x75
const OP_I32_SHR_U     = 0x76
const OP_I32_ROTL      = 0x77
const OP_I32_ROTR      = 0x78
const OP_I64_ADD       = 0x7c
const OP_I64_SUB       = 0x7d
const OP_I64_MUL       = 0x7e
const OP_I64_DIV_S     = 0x7f
const OP_I64_DIV_U     = 0x80
const OP_I64_REM_S     = 0x81
const OP_I64_REM_U     = 0x82
const OP_I64_AND       = 0x83
const OP_I64_OR        = 0x84
const OP_I64_XOR       = 0x85
const OP_I64_SHL       = 0x86
const OP_I64_SHR_S     = 0x87
const OP_I64_SHR_U     = 0x88
const OP_I64_ROTL      = 0x89
const OP_I64_ROTR      = 0x8a
const OP_F32_ABS       = 0x8b
const OP_F32_NEG       = 0x8c
const OP_F32_ADD       = 0x92
const OP_F32_SUB       = 0x93
const OP_F32_MUL       = 0x94
const OP_F32_DIV       = 0x95
const OP_F64_ABS       = 0x99
const OP_F64_NEG       = 0x9a
const OP_F64_ADD       = 0xa0
const OP_F64_SUB       = 0xa1
const OP_F64_MUL       = 0xa2
const OP_F64_DIV       = 0xa3
const OP_I32_WRAP_I64        = 0xa7  // i32 <- i64
// const OP_I32_TRUNC_F32_S     = 0xa8  // i32 <- f32
const OP_I32_TRUNC_F64_S     = 0xaa  // i32 <- f64
const OP_I64_EXTEND_I32_S    = 0xac  // i64 <- i32
// const OP_I64_TRUNC_F32_S     = 0xae  // i64 <- f32
// const OP_I64_TRUNC_F64_S     = 0xb0  // i64 <- f64
// const OP_F32_CONVERT_I32_S   = 0xb2  // f32 <- i32
const OP_F32_DEMOTE_F64      = 0xb6  // f32 <- f64
// const OP_F32_CONVERT_I64_S   = 0xb4  // f32 <- i64
const OP_F64_CONVERT_I32_S   = 0xb7  // f64 <- i32
// const OP_F64_CONVERT_I64_S   = 0xb9  // f64 <- i64
const OP_F64_PROMOTE_F32     = 0xbb  // f64 <- f32
const OP_I32_REINTERPRET_F32 = 0xbc  // i32 <- f32
const OP_I64_REINTERPRET_F64 = 0xbd  // i64 <- f64

const WT_VOID       = 0x40
const WT_FUNC       = 0x60
const WT_F64        = 0x7c
const WT_F32        = 0x7d
const WT_I64        = 0x7e
const WT_I32        = 0x7f

const InstTable = new Map([
  [OP_UNREACHABLE, {op: 'unreachable'}],
  [OP_NOP, {op: 'nop'}],
  [OP_BLOCK, {op: 'block', operands: ['type']}],
  [OP_LOOP, {op: 'loop', operands: ['type']}],
  [OP_IF, {op: 'if', operands: ['type']}],
  [OP_ELSE, {op: 'else'}],
  [OP_END, {op: 'end'}],
  [OP_BR, {op: 'br', operands: ['uleb128']}],
  [OP_BR_IF, {op: 'br_if', operands: ['uleb128']}],
  [OP_BR_TABLE, {op: 'br_table', operands: ['uleb128array', 'uleb128']}],
  [OP_RETURN, {op: 'return'}],
  [OP_CALL, {op: 'call', operands: ['uleb128']}],
  [OP_CALL_INDIRECT, {op: 'call_indirect', operands: ['uleb128', 'uleb128']}],
  [OP_DROP, {op: 'drop'}],
  [OP_SELECT, {op: 'select'}],
  [OP_LOCAL_GET, {op: 'local.get', operands: ['uleb128']}],
  [OP_LOCAL_SET, {op: 'local.set', operands: ['uleb128']}],
  [OP_LOCAL_TEE, {op: 'local.tee', operands: ['uleb128']}],
  [OP_GLOBAL_GET, {op: 'global.get', operands: ['uleb128']}],
  [OP_GLOBAL_SET, {op: 'global.set', operands: ['uleb128']}],
  [OP_I32_LOAD, {op: 'i32.load', operands: ['uleb128', 'uleb128']}],
  [OP_I64_LOAD, {op: 'i64.load', operands: ['uleb128', 'uleb128']}],
  [OP_F32_LOAD, {op: 'f32.load', operands: ['uleb128', 'uleb128']}],
  [OP_F64_LOAD, {op: 'f64.load', operands: ['uleb128', 'uleb128']}],
  [OP_I32_STORE, {op: 'i32.store', operands: ['uleb128', 'uleb128']}],
  [OP_I64_STORE, {op: 'i64.store', operands: ['uleb128', 'uleb128']}],
  [OP_F32_STORE, {op: 'f32.store', operands: ['uleb128', 'uleb128']}],
  [OP_F64_STORE, {op: 'f64.store', operands: ['uleb128', 'uleb128']}],
  [OP_I32_LOAD8_S, {op: 'i32.load8_s', operands: ['uleb128', 'uleb128']}],
  [OP_I32_LOAD8_U, {op: 'i32.load8_u', operands: ['uleb128', 'uleb128']}],
  [OP_I32_LOAD16_S, {op: 'i32.load16_s', operands: ['uleb128', 'uleb128']}],
  [OP_I32_LOAD16_U, {op: 'i32.load16_u', operands: ['uleb128', 'uleb128']}],
  [OP_I32_STORE8, {op: 'i32.store8', operands: ['uleb128', 'uleb128']}],
  [OP_I32_STORE16, {op: 'i32.store16', operands: ['uleb128', 'uleb128']}],
  [OP_I64_STORE8, {op: 'i64.store8', operands: ['uleb128', 'uleb128']}],
  [OP_I64_STORE16, {op: 'i64.store16', operands: ['uleb128', 'uleb128']}],
  [OP_I64_STORE32, {op: 'i64.store32', operands: ['uleb128', 'uleb128']}],
  [OP_MEMORY_SIZE, {op: 'memory.size', operands: ['uleb128']}],
  [OP_MEMORY_GROW, {op: 'memory.grow', operands: ['uleb128']}],
  [OP_I32_CONST, {op: 'i32.const', operands: ['leb128']}],
  [OP_I64_CONST, {op: 'i64.const', operands: ['leb128']}],
  [OP_F32_CONST, {op: 'f32.const', operands: ['f32']}],
  [OP_F64_CONST, {op: 'f64.const', operands: ['f64']}],
  [OP_I32_EQZ, {op: 'i32.eqz'}],
  [OP_I32_EQ, {op: 'i32.eq'}],
  [OP_I32_NE, {op: 'i32.ne'}],
  [OP_I32_LT_S, {op: 'i32.lt_s'}],
  [OP_I32_LT_U, {op: 'i32.lt_u'}],
  [OP_I32_GT_S, {op: 'i32.gt_s'}],
  [OP_I32_GT_U, {op: 'i32.gt_u'}],
  [OP_I32_LE_S, {op: 'i32.le_s'}],
  [OP_I32_LE_U, {op: 'i32.le_u'}],
  [OP_I32_GE_S, {op: 'i32.ge_s'}],
  [OP_I32_GE_U, {op: 'i32.ge_u'}],
  [OP_I64_EQZ, {op: 'i64.eqz'}],
  [OP_I64_EQ, {op: 'i64.eq'}],
  [OP_I64_NE, {op: 'i64.ne'}],
  [OP_I64_LT_S, {op: 'i64.lt_s'}],
  [OP_I64_LT_U, {op: 'i64.lt_u'}],
  [OP_I64_GT_S, {op: 'i64.gt_s'}],
  [OP_I64_GT_U, {op: 'i64.gt_u'}],
  [OP_I64_LE_S, {op: 'i64.le_s'}],
  [OP_I64_LE_U, {op: 'i64.le_u'}],
  [OP_I64_GE_S, {op: 'i64.ge_s'}],
  [OP_I64_GE_U, {op: 'i64.ge_u'}],
  [OP_F32_EQ, {op: 'f32.eq'}],
  [OP_F32_NE, {op: 'f32.ne'}],
  [OP_F32_LT, {op: 'f32.lt'}],
  [OP_F32_GT, {op: 'f32.gt'}],
  [OP_F32_LE, {op: 'f32.le'}],
  [OP_F32_GE, {op: 'f32.ge'}],
  [OP_F64_EQ, {op: 'f64.eq'}],
  [OP_F64_NE, {op: 'f64.ne'}],
  [OP_F64_LT, {op: 'f64.lt'}],
  [OP_F64_GT, {op: 'f64.gt'}],
  [OP_F64_LE, {op: 'f64.le'}],
  [OP_F64_GE, {op: 'f64.ge'}],
  [OP_I32_ADD, {op: 'i32.add'}],
  [OP_I32_SUB, {op: 'i32.sub'}],
  [OP_I32_MUL, {op: 'i32.mul'}],
  [OP_I32_DIV_S, {op: 'i32.div_s'}],
  [OP_I32_DIV_U, {op: 'i32.div_u'}],
  [OP_I32_REM_S, {op: 'i32.rem_s'}],
  [OP_I32_REM_U, {op: 'i32.rem_u'}],
  [OP_I32_AND, {op: 'i32.and'}],
  [OP_I32_OR, {op: 'i32.or'}],
  [OP_I32_XOR, {op: 'i32.xor'}],
  [OP_I32_SHL, {op: 'i32.shl'}],
  [OP_I32_SHR_S, {op: 'i32.shr_s'}],
  [OP_I32_SHR_U, {op: 'i32.shr_u'}],
  [OP_I32_ROTL, {op: 'i32.rotl'}],
  [OP_I32_ROTR, {op: 'i32.rotr'}],
  [OP_I64_ADD, {op: 'i64.add'}],
  [OP_I64_SUB, {op: 'i64.sub'}],
  [OP_I64_MUL, {op: 'i64.mul'}],
  [OP_I64_DIV_S, {op: 'i64.div_s'}],
  [OP_I64_DIV_U, {op: 'i64.div_u'}],
  [OP_I64_REM_S, {op: 'i64.rem_s'}],
  [OP_I64_REM_U, {op: 'i64.rem_u'}],
  [OP_I64_AND, {op: 'i64.and'}],
  [OP_I64_OR, {op: 'i64.or'}],
  [OP_I64_XOR, {op: 'i64.xor'}],
  [OP_I64_SHL, {op: 'i64.shl'}],
  [OP_I64_SHR_S, {op: 'i64.shr_s'}],
  [OP_I64_SHR_U, {op: 'i64.shr_u'}],
  [OP_I64_ROTL, {op: 'i64.rotl'}],
  [OP_I64_ROTR, {op: 'i64.rotr'}],
  [OP_F32_ABS, {op: 'f32.abs'}],
  [OP_F32_NEG, {op: 'f32.neg'}],
  [OP_F32_ADD, {op: 'f32.add'}],
  [OP_F32_SUB, {op: 'f32.sub'}],
  [OP_F32_MUL, {op: 'f32.mul'}],
  [OP_F32_DIV, {op: 'f32.div'}],
  [OP_F64_ABS, {op: 'f64.abs'}],
  [OP_F64_NEG, {op: 'f64.neg'}],
  [OP_F64_ADD, {op: 'f64.add'}],
  [OP_F64_SUB, {op: 'f64.sub'}],
  [OP_F64_MUL, {op: 'f64.mul'}],
  [OP_F64_DIV, {op: 'f64.div'}],

  [OP_I32_WRAP_I64, {op: 'i32.wrap_i64'}],
  [OP_I32_TRUNC_F64_S, {op: 'i32.trunc_f64_s'}],
  [OP_I64_EXTEND_I32_S, {op: 'i64.extend_i32_s'}],
  [OP_F32_DEMOTE_F64, {op: 'f32.demote_f64'}],
  [OP_F64_CONVERT_I32_S, {op: 'f64.convert_i32_s'}],
  [OP_F64_PROMOTE_F32, {op: 'f64.promote_f32'}],
  [OP_I32_REINTERPRET_F32, {op: 'i32.reinterpret_f32'}],
  [OP_I64_REINTERPRET_F64, {op: 'i64.reinterpret_f64'}],
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
      case WT_VOID:  return 'void'
      case WT_I32:   return 'i32'
      case WT_I64:   return 'i64'
      case WT_F32:   return 'f32'
      case WT_F64:   return 'f64'
      default:  throw `Unhandled: ${this.type}`
      }
    }
  }
}

function readType(buffer: ArrayBuffer, offset: number): [Type, number] {
  const t = readu8(buffer, offset)
  switch (t) {
  case WT_VOID:
  case WT_I32:
  case WT_I64:
  case WT_F32:
  case WT_F64:
    return [new Type(t), offset + 1]
  case WT_FUNC:
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
      const sec = readu8(this.buffer, offset)
      const [len, nextOfs] = readUleb128(this.buffer, offset + 1)

console.log(`0x${offset.toString(16)}: sec=${sec}, len=${len}`)
      switch (sec) {
      case SEC_TYPE:
      case SEC_IMPORT:
      case SEC_FUNC:
      case SEC_TABLE:
      case SEC_MEMORY:
      case SEC_GLOBAL:
      case SEC_EXPORT:
      case SEC_ELEM:
      case SEC_DATA:
        // TODO
        break

      case SEC_CODE:
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
