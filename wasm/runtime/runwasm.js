#! /usr/bin/env node

'use strict'

const assert = require('assert')
const fs = require('fs')
const os = require('os')
const path = require('path')

async function createWasm(wasmFile, imports) {
  const buffer = fs.readFileSync(wasmFile)
  const module = await WebAssembly.compile(buffer)
  return new WebAssembly.Instance(module, imports)
}

function ALIGN(x, align) {
  return (x + align - 1) & -align
}

// Decode string in linear memory to JS.
function decodeString(buffer, ptr) {
  const memoryImage = new Uint8Array(buffer, ptr)
  let len
  for (len = 0; len < memoryImage.length && memoryImage[len] !== 0x00; ++len)
    ;
  const arr = new Uint8Array(buffer, ptr, len)
  return new TextDecoder('utf-8').decode(arr)
}

function tmppath() {
  const CHARS = '0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz'
  const len = 8
  return [...Array(len)].map(_ => CHARS[(Math.random() * CHARS.length) | 0]).join('')
}

function tmpfileSync(len) {
  const filePath = path.join(os.tmpdir(), tmppath())
  return fs.openSync(filePath, 'w+', 0o600)
}

;(async () => {
  const memory = new WebAssembly.Memory({initial:10, maximum:256})
  const HEAP_ALIGN = 8
  const HEAP_PAGE_SIZE = 65536
  let breakStartAddress = 0
  let breakAddress = 0

  function brk(ptr) {
    if (ptr >= breakStartAddress) {
      if (ptr > memory.buffer.byteLength) {
        const d = ptr - memory.buffer.byteLength
        const page = Math.floor((d + HEAP_PAGE_SIZE - 1) / HEAP_PAGE_SIZE)
        memory.grow(page)
      }
      breakAddress = ptr
    }
    return breakAddress
  }

  const O_RDONLY  = 0x000
  const O_WRONLY  = 0x001
  const O_RDWR    = 0x002
  const O_TRUNC   = 0x100
  const O_CREAT   = 0x200
  const O_APPEND  = 0x400

  const SEEK_SET = 0
  const SEEK_CUR = 1
  const SEEK_END = 2

  const kOpenFlags = {}
  kOpenFlags[O_RDONLY] = 'r'
  kOpenFlags[O_WRONLY] = 'w'
  kOpenFlags[O_RDWR] = 'w+'
  kOpenFlags[O_WRONLY | O_CREAT | O_TRUNC] = 'w'

  const ERANGE = 34

  const files = new Map()

  function getImports() {
    const imports = {
      c: {
        read: (fd, buf, size) => {
          const memoryImage = new Uint8Array(memory.buffer, buf, size)
          if (fd < 3) {
            return fs.readSync(fd, memoryImage)
          } else {
            const bytes = fs.readSync(fd, memoryImage, files[fd])
            files[fd].position += bytes
            return bytes
          }
        },
        write: (fd, buf, size) => {
          const memoryImage = new Uint8Array(memory.buffer, buf, size)
          if (fd < 3) {
            return fs.writeSync(fd, memoryImage)
          } else {
            const bytes = fs.writeSync(fd, memoryImage)
            files[fd].position += bytes
            return bytes
          }
        },
        open: (filename, flag, mode) => {
          if (filename === 0)
            return -1
          const fn = decodeString(memory.buffer, filename)
          if (fn == null || fn === '')
            return -1

          const flagStr = kOpenFlags[flag]
          if (flagStr == null) {
            console.error(`Unsupported open flag: ${flag}`)
            return -1
          }

          try {
            const fd = fs.openSync(fn, flagStr)
            files[fd] = {
              position: 0,
            }
            return fd
          } catch (e) {
            if (e.code !== 'ENOENT')
              console.error(e)
            return -1
          }
        },
        close: (fd) => {
          fs.closeSync(fd)
          files.delete(fd)
          return 0
        },
        lseek: (fd, offset, where) => {
          let position
          switch (where) {
          default:
          case SEEK_SET:
            position = offset
            break
          case SEEK_CUR:
            position = files[fd].position + offset
            break
          case SEEK_END:
            //position = files[fd].position + offset
            assert(false, 'TODO: Implement')
            break
          }
          files[fd].position = position
          return position
        },
        _tmpfile: () => {
          const fd = tmpfileSync()
          if (fd >= 0) {
            files[fd] = {
              position: 0,
            }
          }
          return fd
        },

        _brk: brk,

        _getcwd: (buffer, size) => {
          const cwd = process.cwd()
          const encoded = new TextEncoder('utf-8').encode(cwd)
          const len = encoded.length
          if (len + 1 > size)
            return -ERANGE
          const memoryImage = new Uint8Array(memory.buffer, buffer, len + 1)
          for (let i = 0; i < len; ++i)
            memoryImage[i] = encoded[i]
          memoryImage[len] = 0
          return len + 1
        },

        sin: Math.sin,
        cos: Math.cos,
        sqrt: Math.sqrt,
        drand48: Math.random,
        fabs: Math.abs,

        putstr: (ptr) => {
          const text = decodeString(memory.buffer, ptr)
          process.stdout.write(text)
        },
        puti: (x) => {
          process.stdout.write(x.toString())
        },
        exit: (x) => {
          process.exit(x)
        },
        _memcpy: (dst, src, len) => {
          const memoryImage = new Uint8Array(memory.buffer)
          memoryImage.copyWithin(dst, src, src + len)
        },
        _memset: (dst, val, len) => {
          const memoryImage = new Uint8Array(memory.buffer)
          for (let i = 0; i < len; ++i)
            memoryImage[dst++] = val
        },
      },
      env: {
        memory,
      },
    }
    return imports
  }

  async function loadWasm(wasmFile) {
    const imports = getImports()
    const instance = await createWasm(wasmFile, imports)
    const stackPointer = instance.exports.$_SP
    if (stackPointer != null)
      breakStartAddress = breakAddress = ALIGN(stackPointer.valueOf(), HEAP_ALIGN)
    return instance
  }

  function putArgs(args) {
    const encodedArgs = args.map(arg => new TextEncoder('utf-8').encode(arg))
    const totalArgsBytes = encodedArgs.reduce((acc, arg) => acc + arg.length + 1, 0)
    const size = 4 * (args.length + 1) + totalArgsBytes
    const ptr = breakAddress
    const pStrStart = ptr + 4 * (args.length + 1)
    brk(ALIGN(ptr + size, HEAP_ALIGN))

    const ptrArgs = new Uint32Array(memory.buffer, ptr, args.length + 1)
    const ptrStr = new Uint8Array(memory.buffer, pStrStart, totalArgsBytes)
    let p = 0
    for (let i = 0; i < args.length; ++i) {
      const encoded = encodedArgs[i]
      const len = encoded.length
      for (let j = 0; j < len; ++j)
        ptrStr[p + j] = encoded[j]
      ptrStr[p + len] = 0
      ptrArgs[i] = pStrStart + p
      p += len + 1
    }
    ptrArgs[args.length] = 0
    return ptr
  }

  async function main(argv) {
    const program = require('commander')
    program
      .option('-e, --entry <func-name>', 'Entry point', 'main')
      .parse(argv)

    if (program.args < 1) {
      program.help()
    }

    const args = program.args
    const wasmFile = args[0]
    const instance = await loadWasm(wasmFile)
    const argsPtr = putArgs(args)
    try {
      const result = instance.exports[program.entry](args.length, argsPtr)
      if (result !== 0)
        process.exit(result)
    } catch (e) {
      console.error(e.toString())
      process.exit(1)
    }
  }

  main(process.argv)
})()
