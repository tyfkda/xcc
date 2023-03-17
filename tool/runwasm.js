#! /usr/bin/env node

'use strict'

const assert = require('assert')
const fs = require('fs')
const os = require('os')
const path = require('path')
const tmp = require('tmp')

const SEEK_SET = 0
const SEEK_CUR = 1
const SEEK_END = 2

async function createWasm(wasmFile, imports) {
  const buffer = fs.readFileSync(wasmFile)
  const module = await WebAssembly.compile(buffer)
  return new WebAssembly.Instance(module, imports)
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

class IFileEntry {
  constructor(fd) {
    this.fd = fd
  }

  readSync(buffer) {
    return fs.readSync(this.fd, buffer)
  }
  writeSync(buffer) {
    return fs.writeSync(this.fd, buffer)
  }
  lseek(offset, where) {
    return -1
  }
}

class StandardEntry extends IFileEntry {
  constructor(fd, name) {
    super(fd)
    this.name = name
  }

  toString() {
    return `${this.name}`
  }
}

class FileEntry extends IFileEntry {
  position = 0

  constructor(fd, fn) {
    super(fd)
    this.fn = fn
  }

  readSync(buffer) {
    const bytes = fs.readSync(this.fd, buffer, 0, buffer.length, this.position)
    this.position += bytes
    return bytes
  }

  writeSync(buffer) {
    const bytes = fs.writeSync(this.fd, buffer, undefined, undefined, this.position)
    this.position += bytes
    return bytes
  }

  lseek(offset, where) {
    switch (where) {
    default:
    case SEEK_SET:
      this.position = offset
      break
    case SEEK_CUR:
      this.position = this.position + offset
      break
    case SEEK_END:
      {
        const stat = fs.fstatSync(this.fd)
        this.position = stat.size + offset
      }
      break
    }
    return this.position
  }
}

;(async () => {
  const program = require('commander')
  program
    .option('--entry-point <func-name>', 'Entry point', '_start')
    .parse(process.argv)
  const encoder = new TextEncoder()
  const encodedArgs = program.args.map(arg => encoder.encode(arg))
  const totalArgsBytes = encodedArgs.reduce((acc, arg) => acc + arg.length + 1, 0)

  let memory

  const O_RDONLY  = 0x00
  const O_WRONLY  = 0x01
  const O_RDWR    = 0x02
  const O_CREAT   = 0x040  //  0100
  const O_EXCL    = 0x080  //  0200
  const O_TRUNC   = 0x200  // 01000
  const O_APPEND  = 0x400  // 02000

  const kOpenFlags = new Map()
  kOpenFlags.set(O_RDONLY, 'r')
  kOpenFlags.set(O_WRONLY | O_CREAT | O_TRUNC, 'w')
  kOpenFlags.set(O_WRONLY | O_CREAT | O_APPEND, 'a')
  kOpenFlags.set(O_RDWR, 'r+')
  kOpenFlags.set(O_RDWR | O_CREAT | O_TRUNC, 'w+')
  kOpenFlags.set(O_RDWR | O_CREAT | O_APPEND, 'a+')

  const ENOENT = 2
  const ERANGE = 34

  const Code2errMap = {
    ENOENT: -ENOENT,
    ERANGE: -ERANGE,
  }

  function code2err(exc, orDefault = -1) {
    if (exc.code in Code2errMap) {
      return Code2errMap[exc.code]
    }
    console.error(exc)
    return orDefault
  }

  const files = new Map()
  files.set(0, new StandardEntry(0, '<stdin>'))
  files.set(1, new StandardEntry(1, '<stdout>'))
  files.set(2, new StandardEntry(2, '<stderr>'))

  function getImports() {
    const imports = {
      c: {
        args_sizes_get: (pargc, plen) => {
          const argc = new Uint32Array(memory.buffer, pargc, 1)
          argc[0] = encodedArgs.length

          const len = new Uint32Array(memory.buffer, plen, 1)
          len[0] = totalArgsBytes
        },
        args_get: (pargv, pstr) => {
          const argv = new Uint32Array(memory.buffer, pargv, encodedArgs.length)
          const str = new Uint8Array(memory.buffer, pstr, totalArgsBytes)
          let offset = 0
          for (let i = 0; i < encodedArgs.length; ++i) {
            argv[i] = pstr + offset
            const encoded = encodedArgs[i]
            const len = encoded.length
            for (let j = 0; j < len; ++j)
              str[j + offset] = encoded[j]
            str[len + offset] = 0
            offset += len + 1
          }
        },

        read: (fd, buf, size) => {
          if (!files.has(fd))
            return 0
          const memoryImage = new Uint8Array(memory.buffer, buf, size)
          return files.get(fd).readSync(memoryImage)
        },
        write: (fd, buf, size) => {
          if (!files.has(fd))
            return 0
          const memoryImage = new Uint8Array(memory.buffer, buf, size)
          return files.get(fd).writeSync(memoryImage)
        },
        open: (filename, flag, mode) => {
          if (filename === 0)
            return -1
          const fn = decodeString(memory.buffer, filename)
          if (fn == null || fn === '')
            return -1

          const flagStr = kOpenFlags.get(flag)
          if (flagStr == null) {
            console.error(`Unsupported open flag: ${flag}`)
            return -1
          }

          try {
            const fd = fs.openSync(fn, flagStr)
            console.assert(!files.has(fd))
            files.set(fd, new FileEntry(fd, fn))
            return fd
          } catch (exc) {
            return code2err(exc)
          }
        },
        close: (fd) => {
          if (files.has(fd)) {
            files.delete(fd)
          }
          fs.closeSync(fd)
          return 0
        },
        lseek: (fd, offset, where) => {
          if (!files.has(fd))
            return 0
          return files.get(fd).lseek(offset, where)
        },
        _unlink: (filename) => {
          const fn = decodeString(memory.buffer, filename)
          try {
            fs.unlinkSync(fn)
            return 0
          } catch (exc) {
            return code2err(exc)
          }
        },
        _tmpfile: () => {
          try {
            const tmpobj = tmp.fileSync()
            const fd = tmpobj.fd
            if (fd >= 0) {
              files.set(fd, new FileEntry(fd, tmpobj.name))
            }
            return fd
          } catch (e) {
            return -1
          }
        },

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

        proc_exit: (x) => {
          process.exit(x)
        },

        clock_gettime: (clkId, tp) => {
          // TODO: Check clkId
          const ts = new Uint32Array(memory.buffer, tp, 2)
          const t = new Date().getTime()
          ts[0] = (t / 1000) | 0
          ts[1] = (t % 1000) * 1000000
          return 0
        },
      },
    }
    return imports
  }

  async function loadWasm(wasmFile) {
    const imports = getImports()
    const instance = await createWasm(wasmFile, imports)
    if (instance.exports.memory)
      memory = instance.exports.memory
    return instance
  }

  async function main() {
    if (program.args < 1) {
      program.help()
    }

    const args = program.args
    const opts = program.opts()
    const wasmFile = args[0]
    const instance = await loadWasm(wasmFile)
    try {
      instance.exports[opts.entryPoint]()
    } catch (e) {
      console.error(e.toString())
      process.exit(1)
    }
  }

  main()
})()
