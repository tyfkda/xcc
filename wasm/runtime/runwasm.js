#! /usr/bin/env node

'use strict'

const fs = require('fs')
const assert = require('assert')

async function createWasm(wasmFile, imports) {
  const buffer = fs.readFileSync(wasmFile)
  const module = await WebAssembly.compile(buffer)
  return new WebAssembly.Instance(module, imports)
}

;(async () => {
  if (process.argv.length < 4) {
    console.error('Usage: <wasmFile> <funcName> ...args')
    process.exit(1)
  }

  const [_node, _file, wasmFile, funcName, ...args] = process.argv

  const memory = new WebAssembly.Memory({initial:10, maximum:100})

  // Decode string in linear memory to JS.
  function decodeString(buffer, ptr) {
    const memoryImage = new Uint8Array(buffer, ptr)
    let len
    for (len = 0; len < memoryImage.length && memoryImage[len] !== 0x00; ++len)
      ;
    const arr = new Uint8Array(buffer, ptr, len)
    return new TextDecoder('utf-8').decode(arr)
  }

  const imports = {
    c: {
      write: (fd, buf, size) => {
        const memoryImage = new Uint8Array(memory.buffer, buf, size)
        return fs.writeSync(fd, memoryImage)
      },

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
    },
    env: {
      memory,
    },
  }

  const instance = await createWasm(wasmFile, imports)
  const exports = instance.exports
  assert(exports, 'no exports found')
  assert(funcName in exports, `${funcName} not found in wasm module exports`)
  const result = exports[funcName](...args)
  if (result !== 0)
    process.exit(result)
})()
