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
  const imports = {
    c: {
      puti: (x) => {
        console.log(x)
      },
      exit: (x) => {
        process.exit(x)
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
