#! /usr/bin/env node

'use strict'

const fs = require('fs')
const path = require('path')
const { WASI } = require('@wasmer/wasi')

function getRealpaths(map) {
  Object.keys(map).forEach(key => {
    try {
      map[key] = fs.realpathSync(map[key])
    } catch (e) {
      if (e.code === 'ENOENT') {
        console.error(`Error: "${map[key]}" not exist`)
      } else {
        console.error(e)
      }
      process.exit(1)
    }
  })
  return map
}

;(async () => {
  const preopens = {}
  function handleDir(value) {
    preopens[value] = value
  }
  function handleMapDir(value) {
    const [virt, actual] = value.split('::', 2)
    preopens[virt] = actual
  }

  const program = require('commander')
  program
    .option('--dir <directory>', 'Make a directory accessible', handleDir)
    .option('--mapdir <virtual-path::actual-path>', 'Map actual as virtual path', handleMapDir)
    .parse(process.argv)
    .usage('[.wasm] <arguments...>')

  if (program.args.length <= 0) {
    program.help()
    process.exit(1)
  }

  const wasmFileName = program.args[0]
  const wasi = new WASI({
    args: program.args,
    env: process.env,
    bindings: {
      ...WASI.defaultBindings,
      fs,
      path,
    },
    preopens: getRealpaths(preopens),
  })

  try {
    const wasmBin = fs.readFileSync(wasmFileName)
    const wasmModule = await WebAssembly.compile(wasmBin)
    const importObject = Object.assign({}, wasi.getImports(wasmModule))
    const instance = await WebAssembly.instantiate(wasmModule, importObject)
    wasi.start(instance)
  } catch (e) {
    console.error(e)
    process.exit(1)
  }
})()
