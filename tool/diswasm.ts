#! /usr/bin/env ts-node

'use strict'

import fsPromise from 'node:fs/promises'
import {DisWasm} from '../src/wcc/www/diswasm'

async function main(argv: string[]) {
  const {program} = require('commander')
  program
    .option('--dump-addr', 'Dump address')
    .parse(argv)
  const opts = program.opts()
  const args = program.args

  if (args < 1) {
    console.error('Usage: [wasm file]')
    process.exit(1)
  }

  const content = await fsPromise.readFile(args[0])
  const buffer = new Uint8Array(content).buffer
  const diswasm = new DisWasm(buffer, opts)
  try {
    diswasm.dump()
  } catch (e) {
    console.error(e)
    process.exit(1)
  }
}

main(process.argv)
