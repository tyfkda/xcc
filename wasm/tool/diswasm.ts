#! /usr/bin/env ts-node

'use strict'

import * as fs from 'fs'
import {DisWasm} from '../src/www/diswasm'

function main(argv: string[]) {
  if (argv.length < 3) {
    console.error('Usage: [wasm file]')
    process.exit(1)
  }

  const content = fs.readFileSync(argv[2])
  const buffer = new Uint8Array(content).buffer
  const diswasm = new DisWasm(buffer)
  try {
    diswasm.dump()
  } catch (e) {
    console.error(e)
    process.exit(1)
  }
}

main(process.argv)
