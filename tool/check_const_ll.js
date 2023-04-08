#! /usr/bin/env node

'use strict'

const fs = require('fs')
const childProcess = require('child_process')
const readline = require('readline')

const HEADER = `#include <stdio.h>
int main(){
`
const FOOTER = `
  return 0;
}
`

async function run_test(min, max) {
  console.error(`run_test: ${min} ~ ${max}`)

  const srcfn = ',check_const_ll.c'
  const exefn = './,check_const_ll'
  const outfn = ',out'

  let fd = fs.openSync(srcfn, 'w')
  fs.writeSync(fd, HEADER)
  for (let i = min; i <= max; ++i) {
    fs.writeSync(fd, `  printf("%lld\\n", ${i}LL);\n`)
  }
  fs.writeSync(fd, FOOTER)
  fs.close(fd)

  childProcess.execSync(`./xcc -o "${exefn}" "${srcfn}"`)  // Exception raised if command failed.

  const res = childProcess.execSync(`${exefn} > ${outfn}`, [], {shell: true})

  const input = fs.createReadStream(outfn)
  const rl = readline.createInterface({
    input,
    crlfDelay: Infinity,
  })

  let i = 0
  for await (const line of rl) {
    if (line !== (i + min).toString()) {
      console.error(`Different: ${i + min}, Result=[${line}]`)
      process.exit(1)
    }
    ++i
  }

  fs.unlinkSync(srcfn)
  fs.unlinkSync(exefn)
  fs.unlinkSync(outfn)
}

async function main() {
  const MAX = 100000
  const STEP = 10000
  for (let i = 0; i < MAX; i += STEP) {
    await run_test(i, i + STEP)
    await run_test(-i - STEP, -i)
  }
}

main()
