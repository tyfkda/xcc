import { assert } from 'console'
import * as fs from 'fs'
import { promisify } from 'util'

const JSON_FN = 'src/wcc/www/lib_list.json'
const LIB_DIR = './libsrc'
const EXCLUDES = ['crt0', 'math', '_wasm']

async function updateLibc(fileList: object): Promise<object> {
  assert(fileList && fileList['usr'] && fileList['usr']['lib'])
  const dirs = (await promisify(fs.readdir)(LIB_DIR))
      .filter(d => !EXCLUDES.includes(d))
      .sort()
  let libcFiles = new Array<string>()
  for (const dir of dirs) {
    const files = (await promisify(fs.readdir)(`${LIB_DIR}/${dir}`))
        .filter(fn => fn.endsWith('.c'))
        .map(fn => `${LIB_DIR}/${dir}/${fn}`)
        .sort()
    libcFiles.push(...files)
  }
  fileList['usr']['lib']['libc.c'] = libcFiles
  return fileList
}

async function main(): Promise<void> {
  const content = await promisify(fs.readFile)(JSON_FN, 'utf8')
  let json = JSON.parse(content) as object

  json = await updateLibc(json)

  const updated = JSON.stringify(json, null, 2)
  await promisify(fs.writeFile)(JSON_FN, `${updated}\n`)
}

main()
