import { assert } from 'console'
import * as fs from 'fs'
import { promisify } from 'util'

const JSON_FN = 'src/wcc/www/lib_list.json'
const LIB_DIR = './libsrc'

const CRT0_DIRS = ['crt0']
const LIBC_DIRS = ['math', 'misc', 'stdio', 'stdlib', 'string', 'unistd']

async function updateFiles(fileList: object, dirs: Array<string>, fn: string): Promise<object> {
  assert(fileList && fileList['usr'] && fileList['usr']['lib'])
  let libcFiles = new Array<string>()
  for (const dir of dirs) {
    const files = (await promisify(fs.readdir)(`${LIB_DIR}/${dir}`))
        .filter(fn => fn.endsWith('.c'))
        .map(fn => `${LIB_DIR}/${dir}/${fn}`)
        .sort()
    libcFiles.push(...files)
  }
  fileList['usr']['lib'][fn] = libcFiles
  return fileList
}

async function main(): Promise<void> {
  const content = await promisify(fs.readFile)(JSON_FN, 'utf8')
  let json = JSON.parse(content) as object

  json = await updateFiles(json, CRT0_DIRS, 'crt0.c')
  json = await updateFiles(json, LIBC_DIRS, 'libc.c')

  const updated = JSON.stringify(json, null, 2)
  await promisify(fs.writeFile)(JSON_FN, `${updated}\n`)
}

main()
