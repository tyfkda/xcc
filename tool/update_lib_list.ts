import { assert } from 'console'
import fs from 'fs'
import { promisify } from 'util'

const JSON_FN = 'src/wcc/www/lib_list.json'
const LIB_DIR = './libsrc'

const CRT0_DIRS = ['crt0']
const LIBC_DIRS = ['math', 'misc', 'stdio', 'stdlib', 'string', 'unistd']

const DST_DIR = './libsrc/_wasm'

const readFile = promisify(fs.readFile)
const writeFile = promisify(fs.writeFile)
const readdir = promisify(fs.readdir)

async function readDirFiles(dirs: string[], ext: string = '.c'): Promise<Map<string, string[]>> {
  const fileEntries = new Map<string, string[]>()
  for (const dir of dirs) {
    const files = (await readdir(`${LIB_DIR}/${dir}`))
        .filter(fn => fn.endsWith(ext))
        .map(fn => `${LIB_DIR}/${dir}/${fn}`)
        .sort()
    fileEntries.set(dir, files)
  }
  return fileEntries
}

function updateJsonLibContent(fileList: Record<string, any>, fn: string, fileEntries: Map<string, string[]>): void {
  assert(fileList && fileList['usr'] && fileList['usr']['lib'])
  fileList['usr']['lib'][fn] = Array.from(fileEntries.values()).flat()
}

async function updateJson(contentMap: Map<string, Map<string, string[]>>): Promise<void> {
  const content = await readFile(JSON_FN, 'utf8')
  const json = JSON.parse(content) as object

  contentMap.forEach((fileEntries, fn) => updateJsonLibContent(json, fn, fileEntries))

  const updated = JSON.stringify(json, null, 2)
  await writeFile(JSON_FN, `${updated}\n`)
}

async function updateLibSource(contentMap: Map<string, Map<string, string[]>>): Promise<void> {
  for (const [fn, fileEntries] of contentMap.entries()) {
    let files = new Array<string>()
    for (const dir of fileEntries.keys()) {
      files = files.concat(fileEntries.get(dir)!.map(fn => `#include "${fn.replace(LIB_DIR, '..')}"`))
    }

    await writeFile(`${DST_DIR}/${fn}`, `${files.join('\n')}\n`)
  }
}

async function main(): Promise<void> {
  const crt0Files = await readDirFiles(CRT0_DIRS)
  const libcFiles = await readDirFiles(LIBC_DIRS)

  const contentMap = new Map<string, Map<string, string[]>>([
    ['crt0.c', crt0Files],
    ['libc.c', libcFiles],
  ])
  updateJson(contentMap)
  updateLibSource(contentMap)
}

main()
