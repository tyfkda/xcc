import { assert } from 'console'
import fs from 'node:fs/promises'
import path from 'path'

const JSON_FN = 'src/wcc/www/lib_list.json'

async function readIncludeFiles(fn: string): Promise<Array<string>> {
  const content = await fs.readFile(fn, 'utf8')
  const files = content.split('\n')
    .map(line => {
      const m = line.match(/^#include\s+<(.+)>$/)
      return m ? m[1] : null
    })
    .filter(src => src != null) as Array<string>
  return files
}

function updateJsonLibContent(fileList: Record<string, any>, fn: string, fileEntries: Array<string>): void {
  assert(fileList && fileList['usr'] && fileList['usr']['lib'])
  fileList['usr']['lib'][fn] = fileEntries
}

async function updateJson(contentMap: Map<string, Array<string>>): Promise<void> {
  const content = await fs.readFile(JSON_FN, 'utf8')
  const json = JSON.parse(content) as object

  contentMap.forEach((fileEntries, fn) => updateJsonLibContent(json, fn, fileEntries))

  const updated = JSON.stringify(json, null, 2)
  await fs.writeFile(JSON_FN, `${updated}\n`)
}

async function main(): Promise<void> {
  let baseDir: string | undefined

  const program = require('commander')
  program
    .option('--base <directory>', 'Specify base directory', (value: string) => baseDir = value)
    .description('Update lib_list.json')
    .usage('[options] <include-only c-sources...>')
    .parse(process.argv)

  const files = program.args
  if (files.length <= 0) {
    program.help()
    process.exit(1)
  }

  const contentMap = new Map<string, Array<string>>()
  for (const fn of files) {
    let files = await readIncludeFiles(fn)
    if (baseDir != null) {
      files = files.map(fn => `${baseDir}/${fn}`)
    }
    contentMap.set(path.basename(fn), files)
  }
  updateJson(contentMap)
}

main()
