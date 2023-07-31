import fs from 'node:fs/promises'

async function readDirFiles(dirs: string[], baseDir?: string, ext: string = '.c'): Promise<Array<string>> {
  const allFiles = new Array<string>()
  for (let dir of dirs) {
    if (baseDir != null)
      dir = `${baseDir}/${dir}`
    const files = (await fs.readdir(dir))
        .filter(fn => fn.endsWith(ext))
        .map(fn => `${dir}/${fn}`)
        .sort()
    allFiles.push(...files)
  }
  return allFiles
}

async function main(): Promise<void> {
  let baseDir: string | undefined

  const program = require('commander')
  program
    .option('--base <directory>', 'Specify base directory', (value: string) => baseDir = value)
    .description('Output source file which includes all .c files')
    .usage('[options] <directories...>')
    .parse(process.argv)

  const dirs = program.args
  if (dirs.length <= 0) {
    program.help()
    process.exit(1)
  }

  const fileEntries = await readDirFiles(dirs, baseDir)
  for (let fn of fileEntries) {
    if (baseDir != null)
      fn = fn.replace(`${baseDir}/`, '')
    console.log(`#include <${fn}>`)
  }
}

main()
