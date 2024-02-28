'use strict'

const {assert} = require('console')
const fsPromise = require('fs').promises
const {zip} = require('fflate')

function isBinFile(fileName) {
  return fileName.match(/\.(wasm|a)$/) != null
}

function replaceRelativeInclude(content) {
  return content.replace(/^(#include\s+)"\..+\/([\w\d\-_.]+)"$/gm, '$1"$2"')
}

async function readFile(path) {
  if (isBinFile(path)) {
    return await fsPromise.readFile(path)
  } else {
    const text = await fsPromise.readFile(path, 'utf8')
    return replaceRelativeInclude(text)
  }
}

async function collectFiles(json) {
  switch (typeof json) {
  case 'string':
    return await readFile(json)
  case 'object':
    if (Array.isArray(json)) {
      const contents = await Promise.all(json.map((fn) => readFile(fn)))
      return contents.join('\n')
    } else {
      const files = {}
      await Promise.all(Object.keys(json).map(async key => {
        let result = await collectFiles(json[key])
        if (result != null) {
          if (typeof result === 'string') {
            // Convert to Uint8Array.
            result = new TextEncoder().encode(result)
          }
          files[key] = result
        }
      }))
      return files
    }
    break
  default:
    assert(false)
    break
  }
  return null
}

async function main() {
  const argv = process.argv
  if (argv.length < 4) {
    console.error('argv < 4')
    process.exit(1)
  }

  const fn = argv[2]
  const dstfn = argv[3]

  const content = await fsPromise.readFile(fn, 'utf8')
  const json = JSON.parse(content)
  const files = await collectFiles(json)
  zip(files, {level: 9}, async (err, data) => {
    if (err) {
      console.error(err)
      process.exit(1)
    }
    await fsPromise.writeFile(dstfn, data)
  })
}

main()
