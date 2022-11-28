'use strict'

const {assert} = require('console')
const fs = require('fs')

function replaceRelativeInclude(content) {
  return content.replace(/^(#include\s+)"\..+\/([\w\d\-_.]+)"$/gm, '$1"$2"')
}

function readFile(path) {
  const content = fs.readFileSync(path, 'utf8')
  return replaceRelativeInclude(content)
}

function mapFileJson(json) {
  switch (typeof json) {
  case 'string':
    return readFile(json)
  case 'object':
    if (Array.isArray(json)) {
      const files = json.map((fn) => readFile(fn))
      return files.join('\n')
    } else {
      Object.keys(json).map(key => json[key] = mapFileJson(json[key]))
      return json
    }
  default:
    assert(false)
    break
  }
}

function main() {
  const argv = process.argv
  if (argv.length < 3) {
    console.error('argv < 3')
    process.exit(1)
  }

  const fn = argv[2]
  const content = fs.readFileSync(fn, 'utf8')
  const json = JSON.parse(content)
  const result = mapFileJson(json)
  console.log(JSON.stringify(result))
}

main()
