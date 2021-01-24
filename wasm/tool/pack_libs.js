'use strict'

const {assert} = require('console')
const fs = require('fs')

function mapFileJson(json) {
  switch (typeof json) {
  case 'string':
    return fs.readFileSync(json, 'utf8')
  case 'object':
    if (Array.isArray(json)) {
      const files = json.map((fn) => fs.readFileSync(fn, 'utf8'))
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
