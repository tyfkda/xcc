#!/usr/bin/env node

const fs = require('fs')

const StructDefinition = {
  elfhdr: [
    {name: 'e_ident', type: 'string', bytes: 16},
    {name: 'e_type', type: 'ushort'},
    {name: 'e_machine', type: 'ushort'},
    {name: 'e_version', type: 'uint'},
    {name: 'e_entry', type: 'uintp'},
    {name: 'e_phoff', type: 'uintp'},
    {name: 'e_shoff', type: 'uintp'},
    {name: 'e_flags', type: 'uint'},
    {name: 'e_ehsize', type: 'ushort'},
    {name: 'e_phentsize', type: 'ushort'},
    {name: 'e_phnum', type: 'ushort'},
    {name: 'e_shentsize', type: 'ushort'},
    {name: 'e_shnum', type: 'ushort'},
    {name: 'e_shstrndx', type: 'ushort'},
  ],
  proghdr: [
    {name: 'p_type', type: 'uint32'},
    {name: 'p_flags', type: 'uint32'},
    {name: 'p_offset', type: 'uint64'},
    {name: 'p_vaddr', type: 'uint64'},
    {name: 'p_paddr', type: 'uint64'},
    {name: 'p_filesz', type: 'uint64'},
    {name: 'p_memsz', type: 'uint64'},
    {name: 'p_align', type: 'uint64'},
  ],
  Elf64_Shdr: [
    {name: 'sh_name', type: 'stroff'},
    {name: 'sh_type', type: 'Elf64_SectionType'},
    {name: 'sh_flags', type: 'Elf64_Xword'},
    {name: 'sh_addr', type: 'Elf64_Addr'},
    {name: 'sh_offset', type: 'Elf64_Off'},
    {name: 'sh_size', type: 'Elf64_Xword'},
    {name: 'sh_link', type: 'Elf64_Word'},
    {name: 'sh_info', type: 'Elf64_Word'},
    {name: 'sh_addralign', type: 'Elf64_Xword'},
    {name: 'sh_entsize', type: 'Elf64_Xword'},
  ],
}

const TypeInfos = {
  uint32_t: {size: 4, unsigned: true},
  int32_t: {size: 4},
  uint64_t: {size: 8, unsigned: true},
  int64_t: {size: 8},

  uchar: {size: 1, unsigned: true},
  ushort: {size: 2, unsigned: true},
  uint: {size: 4, unsigned: true},
  uintp: {size: 8, unsigned: true},
  uint32: {size: 4, unsigned: true},
  uint64: {size: 8, unsigned: true},

  Elf64_Word: {size: 4, unsigned: true},
  Elf64_Xword: {size: 8, unsigned: true},
  Elf64_Addr: {size: 8, unsigned: true},
  Elf64_Off: {size: 8, unsigned: true},

  Elf64_SectionType: {size: 4, unsigned: true},
  stroff: {size: 4, unsigned: true},

  cmd_t: {size: 4, unsigned: true},
}

const SectionTypeNames = new Map([
  [0, 'SHT_NULL'],
  [1, 'SHT_PROGBITS'],
  [2, 'SHT_SYMTAB'],
  [3, 'SHT_STRTAB'],
  [4, 'SHT_RELA'],
  [8, 'SHT_NOBITS'],
  [14, 'SHT_INIT_ARRAY'],
  [15, 'SHT_FINI_ARRAY'],
  [16, 'SHT_PREINIT_ARRAY'],
  [0x70000003, 'SHT_RISCV_ATTRIBUTES'],
])

class ElfAnalyzer {
  constructor() {
    this.header = null
    this.loadCommands = []
    this.data = null
    this.offset = 0
    this.sections = []
    this.strtbl = null
  }

  analyze(fileName) {
    this.setUpFile(fileName)

    this.header = this.readStruct('elfhdr')
    if (!this.isHeaderLegal(this.header)) {
      console.error('Error: not a ELF file')
      process.exit(1)
    }

    this.dumpStruct(this.header)
    for (let i = 0; i < this.header.e_phnum; ++i) {
      this.dumpStruct(this.readStruct('proghdr'))
    }

    if (this.header.e_shnum > 0) {
      this.offset = Number(this.header.e_shoff)
      this.sections = [...Array(this.header.e_shnum)].map(() => this.readStruct('Elf64_Shdr'))

      const strsec = this.sections[this.header.e_shstrndx]
      const offset = Number(strsec.sh_offset)
      this.strtbl = this.data.slice(offset, offset + Number(strsec.sh_size))

      this.sections.forEach(section => this.dumpStruct(section))
    }
  }

  setUpFile(fileName) {
    this.data = fs.readFileSync(fileName)
    this.offset = 0
  }

  isHeaderLegal(header) {
    return header.e_ident.slice(0, 4) === '\x7fELF'
  }

  dumpStruct(element) {
    console.log()
    this.dumpHex(element._rawBytes, element._startOffset)

    console.log(`# struct ${element._structName}`)
    const structMembers = StructDefinition[element._structName]
    for (const member of structMembers) {
      const {name, type} = member
      if (!/^(reserved|_)/.test(name)) {
        let value = element[name]
        switch (type) {
        case 'string':
          value = `"${value}"`
          break
        case 'Elf64_SectionType':
          {
            const n = SectionTypeNames.get(value)
            value = n ? `${n}(${value})` : value.toString()
          }
          break
        case 'stroff':
          {
            const end = this.strtbl.indexOf(0, value)
            const s = this.strtbl.slice(value, end).toString()
            value = `"${s}"`
          }
          break
        default:
          {
            const tinfo = TypeInfos[type]
            const hex = value.toString(16)
            switch (tinfo.size) {
            case 2:
              value = `0x${hex.padStart(4, '0')}`
              break
            case 4:
              value = `0x${hex.padStart(8, '0')}`
              break
            case 8:
              value = `0x${hex.padStart(16, '0')}`
              break
            default:
              throw new Error(`Unknown type: ${type}`)
            }
          }
          break
        }
        console.log(`#    .${name}: ${value},`)
      }
    }
    return element
  }

  dumpHex(array, start) {
    const lineBytes = 16
    const bytes = array.length
    let offset = 0
    while (offset < bytes) {
      let s = 0
      let n
      if (offset === 0) {
        s = start % lineBytes
        n = Math.min(bytes, lineBytes - s)
      } else {
        n = Math.min(bytes - offset, lineBytes)
      }
      const bin = [...Array(n)].map((_, i) => array[offset + i].toString(16).padStart(2, '0')).join(' ')
      const chr = [...Array(n)].map((_, i) => this.toChar(array[offset + i])).join('')

      let binStr = bin
      let chrStr = chr
      if (s > 0) {
        binStr = '   '.repeat(s) + binStr
        chrStr = ' '.repeat(s) + chrStr
      }
      const spaces = ' '.repeat(lineBytes * 3 + 1 - binStr.length)
      const address = (start + offset).toString(16).padStart(8, '0')
      console.log(`${address}: ${binStr}${spaces}${chrStr}`)
      offset += n
    }
  }

  toChar(byte) {
    return byte >= 0x20 && byte <= 0x7e ? String.fromCharCode(byte) : '.'
  }

  readStruct(structName) {
    const structMembers = StructDefinition[structName]
    if (!structMembers) {
      throw new Error(`Unknown struct: ${structName}`)
    }

    const startOffset = this.offset
    const result = {}
    let offset = startOffset
    for (const member of structMembers) {
      const bytes = member.type === 'string' ? member.bytes : TypeInfos[member.type].size
      result[member.name] = ElfAnalyzer.readValue(this.data, offset, member)
      offset += bytes
    }
    result._structName = structName
    result._startOffset = startOffset
    result._rawBytes = this.data.slice(result._startOffset, offset)
    this.offset = offset
    return result
  }

  static readValue(data, offset, member) {
    const {type} = member
    switch (type) {
    case 'string':
      {
        const s = data.slice(offset, offset + member.bytes)
        const nullPos = s.indexOf(0)
        return s.toString('utf8', 0, nullPos !== -1 ? nullPos : s.length)
      }
    default:
      {
        const tinfo = TypeInfos[type]
        switch (tinfo.size) {
        case 2:
          return tinfo.unsigned ? data.readUInt16LE(offset) : data.readInt16LE(offset)
        case 4:
          return tinfo.unsigned ? data.readUInt32LE(offset) : data.readInt32LE(offset)
        case 8:
          return tinfo.unsigned ? data.readBigUInt64LE(offset) : data.readBigInt64LE(offset)
        default:
          throw new Error(`Unknown type: ${member.type}`)
        }
      }
    }
  }
}

if (require.main === module) {
  const args = process.argv.slice(2)
  if (args.length !== 1) {
    console.error('Usage: node elf-analyzer.js <path-to-elf-o-file>')
    process.exit(1)
  }

  const analyzer = new ElfAnalyzer()
  analyzer.analyze(args[0])
}
