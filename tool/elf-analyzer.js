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
  Elf64_Sym: [
    {name: 'st_name', type: 'symstroff'},
    {name: 'st_info', type: 'uchar'},
    {name: 'st_other', type: 'uchar'},
    {name: 'st_shndx', type: 'Elf64_Section'},
    {name: 'st_value', type: 'Elf64_Addr'},
    {name: 'st_size', type: 'Elf64_Xword'},
  ],
  Elf64_Rela: [
    {name: 'r_offset', type: 'Elf64_Addr'},
    {name: 'r_info', type: 'RINFO'},
    {name: 'r_addend', type: 'Elf64_Sxword'},
  ],
}

const SHT_NULL          = 0
const SHT_PROGBITS      = 1
const SHT_SYMTAB        = 2
const SHT_STRTAB        = 3
const SHT_RELA          = 4
const SHT_HASH          = 5
const SHT_DYNAMIC       = 6
const SHT_NOTE          = 7
const SHT_NOBITS        = 8
const SHT_REL           = 9
const SHT_SHLIB         = 10
const SHT_DYNSYM        = 11
const SHT_INIT_ARRAY    = 14
const SHT_FINI_ARRAY    = 15
const SHT_PREINIT_ARRAY = 16

const SHT_LOPROC        = 0x70000000
const SHT_HIPROC        = 0x7fffffff
const SHT_RISCV_ATTRIBUTES = SHT_LOPROC + 3

const CommandNameTable = {
  [SHT_SYMTAB]: 'SHT_SYMTAB',
}

const TypeInfos = {
  uint8_t: {size: 1, unsigned: true},
  int8_t: {size: 1},
  uint16_t: {size: 2, unsigned: true},
  int16_t: {size: 2},
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
  Elf64_Sxword: {size: 8},
  Elf64_Addr: {size: 8, unsigned: true},
  Elf64_Off: {size: 8, unsigned: true},

  Elf64_SectionType: {size: 4, unsigned: true},
  stroff: {size: 4, unsigned: true},
  symstroff: {size: 4, unsigned: true},

  Elf64_Section: {size: 2, unsigned: true},

  cmd_t: {size: 4, unsigned: true},
  RINFO: {size: 8, unsigned: true},
}

const SectionTypeNames = new Map([
  [SHT_NULL, 'SHT_NULL'],
  [SHT_PROGBITS, 'SHT_PROGBITS'],
  [SHT_SYMTAB, 'SHT_SYMTAB'],
  [SHT_STRTAB, 'SHT_STRTAB'],
  [SHT_RELA, 'SHT_RELA'],
  [SHT_HASH, 'SHT_HASH'],
  [SHT_DYNAMIC, 'SHT_DYNAMIC'],
  [SHT_NOTE, 'SHT_NOTE'],
  [SHT_NOBITS, 'SHT_NOBITS'],
  [SHT_REL, 'SHT_REL'],
  [SHT_SHLIB, 'SHT_SHLIB'],
  [SHT_DYNSYM, 'SHT_DYNSYM'],
  [SHT_INIT_ARRAY, 'SHT_INIT_ARRAY'],
  [SHT_FINI_ARRAY, 'SHT_FINI_ARRAY'],
  [SHT_PREINIT_ARRAY, 'SHT_PREINIT_ARRAY'],
  [SHT_RISCV_ATTRIBUTES, 'SHT_RISCV_ATTRIBUTES'],
])

function bigIntToNumber(v) {
  const n = Number(v)
  return Number.isSafeInteger(n) ? n : v
}

class ElfAnalyzer {
  header = null
  loadCommands = []
  data = null
  offset = 0
  sections = []
  strtbl = null

  constructor() {
  }

  analyze(fileName) {
    this.setUpFile(fileName)

    this.header = this.readStruct('elfhdr')
    if (!this.isHeaderLegal(this.header)) {
      console.error('Error: not a ELF file')
      process.exit(1)
    }

    this.dumpHexAndStruct(this.header)
    for (let i = 0; i < this.header.e_phnum; ++i) {
      this.dumpHexAndStruct(this.readStruct('proghdr'))
    }

    if (this.header.e_shnum > 0) {
      this.offset = Number(this.header.e_shoff)
      this.sections = [...Array(this.header.e_shnum)].map(() => this.readStruct('Elf64_Shdr'))

      const strsec = this.sections[this.header.e_shstrndx]
      const offset = Number(strsec.sh_offset)
      this.strtbl = this.data.slice(offset, offset + Number(strsec.sh_size))
    }

    const blocks = []
    for (const section of this.sections) {
      const block = this.analyzeSectionData(section)
      if (block != null) {
        block.section = section
        blocks.push(block)
      }
    }

    blocks.sort((a, b) => a.section.sh_offset - b.section.sh_offset)

    for (const block of blocks) {
      let type = block.type
      const name = CommandNameTable[type] || type
      const section = block.section
      const offset = section.sh_offset
      const size = section.sh_size

      if (offset > this.offset) {
        const slice = this.data.slice(this.offset, offset)
        if (slice.some(b => b !== 0)) {
          console.error(`Warning: data is not empty between 0x${this.offset.toString(16)} and 0x${offset.toString(16)}`)
        }
      }

      this.currentSection = section

      console.log()
      this.dumpHex(this.data.slice(offset, offset + size), offset)
      console.log(`# ${name}`)
      if (block.arrayCount != null) {
        block.elements.forEach((element) => this.dumpStruct(element))
      }

      this.offset = offset + size
    }

    this.sections.forEach(section => this.dumpHexAndStruct(section))
  }

  setUpFile(fileName) {
    this.data = fs.readFileSync(fileName)
    this.offset = 0
  }

  isHeaderLegal(header) {
    return header.e_ident.slice(0, 4) === '\x7fELF'
  }

  analyzeSectionData(section) {
    let size = section.sh_size
    if (size <= 0)
      return null

    this.offset = section.sh_offset
    switch (section.sh_type) {
    case SHT_SYMTAB:
      {
        const elements = []
        const arrayCount = (size / 24) | 0
        for (let i = 0; i < arrayCount; ++i) {
          elements.push(this.readStruct('Elf64_Sym'))
        }
        return {type: `symtab`, arrayCount, elements}
      }
    case SHT_RELA:
      {
        const elements = []
        const arrayCount = (size / 24) | 0
        for (let i = 0; i < arrayCount; ++i) {
          elements.push(this.readStruct('Elf64_Rela'))
        }
        return {type: `symtab`, arrayCount, elements}
      }

    default:
      {
        let type = SectionTypeNames.get(section.sh_type)
        const m = type.match(/^SHT_(.*)/)
        if (m)
          type = m[1].toLowerCase()
        return {type}
      }
    }
  }

  dumpHexAndStruct(element) {
    console.log()
    this.dumpHex(element._rawBytes, element._startOffset)
    this.dumpStruct(element)
  }

  dumpStruct(element) {
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
        case 'symstroff':
          {
            const section = this.currentSection
            const strtab = this.sections[section.sh_link]
            const start = strtab.sh_offset + value
            const end = this.data.indexOf(0, start)
            const s = this.data.slice(start, end).toString()
            value = `"${s}" (0x${value.toString(16)})`
          }
          break
        case 'RINFO':
          {
            const sym = (value / (2 ** 32)) | 0
            const type = value & 0xffffffff
            value = `{sym=${sym}, type=${type}}`
          }
          break
        default:
          {
            const tinfo = TypeInfos[type]
            const hex = value.toString(16)
            switch (tinfo.size) {
            case 1:
              value = `0x${hex.padStart(2, '0')}`
              break
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
        case 1:
          return tinfo.unsigned ? data.readUInt8(offset) : data.readInt8(offset)
        case 2:
          return tinfo.unsigned ? data.readUInt16LE(offset) : data.readInt16LE(offset)
        case 4:
          return tinfo.unsigned ? data.readUInt32LE(offset) : data.readInt32LE(offset)
        case 8:
          return bigIntToNumber(tinfo.unsigned ? data.readBigUInt64LE(offset)
                                               : data.readBigInt64LE(offset))
        default:
          throw new Error(`Unknown type: ${type}`)
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
