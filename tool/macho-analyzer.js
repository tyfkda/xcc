#!/usr/bin/env node

const fs = require('fs')

const LC_REQ_DYLD = 0x80000000

const LC_SEGMENT = 0x01
const LC_SYMTAB = 0x02
const LC_SYMSEG = 0x03
const LC_THREAD = 0x04
const LC_UNIXTHREAD = 0x05
const LC_LOADFVMLIB = 0x06
const LC_IDFVMLIB = 0x07
const LC_IDENT = 0x08
const LC_FVMFILE = 0x09
const LC_PREPAGE = 0x0a
const LC_DYSYMTAB = 0x0b
const LC_LOAD_DYLIB = 0x0c
const LC_ID_DYLIB = 0x0d
const LC_LOAD_DYLINKER = 0x0e
const LC_ID_DYLINKER = 0x0f
const LC_PREBOUND_DYLIB = 0x10
const LC_ROUTINES = 0x11
const LC_SUB_FRAMEWORK = 0x12
const LC_SUB_UMBRELLA = 0x13
const LC_SUB_CLIENT = 0x14
const LC_SUB_LIBRARY = 0x15
const LC_TWOLEVEL_HINTS = 0x16
const LC_PREBIND_CKSUM = 0x17
const LC_LOAD_WEAK_DYLIB = 0x18 | LC_REQ_DYLD
const LC_SEGMENT_64 = 0x19
const LC_ROUTINES_64 = 0x1a
const LC_UUID = 0x1b
const LC_RPATH = 0x1c | LC_REQ_DYLD
const LC_CODE_SIGNATURE = 0x1d
const LC_SEGMENT_SPLIT_INFO = 0x1e
const LC_REEXPORT_DYLIB = 0x1f | LC_REQ_DYLD
const LC_LAZY_LOAD_DYLIB = 0x20
const LC_ENCRYPTION_INFO = 0x21
const LC_DYLD_INFO = 0x22
const LC_DYLD_INFO_ONLY = 0x22 | LC_REQ_DYLD
const LC_LOAD_UPWARD_DYLIB = 0x23 | LC_REQ_DYLD
const LC_VERSION_MIN_MACOSX = 0x24
const LC_VERSION_MIN_IPHONEOS = 0x25
const LC_FUNCTION_STARTS = 0x26
const LC_DYLD_ENVIRONMENT = 0x27
const LC_MAIN = 0x28 | LC_REQ_DYLD
const LC_DATA_IN_CODE = 0x29
const LC_SOURCE_VERSION = 0x2a
const LC_DYLIB_CODE_SIGN_DRS = 0x2b
const LC_ENCRYPTION_INFO_64 = 0x2c
const LC_LINKER_OPTION = 0x2d
const LC_LINKER_OPTIMIZATION_HINT = 0x2e
const LC_VERSION_MIN_TVOS = 0x2f
const LC_VERSION_MIN_WATCHOS = 0x30
const LC_NOTE = 0x31
const LC_BUILD_VERSION = 0x32
const LC_DYLD_EXPORTS_TRIE = 0x33 | LC_REQ_DYLD
const LC_DYLD_CHAINED_FIXUPS = 0x34 | LC_REQ_DYLD
const LC_FILESET_ENTRY = 0x35 | LC_REQ_DYLD
const LC_ATOM_INFO = 0x36

const CommandNameTable = {
  [LC_SEGMENT]: 'LC_SEGMENT', [LC_SYMTAB]: 'LC_SYMTAB', [LC_SYMSEG]: 'LC_SYMSEG',
  [LC_THREAD]: 'LC_THREAD', [LC_UNIXTHREAD]: 'LC_UNIXTHREAD', [LC_LOADFVMLIB]: 'LC_LOADFVMLIB',
  [LC_IDFVMLIB]: 'LC_IDFVMLIB', [LC_IDENT]: 'LC_IDENT', [LC_FVMFILE]: 'LC_FVMFILE',
  [LC_PREPAGE]: 'LC_PREPAGE', [LC_DYSYMTAB]: 'LC_DYSYMTAB', [LC_LOAD_DYLIB]: 'LC_LOAD_DYLIB',
  [LC_ID_DYLIB]: 'LC_ID_DYLIB', [LC_LOAD_DYLINKER]: 'LC_LOAD_DYLINKER',
  [LC_ID_DYLINKER]: 'LC_ID_DYLINKER', [LC_PREBOUND_DYLIB]: 'LC_PREBOUND_DYLIB',
  [LC_ROUTINES]: 'LC_ROUTINES', [LC_SUB_FRAMEWORK]: 'LC_SUB_FRAMEWORK',
  [LC_SUB_UMBRELLA]: 'LC_SUB_UMBRELLA', [LC_SUB_CLIENT]: 'LC_SUB_CLIENT',
  [LC_SUB_LIBRARY]: 'LC_SUB_LIBRARY', [LC_TWOLEVEL_HINTS]: 'LC_TWOLEVEL_HINTS',
  [LC_PREBIND_CKSUM]: 'LC_PREBIND_CKSUM', [LC_LOAD_WEAK_DYLIB]: 'LC_LOAD_WEAK_DYLIB',
  [LC_SEGMENT_64]: 'LC_SEGMENT_64', [LC_ROUTINES_64]: 'LC_ROUTINES_64', [LC_UUID]: 'LC_UUID',
  [LC_RPATH]: 'LC_RPATH', [LC_CODE_SIGNATURE]: 'LC_CODE_SIGNATURE',
  [LC_SEGMENT_SPLIT_INFO]: 'LC_SEGMENT_SPLIT_INFO', [LC_REEXPORT_DYLIB]: 'LC_REEXPORT_DYLIB',
  [LC_LAZY_LOAD_DYLIB]: 'LC_LAZY_LOAD_DYLIB', [LC_ENCRYPTION_INFO]: 'LC_ENCRYPTION_INFO',
  [LC_DYLD_INFO]: 'LC_DYLD_INFO', [LC_DYLD_INFO_ONLY]: 'LC_DYLD_INFO_ONLY',
  [LC_LOAD_UPWARD_DYLIB]: 'LC_LOAD_UPWARD_DYLIB', [LC_VERSION_MIN_MACOSX]: 'LC_VERSION_MIN_MACOSX',
  [LC_VERSION_MIN_IPHONEOS]: 'LC_VERSION_MIN_IPHONEOS', [LC_FUNCTION_STARTS]: 'LC_FUNCTION_STARTS',
  [LC_DYLD_ENVIRONMENT]: 'LC_DYLD_ENVIRONMENT', [LC_MAIN]: 'LC_MAIN',
  [LC_DATA_IN_CODE]: 'LC_DATA_IN_CODE', [LC_SOURCE_VERSION]: 'LC_SOURCE_VERSION',
  [LC_DYLIB_CODE_SIGN_DRS]: 'LC_DYLIB_CODE_SIGN_DRS', [LC_ENCRYPTION_INFO_64]: 'LC_ENCRYPTION_INFO_64',
  [LC_LINKER_OPTION]: 'LC_LINKER_OPTION', [LC_LINKER_OPTIMIZATION_HINT]: 'LC_LINKER_OPTIMIZATION_HINT',
  [LC_VERSION_MIN_TVOS]: 'LC_VERSION_MIN_TVOS', [LC_VERSION_MIN_WATCHOS]: 'LC_VERSION_MIN_WATCHOS',
  [LC_NOTE]: 'LC_NOTE', [LC_BUILD_VERSION]: 'LC_BUILD_VERSION',
  [LC_DYLD_EXPORTS_TRIE]: 'LC_DYLD_EXPORTS_TRIE', [LC_DYLD_CHAINED_FIXUPS]: 'LC_DYLD_CHAINED_FIXUPS',
  [LC_FILESET_ENTRY]: 'LC_FILESET_ENTRY', [LC_ATOM_INFO]: 'LC_ATOM_INFO',
}

const LoadCommandStructName = {
  [LC_SYMTAB]: 'symtab_command',
  [LC_SEGMENT_64]: 'segment_command_64',
  [LC_DYSYMTAB]: 'dysymtab_command',
  [LC_LOAD_DYLINKER]: 'dylinker_command',
  [LC_DYLD_EXPORTS_TRIE]: 'linkedit_data_command',
  [LC_DYLD_CHAINED_FIXUPS]: 'linkedit_data_command',
  [LC_UUID]: 'uuid_command',
  [LC_BUILD_VERSION]: 'build_version_command',
  [LC_SOURCE_VERSION]: 'source_version_command',
  [LC_MAIN]: 'entry_point_command',
  [LC_LOAD_DYLIB]: 'dylib_command',
  [LC_FUNCTION_STARTS]: 'linkedit_data_command',
  [LC_DATA_IN_CODE]: 'linkedit_data_command',
  [LC_CODE_SIGNATURE]: 'linkedit_data_command',
}

const StructDefinition = {
  mach_header_64: [
    {name: 'magic', type: 'uint32_t'},
    {name: 'cputype', type: 'int32_t'},
    {name: 'cpusubtype', type: 'int32_t'},
    {name: 'filetype', type: 'uint32_t'},
    {name: 'ncmds', type: 'uint32_t'},
    {name: 'sizeofcmds', type: 'uint32_t'},
    {name: 'flags', type: 'uint32_t'},
    {name: 'reserved', type: 'uint32_t'},
  ],
  load_command: [
    {name: 'cmd', type: 'cmd_t'},
    {name: 'cmdsize', type: 'uint32_t'},
  ],
  segment_command_64: [
    {name: 'cmd', type: 'cmd_t'},
    {name: 'cmdsize', type: 'uint32_t'},
    {name: 'segname', type: 'string', bytes: 16},
    {name: 'vmaddr', type: 'uint64_t'},
    {name: 'vmsize', type: 'uint64_t'},
    {name: 'fileoff', type: 'uint64_t'},
    {name: 'filesize', type: 'uint64_t'},
    {name: 'maxprot', type: 'int32_t'},
    {name: 'initprot', type: 'int32_t'},
    {name: 'nsects', type: 'uint32_t'},
    {name: 'flags', type: 'uint32_t'},
  ],
  linkedit_data_command: [
    {name: 'cmd', type: 'cmd_t'},
    {name: 'cmdsize', type: 'uint32_t'},
    {name: 'dataoff', type: 'uint32_t'},
    {name: 'datasize', type: 'uint32_t'},
  ],
  section_64: [
    {name: 'sectname', type: 'string', bytes: 16},
    {name: 'segname', type: 'string', bytes: 16},
    {name: 'addr', type: 'uint64_t'},
    {name: 'size', type: 'uint64_t'},
    {name: 'offset', type: 'uint32_t'},
    {name: 'align', type: 'uint32_t'},
    {name: 'reloff', type: 'uint32_t'},
    {name: 'nreloc', type: 'uint32_t'},
    {name: 'flags', type: 'uint32_t'},
    {name: 'reserved1', type: 'uint32_t'},
    {name: 'reserved2', type: 'uint32_t'},
    {name: 'reserved3', type: 'uint32_t'},
  ],
  symtab_command: [
    {name: 'cmd', type: 'cmd_t'},
    {name: 'cmdsize', type: 'uint32_t'},
    {name: 'symoff', type: 'uint32_t'},
    {name: 'nsyms', type: 'uint32_t'},
    {name: 'stroff', type: 'uint32_t'},
    {name: 'strsize', type: 'uint32_t'},
  ],
  dysymtab_command: [
    {name: 'cmd', type: 'cmd_t'},
    {name: 'cmdsize', type: 'uint32_t'},
    {name: 'ilocalsym', type: 'uint32_t'},
    {name: 'nlocalsym', type: 'uint32_t'},
    {name: 'iextdefsym', type: 'uint32_t'},
    {name: 'nextdefsym', type: 'uint32_t'},
    {name: 'iundefsym', type: 'uint32_t'},
    {name: 'nundefsym', type: 'uint32_t'},
    {name: 'tocoff', type: 'uint32_t'},
    {name: 'ntoc', type: 'uint32_t'},
    {name: 'modtaboff', type: 'uint32_t'},
    {name: 'nmodtab', type: 'uint32_t'},
    {name: 'extrefsymoff', type: 'uint32_t'},
    {name: 'nextrefsyms', type: 'uint32_t'},
    {name: 'indirectsymoff', type: 'uint32_t'},
    {name: 'nindirectsyms', type: 'uint32_t'},
    {name: 'extreloff', type: 'uint32_t'},
    {name: 'nextrel', type: 'uint32_t'},
    {name: 'locreloff', type: 'uint32_t'},
    {name: 'nlocrel', type: 'uint32_t'},
  ],
  dylinker_command: [
    {name: 'cmd', type: 'cmd_t'},
    {name: 'cmdsize', type: 'uint32_t'},
    {name: 'name', type: 'uint32_t'},
  ],
  uuid_command: [
    {name: 'cmd', type: 'cmd_t'},
    {name: 'cmdsize', type: 'uint32_t'},
    {name: 'uuid', type: 'string', bytes: 16},
  ],
  build_version_command: [
    {name: 'cmd', type: 'cmd_t'},
    {name: 'cmdsize', type: 'uint32_t'},
    {name: 'platform', type: 'uint32_t'},
    {name: 'minos', type: 'uint32_t'},
    {name: 'sdk', type: 'uint32_t'},
    {name: 'ntools', type: 'uint32_t'},
  ],
  source_version_command: [
    {name: 'cmd', type: 'cmd_t'},
    {name: 'cmdsize', type: 'uint32_t'},
    {name: 'version', type: 'uint64_t'},
  ],
  entry_point_command: [
    {name: 'cmd', type: 'cmd_t'},
    {name: 'cmdsize', type: 'uint32_t'},
    {name: 'entryoff', type: 'uint64_t'},
    {name: 'stacksize', type: 'uint64_t'},
  ],
  dylib_command: [
    {name: 'cmd', type: 'cmd_t'},
    {name: 'cmdsize', type: 'uint32_t'},
    {name: 'name', type: 'uint32_t'},
    {name: 'timestamp', type: 'uint32_t'},
    {name: 'current_version', type: 'uint32_t'},
    {name: 'compatibility_version', type: 'uint32_t'},
  ],
}

const TypeInfos = {
  uint32_t: {size: 4, unsigned: true},
  int32_t: {size: 4},
  uint64_t: {size: 8, unsigned: true},
  int64_t: {size: 8},
  cmd_t: {size: 4, unsigned: true},
}

class MachAnalyzer {
  constructor() {
    this.header = null
    this.loadCommands = []
    this.data = null
    this.offset = 0
  }

  analyze(fileName) {
    this.setUpFile(fileName)

    this.header = this.readStruct('mach_header_64')
    if (!this.isHeaderLegal(this.header)) {
      console.error('Error: not a Mach-O file')
      process.exit(1)
    }

    this.dumpStruct(this.header)

    this.loadCommands = []
    const ncmds = this.header.ncmds
    const sections = []
    const blocks = []
    for (let i = 0; i < ncmds; ++i) {
      const command = this.readLoadCommand()
      this.loadCommands.push(command)

      this.dumpStruct(command)
      switch (command._structName) {
      case 'segment_command_64':
        for (let j = 0; j < command.nsects; ++j) {
          const section = this.readStruct('section_64')
          sections.push(section)
          this.dumpStruct(section)
          blocks.push({offset: section.offset, size: Number(section.size), type: section.sectname})
          if (section.nreloc > 0) {
            blocks.push({offset: section.reloff, size: section.nreloc * 8, type: 'reloc'})
          }
        }
        break
      case 'linkedit_data_command':
        if (command.datasize > 0) {
          blocks.push({offset: command.dataoff, size: command.datasize, type: CommandNameTable[command.cmd]})
        }
        break
      case 'symtab_command':
        blocks.push({offset: command.symoff, size: 0x10 * command.nsyms, type: command.cmd})
        blocks.push({offset: command.stroff, size: command.strsize, type: 'string'})
        break
      case 'dysymtab_command':
        if (command.indirectsymoff !== 0) {
          blocks.push({offset: command.indirectsymoff, size: 0x04 * command.nindirectsyms, type: 'indirectsym'})
        }
        break
      }

      const nextOffset = command._startOffset + command.cmdsize
      if (nextOffset > this.offset) {
        this.dumpHex(this.data.slice(this.offset, nextOffset), this.offset)
      }
      this.offset = nextOffset
    }

    blocks.sort((a, b) => a.offset - b.offset)

    for (const block of blocks) {
      const name = CommandNameTable[block.type] || block.type
      const offset = block.offset
      const size = block.size

      if (offset > this.offset) {
        const slice = this.data.slice(this.offset, offset)
        if (slice.some(b => b !== 0)) {
          console.error(`Warning: data is not empty between 0x${this.offset.toString(16)} and 0x${offset.toString(16)}`)
        }
      }

      console.log()
      this.dumpHex(this.data.slice(offset, offset + size), offset)
      console.log(`# ${name}`)

      this.offset = offset + size
    }
  }

  setUpFile(fileName) {
    this.data = fs.readFileSync(fileName)
    this.offset = 0
  }

  isHeaderLegal(header) {
    const magic64 = 0xfeedfacf
    return header.magic === magic64
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
        case 'cmd_t':
          value = `${CommandNameTable[value]} (0x${value.toString(16)})`
          break
        case 'string':
          value = JSON.stringify(value)
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

  readLoadCommand() {
    const offset = this.offset
    const loadCommand = this.readStruct('load_command')
    const loadCommandType = LoadCommandStructName[loadCommand.cmd]
    if (!loadCommandType) {
      throw new Error(`Unknown load command: ${loadCommand.cmd}`)
    }
    this.offset = offset
    return this.readStruct(loadCommandType)
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
      result[member.name] = MachAnalyzer.readValue(this.data, offset, member)
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
    console.error('Usage: node macho-analyzer.js <path-to-mach-o-file>')
    process.exit(1)
  }

  const analyzer = new MachAnalyzer()
  analyzer.analyze(args[0])
}
