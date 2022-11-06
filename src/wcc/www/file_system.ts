import {Util} from './util'

const enum OpenFlag {
  RDONLY  = 0x000,
  WRONLY  = 0x001,
  RDWR    = 0x002,
  CREAT   = 0x040,  //  0100
  TRUNC   = 0x200,  // 01000
  // APPEND  = 0x400,  // 02000
}

const enum SeekWhere {
  SET = 0,
  CUR = 1,
  END = 2,
}

const kOpenFlags = {}
kOpenFlags[OpenFlag.RDONLY] = 'r'
kOpenFlags[OpenFlag.WRONLY] = 'w'
kOpenFlags[OpenFlag.RDWR] = 'w+'
kOpenFlags[OpenFlag.WRONLY | OpenFlag.CREAT | OpenFlag.TRUNC] = 'w'

export class WaStorage {
  private files = {}

  public putFile(path: string, content: string|Uint8Array): void {
    if (typeof(content) === 'string')
      content = Util.encode(content)
    console.assert(content.constructor === Uint8Array, content)
    this.files[path] = content
  }

  public getFile(path: string): Uint8Array|null {
    return this.files[path]
  }

  public contains(path: string): boolean {
    return path in this.files
  }

  public delete(path: string) {
    delete this.files[path]
  }
}

export class FileSystem {
  private fileDescs: Array<any>

  constructor(private storage: WaStorage) {
    this.fileDescs = ['stdin', 'stdout', 'stderr']
  }

  public saveFile(path: string, content: string|Uint8Array): void {
    this.storage.putFile(path, content)
  }

  public loadFile(path: string): Uint8Array|null {
    return this.storage.getFile(path)
  }

  public open(absPath: string, flag: number, _mode: number): number {
    if (absPath == null || absPath.length === 0)
      return -1
    if ((flag & (OpenFlag.WRONLY | OpenFlag.RDWR)) === 0) {
      if (!this.storage.contains(absPath))
        return -1
    }

    const flagStr = kOpenFlags[flag]
    if (flagStr == null) {
      console.error(`Unsupported open flag: ${flag}`)
      process.exit(1)
    }
    const fd = this.allocFd()
    const desc: any = {
      absPath,
      rp: 0,
    }
    if ((flag & (OpenFlag.WRONLY | OpenFlag.RDWR)) !== 0) {
      desc.write = []
      desc.writeTotal = 0
    }
    this.fileDescs[fd] = desc
    return fd
  }

  public close(fd: number): number {
    if (this.fileDescs[fd] == null)
      return -1
    this.commitDesc(fd)
    this.fileDescs[fd] = null
    return 0
  }

  public read(fd: number, buffer: Uint8Array): number {
    if (fd < 0 || fd >= this.fileDescs.length)
      return 0
    const desc = this.fileDescs[fd]
    if (desc == null)
      return 0
    const file = desc.absPath != null ? this.storage.getFile(desc.absPath) : desc.written
    if (file == null || desc.rp >= file.length)
      return 0

    const end = Math.min(file.length, desc.rp + buffer.length)
    buffer.set(file.subarray(desc.rp, end))
    const readSize = end - desc.rp
    desc.rp = end
    return readSize
  }

  public write(fd: number, buffer: Uint8Array): number {
    if (fd < 0 || fd >= this.fileDescs.length)
      return 0
if (fd < 3) {
  const str = Util.decodeString(buffer, 0, buffer.length)
  if (fd === 1 || fd === 2)
    Util.putTerminal(str)
  return buffer.length
}
    const desc = this.fileDescs[fd]
    if (desc == null)
      return 0
    desc.write.push(buffer.slice(0))
    desc.writeTotal += buffer.byteLength
    return buffer.length
  }

  public lseek(fd: number, offset: number, where: SeekWhere): number {
    const desc = this.fileDescs[fd]
    if (desc == null)
      return -1

    this.commitDesc(fd)

    let position
    switch (where) {
    default:
    case SeekWhere.SET:
      position = offset
      break
    case SeekWhere.CUR:
      position = this.fileDescs[fd].position + offset
      break
    case SeekWhere.END:
      //position = files[fd].position + offset
      console.assert(false, 'TODO: Implement')
      break
    }
    desc.rp = position
    // TODO: write?
    return position
  }

  public delete(absPath: string) {
    this.storage.delete(absPath)
  }

  public tmpfile(): number {
    const fd = this.allocFd()
    this.fileDescs[fd] = {
      absPath: null,  // temporary: not related to storage
      rp: 0,
      write: [],
      writeTotal: 0,
    }
    return fd
  }

  private allocFd(): number {
    const len = this.fileDescs.length
    for (let fd = 0; fd < len; ++fd) {
      if (this.fileDescs[fd] == null)
        return fd
    }
    this.fileDescs.push(null)
    return len
  }

  private commitDesc(fd: number): void {
    if (0 <= fd && fd < this.fileDescs.length) {
      const desc = this.fileDescs[fd]
      if (desc != null) {
        if (desc.write != null && desc.write.length > 0) {
          const content = new Uint8Array(desc.writeTotal)
          let p = 0
          for (let i = 0; i < desc.write.length; ++i) {
            const src = desc.write[i]
            const dst = new Uint8Array(content.buffer, p, src.byteLength)
            dst.set(src)
            p += src.byteLength
          }
          if (desc.absPath != null)
            this.saveFile(desc.absPath, content)
          else
            desc.written = content
          desc.write.length = 0
        }
      }
    }
  }
}
