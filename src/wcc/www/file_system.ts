import {IFs, Buffer} from 'memfs-browser'

import {Util} from './util'

const enum OpenFlag {
  RDONLY  = 0x000,
  WRONLY  = 0x001,
  RDWR    = 0x002,
  CREAT   = 0x040,  //  0100
  TRUNC   = 0x200,  // 01000
  APPEND  = 0x400,  // 02000
}

const enum SeekWhere {
  SET = 0,
  CUR = 1,
  END = 2,
}

const kOpenFlags = new Map<number, string>()
kOpenFlags.set(OpenFlag.RDONLY, 'r')
kOpenFlags.set(OpenFlag.WRONLY | OpenFlag.CREAT | OpenFlag.TRUNC, 'w')
kOpenFlags.set(OpenFlag.WRONLY | OpenFlag.CREAT | OpenFlag.APPEND, 'a')
kOpenFlags.set(OpenFlag.RDWR, 'r+')
kOpenFlags.set(OpenFlag.RDWR | OpenFlag.CREAT | OpenFlag.TRUNC, 'w+')
kOpenFlags.set(OpenFlag.RDWR | OpenFlag.CREAT | OpenFlag.APPEND, 'a+')

interface IFileEntry {
  readSync(buffer: Uint8Array): number
  writeSync(buffer: Uint8Array): number
  lseek(offset: number, where: SeekWhere): number
}

class StandardEntry implements IFileEntry {
  constructor(private readonly name: string, private readonly canWrite: boolean) {
  }

  readSync(_buffer: Uint8Array): number {
    // TODO:
    return 0
  }

  writeSync(buffer: Uint8Array): number {
    if (!this.canWrite)
      return 0
    const str = Util.decodeString(buffer, 0, buffer.length)
    Util.putTerminal(str)
    return buffer.length
  }

  lseek(_offset: number, _where: SeekWhere): number {
    return -1
  }

  toString() {
    return `${this.name}`
  }
}

class FileEntry implements IFileEntry {
  private position = 0

  constructor(private readonly fs: IFs, private readonly absPath: string, private fd: number) {
  }

  public readSync(buffer: Uint8Array): number {
    // TODO: Read to buffer directly.
    const buf = Buffer.from(buffer.buffer)
    const readSize = this.fs.readSync(this.fd, buf, 0, buffer.length, this.position)
    for (let i = 0; i < readSize; ++i)
      buffer[i] = buf[i]
    this.position += readSize
    return readSize
  }

  public writeSync(buffer: Uint8Array): number {
    const writeSize = this.fs.writeSync(this.fd, buffer, undefined, undefined, this.position)
    this.position += writeSize
    return writeSize
  }

  public lseek(offset: number, where: SeekWhere): number {
    let position: number
    switch (where) {
    default:
    case SeekWhere.SET:
      position = offset
      break
    case SeekWhere.CUR:
      position = this.position + offset
      break
    case SeekWhere.END:
      {
        const stat = this.fs.statSync(this.absPath)
        position = stat.size + offset
      }
      break
    }
    this.position = position
    // TODO: write?
    return position
  }
}

class TmpEntry implements IFileEntry {
  private rp = 0
  private write = new Array<Uint8Array>()
  private content: Uint8Array|null = null

  public readSync(buffer: Uint8Array): number {
    const content = this.content
    if (content == null || this.rp >= content.length)
      return 0

    const end = Math.min(content.length, this.rp + buffer.length)
    buffer.set(content.subarray(this.rp, end))
    const readSize = end - this.rp
    this.rp = end
    return readSize
  }

  public writeSync(buffer: Uint8Array): number {
    this.write.push(buffer.slice(0))
    return buffer.length
  }

  public lseek(offset: number, where: SeekWhere): number {
    this.commit()

    let position: number
    switch (where) {
    default:
    case SeekWhere.SET:
      position = offset
      break
    case SeekWhere.CUR:
      position = this.rp + offset
      break
    case SeekWhere.END:
      {
        const file = this.content
        position = (file != null ? file.byteLength : 0) + offset
      }
      break
    }
    this.rp = position
    return position
  }

  private commit(): void {
    if (this.write.length > 0) {
      const written = this.content
      const writeTotal =  this.write.reduce((acc: number, src: Uint8Array) => acc + src.byteLength, 0)
      const content = new Uint8Array(writeTotal + (written?.byteLength || 0))
      let p = 0
      if (written != null) {
        const dst = new Uint8Array(content.buffer, 0, written.byteLength)
        dst.set(written)
        p = written.byteLength
      }

      for (let i = 0; i < this.write.length; ++i) {
        const src = this.write[i]
        const dst = new Uint8Array(content.buffer, p, src.byteLength)
        dst.set(src)
        p += src.byteLength
      }
      this.content = content
      this.write.length = 0
      this.rp = p
    }
  }
}

export class FileSystem {
  private fileEntries = new Map<number, IFileEntry>()

  constructor(private fs: IFs) {
    this.fileEntries.set(0, new StandardEntry('<stdin>', false))
    this.fileEntries.set(1, new StandardEntry('<stdout>', true))
    this.fileEntries.set(2, new StandardEntry('<stderr>', true))
  }

  public readFileSync(absPath: string): Uint8Array|null {
    return this.fs.readFileSync(absPath) as Uint8Array|null
  }

  public writeFileSync(absPath: string, content: string|Uint8Array): void {
    return this.fs.writeFileSync(absPath, content)
  }

  public mkdirSync(absPath: string, options?: any): string | undefined {
    return this.fs.mkdirSync(absPath, options)
  }

  public open(absPath: string, flag: number, _mode: number): number {
    if (absPath == null || absPath.length === 0)
      return -1
    if ((flag & (OpenFlag.WRONLY | OpenFlag.RDWR)) === 0) {
      if (!this.fs.existsSync(absPath))
        return -1
    }

    const flagStr = kOpenFlags.get(flag)
    if (flagStr == null)
      throw new Error(`Unsupported open flag: ${flag}`)

    const fd = this.fs.openSync(absPath, flagStr)
    if (fd >= 0) {
      const desc = new FileEntry(this.fs, absPath, fd)
      this.fileEntries.set(fd, desc)
    }
    return fd
  }

  public close(fd: number): boolean {
    if (!this.fileEntries.has(fd))
      return false
    this.fileEntries.delete(fd)
    return true
  }

  public read(fd: number, buffer: Uint8Array): number {
    if (fd < 0 || !this.fileEntries.has(fd))
      return 0
    const entry = this.fileEntries.get(fd)!
    return entry.readSync(buffer)
  }

  public write(fd: number, buffer: Uint8Array): number {
    if (fd < 0 || !this.fileEntries.has(fd))
      return 0
    const entry = this.fileEntries.get(fd)!
    return entry.writeSync(buffer)
  }

  public lseek(fd: number, offset: number, where: SeekWhere): number {
    if (!this.fileEntries.has(fd))
      return -1
    const entry = this.fileEntries.get(fd)!
    return entry.lseek(offset, where)
  }

  public unlink(absPath: string): boolean {
    if (!this.fs.existsSync(absPath))
      return false
    this.fs.unlinkSync(absPath)
    return true
  }

  public tmpfile(): number {
    const fd = this.allocFd()
    this.fileEntries.set(fd, new TmpEntry())
    return fd
  }

  private allocFd(): number {
    // TODO: Ensure temporary fd and memfs-allocated fd is not conflict.
    for (let fd = 3; ; ++fd) {
      if (!this.fileEntries.has(fd))
        return fd
    }
  }
}
