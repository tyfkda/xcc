import {FileSystem} from './file_system'
import {Util} from './util'
import {WaStorage} from './file_system'

const ERANGE = 34

export class ExitCalledError extends Error {
  public code: number

  constructor(code: number) {
    super(`Exit code: ${code}`)
    this.code = code
  }
}

export class WaProc {
  private fs: FileSystem
  private memory: WebAssembly.Memory
  private cwd = '/'
  private imports: any

  constructor(storage: WaStorage) {
    this.fs = new FileSystem(storage)
    this.imports = this.createImports()
  }

  public getAbsPath(fileName: string): string {
    if (fileName.length > 0 && fileName[0] === '/')
      return fileName
    // TODO: Handle ., ..
    //return `${this.cwd}/${fileName}`
    return `${this.cwd}${this.cwd === '/' ? '' : '/'}${fileName}`
  }

  public chdir(path: string): void {
    // TODO: Validate path.
    this.cwd = path
  }

  public saveFile(fileName: string, content: string): void {
    this.fs.saveFile(this.getAbsPath(fileName), content)
  }

  public loadFile(fileName: string): Uint8Array|null {
    return this.fs.loadFile(this.getAbsPath(fileName))
  }

  public async runWasmMain(wasmUrlOrBuffer: string|ArrayBuffer, entry: string, args: string[]): Promise<any> {
    const instance = await this.loadWasm(wasmUrlOrBuffer)
    const argsPtr = this.putArgs(instance!, args)
    return (instance!.exports[entry] as (c:number, v:number)=>void)(args.length, argsPtr)
  }

  public async loadWasm(pathOrBuffer: string|ArrayBuffer): Promise<WebAssembly.Instance|null> {
    let obj
    if (typeof pathOrBuffer === 'string') {
      if (WebAssembly.instantiateStreaming) {
        obj = await WebAssembly.instantiateStreaming(fetch(pathOrBuffer), this.imports)
      } else {
        const response = await fetch(pathOrBuffer)
        const bytes = await response.arrayBuffer()
        obj = await WebAssembly.instantiate(bytes, this.imports)
      }
    } else if (pathOrBuffer.constructor === Uint8Array) {
      obj = await WebAssembly.instantiate(pathOrBuffer, this.imports)
    } else {
      console.error(`Path or buffer required: ${pathOrBuffer}`)
      return null
    }
    const instance = obj.instance

    if (instance.exports.memory) {
      this.memory = instance.exports.memory
    }
    return instance
  }

  public registerCFunction(funcName: string, func: (...args) => any): void {
    this.imports.c[funcName] = func
  }

  public getLinearMemory(): WebAssembly.Memory {
    return this.memory
  }

  private putArgs(instance: WebAssembly.Instance, args: string[]): number {
    const sbrkFunc = instance.exports['sbrk'] as (((number) => number)|undefined)
    if (sbrkFunc == null) {
      // Cannot put arguments onto wasm memory.
      args.length = 0
      return 0
    }

    const encodedArgs = args.map(Util.encode)
    const ptrArrayBytes = 4 * (args.length + 1)
    const totalArgsBytes = encodedArgs.reduce((acc, arg) => acc + arg.length + 1, 0)
    const ptr = sbrkFunc(ptrArrayBytes + totalArgsBytes)
    const pStrStart = ptr + ptrArrayBytes

    const ptrArgs = new Uint32Array(this.memory.buffer, ptr, args.length + 1)
    const ptrStr = new Uint8Array(this.memory.buffer, pStrStart, totalArgsBytes)
    let p = 0
    for (let i = 0; i < args.length; ++i) {
      const encoded = encodedArgs[i]
      const len = encoded.length
      for (let j = 0; j < len; ++j)
        ptrStr[p + j] = encoded[j]
      ptrStr[p + len] = 0
      ptrArgs[i] = pStrStart + p
      p += len + 1
    }
    ptrArgs[args.length] = 0
    return ptr
  }

  private createImports() {
    const imports = {
      c: {
        read: (fd, buf, size) => {
          const memoryImage = new Uint8Array(this.memory.buffer, buf, size)
          return this.fs.read(fd, memoryImage)
        },
        write: (fd, buf, size) => {
          const memoryImage = new Uint8Array(this.memory.buffer, buf, size)
          return this.fs.write(fd, memoryImage)
        },
        open: (fileNamePtr, flag, mode) => {
          if (fileNamePtr === 0)
            return -1
          const fileName = Util.decodeString(this.memory.buffer, fileNamePtr)
          if (fileName == null || fileName === '')
            return -1
          const absPath = this.getAbsPath(fileName)
          return this.fs.open(absPath, flag, mode)
        },
        close: (fd) => this.fs.close(fd),
        lseek: (fd, offset, where) => this.fs.lseek(fd, offset, where),
        unlink: (fileNamePtr) => {
          const fileName = Util.decodeString(this.memory.buffer, fileNamePtr)
          if (fileName == null || fileName === '')
            return -1
          const absPath = this.getAbsPath(fileName)
          this.fs.delete(absPath)
          return 0
        },
        _tmpfile: () => this.fs.tmpfile(),

        _getcwd: (buffer, size) => {
          const encoded = Util.encode(this.cwd)
          const len = encoded.length
          if (len + 1 > size)
            return -ERANGE
          const memoryImage = new Uint8Array(this.memory.buffer, buffer, len + 1)
          for (let i = 0; i < len; ++i)
            memoryImage[i] = encoded[i]
          memoryImage[len] = 0
          return len + 1
        },

        sin: Math.sin,
        cos: Math.cos,
        tan: Math.tan,
        atan: Math.atan,
        sqrt: Math.sqrt,
        log: Math.log,
        exp: Math.exp,
        pow: Math.pow,
        fabs: Math.abs,
        floor: Math.floor,
        ceil: Math.ceil,
        fmod: (x, y) => x % y,
        drand48: Math.random,
        erand48: Math.random,

        putstr: (ptr) => {
          const text = Util.decodeString(this.memory.buffer, ptr)
          Util.putTerminal(text)
        },
        puti: (x) => {
          Util.putTerminal(x)
        },
        exit: (x) => {
          throw new ExitCalledError(x)
        },
        _memcpy: (dst, src, len) => {
          const memoryImage = new Uint8Array(this.memory.buffer)
          memoryImage.copyWithin(dst, src, src + len)
        },
        _memset: (dst, val, len) => {
          const memoryImage = new Uint8Array(this.memory.buffer)
          for (let i = 0; i < len; ++i)
            memoryImage[dst++] = val
        },
      },
      env: {
        memory: this.memory,
      },
    }
    return imports
  }
}
