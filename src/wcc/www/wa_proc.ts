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

  private encodedArgs = new Array<Uint8Array>()
  private totalArgsBytes = 0

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

  public async runWasmEntry(wasmUrlOrBuffer: string|ArrayBuffer, entry: string, args: string[]): Promise<any> {
    const encoder = new TextEncoder()
    this.encodedArgs = args.map(arg => encoder.encode(arg))
    this.totalArgsBytes = this.encodedArgs.reduce((acc, arg) => acc + arg.length + 1, 0)

    const instance = await this.loadWasm(wasmUrlOrBuffer)
    return (instance!.exports[entry] as ()=>void)()
  }

  public async loadWasm(pathOrBuffer: string|ArrayBuffer): Promise<WebAssembly.Instance|null> {
    let obj: WebAssembly.WebAssemblyInstantiatedSource
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
      this.memory = instance.exports.memory as WebAssembly.Memory
    }
    return instance
  }

  public registerCFunction(funcName: string, func: (...args) => any): void {
    this.imports.c[funcName] = func
  }

  public getLinearMemory(): WebAssembly.Memory {
    return this.memory
  }

  private createImports() {
    const imports = {
      c: {
        args_sizes_get: (pargc, plen) => {
          const argc = new Uint32Array(this.memory.buffer, pargc, 1)
          argc[0] = this.encodedArgs.length

          const len = new Uint32Array(this.memory.buffer, plen, 1)
          len[0] = this.totalArgsBytes
        },
        args_get: (pargv, pstr) => {
          const argv = new Uint32Array(this.memory.buffer, pargv, this.encodedArgs.length)
          const str = new Uint8Array(this.memory.buffer, pstr, this.totalArgsBytes)
          let offset = 0
          for (let i = 0; i < this.encodedArgs.length; ++i) {
            argv[i] = pstr + offset
            const encoded = this.encodedArgs[i]
            const len = encoded.length
            for (let j = 0; j < len; ++j)
              str[j + offset] = encoded[j]
            str[len + offset] = 0
            offset += len + 1
          }
        },

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

        proc_exit: (x) => {
          throw new ExitCalledError(x)
        },
      },
      env: {
        memory: this.memory,
      },
    }
    return imports
  }
}
