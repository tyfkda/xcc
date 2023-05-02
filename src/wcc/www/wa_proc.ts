import {WASI} from '@wasmer/wasi'
import {WasmFs} from '@wasmer/wasmfs'
import path from 'path-browserify'

export class WaProc {
  private memory: WebAssembly.Memory
  private cwd = '/'
  private imports: any
  private wasi: WASI

  constructor(private wasmFs: WasmFs, args: string[], curDir?: string) {
    if (curDir == null)
      curDir = '/'

    this.wasi = new WASI({
      args,
      bindings: {
        ...WASI.defaultBindings,
        fs: this.wasmFs.fs,
        path,
      },
      preopens: {
        '/': '/',
        '.': curDir,
      },
    })

    this.imports = {
      wasi_snapshot_preview1: this.wasi.wasiImport,
    }

    this.chdir(curDir)
  }

  public getAbsPath(fileName: string): string {
    if (fileName.length > 0 && fileName[0] === '/')
      return fileName
    // TODO: Handle ., ..
    //return `${this.cwd}/${fileName}`
    return `${this.cwd}${this.cwd === '/' ? '' : '/'}${fileName}`
  }

  public chdir(absPath: string): boolean {
    const st = this.wasmFs.fs.statSync(absPath)
    if (!st?.isDirectory())
      return false
    this.cwd = absPath
    return true
  }

  public saveFile(fileName: string, content: string): void {
    this.wasmFs.fs.writeFileSync(this.getAbsPath(fileName), content)
  }

  public loadFile(fileName: string): Uint8Array|null {
    return this.wasmFs.fs.readFileSync(this.getAbsPath(fileName)) as Uint8Array
  }

  public async runWasiEntry(wasmUrlOrBuffer: string|ArrayBuffer): Promise<any> {
    const instance = await this.loadWasm(wasmUrlOrBuffer)
    this.wasi.start(instance!)
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
    } else if (pathOrBuffer instanceof Uint8Array) {
      obj = await WebAssembly.instantiate(pathOrBuffer, this.imports)
    } else {
      console.error(`Path or buffer required: ${pathOrBuffer}`)
      return null
    }
    const instance = obj.instance

    if (instance.exports.memory) {
      this.memory = instance.exports.memory as WebAssembly.Memory
      this.wasi.setMemory(this.memory)
    }
    return instance
  }

  public registerCFunction(funcName: string, func: (...args: Array<any>) => any): void {
    this.imports.c[funcName] = func
  }

  public getLinearMemory(): WebAssembly.Memory {
    return this.memory
  }
}
