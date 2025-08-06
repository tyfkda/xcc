import {WASI} from '@wasmer/wasi'
import {WasmFs} from '@wasmer/wasmfs'
import path from 'path-browserify'

export class WaProc {
  private memory: WebAssembly.Memory
  private cwd = '/'
  private imports: any
  private wasi: WASI

  constructor(private wasmFs: WasmFs, args: string[], env: Record<string, string>) {
    const curDir = env['PWD'] || '/'

    this.wasi = new WASI({
      args,
      env,
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

  public async runWasiEntry(wasmPath: string): Promise<any> {
    const instance = await this.loadWasm(wasmPath)
    this.wasi.start(instance!)
  }

  private async loadWasm(wasmPath: string): Promise<WebAssembly.Instance|null> {
    let obj: WebAssembly.WebAssemblyInstantiatedSource
    if (typeof wasmPath === 'string') {
      const bin = this.wasmFs.fs.readFileSync(this.getAbsPath(wasmPath)) as Uint8Array

      if (bin == null) {
        throw 'File not found'
      }
      obj = await WebAssembly.instantiate(bin, this.imports)
    } else {
      console.error(`Path or buffer required: ${wasmPath}`)
      return null
    }
    const instance = obj.instance

    if (instance.exports.memory) {
      this.memory = instance.exports.memory as WebAssembly.Memory
      this.wasi.setMemory(this.memory)
    }
    return instance
  }

  private chdir(absPath: string): boolean {
    const st = this.wasmFs.fs.statSync(absPath)
    if (!st?.isDirectory())
      return false
    this.cwd = absPath
    return true
  }

  private getAbsPath(fileName: string): string {
    if (fileName.length > 0 && fileName[0] === '/')
      return fileName
    // TODO: Handle ., ..
    //return `${this.cwd}/${fileName}`
    return `${this.cwd}${this.cwd === '/' ? '' : '/'}${fileName}`
  }
}
