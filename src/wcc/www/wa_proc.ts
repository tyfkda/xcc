import {WASI} from '@wasmer/wasi'
import {WasmFs} from '@wasmer/wasmfs'
import path from 'path-browserify'

export class WaProc {
  private imports: WebAssembly.Imports
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
  }

  public async runWasiEntry(wasmPath: string): Promise<void> {
    const instance = await this.loadWasm(wasmPath)
    if (WebAssembly.promising == null)
      return this.wasi.start(instance)

    // Use JSPI to support suspending.
    if (instance.exports._start) {
      const promise = WebAssembly.promising(instance.exports._start as Function)
      return await promise()
    }
  }

  private async loadWasm(wasmPath: string): Promise<WebAssembly.Instance> {
    const bin = this.wasmFs.fs.readFileSync(wasmPath) as Uint8Array
    if (bin == null) {
      throw 'File not found'
    }

    const obj = await WebAssembly.instantiate(bin.buffer, this.imports)
    const instance = obj.instance

    const memory = instance.exports?.memory
    if (!(memory instanceof WebAssembly.Memory)) {
      throw new Error(`instance.exports.memory must be a WebAssembly.Memory. Recceived ${memory}.`)
    }
    this.wasi.setMemory(memory)
    return instance
  }
}
