import {WASI} from '@wasmer/wasi'
import {WasmFs} from '@wasmer/wasmfs'
import path from 'path-browserify'

export class WaProc {
  private memory: WebAssembly.Memory
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
    this.wasi.start(instance)
  }

  private async loadWasm(wasmPath: string): Promise<WebAssembly.Instance> {
    const bin = this.wasmFs.fs.readFileSync(wasmPath) as Uint8Array
    if (bin == null) {
      throw 'File not found'
    }

    const obj = await WebAssembly.instantiate(bin.buffer, this.imports)
    const instance = obj.instance

    if (instance.exports.memory) {
      this.memory = instance.exports.memory as WebAssembly.Memory
      this.wasi.setMemory(this.memory)
    }
    return instance
  }
}
