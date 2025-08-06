import {WaProc} from './wa_proc'
import {WasmFs} from '@wasmer/wasmfs'
import {WASIExitError} from '@wasmer/wasi'

export class WasiWorker {
  private wasmFs = new WasmFs()
  private env: Record<string, string> = {
    PWD: '/',
  }

  public constructor(private self: any) {
    const originalWriteSync = this.wasmFs.fs.writeSync.bind(this.wasmFs.fs)
    this.wasmFs.fs.writeSync = (fd: number, buffer: Uint8Array|string|any, offset?: number, length?: any, position?: any) => {
      switch (fd) {
      case 1: case 2:
        {
          const text = typeof buffer === 'string' ? buffer : new TextDecoder('utf-8').decode(buffer)
          this.self.postMessage({
            action: 'consoleOut',
            text,
            isError: fd === 2,
          })
        }
        break
      }
      return originalWriteSync(fd, buffer, offset, length, position)
    }

    this.self.onmessage = async (ev: MessageEvent<any>) => {
      const data = ev.data
      let result: any
      try {
        switch (data.action) {
        case 'setEnv':
          this.env = JSON.parse(data.envJson)
          break
        case 'writeFile':
          this.writeFile(data.filePath, data.content)
          break
        case 'readFile':
          result = this.readFile(data.filePath)
          break
        case 'unlink':
          this.unlink(data.filePath)
          break
        case 'chdir':
          result = this.chdir(data.filePath)
          break
        case 'mkdir':
          this.mkdir(data.filePath, data.option)
          break
        case 'readdir':
          result = this.readdir(data.filePath)
          break
        case 'runWasi':
          result = await this.runWasi(data.filePath, data.args)
          break
        default:
          throw `${data.action}: Not handled`
        }
        this.self.postMessage({messageId: data.messageId, result})
      } catch (e) {
        this.self.postMessage({messageId: data.messageId, error: e.toString()})
      }
    }
  }

  private writeFile(filePath: string, content: string): void {
    this.wasmFs.fs.writeFileSync(filePath, content)
  }

  private readFile(filePath: string): Uint8Array {
    const content = this.wasmFs.fs.readFileSync(filePath) as Uint8Array
    if (content != null) {
      return content
    } else {
      throw `File not found: ${filePath}`
    }
  }

  private unlink(filePath: string): void {
    this.wasmFs.fs.unlinkSync(filePath)
  }

  private chdir(filePath: string): boolean {
    const stat = this.wasmFs.fs.statSync(filePath)
    if (!stat.isDirectory())
      return false
    this.env.PWD = filePath
    return true
  }

  private mkdir(filePath: string, option: any): void {
    this.wasmFs.fs.mkdirSync(filePath, option)
  }

  private readdir(filePath: string): string[] {
    return this.wasmFs.fs.readdirSync(filePath) as string[]
  }

  private async runWasi(filePath: string, args: string[]): Promise<number> {
    const waProc = new WaProc(this.wasmFs, args, this.env)
    let exitCode = 0
    try {
      await waProc.runWasiEntry(filePath)
    } catch (e) {
      if (!(e instanceof WASIExitError))
        throw e
      const err = e as WASIExitError
      exitCode = err.code!
    }
    return exitCode
  }
}

new WasiWorker(self)
