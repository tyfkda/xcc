import path from 'path-browserify'
import {unzip} from 'fflate'

import WasiWorker from './wasi_worker.ts?worker'

const PACKED_ZIP_PATH = 'wccfiles.zip'

const CC_PATH = '/usr/bin/cc'
const USER = 'wasm'
const TMP_PATH = '/tmp'

type ResolveFunc = (value?: any | PromiseLike<any>) => void
type RejectFunc = (value?: any | PromiseLike<any>) => void
type ActionHandler = {
  resolve: ResolveFunc
  reject: RejectFunc
}

async function loadFromServer(path: string, opt: any = null): Promise<string|ArrayBuffer> {
  const response = await fetch(path, {method: 'GET'})
  if (!response.ok)
    throw new Error(`${response.status} ${response.statusText}\n${response.url}`)
  if (opt != null && opt.binary)
    return await response.arrayBuffer()
  return await response.text()
}

export class WccRunner {
  private worker: Worker
  private messageId = 0
  private actionHandlerMap = new Map<number, ActionHandler>()
  private consoleOut: (text: string, isError: boolean) => void
  private curDir = `/home/${USER}`

  public constructor() {
    this.consoleOut = (text: string, isError: boolean) => {
      if (isError)
        console.error(text)
      else
        console.log(text)
    }

    this.worker = new WasiWorker()
    this.worker.onmessage = (ev: MessageEvent<any>) => {
      const data = ev.data
      if (data.messageId != null && this.actionHandlerMap.has(data.messageId)) {
        const handler = this.actionHandlerMap.get(data.messageId)!
        this.actionHandlerMap.delete(data.messageId)
        if (data.error != null) {
          handler.reject(data.error)
        } else {
          handler.resolve(data.result)
        }
      } else {
        switch (data.action) {
        case 'consoleOut':
          this.consoleOut(data.text, data.isError)
          break
        }
      }
    }
  }

  public setConsoleOutFunction(consoleOut: (text: string, isError: boolean) => void) {
    this.consoleOut = consoleOut
  }

  public async setUp(): Promise<void> {
    const recursiveTrue = {recursive: true}

    await Promise.all([
      loadFromServer(PACKED_ZIP_PATH, {binary: true})
        .then((binary) => new Promise((resolve, reject) => {
(async () => {
          const blob = new Blob([binary], { type: 'application/zip' })
console.log(blob)
          const readableStream = blob.stream()
          const decompressedStream = readableStream.pipeThrough(new DecompressionStream('deflate'))
          const unzipped = new Response(decompressedStream)
console.log(unzipped)
try {
            const ab = await unzipped.arrayBuffer()
console.log(ab)
} catch (e) {
  console.error(e)
}
})()

          return unzip(new Uint8Array(binary as ArrayBuffer), (err, unzipped) => {
            if (err) {
              reject(err)
              return
            }

            let ccExists = false
            const promises = Object.entries(unzipped).map(async ([filename, data]) => {
              if (data == null || data.byteLength === 0)  // Skip directories.
                return
              const filepath = `/${filename}`
              await this.mkdir(path.dirname(filepath), recursiveTrue)
              await this.writeFile(filepath, data)
              ccExists ||= filepath === CC_PATH
            })
            Promise.all(promises)
              .then((result) => {
                if (!ccExists)
                  throw 'C-compiler not found in the zip file'
                resolve(result)
              })
              .catch(reject)
          })
        })),

      this.mkdir(TMP_PATH, recursiveTrue),

      this.mkdir(this.curDir, recursiveTrue),
    ])

    await this.chdir(this.curDir)
  }

  public async writeFile(filePath: string, content: string|Uint8Array): Promise<void> {
    await this.postMessage('writeFile', {filePath: this.abspath(filePath), content})
  }

  public async readFile(filePath: string): Promise<Uint8Array> {
    return await this.postMessage('readFile', {filePath: this.abspath(filePath)})
  }

  public chdir(filePath: string): Promise<boolean> {
    return this.postMessage('chdir', {filePath: this.abspath(filePath)})
  }

  public mkdir(filePath: string, option?: any): Promise<void> {
    return this.postMessage('mkdir', {filePath: this.abspath(filePath), option})
  }

  public compile(sourceName: string, extraOptions?: string[]): Promise<number> {
    let args = [CC_PATH]
    if (extraOptions != null)
      args = args.concat(extraOptions)
    args.push(sourceName)

    return this.runWasi(args[0], args)
  }

  public async runWasi(filePath: string, args: string[]): Promise<number> {
    return await this.postMessage('runWasi', {filePath, args})
  }

  public async clearTemporaries(): Promise<void> {
    const files = await this.postMessage('readdir', {filePath: TMP_PATH})
    await Promise.all(files.map((file: string) => this.postMessage('unlink', {filePath: `${TMP_PATH}/${file}`})))
  }

  private postMessage(action: string, data: any = {}): Promise<any> {
    return new Promise<any>((resolve: ResolveFunc, reject: RejectFunc) => {
      const messageId = ++this.messageId
      this.actionHandlerMap.set(messageId, {resolve, reject})

      data.action = action
      data.messageId = messageId
      this.worker.postMessage(data)
    })
  }

  private abspath(path2: string): string {
    if (path2[0] === '/')
      return path2
    return path.join(this.curDir, path2)
  }
}
