import path from 'path-browserify'

const WASI_WORKER_PATH = 'wasi_worker.js'

const WCC_PATH = 'cc.wasm'
const LIBS_PATH = 'libs.json'

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

    this.worker = new Worker(WASI_WORKER_PATH)
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
      loadFromServer(WCC_PATH, {binary: true})
        .then(async (wasm) => {
          await this.mkdir(path.dirname(CC_PATH), recursiveTrue)
          await this.writeFile(CC_PATH, new Uint8Array(wasm as ArrayBuffer))
        }),

      loadFromServer(LIBS_PATH)
        .then(async (libs) => {
          const setFiles = async (path: string, json: any) => {
            for (const key of Object.keys(json)) {
              const newPath = `${path}/${key}`
              if (typeof json[key] === 'string') {
                await this.writeFile(newPath, json[key])
              } else {
                await this.mkdir(newPath, recursiveTrue)
                await setFiles(newPath, json[key])
              }
            }
          }

          await setFiles('', JSON.parse(libs as string))
        }),

      this.mkdir(TMP_PATH, recursiveTrue),

      this.mkdir(this.curDir, recursiveTrue)
        .then(_ => this.chdir(this.curDir)),
    ])
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
