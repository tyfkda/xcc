let terminal: any

export class CompileErrorInfo {
  constructor(public terminalLineNo: number, public sourceLineNo: number, public message: string,
              public colStart: number = 0, public tokenLength: number = Infinity) {
  }
}

export class Util {
  public static compileErrors: Array<CompileErrorInfo> | null = null

  public static clamp(x: number, min: number, max: number): number {
    if (max < min)
      return min
    return x < min ? min : x > max ? max : x
  }

  // Decode string in buffer to JS.
  public static decodeString(buffer: ArrayBuffer, ptr: number, size: number|undefined = undefined): string {
    const memoryImage = new Uint8Array(buffer, ptr, size)
    let len
    for (len = 0; len < memoryImage.length && memoryImage[len] !== 0x00; ++len)
      ;
    const arr = new Uint8Array(buffer, ptr, len)
    return new TextDecoder('utf-8').decode(arr)
  }

  public static async loadFromServer(path: string, opt: any = null): Promise<string|ArrayBuffer> {
    const response = await fetch(path, {method: 'GET'})
    if (!response.ok)
      return Promise.reject(response)
    if (opt != null && opt.binary)
      return await response.arrayBuffer()
    const text = await response.text()
    return text
  }

  public static encode(text: string): Uint8Array {
    return new TextEncoder().encode(text)
  }

  public static setTerminal(terminal_: any): void {
    terminal = terminal_
  }

  public static putTerminal(x: any): void {
    terminal.session.insert(terminal.getCursorPosition(), x.toString())
  }

  public static putTerminalError(e: Error|string): void {
    console.error(e)
    Util.putTerminal(e)
  }

  public static clearTerminal(): void {
    terminal.setValue('', -1)
  }

  public static analyzeCompileErrors(): void {
    const lines = terminal.getValue().split('\n')
    const errors: Array<CompileErrorInfo> = []
    for (let i = 0; i < lines.length; ++i) {
      const line = lines[i]
      if (line === '')
        continue
      let m = line.match(/^main\.c\((\d+)\):\s?(.*)$/)
      if (m) {
        const lineNo = parseInt(m[1])
        const message = m[2]
        errors.push(new CompileErrorInfo(i, lineNo - 1, message))
      }
      m = line.match(/^(\s*)(\^~*)/)
      if (m) {
        const spaces = m[1]
        const token = m[2]
        if (errors.length > 0) {
          const err = errors[errors.length - 1]
          err.colStart = spaces.length
          err.tokenLength = token.length
        }
      }
    }
    Util.compileErrors = errors
  }

  public static clearCompileErrors(): void {
    Util.compileErrors = null
  }
}
