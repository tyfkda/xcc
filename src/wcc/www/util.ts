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

  public static async bolbToBase64(blob: Blob): Promise<string> {
    return await new Promise(resolve => {
      const reader = new FileReader()
      reader.onloadend = () => resolve((reader.result as string).replace(/data:.*\/.*;base64,/, ''))
      reader.readAsDataURL(blob)
    })
  }

  public static async base64ToBlob(base64: string): Promise<Blob> {
    return await fetch('data:application/octet-stream;base64,' + base64).then(res => res.blob())
  }

  public static async compressText(text: string): Promise<Blob> {
    const readableStream = new Response(text).body!.pipeThrough(new CompressionStream('deflate'))
    return await new Response(readableStream).blob()
  }

  public static async decompressText(blob: Blob): Promise<string> {
    const readableStream = blob.stream().pipeThrough(new DecompressionStream('deflate'))
    return new Response(readableStream).text()
  }

  public static setTerminal(terminal_: any): void {
    terminal = terminal_
  }

  public static putTerminal(x: any): void {
    terminal.updateOptions({readOnly: false})
    terminal.executeEdits('', [{
      range: terminal.getSelection(),
      text: x.toString(),
    }])
    terminal.updateOptions({readOnly: true})
  }

  public static putTerminalError(e: Error|string): void {
    console.error(e)
    Util.putTerminal(e)
  }

  public static clearTerminal(): void {
    terminal.setScrollTop(0)
    terminal.setValue('')
  }

  public static analyzeCompileErrors(): void {
    const lines = terminal.getValue().split('\n')
    const errors: Array<CompileErrorInfo> = []
    for (let i = 0; i < lines.length; ++i) {
      const line = lines[i]
      if (line === '')
        continue
      let m = line.match(/^main\.c\((\d+)\):\s?(.*)(\s*)$/)
      if (m) {
        const lineNo = parseInt(m[1])
        const message = m[2]
        errors.push(new CompileErrorInfo(i, lineNo, message))
      }
      m = line.match(/^(\s*)(\^~*)/)
      if (m) {
        const spaces = m[1]
        const token = m[2]
        if (errors.length > 0) {
          const err = errors[errors.length - 1]
          err.colStart = spaces.length + 1
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
