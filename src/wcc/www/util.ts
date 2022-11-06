let terminal

export class Util {
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
}
