// Patches for type declarations.

interface Window {
  initialData: any
  showOpenFilePicker(option?: any): Promise<[FileSystemFileHandle]>
  showSaveFilePicker(option?: any): Promise<FileSystemFileHandle>

  MonacoEnvironment: any
  require: any
  Alpine: any
}

interface FileSystemHandle {
  readonly kind: 'file' | 'directory'
  readonly name: string
}

interface FileSystemFileHandle extends FileSystemHandle {
  getFile(): Promise<File>
  createWritable(): Promise<FileSystemWritableFileStream>
}

interface WritableStream {
  close(): any
}

interface FileSystemWritableFileStream extends WritableStream {
  write(content: any): Promise<any>
}

declare namespace WebAssembly {
  function instantiate(bytes: ArrayBufferLike, importObject?: Imports): Promise<WebAssemblyInstantiatedSource>
}

declare let monaco: any
