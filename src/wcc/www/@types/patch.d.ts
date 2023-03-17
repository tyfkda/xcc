// Patches for type declarations.

interface Window {
  initialData: any
  showOpenFilePicker(option?: any): Promise<[FileSystemFileHandle]>
  showSaveFilePicker(option?: any): Promise<FileSystemFileHandle>
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
