import {DisWasm} from './diswasm'
import * as Split from 'split.js'
import {Util} from './util'
import {WaProc, ExitCalledError} from './wa_proc'
import {FileSystem} from './file_system'
import {fs} from 'memfs-browser'

const FONT_SIZE = 16

const USER = 'wasm'
const KEY_CODE = 'wcc-code'
const TMP_PATH = '/tmp'

const UndoManager = ace.require('ace/undomanager').UndoManager as typeof AceAjax.UndoManager
const Range = ace.require('ace/range').Range as typeof AceAjax.Range

const editor = (() => {
  const editor = ace.edit('editor')
  editor.setOptions({
    fontSize: FONT_SIZE,
    tabSize: 2,
    useSoftTabs: true,
    printMarginColumn: false,
  })
  editor.setTheme('ace/theme/monokai')
  editor.getSession().setMode('ace/mode/c_cpp')
  editor.renderer.$cursorLayer.setBlinking(true)
  editor.container.classList.add('code-editor')

  const code = document.getElementById('default-code')
  if (code)
    editor.setValue(code.innerText.trim() + '\n', -1)
  editor.gotoLine(0, 0, false)
  editor.focus()

  const undoManager = new UndoManager()
  editor.getSession().setUndoManager(undoManager)
  editor.commands.addCommands([
    {
      name : 'Undo',
      bindKey: {
        win : 'Ctrl-Z',
        mac : 'Command-Z'
      },
      exec: (editor) => editor.session.getUndoManager().undo(),
    },
    {
      name : 'Redo',
      bindKey: {
        win : 'Ctrl-Shift-Z',
        mac : 'Command-Shift-Z'
      },
      exec: (editor) => editor.session.getUndoManager().redo(),
    },
  ])

  return editor
})()

function isCodeModified(): boolean {
  return !editor.session.getUndoManager().isAtBookmark()
}

function clearUndoHistory(): void {
  editor.session.getUndoManager().reset()
}

function setCodeUnmodified(): void {
  editor.session.getUndoManager().bookmark()
}

function loadCodeToEditor(code: string, message: string): boolean {
  if (code == null)
    return false

  if (isCodeModified() &&
      !window.confirm(`Buffer modified. ${message} anyway?`))
    return false

  Util.clearTerminal()

  editor.setValue(code, -1)
  clearUndoHistory()
  editor.gotoLine(0, 0, false)
  editor.focus()
  return true
}

function loadCodeFromStorage() {
  const code = localStorage.getItem(KEY_CODE)
  if (code == null)
    return false
  loadCodeToEditor(code, '')
  return true
}

function saveCodeToStorage() {
  const code = editor.getValue()
  localStorage.setItem(KEY_CODE, code)
  setCodeUnmodified()
}

const terminal = (() => {
  const terminal = ace.edit('terminal')
  terminal.$blockScrolling = Infinity
  terminal.setOptions({
    fontSize: FONT_SIZE,
    tabSize: 2,
    useSoftTabs: true,
    printMarginColumn: false,
    showLineNumbers: false,
    showGutter: false,
    highlightActiveLine: false,
  })
  terminal.setReadOnly(true)
  terminal.getSession().setMode('ace/mode/text')
  terminal.setValue('XCC browser version.\n', -1)

  terminal.on('click', (e) => {
    const compileErrors = Util.compileErrors
    if (compileErrors == null)
      return
    const pos = e.getDocumentPosition()
    // TODO: Use Array.findLast in the future.
    for (let i = compileErrors.length; --i >= 0; ) {
      const err = compileErrors[i]
      if (err.terminalLineNo <= pos.row) {
        editor.gotoLine(err.sourceLineNo, err.colStart, true)

        const range = new Range(err.sourceLineNo, err.colStart + err.tokenLength, err.sourceLineNo, err.colStart)
        editor.selection.setRange(range, false)

        setTimeout(() => editor.focus(), 0)  // Sometimes focus is not set, without `setTimeout`.
        break
      }
    }
  })

  return terminal
})()

Util.setTerminal(terminal)

let terminalRatio = 33

const mysplit = (Split as any)['default'](['#editor', '#terminal-container'], {
  direction: 'vertical',
  sizes: [100, 0],
  minSize: [100, 0],
  onDrag: () => {
    editor.resize()
    terminal.resize()
  },
  onDragEnd: (sizes: Array<number>) => {
    if (sizes[1] >= 5)
      terminalRatio = sizes[1]
  },
})

function showTerminal(show = true) {
  if (show) {
    mysplit.setSizes([100 - terminalRatio, terminalRatio])
  } else {
    mysplit.collapse(1)
  }
  editor.resize()
  terminal.resize()
}

function toggleTerminal() {
  showTerminal(mysplit.getSizes()[1] < 5)
}

////////////////////////////////////////////////

function encodeForHashString(str: string) {
  return encodeURIComponent(str).replace(/[!'()*]/g, c => {
    return '%' + c.charCodeAt(0).toString(16)
  })
}

function clearSharingUrlParameters() {
  if (window.location.search === '')
    return
  window.history.replaceState(null, document.title, window.location.pathname)
}

function save(): boolean {
  saveCodeToStorage()
  alert('Saved!')
  window.location.hash = ''
  return true
}

async function saveToLocalFile(fileHandle: FileSystemFileHandle): Promise<boolean> {
  try {
    const writable = await fileHandle.createWritable()
    const code = editor.getValue()
    await writable.write(code)
    await writable.close()
    setCodeUnmodified()
    alert(`${fileHandle.name} Saved!`)
    return true
  } catch (e) {
    console.error(e)
    return false
  }
}

const WCC_PATH = 'cc.wasm'
const LIBS_PATH = 'libs.json'

const fileSystem = new FileSystem(fs)

let wccWasm: Uint8Array

async function compile(sourceCode: string, extraOptions?: string[]): Promise<Uint8Array|null> {
  const sourceName = 'main.c'
  const waproc = new WaProc(fileSystem)
  waproc.chdir(`/home/${USER}`)
  waproc.saveFile(sourceName, sourceCode)

  Util.clearCompileErrors()
  let args = ['cc', '-I/usr/include', '-L/usr/lib']
  if (extraOptions != null)
    args = args.concat(extraOptions)
  args.push(sourceName)

  let err = false
  try {
    await waproc.runWasmEntry(wccWasm, '_start', args)
  } catch (e) {
    if (!(e instanceof ExitCalledError)) {
      Util.putTerminalError(e)
      showTerminal()
      return null
    }
    const ec = e as ExitCalledError
    err = ec.code !== 0
  }
  if (err) {
    Util.analyzeCompileErrors()
    showTerminal()
    return null
  }

  return waproc.loadFile('a.wasm')
}

async function clearTemporaryFiles() {
  const files = fs.readdirSync(TMP_PATH)
  // TODO: Remove directory, too.
  files.forEach((file) => fs.unlinkSync(`${TMP_PATH}/${file}`))
}

async function run(argStr: string, compileAndDump: boolean) {
  if (wccWasm == null)
    return  // TODO: Error message

  Util.clearTerminal()
  editor.focus()

  // Compile
  const extraOptions = compileAndDump ? ['-nostdlib', '--entry-point=', '--export-all-non-static'] : undefined
  const compiledCode = await compile(editor.getValue(), extraOptions)
  if (compiledCode == null)
    return

  if (compileAndDump) {
    const disWasm = new DisWasm(compiledCode.buffer)
    disWasm.setLogFunc(s => Util.putTerminal(`${s}\n`))
    disWasm.dump()
    showTerminal()
    return
  }

  // Run
  const waproc = new WaProc(fileSystem)
  waproc.chdir(`/home/${USER}`)
  const args = argStr === '' ? [] : argStr.trim().split(/\s+/)
  args.unshift('a.wasm')
  showTerminal()
  try {
    await waproc.runWasmEntry(compiledCode, '_start', args)
  } catch (e) {
    if (!(e instanceof ExitCalledError))
      Util.putTerminalError(e)
  }

  clearTemporaryFiles()
}

window.addEventListener('load', () => {
  window.addEventListener('resize', () => {
    // split.resize()
    editor.resize()
    terminal.resize()
  }, false)

  window.addEventListener('beforeunload', event => {
    if (!isCodeModified())
      return
    event.preventDefault()
    event.returnValue = ''
  })

  editor.resize()
  terminal.resize()
})

const kFilePickerOption = {
  types: [
    {
      description: 'C source',
      accept: {'text/c': ['.c']},
    },
  ],
}

const RUN = 'Run'
const COMPILE = 'Compile'
type RunMode = typeof RUN | typeof COMPILE

import hello from './examples/hello.c'
import sieve from './examples/sieve.c'
import qsort from './examples/qsort.c'
import aobench from './examples/aobench.c'

const ExampleCodes: Record<string, string> = {
  hello,
  sieve,
  qsort,
  aobench,
}

window.initialData = {
  showSysmenu: false,
  example: '',
  shareUrl: null,
  args: '',
  loaded: false,
  canAccessLocalFile: !!window.showOpenFilePicker,
  fileHandle: null,
  runMode: RUN,
  showRunModeDropdown: false,

  init() {
    Promise.all([
      Util.loadFromServer(WCC_PATH, {binary: true})
        .then(wasm => wccWasm = new Uint8Array(wasm as ArrayBuffer))
        .catch(error => {
          Util.putTerminalError('Failed to load compiler.wasm\n')
          throw error
        }),

      Util.loadFromServer(LIBS_PATH)
        .then(libs => {
          function setFiles(path: string, json: any) {
            for (const key of Object.keys(json)) {
              const newPath = `${path}/${key}`
              if (typeof json[key] === 'string') {
                fileSystem.writeFileSync(newPath, json[key])
              } else {
                fileSystem.mkdirSync(newPath)
                setFiles(newPath, json[key])
              }
            }
          }

          setFiles('', JSON.parse(libs as string))
        })
        .catch(error => {
          Util.putTerminalError('Failed to load libs\n')
          throw error
        }),
    ]).then((_) => {
      this.loaded = true
    })

    fileSystem.mkdirSync(`/home/${USER}`, {recursive: true})
    fileSystem.mkdirSync(TMP_PATH)

    const searchParams = new URLSearchParams(window.location.search)
    if (searchParams.has('code')) {
      loadCodeToEditor(searchParams.get('code') || '', '')
      this.args = searchParams.get('args') || ''
    } else if (!loadCodeFromStorage()) {
      loadCodeToEditor(ExampleCodes.hello, 'Hello')
    }

    editor.commands.addCommands([
      {
        name : 'Run',
        bindKey: {
          win : 'Ctrl-Enter',
          mac : 'Command-Enter',
        },
        exec: (_editor) => this.loaded && run(this.args, this.runMode === COMPILE),
      },
      {
        name: 'Save',
        bindKey: {
          win: 'Ctrl-S',
          mac: 'Command-S',
        },
        exec: (_editor, _args) => this.saveFile(),
      },
    ])

    this.$watch('example', (selected: string) => {
      this.closeSysmenu()
      clearSharingUrlParameters()

      const code = ExampleCodes[selected]
      const selectElement = document.getElementById('example-select') as HTMLSelectElement
      const option = [].slice.call(selectElement.options).find((o: HTMLOptionElement) => o.value === selected)
      loadCodeToEditor(code, `Load "${option.text}"`)
      this.args = ''

      this.example = ''
      this.fileHandle = null
    })
  },
  onClickNavOpen() {
    const code = editor.getValue().trim()
    if (code !== '') {
      const params: Record<string, string> = {
        code,
      }
      const args = this.args.trim()
      if (args !== '')
        params.args = args
      const query = Object.keys(params).map(key => `${key}=${encodeForHashString(params[key])}`).join('&')
      this.shareUrl = `?${query}`
    } else {
      this.shareUrl = null
    }

    this.showSysmenu = true
  },
  closeSysmenu() {
    this.showSysmenu = false
  },
  newFile(event: Event) {
    event.preventDefault()
    this.closeSysmenu()
    loadCodeToEditor('', 'New')
    this.fileHandle = null
    clearSharingUrlParameters()
  },
  async loadFile(event: Event) {
    event.preventDefault()
    try {
      const [fileHandle] = await window.showOpenFilePicker(kFilePickerOption)
      const file = await fileHandle.getFile()
      const contents = await file.text()
      loadCodeToEditor(contents, `Load "${fileHandle.name}"`)
      this.fileHandle = fileHandle
    } finally {
      this.closeSysmenu()
      clearSharingUrlParameters()
    }
  },
  async saveFile(event: Event): Promise<boolean> {
    event?.preventDefault()
    let result = false
    if (this.canAccessLocalFile) {
      if (this.fileHandle == null)
        return await this.saveFileAs(event)
      this.closeSysmenu()
      result = await saveToLocalFile(this.fileHandle)
    } else {
      this.closeSysmenu()
      result = save()
    }
    if (result)
      clearSharingUrlParameters()
    return result
  },
  async saveFileAs(event: Event): Promise<boolean> {
    event?.preventDefault()
    let result = false
    try {
      const fileHandle = await window.showSaveFilePicker(kFilePickerOption)
      result = await saveToLocalFile(fileHandle)
      if (result)
        this.fileHandle = fileHandle
    } catch (e) {
      console.error(e)
    }
    this.closeSysmenu()
    if (result)
      clearSharingUrlParameters()
    return result
  },
  shareLink(event: Event) {
    event.preventDefault()
    if (this.shareUrl != null) {
      this.closeSysmenu()
      const url = new URL(this.shareUrl, window.location.href)
      let path = url.pathname
      if (url.search)
        path += url.search
       window.history.replaceState(null, document.title, path)
      navigator.clipboard.writeText(url.toString())
        .then(_ => alert('URL copied!'))
    }
    return false
  },
  runCode() {
    this.loaded && run(this.args, this.runMode === COMPILE)
  },
  toggleRunModeDropdown() {
    this.showRunModeDropdown = !this.showRunModeDropdown
    if (!this.showRunModeDropdown)
      editor.focus()
  },
  toggleTerminal() {
    toggleTerminal()
  },
  setRunMode(mode: RunMode) {
    this.runMode = mode
    editor.focus()
  },
}
