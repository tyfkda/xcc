import {DisWasm} from './diswasm'
import {DomUtil} from './dom_util'
import {Util} from './util'
import {WaProc, ExitCalledError} from './wa_proc'
import {WaStorage} from './file_system'
import {ExampleCodes} from './example_code'

const FONT_SIZE = 16

const USER = 'wasm'
const KEY_CODE = 'wcc-code'

const aceSplit = ace.require('ace/ext/split') as typeof AceAjax.Split
const split = new aceSplit.Split(document.getElementById('editor')!) as AceAjax.Split
split.setOrientation(split.BESIDE)
split.setSplits(2)
split.setFontSize(FONT_SIZE)
const UndoManager = ace.require('ace/undomanager').UndoManager as typeof AceAjax.UndoManager

const editor = (() => {
  const editor = split.getEditor(0)
  editor.setOptions({
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

  if (code !== '')
    code = code.trim() + '\n'
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
  const terminal = split.getEditor(1)
  terminal.$blockScrolling = Infinity
  terminal.setOptions({
    tabSize: 2,
    useSoftTabs: true,
    printMarginColumn: false,
  })
  terminal.setReadOnly(true)
  // terminal.renderer.setShowGutter(false)
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

        const range = new ace.Range(err.sourceLineNo, err.colStart + err.tokenLength, err.sourceLineNo, err.colStart)
        editor.selection.setRange(range, false)

        setTimeout(() => editor.focus(), 0)  // Sometimes focus is not set, without `setTimeout`.
        break
      }
    }
  })

  return terminal
})()

Util.setTerminal(terminal)

////////////////////////////////////////////////

function putPixels(canvas: HTMLCanvasElement, memory: WebAssembly.Memory, img: number): void {
  const context = canvas.getContext('2d')!
  const imageData = context.getImageData(0, 0, canvas.width, canvas.height)
  const pixels = imageData.data
  pixels.set(new Uint8Array(memory.buffer, img, canvas.width * canvas.height * 4))
  context.putImageData(imageData, 0, 0)
}

function encodeForHashString(str) {
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

const storage = new WaStorage()

let wccWasm: Uint8Array

async function compile(sourceCode: string, extraOptions?: string[]): Promise<Uint8Array|null> {
  const sourceName = 'main.c'
  const waproc = new WaProc(storage)
  waproc.chdir(`/home/${USER}`)
  waproc.saveFile(sourceName, sourceCode)

  Util.clearCompileErrors()
  let args = ['cc', '-I/usr/include', '-L/usr/lib']
  if (extraOptions != null)
    args = args.concat(extraOptions)
  args.push(sourceName)

  try {
    const result = await waproc.runWasmEntry(wccWasm, '_start', args)
    if (result === 0)
      return waproc.loadFile('a.wasm')
  } catch (e) {
    if (!(e instanceof ExitCalledError)) {
      Util.putTerminalError(e)
      return null
    }
  }

  Util.analyzeCompileErrors()
  return null
}

async function run(argStr: string, compileAndDump: boolean) {
  if (wccWasm == null)
    return  // TODO: Error message

  Util.clearTerminal()
  editor.focus()

  // Compile
  const extraOptions = compileAndDump ? ['-nodefaultlibs'] : undefined
  const compiledCode = await compile(editor.getValue(), extraOptions)
  if (compiledCode == null)
    return

  if (compileAndDump) {
    const disWasm = new DisWasm(compiledCode.buffer)
    disWasm.setLogFunc(s => Util.putTerminal(`${s}\n`))
    disWasm.dump()
    return
  }

  // Run
  const waproc = new WaProc(storage)
  waproc.chdir(`/home/${USER}`)
  waproc.registerCFunction(
    'showGraphic',
    (width: number, height: number, img: number) => {
      const canvas = DomUtil.createCanvas(width, height)
      DomUtil.setStyles(canvas, {display: 'block'})
      putPixels(canvas, waproc.getLinearMemory(), img)

      const div = document.createElement('div')
      div.className = 'wnd draggable'
      DomUtil.setStyles(div, {zIndex: 10000})
      div.appendChild(canvas)
      DomUtil.makeDraggable(div)

      const closeButton = DomUtil.addDivButton(div, () => {
        div.parentNode?.removeChild(div)
      })
      closeButton.className = 'close-button'

      document.body.appendChild(div)
      DomUtil.putOnCenter(div)
    })
  const args = argStr === '' ? [] : argStr.trim().split(/\s+/)
  args.unshift('a.wasm')
  try {
    const result = await waproc.runWasmEntry(compiledCode, '_start', args)
    if (result !== 0)
      console.error(`Exit code=${result}`)
  } catch (e) {
    if (!(e instanceof ExitCalledError))
      Util.putTerminalError(e)
    return
  }
}

window.addEventListener('load', () => {
  window.addEventListener('resize', () => {
    split.resize()
  }, false)

  window.addEventListener('beforeunload', event => {
    if (!isCodeModified())
      return
    event.preventDefault()
    event.returnValue = ''
  })
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
          function setFiles(path, json) {
            for (const key of Object.keys(json)) {
              const newPath = `${path}/${key}`
              if (typeof json[key] === 'string')
                storage.putFile(newPath, json[key])
              else
                setFiles(newPath, json[key])
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
      const args = this.args.trim()
      this.shareUrl = `?code=${encodeForHashString(code)}&args=${encodeForHashString(args)}`
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
  setRunMode(mode: RunMode) {
    this.runMode = mode
    editor.focus()
  },
}
