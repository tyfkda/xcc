import {DisWasm} from './diswasm'
import * as Split from 'split.js'
import {Util} from './util'
import {WccRunner} from './wcc_runner'
import Alpine from 'alpinejs'

import hello from './examples/hello.c'
import sieve from './examples/sieve.c'
import qsort from './examples/qsort.c'
import aobench from './examples/aobench.c'

const FONT_SIZE = 16

const KEY_CODE = 'wcc-code'

function start() {
  const editor = monaco.editor.create(document.getElementById('editor')!, {
    value: '',
    language: 'c',
    minimap: {
      enabled: false,
    },
    fontSize: FONT_SIZE,
  })
  let editorSavedVersionId = 0
  editor.getModel()!.updateOptions({ tabSize: 4 })

  function isCodeModified(): boolean {
    return (editorSavedVersionId > 0 &&
            editorSavedVersionId !== editor.getModel()!.getAlternativeVersionId())
  }

  function clearUndoHistory(): void {
    editor.pushUndoStop()
    editorSavedVersionId = editor.getModel()!.getAlternativeVersionId()
  }

  function setCodeUnmodified(): void {
    editorSavedVersionId = editor.getModel()!.getAlternativeVersionId()
  }

  function loadCodeToEditor(code: string, message: string): boolean {
    if (code == null)
      return false

    if (isCodeModified() &&
        !window.confirm(`Buffer modified. ${message} anyway?`))
      return false

    Util.clearTerminal()

    editor.setValue(code)
    clearUndoHistory()
    editor.setScrollPosition({scrollTop: 0})
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

  const terminal = monaco.editor.create(document.getElementById('terminal')!, {
    value: '',
    language: 'txt',
    lineNumbers: 'off',
    minimap: {
      enabled: false,
    },
    fontSize: FONT_SIZE,
    readOnly: true,
  })

  terminal.onMouseDown((e: any) => {
    const compileErrors = Util.compileErrors
    if (compileErrors == null)
      return

    const pos = e.target.position!
    // TODO: Use Array.findLast in the future.
    for (let i = compileErrors.length; --i >= 0; ) {
      const err = compileErrors[i]
      if (err.terminalLineNo <= pos.lineNumber) {
        editor.revealLineInCenter(err.sourceLineNo)

        const range = new monaco.Range(err.sourceLineNo, err.colStart, err.sourceLineNo, err.colStart + err.tokenLength)
        editor.setSelection(range)

        editor.focus()
        break
      }
    }
  })

  Util.setTerminal(terminal)

  let terminalRatio = 33

  const mysplit = (Split as any)['default'](['#editor', '#terminal-container'], {
    direction: 'vertical',
    sizes: [100, 0],
    minSize: [100, 0],
    onDrag: () => {
      editor.layout()
      terminal.layout()
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
    editor.layout()
    terminal.layout()
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

  const wccRunner = new WccRunner()

  async function compile(sourceCode: string, extraOptions?: string[]): Promise<string|null> {
    const sourceName = 'main.c'
    await wccRunner.writeFile(sourceName, sourceCode)

    Util.clearCompileErrors()
    const exitCode = await wccRunner.compile(sourceName, extraOptions)
    if (exitCode !== 0) {
      Util.analyzeCompileErrors()
      showTerminal()
      return null
    }

    return 'a.wasm'
  }

  async function run(argStr: string, compileAndDump: boolean) {
    Util.clearTerminal()
    editor.focus()

    // Compile
    const objFn = 'main.o'
    const extraOptions = compileAndDump ? ['-c', '-o', objFn, '--import-module-name=env'] : undefined
    const compiledPath = await compile(editor.getValue(), extraOptions)
    if (compiledPath == null)
      return

    if (compileAndDump) {
      const compiledCode = await wccRunner.readFile(objFn)!
      const disWasm = new DisWasm(compiledCode!.buffer)
      disWasm.setLogFunc(s => Util.putTerminal(`${s}\n`))
      disWasm.dump()
      showTerminal()
      return
    }

    // Run
    const args = argStr === '' ? [] : argStr.trim().split(/\s+/)
    args.unshift('a.wasm')
    showTerminal()
    await wccRunner.runWasi(compiledPath, args)

    await wccRunner.clearTemporaries()
  }

  window.addEventListener('load', () => {
    window.addEventListener('resize', () => {
      // split.resize()
      editor.layout()
      terminal.layout()
    }, false)

    window.addEventListener('beforeunload', event => {
      if (!isCodeModified())
        return
      event.preventDefault()
      event.returnValue = ''
    })

    editor.layout()
    terminal.layout()
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
    busy: false,
    canAccessLocalFile: !!window.showOpenFilePicker,
    fileHandle: null,
    runMode: RUN,
    showRunModeDropdown: false,

    init() {
      wccRunner.setConsoleOutFunction((text, _isError) => Util.putTerminal(text))
      wccRunner.setUp()
        .then(() => this.loaded = true)

      const searchParams = new URLSearchParams(window.location.search)
      if (searchParams.has('code')) {
        loadCodeToEditor(searchParams.get('code') || '', '')
        this.args = searchParams.get('args') || ''
      } else if (!loadCodeFromStorage()) {
        loadCodeToEditor(ExampleCodes.hello, 'Hello')
      }

      editor.addAction({
        id: 'run',
        label: 'Run',
        keybindings: [
          monaco.KeyMod.CtrlCmd | monaco.KeyCode.Enter,
        ],
        run: () => this.runCode(),
      })
      editor.addAction({
        id: 'save',
        label: 'Save',
        keybindings: [
          monaco.KeyMod.CtrlCmd | monaco.KeyCode.KeyS,
        ],
        run: () => this.saveFile(),
      })

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
    async runCode() {
      if (!this.loaded && this.busy)
        return
      this.busy = true
      await run(this.args, this.runMode === COMPILE)
      this.busy = false
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

  window.Alpine = Alpine
  Alpine.start()
}

// Load Monaco editor.
window.require.config({ paths: { 'vs': 'https://cdnjs.cloudflare.com/ajax/libs/monaco-editor/0.45.0/min/vs' }})
window.require(['vs/editor/editor.main'], () => start())
