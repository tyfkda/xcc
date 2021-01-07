import {Util} from './util'
import {WaProc, ExitCalledError} from './wa_proc'
import {WaStorage} from './file_system'

const editor = (() => {
  const editor = ace.edit('editor')
  editor.$blockScrolling = Infinity
  editor.setOptions({
    // enableBasicAutocompletion: true,
    // enableSnippets: true,
    // enableLiveAutocompletion: true,
    tabSize: 2,
    useSoftTabs: true,
    printMarginColumn: false,
    // showInvisibles: true,
  })
  editor.setTheme('ace/theme/monokai')
  editor.getSession().setMode('ace/mode/c_cpp')
  editor.setKeyboardHandler('ace/keyboard/emacs')
  editor.setFontSize(16)

  const code = document.getElementById('default-code')
  if (code)
    editor.setValue(code.innerText.trim() + '\n', -1)
  editor.gotoLine(0, 0)
  editor.focus()
  return editor
})()

const terminal = (() => {
  const terminal = ace.edit('logs')
  terminal.$blockScrolling = Infinity
  terminal.setOptions({
    // enableBasicAutocompletion: true,
    // enableSnippets: true,
    // enableLiveAutocompletion: true,
    tabSize: 2,
    useSoftTabs: true,
    printMarginColumn: false,
    // showInvisibles: true,
  })
  terminal.setReadOnly(true)
  terminal.setTheme('ace/theme/monokai')
  terminal.getSession().setMode('ace/mode/sh')
  // terminal.setKeyboardHandler('ace/keyboard/emacs')
  terminal.renderer.setShowGutter(false)
  terminal.setFontSize(16)
  return terminal
})()

Util.setTerminal(terminal)

////////////////////////////////////////////////

window.addEventListener('load', () => {
  const WCC_PATH = 'cc.wasm'
  const LIBS_PATH = 'libs.json'
  const LIBC_FILE_NAME = '/usr/lib/lib.c'

  const storage = new WaStorage()
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

  let wccWasm
  Util.loadFromServer(WCC_PATH, {binary: true})
    .then(wasm => wccWasm = new Uint8Array(wasm as ArrayBuffer))

  async function compile(sourceCode: string): Promise<Uint8Array|null> {
    const sourceName = 'main.c'
    const waproc = new WaProc(storage)
    waproc.chdir('/home/wasm')
    waproc.saveFile(sourceName, sourceCode)

    const args = ['cc', '-I/usr/include', '-emain', sourceName, LIBC_FILE_NAME]
    const result = await waproc.runWasmMain(wccWasm, args)
    if (result !== 0)
      return null

    return waproc.loadFile('a.wasm')
  }

  async function run() {
    if (wccWasm == null)
      return  // TODO: Error message

    Util.clearTerminal()
    editor.focus()

    // Compile
    let compiledCode
    try {
      compiledCode = await compile(editor.getValue())
      if (compiledCode == null)
        return
    } catch (e) {
      if (!(e instanceof ExitCalledError))
        Util.putTerminalError(e)
      return
    }

    // Run
    const waproc = new WaProc(storage)
    const argStr = (document.getElementById('args')! as HTMLInputElement).value.trim()
    const args = argStr === '' ? [] : argStr.trim().split(/\s+/)
    args.unshift('a.wasm')
    try {
      const result = await waproc.runWasmMain(compiledCode, args)
      if (result != 0)
        console.error(`Exit code=${result}`)
    } catch (e) {
      if (!(e instanceof ExitCalledError))
        Util.putTerminalError(e)
      return
    }
  }

  document.getElementById('run')!.addEventListener('click', run)

  editor.commands.addCommands([
    {
      Name : 'Run',
      bindKey: {
        win : 'Ctrl-Enter',
        mac : 'Command-Enter'
      },
      exec: (_editor) => run(),
    },
    {
      name: 'Save',
      bindKey: {
        win: 'Ctrl-S',
        mac: 'Command-S',
        sender: 'editor|cli'
      },
      exec: function(_editor, _args, _request) {
        alert('Saved!')
      },
    },
  ])
})
