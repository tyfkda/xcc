import {DomUtil} from './dom_util'
import {Util} from './util'
import {WaProc, ExitCalledError} from './wa_proc'
import {WaStorage} from './file_system'

const FONT_SIZE = 16

const aceSplit = ace.require('ace/ext/split')
const split = new aceSplit.Split(document.getElementById('editor'))
split.setOrientation(split.BESIDE)
split.setSplits(2)
split.setFontSize(FONT_SIZE)
const UndoManager = ace.require('ace/undomanager').UndoManager

const editor = (() => {
  const editor = split.getEditor(0)
  editor.$blockScrolling = Infinity
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
  editor.gotoLine(0, 0)
  editor.focus()

  const undoManager = new UndoManager()
  editor.getSession().setUndoManager(undoManager)
  editor.commands.addCommands([
    {
      Name : 'Undo',
      bindKey: {
        win : 'Ctrl-Z',
        mac : 'Command-Z'
      },
      exec: (editor) => editor.session.getUndoManager().undo(),
    },
    {
      Name : 'Redo',
      bindKey: {
        win : 'Ctrl-Shift-Z',
        mac : 'Command-Shift-Z'
      },
      exec: (editor) => editor.session.getUndoManager().redo(),
    },
  ])

  return editor
})()

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
    waproc.registerCFunction(
      'showGraphic',
      (width, height, img) => {
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

  window.addEventListener('resize', () => {
    split.resize()
  }, false)
})
