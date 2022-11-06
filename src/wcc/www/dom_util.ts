import {Util} from './util'

export class DomUtil {
  public static setStyles(elem: HTMLElement, styles: Record<string, unknown>): void {
    Object.assign(elem.style, styles)
  }

  // Register mouse drag event listener.
  public static setMouseDragListener(mouseMove: any, mouseUp?: any, useCapture?: boolean): void {
    let mouseLeave: ((event: Event) => void) | null = null
    let mouseLeaveTarget: HTMLElement | null = null
    if (typeof mouseMove === 'object') {
      const option = mouseMove
      mouseMove = option.move
      mouseUp = option.up
      mouseLeave = option.leave
      useCapture = option.useCapture

      mouseLeaveTarget = mouseLeave == null ? null : option.leaveTarget || document
    }

    const unlisten = () => {
      document.removeEventListener('mousemove', mouseMove, useCapture)
      document.removeEventListener('mouseup', mouseUpDelegate, useCapture)
      document.removeEventListener('touchmove', mouseMove, useCapture)
      document.removeEventListener('touchend', mouseUpDelegate, useCapture)
      if (mouseLeaveDelegate && mouseLeaveTarget) {
        mouseLeaveTarget.removeEventListener('mouseleave', mouseLeaveDelegate, useCapture)
        mouseLeaveTarget.removeEventListener('touchcancel', mouseLeaveDelegate, useCapture)
      }
    }

    const mouseUpDelegate = ($event: Event) => {
      if (mouseUp)
        mouseUp($event)
      unlisten()
    }

    const mouseLeaveDelegate = (mouseLeave == null ? null : ($event: Event) => {
      if (mouseLeave && mouseLeave($event))
        unlisten()
    })

    document.addEventListener('mousemove', mouseMove, useCapture)
    document.addEventListener('mouseup', mouseUpDelegate, useCapture)
    document.addEventListener('touchmove', mouseMove, useCapture)
    document.addEventListener('touchend', mouseUpDelegate, useCapture)
    if (mouseLeaveDelegate && mouseLeaveTarget) {
      mouseLeaveTarget.addEventListener('mouseleave', mouseLeaveDelegate, useCapture)
      mouseLeaveTarget.addEventListener('touchcancel', mouseLeaveDelegate, useCapture)
    }
  }

  public static getMousePosIn(event: Event, elem: HTMLElement): [number, number]|null {
    const rect = elem.getBoundingClientRect()
    const scrollLeft = document.body.scrollLeft
    const scrollTop = document.body.scrollTop

    const touches = (event as TouchEvent).changedTouches
    let x: number, y: number
    if (touches) {
      const touch = touches[0]
      if (touch.identifier !== 0)
        return null
      x = touch.clientX
      y = touch.clientY
    } else {
      const mouseEvent = event as MouseEvent
      x = mouseEvent.pageX
      y = mouseEvent.pageY
    }
    return [x - rect.left - scrollLeft,
            y - rect.top - scrollTop]
  }

  public static createCanvas(width: number, height: number): HTMLCanvasElement {
    const canvas = document.createElement('canvas')
    canvas.width = width
    canvas.height = height
    return canvas
  }

  public static makeDraggable(element: HTMLElement): void {
    const mouseDown = (event: Event) => {
      if (event.type === 'mousedown' && (event as MouseEvent).button !== 0)
        return false

      const rootRect = document.body.getBoundingClientRect()

      event.preventDefault()
      const [mx, my] = DomUtil.getMousePosIn(event, element)!
      const dragOfsX = -mx
      const dragOfsY = -my

      const elemRect = element.getBoundingClientRect()
      const width = elemRect.width
      const height = elemRect.height

      const pos = {x: mx, y: my}
      DomUtil.setMouseDragListener({
        move: (event2: MouseEvent) => {
          const movePos = DomUtil.getMousePosIn(event2, element.parentNode as HTMLElement)
          if (movePos == null)
            return
          pos.x = Util.clamp(movePos[0] + dragOfsX, 0, Math.floor(rootRect.width - width))
          pos.y = Util.clamp(movePos[1] + dragOfsY, 0, Math.floor(rootRect.height - height))

          DomUtil.setStyles(element, {
            left: `${Math.round(pos.x)}px`,
            top: `${Math.round(pos.y)}px`,
          })
          //onEvent(WndEvent.DRAG_MOVE, pos)
        },
        up: (_event2: MouseEvent) => {
          //onEvent(WndEvent.DRAG_END)
        },
      })
      return true
    }

    element.addEventListener('mousedown', mouseDown)
    element.addEventListener('touchstart', mouseDown)
  }

  public static addDivButton(element: HTMLElement, onClick: () => void): HTMLDivElement {
    const div = document.createElement('div')
    DomUtil.setStyles(div, {
      position: 'absolute',
      right: 0,
      top: 0,
    })
    div.addEventListener('click', onClick)
    div.addEventListener('mousedown', (event) => {
      if (event.button !== 0)
        return false
      event.stopPropagation()
      return true
    })
    div.addEventListener('touchstart', onClick)
    element.appendChild(div)
    return div
  }

  public static putOnCenter(element: HTMLElement): void {
    const rootRect = document.body.getBoundingClientRect()
    const elemRect = element.getBoundingClientRect()
    const w = elemRect.width, h = elemRect.height
    const x = Util.clamp((rootRect.width - w) * 0.5, 0, Math.floor(rootRect.width - w))
    const y = Util.clamp((rootRect.height - h) * 0.5, 0, Math.floor(rootRect.height - h))
    DomUtil.setStyles(element, {
      left: `${Math.round(x)}px`,
      top: `${Math.round(y)}px`,
    })
  }
}
