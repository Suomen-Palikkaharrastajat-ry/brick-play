import './main.css'
import { strToU8, zipSync } from 'fflate'
import { initBricksRuntime } from './vendor/brick-studio/packages/brick-frame-viewport/runtime/initBricksRuntime.js'
import { initSnapshots } from './vendor/brick-studio/packages/brick-frame-viewport/runtime/snapshot.js'
import { initThumbnails } from './vendor/brick-studio/packages/brick-frame-viewport/runtime/thumbnail-pipeline.js'
import { encodeHash, toUint8Array } from './vendor/brick-studio/packages/brick-frame-viewport/runtime/url-hash-codec.js'
import { Broadcaster, Viewer } from './share.js'
import { Elm } from './src/Main.elm'

const siteRoot = new URL(import.meta.env.VITE_SITE_ROOT ?? '../', window.location.href).href
const ldrawBase = import.meta.env.VITE_LDRAW_BASE || 'https://catalog.palikkaharrastajat.fi/raw'
const indicesBase = import.meta.env.VITE_LDRAW_INDICES_BASE || 'https://catalog.palikkaharrastajat.fi/indices'

// The first-selection hint is shown once per browser. The flag lives in
// localStorage; a private window or blocked storage just shows it again.
const HINT_KEY = 'palikkakehys.play.hintSeen'
const LANG_KEY = 'palikkakehys.language'

function readLanguage() {
  try {
    const hashParams = new URLSearchParams(window.location.hash.replace(/^#/, ''))
    const searchParams = new URLSearchParams(window.location.search)
    const lang = searchParams.get('lang') || hashParams.get('lang')
    if (lang) {
      return lang.toLowerCase().startsWith('en') ? 'en' : 'fi'
    }
    const stored = window.localStorage.getItem(LANG_KEY)
    if (stored) {
      return stored.toLowerCase().startsWith('en') ? 'en' : 'fi'
    }
    return 'fi'
  } catch {
    return 'fi'
  }
}

function readHintSeen() {
  try {
    return window.localStorage.getItem(HINT_KEY) === '1'
  } catch {
    return false
  }
}

function rememberHintSeen() {
  try {
    window.localStorage.setItem(HINT_KEY, '1')
  } catch {
    // Storage is unavailable: the hint may show again next time.
  }
}

// Touch screens say "Double-tap" in the hint; pointer devices say "Double-click".
const coarsePointer =
  typeof window.matchMedia === 'function' && window.matchMedia('(pointer: coarse)').matches
const testApiEnabled =
  import.meta.env.DEV && new URLSearchParams(window.location.search).get('testApi') === '1'

// Tab selects parts, so it must not move focus while the canvas has it. Form
// fields and controls keep Tab for moving focus.
const FOCUS_CONTROL =
  'input, textarea, select, button, a[href], summary, [contenteditable]:not([contenteditable="false"]), ' +
  '[role="button"], [role="link"], [role="tab"], [role="option"], [role="radio"], [role="checkbox"], ' +
  '[role="switch"], [role="menuitem"], [role="slider"], [role="textbox"], [role="combobox"]'

window.addEventListener(
  'keydown',
  (event) => {
    if (event.key !== 'Tab' || event.ctrlKey || event.metaKey || event.altKey) return
    const target = event.target
    if (target instanceof Element && target.closest(FOCUS_CONTROL)) return
    event.preventDefault()
  },
  true,
)

const initialHash = window.location.hash ?? ''
const watchMatch = initialHash.match(/^#\/(?:watch|live)\/([A-Za-z0-9]{4,16})/i)
const isViewer = Boolean(watchMatch)
const viewerRoomId = watchMatch ? watchMatch[1].toUpperCase() : null
const signalingUrl = import.meta.env.VITE_SIGNALING_URL || 'https://broadcast-signaling.palikkaharrastajat.workers.dev'

const runtime = initBricksRuntime({
  elm: Elm.Main,
  node: document.getElementById('app'),
  mode: 'simulator',
  ldrawBase,
  indicesBase,
  ldrawFallbackBase: import.meta.env.VITE_LDRAW_FALLBACK_BASE ?? '',
  defaultModel: '',
  loadModelInJs: false,
  extraFlags: {
    hintSeen: readHintSeen(),
    coarsePointer,
    language: readLanguage(),
    testApiEnabled,
    baseUrl: window.location.origin + window.location.pathname,
  },
  initialHash,
  syncUrlHash: !isViewer,
  useWindowResize: true,
  dragDropTarget: window,
  suppressGestureTarget: window,
})

runtime.app?.ports?.rememberHintSeen?.subscribe(rememberHintSeen)

// A test-only bridge for deterministic browser acceptance tests. It is opt-in,
// only present in Vite development mode, and routes pointer events through the
// same Elm viewport/update path as the interactive canvas.
if (testApiEnabled && runtime.app?.ports?.testCommand && runtime.app?.ports?.testReply) {
  let nextTestRequest = 0
  const waitingForTestReply = new Map()

  runtime.app.ports.testReply.subscribe((reply) => {
    const resolve = waitingForTestReply.get(reply.id)
    if (resolve) {
      waitingForTestReply.delete(reply.id)
      resolve(reply.result)
    }
  })

  const sendTestCommand = (type, payload = {}) =>
    new Promise((resolve) => {
      const id = `brick-play-test-${++nextTestRequest}`
      waitingForTestReply.set(id, resolve)
      runtime.app.ports.testCommand.send({ id, type, ...payload })
    })

  window.__brickPlayTest = Object.freeze({
    snapshot: () => sendTestCommand('snapshot'),
    projectLDrawPoint: (point) => sendTestCommand('projectLDrawPoint', { point }),
    dragLDraw: (from, to) => sendTestCommand('dragLDraw', { from, to }),
    mouseDown: (x, y, shift = false) => sendTestCommand('mouseDown', { x, y, shift }),
    mouseMove: (x, y) => sendTestCommand('mouseMove', { x, y }),
    mouseUp: (x, y) => sendTestCommand('mouseUp', { x, y }),
  })
}

runtime.app?.ports?.storeLanguage?.subscribe((lang) => {
  try {
    window.localStorage.setItem(LANG_KEY, lang)
  } catch {
    // Storage unavailable
  }
})

if (runtime.app?.ports?.copyToClipboard) {
  runtime.app.ports.copyToClipboard.subscribe((data) => {
    const text = typeof data === 'string'
      ? data
      : encodeHash(toUint8Array(data))
    const toCopy = text.startsWith('#')
      ? new URL(text, window.location.href).href
      : text
    if (navigator.clipboard && navigator.clipboard.writeText) {
      navigator.clipboard.writeText(toCopy).catch(() => {})
    }
  })
}

if (runtime.app?.ports?.downloadStudioIo) {
  runtime.app.ports.downloadStudioIo.subscribe(({ filename, ldrText }) => {
    const zipData = zipSync({
      'model.ldr': strToU8(ldrText),
    })
    const blob = new Blob([zipData], { type: 'application/octet-stream' })
    const url = URL.createObjectURL(blob)
    const link = document.createElement('a')
    link.href = url
    link.download = filename || 'model.io'
    document.body.appendChild(link)
    link.click()
    link.remove()
    URL.revokeObjectURL(url)
  })
}

if (runtime.app?.ports?.copyTextToClipboard) {
  runtime.app.ports.copyTextToClipboard.subscribe((text) => {
    if (navigator.clipboard && navigator.clipboard.writeText) {
      navigator.clipboard.writeText(text).catch(() => {})
    }
  })
}

// Live sharing: WebRTC broadcaster and viewer lifecycle
let broadcaster = null

if (runtime.app?.ports?.startBroadcast) {
  runtime.app.ports.startBroadcast.subscribe(() => {
    if (broadcaster) broadcaster.stop()
    broadcaster = new Broadcaster(
      signalingUrl,
      {
        onViewers: (count) => runtime.app?.ports?.broadcastViewersChanged?.send(count),
        onStatus: (status) => {
          if (status === 'ended') {
            runtime.app?.ports?.broadcastStopped?.send(null)
          }
        },
        onError: (err) => runtime.app?.ports?.broadcastError?.send(err),
      }
    )
    broadcaster.start().then(() => {
      const watchUrl = `${window.location.origin}${window.location.pathname}#/watch/${broadcaster.roomId}`
      runtime.app?.ports?.broadcastStarted?.send({
        roomId: broadcaster.roomId,
        watchUrl,
      })
    }).catch((err) => {
      runtime.app?.ports?.broadcastError?.send(err?.message || 'Could not start broadcast')
    })
  })
}

if (runtime.app?.ports?.stopBroadcast) {
  runtime.app.ports.stopBroadcast.subscribe(() => {
    if (broadcaster) {
      broadcaster.stop()
      broadcaster = null
      runtime.app?.ports?.broadcastStopped?.send(null)
    }
  })
}

if (runtime.app?.ports?.pushLiveSnapshot) {
  runtime.app.ports.pushLiveSnapshot.subscribe((bytes) => {
    if (broadcaster) {
      broadcaster.push(bytes)
    }
  })
}

let viewer = null
if (isViewer && viewerRoomId) {
  viewer = new Viewer(
    signalingUrl,
    viewerRoomId,
    {
      onSnapshot: (bytes) => runtime.app?.ports?.liveSnapshotReceived?.send(bytes),
      onStatus: (status) => runtime.app?.ports?.liveStatusChanged?.send(status),
      onError: (err) => runtime.app?.ports?.liveError?.send(err),
    }
  )
  viewer.start().catch((err) => {
    runtime.app?.ports?.liveError?.send(err?.message || 'Could not connect to live view')
  })
}

initSnapshots(runtime.app?.ports)

// Rendered part images for the dock (the kit, or the Technic catalog), with the parts' .dat titles as names.
initThumbnails(runtime.app?.ports, ldrawBase, { titles: true })

// Palettes: dynamic edge fade masks indicating scroll overflow, and wheel-to-horizontal scrolling
function setupPaletteScrolling() {
  function updateScrollMask(el) {
    if (!el || !(el instanceof HTMLElement)) return
    if (el.classList.contains('palette-scroll-x')) {
      const scrollWidth = el.scrollWidth
      const clientWidth = el.clientWidth
      if (scrollWidth <= clientWidth + 2) {
        el.style.webkitMaskImage = 'none'
        el.style.maskImage = 'none'
        return
      }
      const atStart = el.scrollLeft <= 4
      const atEnd = el.scrollLeft + clientWidth >= scrollWidth - 4
      if (atStart && !atEnd) {
        el.style.webkitMaskImage = 'linear-gradient(to right, black calc(100% - 24px), transparent 100%)'
        el.style.maskImage = 'linear-gradient(to right, black calc(100% - 24px), transparent 100%)'
      } else if (!atStart && atEnd) {
        el.style.webkitMaskImage = 'linear-gradient(to right, transparent 0, black 24px)'
        el.style.maskImage = 'linear-gradient(to right, transparent 0, black 24px)'
      } else {
        el.style.webkitMaskImage = 'linear-gradient(to right, transparent 0, black 24px, black calc(100% - 24px), transparent 100%)'
        el.style.maskImage = 'linear-gradient(to right, transparent 0, black 24px, black calc(100% - 24px), transparent 100%)'
      }
    } else if (el.classList.contains('palette-scroll-y')) {
      const scrollHeight = el.scrollHeight
      const clientHeight = el.clientHeight
      if (scrollHeight <= clientHeight + 2) {
        el.style.webkitMaskImage = 'none'
        el.style.maskImage = 'none'
        return
      }
      const atStart = el.scrollTop <= 4
      const atEnd = el.scrollTop + clientHeight >= scrollHeight - 4
      if (atStart && !atEnd) {
        el.style.webkitMaskImage = 'linear-gradient(to bottom, black calc(100% - 20px), transparent 100%)'
        el.style.maskImage = 'linear-gradient(to bottom, black calc(100% - 20px), transparent 100%)'
      } else if (!atStart && atEnd) {
        el.style.webkitMaskImage = 'linear-gradient(to bottom, transparent 0, black 20px)'
        el.style.maskImage = 'linear-gradient(to bottom, transparent 0, black 20px)'
      } else {
        el.style.webkitMaskImage = 'linear-gradient(to bottom, transparent 0, black 20px, black calc(100% - 20px), transparent 100%)'
        el.style.maskImage = 'linear-gradient(to bottom, transparent 0, black 20px, black calc(100% - 20px), transparent 100%)'
      }
    }
  }

  window.addEventListener(
    'scroll',
    (event) => {
      const target = event.target
      if (target instanceof HTMLElement && (target.classList.contains('palette-scroll-x') || target.classList.contains('palette-scroll-y'))) {
        updateScrollMask(target)
      }
    },
    { capture: true, passive: true }
  )

  window.addEventListener(
    'wheel',
    (event) => {
      const target = event.target
      if (!(target instanceof Element)) return
      const scroller = target.closest('.palette-scroll-x')
      if (!scroller || !(scroller instanceof HTMLElement)) return
      if (Math.abs(event.deltaY) > Math.abs(event.deltaX) && event.deltaY !== 0) {
        scroller.scrollLeft += event.deltaY
        event.preventDefault()
        updateScrollMask(scroller)
      }
    },
    { passive: false }
  )

  const refreshAll = () => {
    document.querySelectorAll('.palette-scroll-x, .palette-scroll-y').forEach((el) => {
      if (el instanceof HTMLElement) updateScrollMask(el)
    })
  }

  document.addEventListener(
    'pointerenter',
    (event) => {
      const target = event.target
      if (target instanceof HTMLElement) {
        const scroller = target.closest('.palette-scroll-x, .palette-scroll-y')
        if (scroller instanceof HTMLElement) {
          updateScrollMask(scroller)
        }
      }
    },
    { capture: true, passive: true }
  )

  const appRoot = document.getElementById('app')
  if (appRoot && typeof MutationObserver === 'function') {
    const observer = new MutationObserver(refreshAll)
    observer.observe(appRoot, { childList: true, subtree: true })
  }

  requestAnimationFrame(refreshAll)
}

setupPaletteScrolling()
