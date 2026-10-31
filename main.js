import './main.css'
import { strToU8, zipSync } from 'fflate'
import { initBricksRuntime } from './vendor/brick-studio/packages/brick-frame-viewport/runtime/initBricksRuntime.js'
import { initSnapshots } from './vendor/brick-studio/packages/brick-frame-viewport/runtime/snapshot.js'
import { initThumbnails } from './vendor/brick-studio/packages/brick-frame-viewport/runtime/thumbnail-pipeline.js'
import { encodeHash, toUint8Array } from './vendor/brick-studio/packages/brick-frame-viewport/runtime/url-hash-codec.js'
import { Elm } from './src/Main.elm'

const siteRoot = new URL(import.meta.env.VITE_SITE_ROOT ?? '../', window.location.href).href
const ldrawBase = import.meta.env.VITE_LDRAW_BASE || 'https://catalog.palikkaharrastajat.fi/raw'
const indicesBase = import.meta.env.VITE_LDRAW_INDICES_BASE || 'https://catalog.palikkaharrastajat.fi/indices'

// The first-selection hint is shown once per browser. The flag lives in
// localStorage; a private window or blocked storage just shows it again.
const HINT_KEY = 'palikkakehys.play.hintSeen'

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

const runtime = initBricksRuntime({
  elm: Elm.Main,
  node: document.getElementById('app'),
  mode: 'simulator',
  ldrawBase,
  indicesBase,
  ldrawFallbackBase: import.meta.env.VITE_LDRAW_FALLBACK_BASE ?? '',
  defaultModel: '',
  loadModelInJs: false,
  extraFlags: { hintSeen: readHintSeen(), coarsePointer },
  initialHash: window.location.hash ?? '',
  syncUrlHash: true,
  useWindowResize: true,
  dragDropTarget: window,
  suppressGestureTarget: window,
})

runtime.app?.ports?.rememberHintSeen?.subscribe(rememberHintSeen)

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

initSnapshots(runtime.app?.ports)

// Rendered part images for the dock (the kit, or the Technic catalog), with the parts' .dat titles as names.
initThumbnails(runtime.app?.ports, ldrawBase, { titles: true })
