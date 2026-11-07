// Live-model sharing over WebRTC for Brick Play.
// The broadcaster opens an RTCPeerConnection and RTCDataChannel ("state")
// per viewer and pushes model snapshots over them; viewers receive and render.
// A Cloudflare Worker + Durable Object signaling service relays SDP offers/answers
// and ICE candidates without seeing model data (DataChannels are DTLS-encrypted end-to-end).

const DEFAULT_SIGNALING_URL = (
  typeof import.meta !== 'undefined' && import.meta.env?.VITE_SIGNALING_URL
    ? import.meta.env.VITE_SIGNALING_URL
    : ''
).replace(/\/+$/, '')

export const DEFAULT_ICE = [
  { urls: ['stun:stun.cloudflare.com:3478', 'stun:stun.l.google.com:19302'] },
]

export const ROOM_ALPHABET = 'ABCDEFGHJKLMNPQRSTUVWXYZ23456789'

export const DATA_CHANNEL = 'state'

export const CHUNK_SIZE = 48 * 1024

export function isSharingEnabled(signalingUrl = DEFAULT_SIGNALING_URL) {
  return !!(signalingUrl || DEFAULT_SIGNALING_URL)
}

export function randomRoomId(len = 7) {
  const rnd = new Uint32Array(len)
  if (typeof crypto !== 'undefined' && crypto.getRandomValues) {
    crypto.getRandomValues(rnd)
  } else {
    for (let i = 0; i < len; i++) {
      rnd[i] = Math.floor(Math.random() * 0xffffffff)
    }
  }
  let id = ''
  for (let i = 0; i < len; i++) {
    id += ROOM_ALPHABET[rnd[i] % ROOM_ALPHABET.length]
  }
  return id
}

export function roomWsUrl(signalingUrl, roomId, role) {
  const base = (signalingUrl || DEFAULT_SIGNALING_URL || '').replace(/\/+$/, '')
  if (base) {
    const wsBase = base.replace(/^http/, 'ws')
    return `${wsBase}/room/${roomId}?role=${role}`
  }
  if (typeof window !== 'undefined' && window.location) {
    const proto = window.location.protocol === 'https:' ? 'wss:' : 'ws:'
    return `${proto}//${window.location.host}/room/${roomId}?role=${role}`
  }
  return `ws://localhost:8787/room/${roomId}?role=${role}`
}

export async function getIceConfig(signalingUrl = DEFAULT_SIGNALING_URL) {
  const base = (signalingUrl || DEFAULT_SIGNALING_URL || '').replace(/\/+$/, '')
  let iceServers = DEFAULT_ICE
  if (!base) {
    return {
      iceServers,
      bundlePolicy: 'max-bundle',
    }
  }
  if (typeof fetch === 'function') {
    const controller = typeof AbortController !== 'undefined' ? new AbortController() : null
    const timer = controller ? setTimeout(() => controller.abort(), 5000) : null
    try {
      const res = await fetch(`${base}/turn-credentials`, {
        signal: controller?.signal,
      })
      if (res.ok) {
        const data = await res.json()
        if (Array.isArray(data?.iceServers) && data.iceServers.length > 0) {
          iceServers = data.iceServers
        }
      }
    } catch {
      // offline or no TURN configured: STUN only
    } finally {
      if (timer) clearTimeout(timer)
    }
  }
  return {
    iceServers,
    bundlePolicy: 'max-bundle',
  }
}

export function splitChunks(payload, id = 1, chunkSize = CHUNK_SIZE) {
  let actualId = id
  let actualChunkSize = chunkSize

  if (typeof id === 'object' && id !== null) {
    if (typeof id.id === 'number') actualId = id.id
    if (typeof id.chunkSize === 'number') actualChunkSize = id.chunkSize
  }

  const raw = typeof payload === 'string' ? payload : JSON.stringify(payload)
  const total = Math.ceil(raw.length / actualChunkSize) || 1
  const chunks = []
  for (let index = 0; index < total; index++) {
    const data = raw.slice(index * actualChunkSize, (index + 1) * actualChunkSize)
    chunks.push({ type: 'chunk', id: actualId, index, total, data })
  }
  return chunks
}

export class ChunkReassembler {
  constructor(maxSessions = 10) {
    this.maxSessions = maxSessions
    this.sessions = new Map()
  }

  addChunk(msg) {
    if (!msg || typeof msg !== 'object') return null
    const { id, index, total, data } = msg
    if (
      typeof id !== 'number' ||
      !Number.isInteger(index) ||
      !Number.isInteger(total) ||
      total <= 0 ||
      index < 0 ||
      index >= total ||
      typeof data !== 'string'
    ) {
      return null
    }
    let session = this.sessions.get(id)
    if (!session) {
      session = { total, received: 0, chunks: new Array(total) }
      this.sessions.set(id, session)
      if (this.sessions.size > this.maxSessions) {
        const oldestKey = this.sessions.keys().next().value
        if (oldestKey !== undefined) this.sessions.delete(oldestKey)
      }
    }
    if (session.chunks[index] === undefined) {
      session.chunks[index] = data
      session.received++
    }
    if (session.received === session.total) {
      this.sessions.delete(id)
      return session.chunks.join('')
    }
    return null
  }

  push(msg) {
    return this.addChunk(msg)
  }

  addChunkAndParse(msg) {
    const full = this.addChunk(msg)
    if (full === null) return null
    try {
      return JSON.parse(full)
    } catch {
      return null
    }
  }

  clear() {
    this.sessions.clear()
  }
}

function normalizeBytes(bytes) {
  if (bytes instanceof Uint8Array) {
    return Array.from(bytes)
  }
  if (bytes && bytes.buffer instanceof ArrayBuffer && typeof bytes.byteLength === 'number') {
    return Array.from(new Uint8Array(bytes.buffer, bytes.byteOffset || 0, bytes.byteLength))
  }
  return bytes
}

function parseBroadcasterArgs(arg1, arg2, arg3) {
  let opts = {}
  if (typeof arg1 === 'function') {
    opts = {
      onViewers: arg1,
      onError: arg2,
      onStatus: typeof arg3 === 'function' ? arg3 : undefined,
    }
  } else if (typeof arg1 === 'string') {
    opts = {
      signalingUrl: arg1,
      ...(typeof arg2 === 'object' ? arg2 : {}),
    }
  } else if (typeof arg1 === 'object' && arg1 !== null) {
    opts = { ...arg1 }
  }
  return opts
}

function parseViewerArgs(arg1, arg2, arg3, arg4) {
  let opts = {}
  if (typeof arg1 === 'string' && typeof arg2 === 'function') {
    opts = {
      roomId: arg1,
      onSnapshot: arg2,
      onStatus: arg3,
      onError: arg4,
    }
  } else if (typeof arg1 === 'string' && typeof arg2 === 'string') {
    opts = {
      signalingUrl: arg1,
      roomId: arg2,
      ...(typeof arg3 === 'object' ? arg3 : {}),
    }
  } else if (typeof arg1 === 'string' && typeof arg2 === 'object' && arg2 !== null) {
    if (arg2.roomId) {
      opts = { signalingUrl: arg1, ...arg2 }
    } else {
      opts = { roomId: arg1, ...arg2 }
    }
  } else if (typeof arg1 === 'object' && arg1 !== null) {
    opts = { ...arg1 }
  }
  return opts
}

export class Broadcaster {
  constructor(signalingUrlOrOptions = {}, maybeOptions = {}, maybeThird = undefined) {
    const opts = parseBroadcasterArgs(signalingUrlOrOptions, maybeOptions, maybeThird)
    this.signalingUrl = (opts.signalingUrl || DEFAULT_SIGNALING_URL || '').replace(/\/+$/, '')
    this.roomId = opts.roomId || randomRoomId()
    this.onViewers = opts.onViewers || (() => {})
    this.onStatus = opts.onStatus || (() => {})
    this.onError = opts.onError || (() => {})
    this.onWarning = opts.onWarning || opts.onError || (() => {})
    this.WebSocketClass = opts.WebSocket || (typeof globalThis !== 'undefined' ? globalThis.WebSocket : undefined)
    this.RTCPeerConnectionClass = opts.RTCPeerConnection || (typeof globalThis !== 'undefined' ? globalThis.RTCPeerConnection : undefined)

    this.ws = undefined
    this.pcs = new Map()
    this.dcs = new Map()
    this.pendingCandidates = new Map()
    this.optsIceServers = !!opts.iceServers
    this.iceConfig = { iceServers: opts.iceServers || DEFAULT_ICE, bundlePolicy: 'max-bundle' }
    this.latestBytes = undefined
    this.stopped = false
    this.opened = false
    this.rejectConnect = undefined
    this.connectTimeout = null
    this.nextChunkId = 1
  }

  watchUrl(baseUrl) {
    if (baseUrl) {
      const clean = baseUrl.replace(/#.*$/, '').replace(/\/+$/, '')
      return `${clean}/#watch=${this.roomId}`
    }
    if (typeof location !== 'undefined') {
      return `${location.origin}${location.pathname}#watch=${this.roomId}`
    }
    return `#watch=${this.roomId}`
  }

  async start(initialBytes) {
    if (initialBytes !== undefined) this.latestBytes = initialBytes
    const icePromise = (!this.optsIceServers && this.signalingUrl)
      ? getIceConfig(this.signalingUrl)
      : Promise.resolve(this.iceConfig)
    const connectPromise = this.connect()
    const [iceConfig] = await Promise.all([icePromise, connectPromise])
    this.iceConfig = iceConfig
    if (this.stopped) return
    if (typeof window !== 'undefined' && window.addEventListener) {
      window.addEventListener('beforeunload', this.beforeUnload)
    }
  }

  beforeUnload = () => {
    try {
      this.send({ type: 'close' })
    } catch {
      // ignore
    }
  }

  connect() {
    this.onStatus('connecting')
    const url = roomWsUrl(this.signalingUrl, this.roomId, 'broadcaster')
    const WS = this.WebSocketClass || globalThis.WebSocket
    if (!WS) {
      const err = new Error('WebSocket is not available')
      this.onError(err.message)
      return Promise.reject(err)
    }
    const ws = new WS(url)
    this.ws = ws

    return new Promise((resolve, reject) => {
      let settled = false

      const finish = (error) => {
        if (settled) return
        settled = true
        if (this.connectTimeout) {
          clearTimeout(this.connectTimeout)
          this.connectTimeout = null
        }
        this.rejectConnect = undefined
        if (error) reject(error)
        else resolve()
      }

      const fail = () => {
        if (this.stopped || this.ws !== ws) return
        if (!this.opened) {
          finish(new Error('Failed to connect to signaling service.'))
          try { ws.close() } catch {}
          return
        }
        this.stop()
        this.onError('Connection to signaling service lost.')
      }

      this.connectTimeout = setTimeout(() => {
        if (!this.opened) {
          finish(new Error('Signaling connection timed out.'))
          try { ws.close() } catch {}
        }
      }, 15000)

      this.rejectConnect = (error) => finish(error)

      ws.addEventListener('open', () => {
        if (this.stopped || this.ws !== ws) {
          finish(new Error('Broadcast cancelled.'))
          try { ws.close() } catch {}
          return
        }
        this.opened = true
        this.onStatus('live')
        finish()
      })

      ws.addEventListener('message', async (e) => {
        try {
          const msg = typeof e.data === 'string' ? JSON.parse(e.data) : e.data
          if (msg && typeof msg === 'object') {
            await this.onSignal(msg)
          }
        } catch {
          // ignore malformed signal
        }
      })

      ws.addEventListener('error', fail)
      ws.addEventListener('close', fail)
    })
  }

  push(bytes) {
    this.latestBytes = bytes
    for (const dc of this.dcs.values()) {
      this.sendSnapshot(dc, bytes)
    }
  }

  sendSnapshot(dc, bytes) {
    if (!dc || dc.readyState !== 'open') return
    const normalized = normalizeBytes(bytes)
    const payload = { type: 'snapshot', bytes: normalized }
    const raw = JSON.stringify(payload)
    if (raw.length <= CHUNK_SIZE) {
      try {
        dc.send(raw)
      } catch {
        // ignore send error
      }
      return
    }
    const id = this.nextChunkId++
    if (this.nextChunkId > 0x7fffffff) this.nextChunkId = 1
    const chunks = splitChunks(raw, id, CHUNK_SIZE)
    for (const chunk of chunks) {
      if (dc.readyState !== 'open') break
      try {
        dc.send(JSON.stringify(chunk))
      } catch {
        break
      }
    }
  }

  async onSignal(msg) {
    switch (msg.type) {
      case 'viewer_join':
        if (msg.viewerId) await this.addViewer(msg.viewerId)
        break
      case 'answer':
        if (msg.viewerId) {
          const pc = this.pcs.get(msg.viewerId)
          if (pc) {
            await pc.setRemoteDescription(msg.answer)
            const queued = this.pendingCandidates.get(msg.viewerId) ?? []
            this.pendingCandidates.delete(msg.viewerId)
            for (const c of queued) {
              try {
                await pc.addIceCandidate(c)
              } catch {
                // ignore late candidate
              }
            }
          }
        }
        break
      case 'candidate':
        if (msg.viewerId && msg.candidate) {
          const pc = this.pcs.get(msg.viewerId)
          if (!pc || !pc.remoteDescription) {
            const queued = this.pendingCandidates.get(msg.viewerId) ?? []
            queued.push(msg.candidate)
            this.pendingCandidates.set(msg.viewerId, queued)
          } else {
            try {
              await pc.addIceCandidate(msg.candidate)
            } catch {
              // ignore late candidate
            }
          }
        }
        break
      case 'viewer_leave':
        if (msg.viewerId) this.dropViewer(msg.viewerId)
        break
      case 'viewers':
        if (typeof msg.count === 'number') this.onViewers(msg.count)
        break
    }
  }

  async addViewer(viewerId) {
    this.dropViewer(viewerId)
    const PeerConn = this.RTCPeerConnectionClass || globalThis.RTCPeerConnection
    if (!PeerConn) {
      this.onError('RTCPeerConnection is not available')
      return
    }
    const pc = new PeerConn(this.iceConfig)
    this.pcs.set(viewerId, pc)

    const dc = pc.createDataChannel(DATA_CHANNEL, { ordered: true })
    this.dcs.set(viewerId, dc)

    dc.addEventListener('open', () => {
      if (this.latestBytes !== undefined && this.latestBytes !== null) {
        this.sendSnapshot(dc, this.latestBytes)
      }
    })

    const pendingOutgoing = []
    let offerSent = false

    pc.addEventListener('icecandidate', (e) => {
      if (e.candidate) {
        const candidate = typeof e.candidate.toJSON === 'function' ? e.candidate.toJSON() : e.candidate
        if (!offerSent) {
          pendingOutgoing.push(candidate)
        } else {
          this.send({ type: 'candidate', viewerId, candidate })
        }
      }
    })

    let failed = false
    const handleFailure = () => {
      if (failed) return
      failed = true
      this.onWarning('WebRTC connection to viewer failed (ICE failed).')
      this.dropViewer(viewerId)
    }

    pc.addEventListener('connectionstatechange', () => {
      if (pc.connectionState === 'failed') handleFailure()
    })
    pc.addEventListener('iceconnectionstatechange', () => {
      if (pc.iceConnectionState === 'failed') handleFailure()
    })

    try {
      const offer = await pc.createOffer()
      await pc.setLocalDescription(offer)
      if (this.pcs.get(viewerId) !== pc) return
      this.send({ type: 'offer', viewerId, offer })
      offerSent = true
      for (const candidate of pendingOutgoing) {
        this.send({ type: 'candidate', viewerId, candidate })
      }
    } catch {
      handleFailure()
    }
  }

  dropViewer(viewerId) {
    this.pendingCandidates.delete(viewerId)
    const dc = this.dcs.get(viewerId)
    if (dc) {
      try { dc.close() } catch {}
      this.dcs.delete(viewerId)
    }
    const pc = this.pcs.get(viewerId)
    if (pc) {
      try { pc.close() } catch {}
      this.pcs.delete(viewerId)
    }
  }

  send(msg) {
    if (this.ws?.readyState === 1) {
      try {
        this.ws.send(JSON.stringify(msg))
      } catch {
        // ignore send error
      }
    }
  }

  stop() {
    if (this.stopped) return
    this.stopped = true
    if (this.connectTimeout) {
      clearTimeout(this.connectTimeout)
      this.connectTimeout = null
    }
    if (typeof window !== 'undefined' && window.removeEventListener) {
      window.removeEventListener('beforeunload', this.beforeUnload)
    }
    this.rejectConnect?.(new Error('Broadcast cancelled.'))
    this.rejectConnect = undefined
    this.send({ type: 'close' })
    for (const id of Array.from(this.pcs.keys())) {
      this.dropViewer(id)
    }
    this.pendingCandidates.clear()
    if (this.ws) {
      try { this.ws.close() } catch {}
      this.ws = undefined
    }
    this.onStatus('ended')
  }
}

export class Viewer {
  constructor(signalingUrlOrRoomId, maybeRoomIdOrOptions = {}, maybeOptions = {}, maybeFourth = undefined) {
    const opts = parseViewerArgs(signalingUrlOrRoomId, maybeRoomIdOrOptions, maybeOptions, maybeFourth)
    this.signalingUrl = (opts.signalingUrl || DEFAULT_SIGNALING_URL || '').replace(/\/+$/, '')
    this.roomId = opts.roomId || ''
    this.onSnapshot = opts.onSnapshot || (() => {})
    this.onStatus = opts.onStatus || (() => {})
    this.onError = opts.onError || (() => {})
    this.WebSocketClass = opts.WebSocket || (typeof globalThis !== 'undefined' ? globalThis.WebSocket : undefined)
    this.RTCPeerConnectionClass = opts.RTCPeerConnection || (typeof globalThis !== 'undefined' ? globalThis.RTCPeerConnection : undefined)

    this.ws = undefined
    this.pc = undefined
    this.optsIceServers = !!opts.iceServers
    this.iceConfig = { iceServers: opts.iceServers || DEFAULT_ICE, bundlePolicy: 'max-bundle' }
    this.pendingCandidates = []
    this.reassembler = new ChunkReassembler()
    this.stopped = false
    this.backoff = 0
    this.reconnectTimer = null
  }

  async start() {
    if (!this.optsIceServers && this.signalingUrl) {
      this.iceConfig = await getIceConfig(this.signalingUrl)
    }
    this.connect()
  }

  connect() {
    this.onStatus(this.backoff === 0 ? 'connecting' : 'reconnecting')
    const url = roomWsUrl(this.signalingUrl, this.roomId, 'viewer')
    const WS = this.WebSocketClass || globalThis.WebSocket
    if (!WS) {
      const err = new Error('WebSocket is not available')
      this.onError(err.message)
      return
    }
    const ws = new WS(url)
    this.ws = ws

    ws.addEventListener('message', async (e) => {
      try {
        const msg = typeof e.data === 'string' ? JSON.parse(e.data) : e.data
        if (msg && typeof msg === 'object') {
          await this.onSignal(msg)
        }
      } catch {
        // ignore malformed signal
      }
    })

    ws.addEventListener('close', () => this.onWsClose())
    ws.addEventListener('error', () => {
      // ws close handles reconnect
    })
  }

  onWsClose() {
    if (this.stopped) return
    const delay = Math.min(16000, 2000 * 2 ** this.backoff)
    this.backoff++
    this.reconnectTimer = setTimeout(() => {
      if (!this.stopped) this.connect()
    }, delay)
  }

  async onSignal(msg) {
    switch (msg.type) {
      case 'offer':
        this.backoff = 0
        await this.accept(msg.offer)
        break
      case 'candidate':
        if (msg.candidate) await this.addCandidate(msg.candidate)
        break
      case 'closed':
        this.onStatus('ended')
        this.teardown()
        break
    }
  }

  async accept(offer) {
    if (this.pc) {
      try { this.pc.close() } catch {}
    }
    this.reassembler.clear()
    const PeerConn = this.RTCPeerConnectionClass || globalThis.RTCPeerConnection
    if (!PeerConn) {
      this.onError('RTCPeerConnection is not available')
      return
    }
    const pc = new PeerConn(this.iceConfig)
    this.pc = pc

    pc.addEventListener('datachannel', (e) => {
      const dc = e.channel
      dc.addEventListener('message', (ev) => {
        try {
          const msg = typeof ev.data === 'string' ? JSON.parse(ev.data) : ev.data
          if (msg && typeof msg === 'object') {
            if (msg.type === 'snapshot') {
              const bytes = msg.bytes !== undefined ? msg.bytes : msg.data
              this.onSnapshot(bytes)
              this.onStatus('live')
            } else if (msg.type === 'chunk') {
              const full = this.reassembler.addChunk(msg)
              if (full !== null) {
                try {
                  const parsed = JSON.parse(full)
                  const bytes = (parsed && typeof parsed === 'object' && 'bytes' in parsed)
                    ? parsed.bytes
                    : ((parsed && typeof parsed === 'object' && 'data' in parsed) ? parsed.data : parsed)
                  this.onSnapshot(bytes)
                  this.onStatus('live')
                } catch {
                  // ignore malformed chunk json
                }
              }
            } else if (Array.isArray(msg)) {
              this.onSnapshot(msg)
              this.onStatus('live')
            }
          }
        } catch {
          // ignore malformed frame
        }
      })

      dc.addEventListener('close', () => {
        if (!this.stopped) this.onStatus('reconnecting')
      })
    })

    const pendingOutgoing = []
    let answerSent = false

    pc.addEventListener('icecandidate', (e) => {
      if (e.candidate) {
        const candidate = typeof e.candidate.toJSON === 'function' ? e.candidate.toJSON() : e.candidate
        if (!answerSent) {
          pendingOutgoing.push(candidate)
        } else {
          this.send({ type: 'candidate', candidate })
        }
      }
    })

    let failed = false
    const handleFailure = () => {
      if (failed) return
      failed = true
      this.onError('WebRTC connection to broadcaster failed (ICE failed).')
    }

    pc.addEventListener('connectionstatechange', () => {
      if (pc.connectionState === 'failed') handleFailure()
    })
    pc.addEventListener('iceconnectionstatechange', () => {
      if (pc.iceConnectionState === 'failed') handleFailure()
    })

    try {
      await pc.setRemoteDescription(offer)
      const queued = this.pendingCandidates
      this.pendingCandidates = []
      for (const c of queued) {
        try {
          await pc.addIceCandidate(c)
        } catch {
          // ignore late candidate
        }
      }

      const answer = await pc.createAnswer()
      await pc.setLocalDescription(answer)
      if (this.pc !== pc) return
      this.send({ type: 'answer', answer })
      answerSent = true
      for (const candidate of pendingOutgoing) {
        this.send({ type: 'candidate', candidate })
      }
    } catch {
      handleFailure()
    }
  }

  async addCandidate(candidate) {
    if (!this.pc || !this.pc.remoteDescription) {
      this.pendingCandidates.push(candidate)
      return
    }
    try {
      await this.pc.addIceCandidate(candidate)
    } catch {
      // ignore late candidate
    }
  }

  send(msg) {
    if (this.ws?.readyState === 1) {
      try {
        this.ws.send(JSON.stringify(msg))
      } catch {
        // ignore send error
      }
    }
  }

  teardown() {
    this.stopped = true
    if (this.reconnectTimer) {
      clearTimeout(this.reconnectTimer)
      this.reconnectTimer = null
    }
    this.pendingCandidates = []
    this.reassembler.clear()
    if (this.pc) {
      try { this.pc.close() } catch {}
      this.pc = undefined
    }
    if (this.ws) {
      try { this.ws.close() } catch {}
      this.ws = undefined
    }
  }

  stop() {
    this.teardown()
    this.onStatus('ended')
  }
}
