import test from 'node:test'
import assert from 'node:assert/strict'
import {
  ROOM_ALPHABET,
  DEFAULT_ICE,
  CHUNK_SIZE,
  DATA_CHANNEL,
  randomRoomId,
  roomWsUrl,
  getIceConfig,
  splitChunks,
  ChunkReassembler,
  Broadcaster,
  Viewer,
} from '../share.js'

// Mock fetch by default to avoid DNS lookup thread allocation in constrained environments
globalThis.fetch = async (url) => {
  if (url && url.includes('/turn-credentials')) {
    return {
      ok: true,
      json: async () => ({ iceServers: DEFAULT_ICE }),
    }
  }
  return { ok: false, status: 404 }
}

// --- Mock Implementations for Headless Testing ---

class MockDataChannel {
  constructor(label, options = {}) {
    this.label = label
    this.ordered = options.ordered ?? true
    this.readyState = 'open'
    this.listeners = new Map()
    this.sent = []
  }

  addEventListener(type, cb) {
    if (!this.listeners.has(type)) this.listeners.set(type, [])
    this.listeners.get(type).push(cb)
  }

  removeEventListener(type, cb) {
    const list = this.listeners.get(type)
    if (list) {
      const idx = list.indexOf(cb)
      if (idx !== -1) list.splice(idx, 1)
    }
  }

  dispatchEvent(type, event = {}) {
    const list = this.listeners.get(type) || []
    for (const cb of list) cb(event)
  }

  send(data) {
    if (this.readyState !== 'open') throw new Error('Data channel not open')
    this.sent.push(data)
  }

  close() {
    this.readyState = 'closed'
    this.dispatchEvent('close', {})
  }
}

class MockRTCPeerConnection {
  constructor(config = {}) {
    this.config = config
    this.connectionState = 'new'
    this.iceConnectionState = 'new'
    this.remoteDescription = null
    this.localDescription = null
    this.iceCandidates = []
    this.dataChannels = []
    this.listeners = new Map()
  }

  addEventListener(type, cb) {
    if (!this.listeners.has(type)) this.listeners.set(type, [])
    this.listeners.get(type).push(cb)
  }

  dispatchEvent(type, event = {}) {
    const list = this.listeners.get(type) || []
    for (const cb of list) cb(event)
  }

  createDataChannel(label, options) {
    const dc = new MockDataChannel(label, options)
    this.dataChannels.push(dc)
    return dc
  }

  async createOffer() {
    return { type: 'offer', sdp: 'mock-offer-sdp' }
  }

  async createAnswer() {
    return { type: 'answer', sdp: 'mock-answer-sdp' }
  }

  async setLocalDescription(desc) {
    this.localDescription = desc
  }

  async setRemoteDescription(desc) {
    this.remoteDescription = desc
  }

  async addIceCandidate(candidate) {
    this.iceCandidates.push(candidate)
  }

  simulateIceCandidate(candidate) {
    this.dispatchEvent('icecandidate', {
      candidate: candidate
        ? { candidate: candidate.candidate, toJSON: () => candidate }
        : null,
    })
  }

  simulateConnectionFailure() {
    this.connectionState = 'failed'
    this.dispatchEvent('connectionstatechange', {})
  }

  simulateIceFailure() {
    this.iceConnectionState = 'failed'
    this.dispatchEvent('iceconnectionstatechange', {})
  }

  close() {
    this.connectionState = 'closed'
    for (const dc of this.dataChannels) {
      dc.close()
    }
  }
}

class MockWebSocket {
  static instances = []
  static OPEN = 1
  static CLOSED = 3

  constructor(url) {
    this.url = url
    this.readyState = 0 // CONNECTING
    this.listeners = new Map()
    this.sent = []
    MockWebSocket.instances.push(this)
  }

  addEventListener(type, cb) {
    if (!this.listeners.has(type)) this.listeners.set(type, [])
    this.listeners.get(type).push(cb)
  }

  removeEventListener(type, cb) {
    const list = this.listeners.get(type)
    if (list) {
      const idx = list.indexOf(cb)
      if (idx !== -1) list.splice(idx, 1)
    }
  }

  dispatchEvent(type, event = {}) {
    const list = this.listeners.get(type) || []
    for (const cb of list) cb(event)
  }

  simulateOpen() {
    this.readyState = MockWebSocket.OPEN
    this.dispatchEvent('open', {})
  }

  async simulateMessage(data) {
    const payload = typeof data === 'string' ? data : JSON.stringify(data)
    const list = this.listeners.get('message') || []
    for (const cb of list) {
      await cb({ data: payload })
    }
  }

  simulateError(err = new Error('WebSocket error')) {
    this.dispatchEvent('error', { error: err })
  }

  simulateClose(code = 1000, reason = '') {
    this.readyState = MockWebSocket.CLOSED
    this.dispatchEvent('close', { code, reason })
  }

  send(data) {
    if (this.readyState !== MockWebSocket.OPEN) throw new Error('WebSocket not open')
    this.sent.push(data)
  }

  close() {
    this.readyState = MockWebSocket.CLOSED
    this.dispatchEvent('close', { code: 1000, reason: 'closed by client' })
  }
}

// --- Test Suites ---

test('Room ID format and distribution', async (t) => {
  await t.test('generates 7-character string by default using base31 alphabet', () => {
    const id = randomRoomId()
    assert.equal(typeof id, 'string')
    assert.equal(id.length, 7)
    for (const ch of id) {
      assert.ok(ROOM_ALPHABET.includes(ch), `Character "${ch}" must be in ROOM_ALPHABET`)
    }
  })

  await t.test('supports custom lengths', () => {
    assert.equal(randomRoomId(4).length, 4)
    assert.equal(randomRoomId(10).length, 10)
    assert.equal(randomRoomId(12).length, 12)
  })

  await t.test('excludes ambiguous characters (0, O, 1, I)', () => {
    assert.ok(!ROOM_ALPHABET.includes('0'))
    assert.ok(!ROOM_ALPHABET.includes('O'))
    assert.ok(!ROOM_ALPHABET.includes('1'))
    assert.ok(!ROOM_ALPHABET.includes('I'))
    assert.equal(ROOM_ALPHABET.length, 32)
  })

  await t.test('distribution over 1000 IDs is unique and covers full alphabet', () => {
    const ids = new Set()
    const charCounts = new Map()
    for (const ch of ROOM_ALPHABET) {
      charCounts.set(ch, 0)
    }

    const COUNT = 1000
    for (let i = 0; i < COUNT; i++) {
      const id = randomRoomId(7)
      ids.add(id)
      for (const ch of id) {
        charCounts.set(ch, (charCounts.get(ch) || 0) + 1)
      }
    }

    // High uniqueness (1000 samples out of ~27.5 billion possible IDs)
    assert.equal(ids.size, COUNT, 'All 1000 generated IDs should be unique')

    // Every character in the base31 alphabet should appear at least once
    for (const [ch, count] of charCounts.entries()) {
      assert.ok(count > 0, `Character "${ch}" should appear in 7000 random characters`)
    }
  })
})

test('roomWsUrl helper', () => {
  assert.equal(
    roomWsUrl('https://example.com', 'ABCDEFG', 'broadcaster'),
    'wss://example.com/room/ABCDEFG?role=broadcaster',
  )
  assert.equal(
    roomWsUrl('http://127.0.0.1:8787/', 'XYZ2345', 'viewer'),
    'ws://127.0.0.1:8787/room/XYZ2345?role=viewer',
  )
  assert.equal(
    roomWsUrl('ws://custom-ws.internal', 'ROOM123', 'broadcaster'),
    'ws://custom-ws.internal/room/ROOM123?role=broadcaster',
  )
})

test('getIceConfig', async (t) => {
  await t.test('returns DEFAULT_ICE when signalingUrl is empty', async () => {
    const config = await getIceConfig('')
    assert.deepEqual(config.iceServers, DEFAULT_ICE)
    assert.equal(config.bundlePolicy, 'max-bundle')
  })

  await t.test('fetches TURN credentials from signaling endpoint if available', async () => {
    const originalFetch = globalThis.fetch
    const customServers = [
      { urls: 'turn:turn.example.com:3478', username: 'u', credential: 'p' },
    ]
    globalThis.fetch = async (url) => {
      assert.ok(url.includes('/turn-credentials'))
      return {
        ok: true,
        json: async () => ({ iceServers: customServers }),
      }
    }
    try {
      const config = await getIceConfig('https://signaling.example.com')
      assert.deepEqual(config.iceServers, customServers)
      assert.equal(config.bundlePolicy, 'max-bundle')
    } finally {
      globalThis.fetch = originalFetch
    }
  })

  await t.test('falls back to DEFAULT_ICE on network error', async () => {
    const originalFetch = globalThis.fetch
    globalThis.fetch = async () => {
      throw new Error('Network failure')
    }
    try {
      const config = await getIceConfig('https://offline.example.com')
      assert.deepEqual(config.iceServers, DEFAULT_ICE)
      assert.equal(config.bundlePolicy, 'max-bundle')
    } finally {
      globalThis.fetch = originalFetch
    }
  })
})

test('splitChunks and ChunkReassembler', async (t) => {
  await t.test('payload under 48 KB produces a single chunk', () => {
    const payload = 'Short string payload'
    const chunks = splitChunks(payload, 1, CHUNK_SIZE)
    assert.equal(chunks.length, 1)
    assert.equal(chunks[0].type, 'chunk')
    assert.equal(chunks[0].id, 1)
    assert.equal(chunks[0].index, 0)
    assert.equal(chunks[0].total, 1)
    assert.equal(chunks[0].data, payload)

    const reassembler = new ChunkReassembler()
    const reassembled = reassembler.addChunk(chunks[0])
    assert.equal(reassembled, payload)
  })

  await t.test('payload over 48 KB is split into multiple chunks and reassembled in order', () => {
    // Generate ~120 KB string
    const targetSize = 120 * 1024
    let raw = ''
    while (raw.length < targetSize) {
      raw += 'ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789!@#$%^&*()'
    }
    const chunks = splitChunks(raw, 42, CHUNK_SIZE)
    const expectedTotal = Math.ceil(raw.length / CHUNK_SIZE)
    assert.equal(chunks.length, expectedTotal)
    assert.ok(expectedTotal > 2, `Expected at least 3 chunks, got ${expectedTotal}`)

    const reassembler = new ChunkReassembler()
    for (let i = 0; i < chunks.length - 1; i++) {
      const result = reassembler.addChunk(chunks[i])
      assert.equal(result, null, `Intermediate chunk ${i} should return null`)
    }

    const finalResult = reassembler.addChunk(chunks[chunks.length - 1])
    assert.equal(finalResult, raw, 'Reassembled string should match original payload exactly')
  })

  await t.test('reassembles chunks delivered out of order', () => {
    const raw = 'The quick brown fox jumps over the lazy dog. WebRTC chunking works reliably!'
    const chunkSize = 16
    const chunks = splitChunks(raw, 7, chunkSize)
    assert.ok(chunks.length > 3)

    // Reverse order
    const reversed = [...chunks].reverse()
    const reassembler = new ChunkReassembler()
    for (let i = 0; i < reversed.length - 1; i++) {
      assert.equal(reassembler.addChunk(reversed[i]), null)
    }
    const result = reassembler.addChunk(reversed[reversed.length - 1])
    assert.equal(result, raw)
  })

  await t.test('handles duplicate chunks idempotently', () => {
    const raw = 'Idempotent chunk transmission test with duplicates'
    const chunks = splitChunks(raw, 9, 10)
    const reassembler = new ChunkReassembler()

    // Feed chunk 0 twice
    assert.equal(reassembler.addChunk(chunks[0]), null)
    assert.equal(reassembler.addChunk(chunks[0]), null)

    for (let i = 1; i < chunks.length - 1; i++) {
      assert.equal(reassembler.addChunk(chunks[i]), null)
    }
    const result = reassembler.addChunk(chunks[chunks.length - 1])
    assert.equal(result, raw)
  })

  await t.test('addChunkAndParse parses valid JSON and handles invalid JSON', () => {
    const obj = { model: 'car', parts: [1, 2, 3], nested: { count: 99 } }
    const chunks = splitChunks(obj, 101, 15)
    const reassembler = new ChunkReassembler()

    for (let i = 0; i < chunks.length - 1; i++) {
      assert.equal(reassembler.addChunkAndParse(chunks[i]), null)
    }
    const parsed = reassembler.addChunkAndParse(chunks[chunks.length - 1])
    assert.deepEqual(parsed, obj)

    // Invalid JSON
    const malformed = splitChunks('{not valid json', 102, 5)
    for (let i = 0; i < malformed.length - 1; i++) {
      assert.equal(reassembler.addChunkAndParse(malformed[i]), null)
    }
    const invalidResult = reassembler.addChunkAndParse(malformed[malformed.length - 1])
    assert.equal(invalidResult, null)
  })

  await t.test('evicts oldest session when maxSessions is exceeded', () => {
    const reassembler = new ChunkReassembler(2)
    // Start session 1
    reassembler.addChunk({ id: 1, index: 0, total: 2, data: 'part1' })
    // Start session 2
    reassembler.addChunk({ id: 2, index: 0, total: 2, data: 'part1' })
    // Start session 3 -> evicts session 1
    reassembler.addChunk({ id: 3, index: 0, total: 2, data: 'part1' })

    // Finishing session 1 should now create a new session instead of finishing
    const result1 = reassembler.addChunk({ id: 1, index: 1, total: 2, data: 'part2' })
    assert.equal(result1, null, 'Session 1 was evicted, so index 1 alone is incomplete')

    // Finishing session 3 should succeed
    const result3 = reassembler.addChunk({ id: 3, index: 1, total: 2, data: 'part2' })
    assert.equal(result3, 'part1part2')
  })

  await t.test('rejects malformed or out-of-bounds chunks', () => {
    const reassembler = new ChunkReassembler()
    assert.equal(reassembler.addChunk(null), null)
    assert.equal(reassembler.addChunk(undefined), null)
    assert.equal(reassembler.addChunk('string'), null)
    assert.equal(reassembler.addChunk({ id: 'bad', index: 0, total: 1, data: 'hi' }), null)
    assert.equal(reassembler.addChunk({ id: 1, index: 0.5, total: 1, data: 'hi' }), null)
    assert.equal(reassembler.addChunk({ id: 1, index: 0, total: 1.5, data: 'hi' }), null)
    assert.equal(reassembler.addChunk({ id: 1, index: 0, total: 0, data: 'hi' }), null)
    assert.equal(reassembler.addChunk({ id: 1, index: 0, total: -1, data: 'hi' }), null)
    assert.equal(reassembler.addChunk({ id: 1, index: -1, total: 2, data: 'hi' }), null)
    assert.equal(reassembler.addChunk({ id: 1, index: 2, total: 2, data: 'hi' }), null)
    assert.equal(reassembler.addChunk({ id: 1, index: 0, total: 1, data: 123 }), null)
  })

  await t.test('clear resets all active sessions', () => {
    const reassembler = new ChunkReassembler()
    reassembler.addChunk({ id: 5, index: 0, total: 2, data: 'first' })
    reassembler.clear()
    const result = reassembler.addChunk({ id: 5, index: 1, total: 2, data: 'second' })
    assert.equal(result, null, 'Session was cleared')
  })
})

test('Broadcaster lifecycle and status reporting', async (t) => {
  await t.test('initializes with default options and generates roomId', () => {
    const b = new Broadcaster({
      signalingUrl: 'https://sig.example.com',
    })
    assert.ok(b.roomId)
    assert.equal(b.roomId.length, 7)
    assert.equal(b.signalingUrl, 'https://sig.example.com')
    assert.equal(b.watchUrl('https://app.example.com/play'), 'https://app.example.com/play/#watch=' + b.roomId)
    b.stop()
  })

  await t.test('transitions connecting -> live on WebSocket open', async () => {
    const statuses = []
    const b = new Broadcaster({
      signalingUrl: 'https://sig.example.com',
      WebSocket: MockWebSocket,
      RTCPeerConnection: MockRTCPeerConnection,
      onStatus: (s) => statuses.push(s),
    })

    const startPromise = b.start()
    const ws = b.ws
    assert.ok(ws)
    assert.deepEqual(statuses, ['connecting'])

    ws.simulateOpen()
    await startPromise
    assert.deepEqual(statuses, ['connecting', 'live'])
    b.stop()
    assert.deepEqual(statuses, ['connecting', 'live', 'ended'])
  })

  await t.test('reports viewer counts from signaling server', async () => {
    const viewerCounts = []
    const b = new Broadcaster({
      signalingUrl: 'https://sig.example.com',
      WebSocket: MockWebSocket,
      RTCPeerConnection: MockRTCPeerConnection,
      onViewers: (cnt) => viewerCounts.push(cnt),
    })

    const startPromise = b.start()
    b.ws.simulateOpen()
    await startPromise

    b.ws.simulateMessage({ type: 'viewers', count: 5 })
    assert.deepEqual(viewerCounts, [5])
    b.stop()
  })

  await t.test('creates peer connection and data channel on viewer_join', async () => {
    const b = new Broadcaster({
      signalingUrl: 'https://sig.example.com',
      WebSocket: MockWebSocket,
      RTCPeerConnection: MockRTCPeerConnection,
    })

    const startPromise = b.start()
    b.ws.simulateOpen()
    await startPromise

    await b.ws.simulateMessage({ type: 'viewer_join', viewerId: 'v123' })
    assert.ok(b.pcs.has('v123'), 'RTCPeerConnection created for viewer')
    assert.ok(b.dcs.has('v123'), 'RTCDataChannel created for viewer')
    assert.equal(b.dcs.get('v123').label, DATA_CHANNEL)

    // Check offer was sent to signaling socket
    const offerSent = b.ws.sent.map((s) => JSON.parse(s)).find((m) => m.type === 'offer')
    assert.ok(offerSent, 'Offer signal was sent')
    assert.equal(offerSent.viewerId, 'v123')
    assert.equal(offerSent.offer.type, 'offer')

    b.stop()
  })

  await t.test('flushes latest snapshot bytes when data channel opens', async () => {
    const b = new Broadcaster({
      signalingUrl: 'https://sig.example.com',
      WebSocket: MockWebSocket,
      RTCPeerConnection: MockRTCPeerConnection,
    })

    const testBytes = [10, 20, 30, 40, 50]
    const startPromise = b.start(testBytes)
    b.ws.simulateOpen()
    await startPromise

    await b.ws.simulateMessage({ type: 'viewer_join', viewerId: 'viewerA' })
    const dc = b.dcs.get('viewerA')

    // Simulate data channel open
    dc.dispatchEvent('open', {})
    assert.equal(dc.sent.length, 1)
    const sentMsg = JSON.parse(dc.sent[0])
    assert.equal(sentMsg.type, 'snapshot')
    assert.deepEqual(sentMsg.bytes, testBytes)

    b.stop()
  })

  await t.test('push sends chunks when payload exceeds 48 KB', async () => {
    const b = new Broadcaster({
      signalingUrl: 'https://sig.example.com',
      WebSocket: MockWebSocket,
      RTCPeerConnection: MockRTCPeerConnection,
    })

    const startPromise = b.start()
    b.ws.simulateOpen()
    await startPromise

    await b.ws.simulateMessage({ type: 'viewer_join', viewerId: 'viewerLarge' })
    const dc = b.dcs.get('viewerLarge')
    dc.dispatchEvent('open', {})

    // Large payload: 15,000 integers (~60+ KB in JSON)
    const largeBytes = new Array(15000).fill(255)
    b.push(largeBytes)

    assert.ok(dc.sent.length > 1, `Large payload should be chunked, sent ${dc.sent.length} messages`)
    const firstChunk = JSON.parse(dc.sent[0])
    assert.equal(firstChunk.type, 'chunk')
    assert.equal(firstChunk.index, 0)
    assert.ok(firstChunk.total > 1)

    // Reassemble dc sent chunks to verify integrity
    const reassembler = new ChunkReassembler()
    let full = null
    for (const raw of dc.sent) {
      full = reassembler.addChunk(JSON.parse(raw))
    }
    assert.ok(full !== null, 'All chunks should reassemble')
    const parsed = JSON.parse(full)
    assert.equal(parsed.type, 'snapshot')
    assert.deepEqual(parsed.bytes, largeBytes)

    // Verify chunk ID rollover at 0x7fffffff
    b.nextChunkId = 0x7fffffff
    dc.sent = []
    b.push(largeBytes)
    const rolloverChunk = JSON.parse(dc.sent[0])
    assert.equal(rolloverChunk.id, 0x7fffffff)
    assert.equal(b.nextChunkId, 1, 'nextChunkId should roll over to 1')

    b.stop()
  })

  await t.test('error handling when signaling fails before connect', async () => {
    const errors = []
    const b = new Broadcaster({
      signalingUrl: 'https://sig.example.com',
      WebSocket: MockWebSocket,
      onError: (e) => errors.push(e),
    })

    const startPromise = b.start()
    b.ws.simulateError(new Error('Signaling unavailable'))
    await assert.rejects(startPromise, /Failed to connect/)
    b.stop()
  })

  await t.test('stop tears down connections and sends close signal', async () => {
    const b = new Broadcaster({
      signalingUrl: 'https://sig.example.com',
      WebSocket: MockWebSocket,
      RTCPeerConnection: MockRTCPeerConnection,
    })

    const startPromise = b.start()
    b.ws.simulateOpen()
    await startPromise

    await b.ws.simulateMessage({ type: 'viewer_join', viewerId: 'v1' })
    const pc = b.pcs.get('v1')
    const dc = b.dcs.get('v1')

    b.stop()
    assert.equal(pc.connectionState, 'closed')
    assert.equal(dc.readyState, 'closed')
    assert.equal(b.pcs.size, 0)
    assert.equal(b.dcs.size, 0)
    assert.equal(b.ws, undefined)

    // Check close message was sent before socket cleared
    const lastSent = MockWebSocket.instances[MockWebSocket.instances.length - 1].sent
    const closeSent = lastSent.map((s) => JSON.parse(s)).find((m) => m.type === 'close')
    assert.ok(closeSent, 'Close signal was emitted')
  })
})

test('Viewer lifecycle, chunk handling and error reporting', async (t) => {
  await t.test('connects and reports connecting status', async () => {
    const statuses = []
    const v = new Viewer({
      signalingUrl: 'https://sig.example.com',
      roomId: 'TEST1234',
      WebSocket: MockWebSocket,
      RTCPeerConnection: MockRTCPeerConnection,
      onStatus: (s) => statuses.push(s),
    })

    await v.start()
    assert.deepEqual(statuses, ['connecting'])
    assert.equal(v.roomId, 'TEST1234')
    v.stop()
    assert.deepEqual(statuses, ['connecting', 'ended'])
  })

  await t.test('handles SDP offer and trickles ICE candidate', async () => {
    const v = new Viewer({
      signalingUrl: 'https://sig.example.com',
      roomId: 'TEST1234',
      WebSocket: MockWebSocket,
      RTCPeerConnection: MockRTCPeerConnection,
    })

    await v.start()
    v.ws.simulateOpen()

    // Broadcaster sends offer via signaling
    await v.ws.simulateMessage({
      type: 'offer',
      offer: { type: 'offer', sdp: 'mock-offer-sdp' },
    })

    assert.ok(v.pc, 'RTCPeerConnection was created on offer')
    assert.deepEqual(v.pc.remoteDescription, { type: 'offer', sdp: 'mock-offer-sdp' })

    // Check answer signal sent back to broadcaster
    const answerMsg = v.ws.sent.map((s) => JSON.parse(s)).find((m) => m.type === 'answer')
    assert.ok(answerMsg, 'Answer was sent')
    assert.equal(answerMsg.answer.type, 'answer')

    // Simulate candidate trickle from PC
    v.pc.simulateIceCandidate({ candidate: 'candidate:1 1 UDP 12345' })
    const candidateMsg = v.ws.sent.map((s) => JSON.parse(s)).find((m) => m.type === 'candidate')
    assert.ok(candidateMsg, 'ICE candidate was sent')

    v.stop()
  })

  await t.test('receives direct snapshot on data channel and fires onSnapshot', async () => {
    const receivedSnapshots = []
    const statuses = []
    const v = new Viewer({
      signalingUrl: 'https://sig.example.com',
      roomId: 'TEST1234',
      WebSocket: MockWebSocket,
      RTCPeerConnection: MockRTCPeerConnection,
      onSnapshot: (snap) => receivedSnapshots.push(snap),
      onStatus: (s) => statuses.push(s),
    })

    await v.start()
    v.ws.simulateOpen()
    await v.ws.simulateMessage({
      type: 'offer',
      offer: { type: 'offer', sdp: 'mock-offer-sdp' },
    })

    // Simulate data channel creation from peer connection
    const dc = new MockDataChannel(DATA_CHANNEL)
    v.pc.dispatchEvent('datachannel', { channel: dc })

    const testBytes = [1, 2, 3, 4, 5, 42]
    dc.dispatchEvent('message', {
      data: JSON.stringify({ type: 'snapshot', bytes: testBytes }),
    })

    assert.deepEqual(receivedSnapshots, [testBytes])
    assert.ok(statuses.includes('live'))

    v.stop()
  })

  await t.test('reassembles incoming chunks and forwards complete bytes to onSnapshot', async () => {
    const receivedSnapshots = []
    const statuses = []
    const v = new Viewer({
      signalingUrl: 'https://sig.example.com',
      roomId: 'TEST1234',
      WebSocket: MockWebSocket,
      RTCPeerConnection: MockRTCPeerConnection,
      onSnapshot: (snap) => receivedSnapshots.push(snap),
      onStatus: (s) => statuses.push(s),
    })

    await v.start()
    v.ws.simulateOpen()
    await v.ws.simulateMessage({
      type: 'offer',
      offer: { type: 'offer', sdp: 'mock-offer-sdp' },
    })

    const dc = new MockDataChannel(DATA_CHANNEL)
    v.pc.dispatchEvent('datachannel', { channel: dc })

    // Split large snapshot payload (>48 KB) into chunks
    const largeBytes = new Array(20000).fill(123)
    const payload = JSON.stringify({ type: 'snapshot', bytes: largeBytes })
    const chunks = splitChunks(payload, 888, 32 * 1024)

    assert.ok(chunks.length > 1)

    // Feed all but last chunk
    for (let i = 0; i < chunks.length - 1; i++) {
      dc.dispatchEvent('message', { data: JSON.stringify(chunks[i]) })
      assert.equal(receivedSnapshots.length, 0, 'Incomplete chunks should not trigger snapshot')
    }

    // Feed last chunk
    dc.dispatchEvent('message', { data: JSON.stringify(chunks[chunks.length - 1]) })
    assert.equal(receivedSnapshots.length, 1)
    assert.deepEqual(receivedSnapshots[0], largeBytes)
    assert.ok(statuses.includes('live'))

    v.stop()
  })

  await t.test('reports reconnecting when data channel closes', async () => {
    const statuses = []
    const v = new Viewer({
      signalingUrl: 'https://sig.example.com',
      roomId: 'TEST1234',
      WebSocket: MockWebSocket,
      RTCPeerConnection: MockRTCPeerConnection,
      onStatus: (s) => statuses.push(s),
    })

    await v.start()
    v.ws.simulateOpen()
    await v.ws.simulateMessage({
      type: 'offer',
      offer: { type: 'offer', sdp: 'mock-offer-sdp' },
    })

    const dc = new MockDataChannel(DATA_CHANNEL)
    v.pc.dispatchEvent('datachannel', { channel: dc })

    dc.close()
    assert.ok(statuses.includes('reconnecting'))
    v.stop()
  })

  await t.test('broadcaster closed signal ends viewer session', async () => {
    const statuses = []
    const v = new Viewer({
      signalingUrl: 'https://sig.example.com',
      roomId: 'TEST1234',
      WebSocket: MockWebSocket,
      RTCPeerConnection: MockRTCPeerConnection,
      onStatus: (s) => statuses.push(s),
    })

    await v.start()
    v.ws.simulateOpen()
    await v.ws.simulateMessage({ type: 'closed' })

    assert.ok(statuses.includes('ended'))
    assert.equal(v.pc, undefined)
    assert.equal(v.ws, undefined)
  })

  await t.test('reports error on ICE failure', async () => {
    const errors = []
    const v = new Viewer({
      signalingUrl: 'https://sig.example.com',
      roomId: 'TEST1234',
      WebSocket: MockWebSocket,
      RTCPeerConnection: MockRTCPeerConnection,
      onError: (err) => errors.push(err),
    })

    await v.start()
    v.ws.simulateOpen()
    await v.ws.simulateMessage({
      type: 'offer',
      offer: { type: 'offer', sdp: 'mock-offer-sdp' },
    })

    v.pc.simulateIceFailure()
    assert.ok(errors.length > 0)
    assert.match(errors[0], /ICE failed/)
    v.stop()
  })
})
