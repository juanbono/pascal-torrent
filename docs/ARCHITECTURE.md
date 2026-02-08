# PascalTorrent Architecture

High-level architecture and module interactions.

---

## System Overview

```
┌─────────────────────────────────────────────────────────────────┐
│                      PascalTorrent Client                        │
├─────────────────────────────────────────────────────────────────┤
│                                                                   │
│  ┌──────────────┐    ┌──────────────┐    ┌──────────────┐      │
│  │   BitTorrent │    │   File I/O   │    │   Network    │      │
│  │   Protocol   │    │   Manager    │    │   Layer      │      │
│  └──────┬───────┘    └──────┬───────┘    └──────┬───────┘      │
│         │                   │                   │               │
│         └───────────────────┼───────────────────┘               │
│                             │                                   │
│  ┌──────────────────────────┴──────────────────────────┐       │
│  │                  Core Modules                        │       │
│  │  ┌──────────┐ ┌──────────┐ ┌──────────┐           │       │
│  │  │ Bencode  │ │ Metainfo │ │  Utils   │           │       │
│  │  └──────────┘ └──────────┘ └──────────┘           │       │
│  │  ┌──────────┐ ┌──────────┐ ┌──────────┐           │       │
│  │  │  SHA1    │ │ Logging  │ │Protocol  │           │       │
│  │  └──────────┘ └──────────┘ └──────────┘           │       │
│  └────────────────────────────────────────────────────┘       │
│                                                                   │
│  ┌────────────────────────────────────────────────────┐       │
│  │               Infrastructure Layer                   │       │
│  │         (Socket Wrapper, OS Abstractions)           │       │
│  └────────────────────────────────────────────────────┘       │
│                                                                   │
└─────────────────────────────────────────────────────────────────┘
```

---

## Module Dependency Graph

```
                         ┌──────────────┐
                         │    utils     │
                         └──────┬───────┘
                                │
         ┌──────────────────────┼──────────────────────┐
         │                      │                      │
         ▼                      ▼                      ▼
  ┌──────────────┐      ┌──────────────┐      ┌──────────────┐
  │   bencode    │      │  sha1utils   │      │   logging    │
  └──────┬───────┘      └──────┬───────┘      └──────┬───────┘
         │                      │                      │
         └──────────────────────┼──────────────────────┘
                                │
                                ▼
                       ┌──────────────┐
                       │  metainfo    │
                       └──────┬───────┘
                              │
              ┌───────────────┼───────────────┐
              │               │               │
              ▼               ▼               ▼
       ┌──────────────┐ ┌──────────────┐ ┌──────────────┐
       │  filemgr     │ │  protocol    │ │   sockwrap   │
       └──────────────┘ └──────┬───────┘ └──────────────┘
                               │
                               ▼
                       ┌──────────────┐
                       │  Application │
                       └──────────────┘
```

---

## Data Flow Diagrams

### 1. Loading a Torrent File

```
┌─────────────┐     ┌─────────────────┐     ┌──────────────┐
│   .torrent  │────▶│ BencodeDecode   │────▶│ PBencodeValue│
│    File     │     │     File        │     │   (Root)     │
└─────────────┘     └─────────────────┘     └──────┬───────┘
                                                   │
                                                   ▼
┌─────────────┐     ┌─────────────────┐     ┌──────────────┐
│ PTorrentMeta│◀────│ParseTorrent     │◀────│  Info Dict   │
│    (Meta)   │     │   Bencode       │     │  (from Root) │
└──────┬──────┘     └─────────────────┘     └──────────────┘
       │
       ▼
┌──────────────────────────────────────────────────────────────┐
│  Extracted Information:                                      │
│  • Name, PieceLength, Pieces (hashes)                       │
│  • Files list (for multi-file torrents)                     │
│  • Announce URL(s)                                          │
│  • Info Hash (computed from info dict)                      │
└──────────────────────────────────────────────────────────────┘
```

### 2. Downloading a Piece

```
┌─────────────┐     ┌─────────────────┐     ┌──────────────┐
│   Peer      │────▶│ SocketReceive   │────▶│   Buffer     │
│ Connection  │     │                 │     │  (Raw Data)  │
└─────────────┘     └─────────────────┘     └──────┬───────┘
                                                   │
                                                   ▼
┌─────────────┐     ┌─────────────────┐     ┌──────────────┐
│  TWireMsg   │◀────│  DecodeMessage  │◀────│   Validate   │
│  (PIECE)    │     │                 │     │   (Length)   │
└──────┬──────┘     └─────────────────┘     └──────────────┘
       │
       ▼
┌──────────────────────────────────────────────────────────────┐
│  Write to Disk:                                              │
│  • FileManagerWritePiece()                                  │
│  • Verify SHA1 hash                                         │
│  • Mark as verified in bitfield                             │
└──────────────────────────────────────────────────────────────┘
```

### 3. Saving Progress (Resume)

```
┌─────────────┐     ┌─────────────────┐     ┌──────────────┐
│ FileManager │────▶│VerifiedPieces   │────▶│   Bitfield   │
│             │     │   (Bitfield)    │     │   (Bytes)    │
└─────────────┘     └─────────────────┘     └──────┬───────┘
                                                   │
                                                   ▼
                                            ┌──────────────┐
                                            │  .bitfield   │
                                            │    File      │
                                            └──────────────┘
```

---

## Module Responsibilities

### Bencode (`src/bencode.pas`)
**Purpose:** Encode/decode BitTorrent's bencode format

**Key Responsibilities:**
- Parse bencoded strings, integers, lists, dictionaries
- Encode data structures to bencode format
- Memory management for parsed structures

**Public Interface:**
- `BencodeDecode*()` - Decoding functions
- `BencodeEncode*()` - Encoding functions
- `BencodeNew*()` - Value creation
- `BencodeDict*/List*()` - Structure manipulation

---

### Metainfo (`src/metainfo.pas`)
**Purpose:** Parse and manage .torrent file metadata

**Key Responsibilities:**
- Parse .torrent files
- Extract file list, piece hashes, tracker info
- Compute info hash
- Handle single-file and multi-file torrents

**Public Interface:**
- `ParseTorrentFile()` - Parse .torrent from disk
- `GetPieceHash()` - Get hash for specific piece
- `GetInfoHashHex()` - Get hex string of info hash

---

### File Manager (`src/filemgr.pas`)
**Purpose:** Piece-based file I/O with verification

**Key Responsibilities:**
- Read/write pieces to disk
- Handle multi-file torrents (file boundaries)
- Verify pieces against SHA1 hashes
- Track verified pieces (bitfield)

**Public Interface:**
- `FileManagerCreate/Destroy()` - Lifecycle
- `FileManagerRead/WritePiece()` - I/O operations
- `FileManagerVerifyPiece()` - Hash verification

---

### Protocol (`src/protocol.pas`)
**Purpose:** BitTorrent Peer Wire Protocol

**Key Responsibilities:**
- Encode/decode protocol messages
- Handshake encoding/decoding
- Message validation

**Public Interface:**
- `Build*()` - Build messages (Choke, Request, Piece, etc.)
- `DecodeMessage()` - Decode wire messages
- `Encode/DecodeHandshake()` - Handshake handling

---

### Socket Wrapper (`src/sockwrap.pas`)
**Purpose:** Cross-platform TCP socket abstraction

**Key Responsibilities:**
- Cross-platform socket operations
- Connection management (client/server)
- Non-blocking I/O support

**Public Interface:**
- `SocketCreate/Destroy()` - Lifecycle
- `SocketConnect/Bind/Listen/Accept()` - Connection
- `SocketSend/Receive()` - Data transfer

---

### SHA1 Utils (`src/sha1utils.pas`)
**Purpose:** SHA1 hashing for piece verification

**Key Responsibilities:**
- SHA1 hashing (one-shot and incremental)
- File hashing with progress callbacks
- Hex conversion

**Public Interface:**
- `SHA1String/Buffer/File()` - One-shot hashing
- `SHA1Init/Update/Final()` - Incremental hashing
- `SHA1DigestToHex()` - Hex conversion

---

### Utils (`src/utils.pas`)
**Purpose:** General utility functions

**Key Responsibilities:**
- Linked list operations
- Dynamic buffers
- String formatting (bytes, speed, duration)
- Path utilities
- Binary operations (hex, big-endian)

---

### Logging (`src/logging.pas`)
**Purpose:** Thread-safe logging

**Key Responsibilities:**
- Log message queuing and output
- Multiple destinations (console, file)
- Log level filtering
- Colored output support

---

## Memory Management

### Ownership Rules

```
┌─────────────────────────────────────────────────────────────┐
│                    Memory Ownership                          │
├─────────────────────────────────────────────────────────────┤
│                                                              │
│  Allocated By        │  Owned By          │  Freed By       │
│  ────────────────────┼────────────────────┼──────────────── │
│  BencodeDecode()     │  Caller            │  BencodeFree()  │
│  ParseTorrentFile()  │  Caller            │ FreeTorrentMeta │
│  FileManagerCreate() │  Caller            │ FileManagerDest │
│  SocketCreate()      │  Caller            │ SocketDestroy() │
│  LoggerCreate()      │  Caller            │ LoggerDestroy() │
│  DynBufferCreate()   │  Caller            │ DynBufferFree() │
│                                                              │
└─────────────────────────────────────────────────────────────┘
```

### Allocation Patterns

**Pattern 1: Create/Destroy Pair**
```pascal
FM := FileManagerCreate(...);
try
  // Use FM
finally
  FileManagerDestroy(FM);
end;
```

**Pattern 2: Parse/Free**
```pascal
Result := ParseTorrentFile('file.torrent', Meta);
if Result.Success then
try
  // Use Meta
finally
  FreeTorrentMeta(Meta);
end;
```

**Pattern 3: Decode/Free**
```pascal
Result := BencodeDecodeString(Data, Value);
if Result.Success then
try
  // Use Value
finally
  BencodeFree(Value);
end;
```

---

## Threading Model

```
┌─────────────────────────────────────────────────────────────┐
│                   Thread Safety Map                          │
├─────────────────────────────────────────────────────────────┤
│                                                              │
│  Module              │  Thread-Safe │  Notes                 │
│  ────────────────────┼──────────────┼─────────────────────── │
│  Bencode             │     No       │ Create per thread      │
│  Metainfo            │     No       │ Read-only after init   │
│  File Manager        │     No       │ Single thread only     │
│  Protocol            │     Yes      │ Stateless functions    │
│  Socket              │     No       │ One context/thread     │
│  SHA1                │     Yes      │ Context per thread     │
│  Utils               │     Yes      │ Stateless              │
│  Logging             │     Yes      │ Internal locking       │
│                                                              │
└─────────────────────────────────────────────────────────────┘
```

---

## Error Handling Strategy

```
┌─────────────────────────────────────────────────────────────┐
│                   Error Handling                             │
├─────────────────────────────────────────────────────────────┤
│                                                              │
│  1. Boolean Returns                                          │
│     function DoSomething(): Boolean;                        │
│     - True = Success, False = Failure                       │
│                                                              │
│  2. Integer Error Codes                                      │
│     function SocketOp(): Integer;                           │
│     - 0 or positive = Success                               │
│     - Negative = Error code                                 │
│                                                              │
│  3. Result Records                                           │
│     TResult = record                                        │
│       Success: Boolean;                                     │
│       ErrorMsg: string;                                     │
│     end;                                                    │
│                                                              │
│  4. Nil Pointer Returns                                      │
│     function CreateX(): PX;                                 │
│     - Returns nil on failure                                │
│                                                              │
└─────────────────────────────────────────────────────────────┘
```

---

## Build System

```
Makefile Targets:
├── all          - Build all tests
├── test         - Run all tests
├── clean        - Clean build artifacts
├── debug        - Debug build
├── release      - Release build
├── lint         - Strict warning check
├── coverage     - Coverage build
└── Individual test targets:
    ├── test-bencode
    ├── test-protocol
    ├── test-filemgr
    └── ...
```

---

## Extension Points

### Adding a New Module

1. Create `src/newmodule.pas`
2. Create `tests/test_newmodule.pas`
3. Add to `Makefile`
4. Update `AGENTS.md` documentation

### Adding Protocol Extensions

1. Define new message ID constant
2. Add `BuildNewMessage()` function
3. Update `DecodeMessage()` case statement
4. Add test cases

### Adding Socket Features

1. Extend `TSocketContext` if needed
2. Add platform-specific code with `{$IFDEF}`
3. Update test suite

---

*End of Architecture Document*
