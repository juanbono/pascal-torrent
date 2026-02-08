# PascalTorrent API Reference

Complete API reference for all public functions in PascalTorrent.

---

## Table of Contents

1. [Bencode Module](#bencode-module)
2. [Metainfo Module](#metainfo-module)
3. [File Manager Module](#file-manager-module)
4. [Protocol Module](#protocol-module)
5. [Socket Wrapper Module](#socket-wrapper-module)
6. [SHA1 Utils Module](#sha1-utils-module)
7. [Utils Module](#utils-module)
8. [Logging Module](#logging-module)

---

## Bencode Module

`src/bencode.pas`

Handles encoding and decoding of bencoded data (BitTorrent format).

### Types

```pascal
TBencodeType = (btString, btInteger, btList, btDict);
PBencodeValue = ^TBencodeValue;

TBencodeValue = record
  ValueType: TBencodeType;
  case Tag: Byte of
    0: (StrVal: PChar; StrLen: Integer);
    1: (IntVal: Int64);
    2: (ListHead: PBencodeListEntry);
    3: (DictHead: PBencodeDictEntry);
end;

TParseResult = record
  Success: Boolean;
  BytesConsumed: Integer;
  ErrorMsg: string;
end;
```

### Decoding Functions

#### `BencodeDecode`
```pascal
function BencodeDecode(const Data: PChar; Len: Integer; 
                       var Value: PBencodeValue): TParseResult;
```
Decode bencoded data from a buffer.

**Parameters:**
- `Data`: Pointer to bencoded data
- `Len`: Length of data in bytes
- `Value`: Output parameter for decoded value

**Returns:** `TParseResult` with success status and error message

**Example:**
```pascal
var
  Result: TParseResult;
  Value: PBencodeValue;
begin
  Result := BencodeDecode(PChar('4:spam'), 6, Value);
  if Result.Success then
  begin
    // Use Value
    BencodeFree(Value);
  end;
end;
```

#### `BencodeDecodeString`
```pascal
function BencodeDecodeString(const S: string; var Value: PBencodeValue): TParseResult;
```
Decode bencoded data from a Pascal string.

#### `BencodeDecodeFile`
```pascal
function BencodeDecodeFile(const Filename: string; 
                           var Value: PBencodeValue): TParseResult;
```
Decode bencoded data from a file (max 10MB).

### Encoding Functions

#### `BencodeEncode`
```pascal
function BencodeEncode(Value: PBencodeValue; var Buffer: PChar; 
                       var BufLen: Integer): Boolean;
```
Encode a value to a newly allocated buffer.

**Returns:** True on success, caller must free Buffer with `FreeMem`

#### `BencodeEncodeString`
```pascal
function BencodeEncodeString(Value: PBencodeValue; var S: string): Boolean;
```
Encode a value to a Pascal string.

### Memory Management

#### `BencodeFree`
```pascal
procedure BencodeFree(Value: PBencodeValue);
```
Free a bencode value and all its children recursively.

### Value Creation

#### `BencodeNewString`
```pascal
function BencodeNewString(const S: string): PBencodeValue;
```
Create a new string value.

#### `BencodeNewInteger`
```pascal
function BencodeNewInteger(V: Int64): PBencodeValue;
```
Create a new integer value.

#### `BencodeNewList`
```pascal
function BencodeNewList: PBencodeValue;
```
Create a new empty list.

#### `BencodeNewDict`
```pascal
function BencodeNewDict: PBencodeValue;
```
Create a new empty dictionary.

### List Operations

#### `BencodeListAdd`
```pascal
function BencodeListAdd(List: PBencodeValue; Value: PBencodeValue): Boolean;
```
Add a value to the end of a list.

#### `BencodeListCount`
```pascal
function BencodeListCount(List: PBencodeValue): Integer;
```
Get the number of elements in a list.

#### `BencodeListGet`
```pascal
function BencodeListGet(List: PBencodeValue; Index: Integer): PBencodeValue;
```
Get an element by index (0-based). Returns nil if out of bounds.

### Dictionary Operations

#### `BencodeDictAdd`
```pascal
function BencodeDictAdd(Dict: PBencodeValue; const Key: string; 
                        Value: PBencodeValue): Boolean;
```
Add a key-value pair to a dictionary.

#### `BencodeDictGet`
```pascal
function BencodeDictGet(Dict: PBencodeValue; const Key: string): PBencodeValue;
```
Look up a value by key. Returns nil if not found.

#### `BencodeDictHasKey`
```pascal
function BencodeDictHasKey(Dict: PBencodeValue; const Key: string): Boolean;
```
Check if a key exists in the dictionary.

#### `BencodeDictCount`
```pascal
function BencodeDictCount(Dict: PBencodeValue): Integer;
```
Get the number of key-value pairs.

#### `BencodeDictGetStr`
```pascal
function BencodeDictGetStr(Dict: PBencodeValue; const Key: string; 
                           var Value: string): Boolean;
```
Get a string value from dictionary by key.

#### `BencodeDictGetInt`
```pascal
function BencodeDictGetInt(Dict: PBencodeValue; const Key: string; 
                           var Value: Int64): Boolean;
```
Get an integer value from dictionary by key.

---

## Metainfo Module

`src/metainfo.pas`

Parses .torrent files and manages torrent metadata.

### Types

```pascal
PTorrentMeta = ^TTorrentMeta;

TTorrentMeta = record
  PieceLength: Cardinal;
  Pieces: PByteArray;       { Concatenated SHA1 hashes }
  PieceCount: Integer;
  Name: string;             { File or directory name }
  Length: QWord;            { Total length (single file) }
  Files: PFileEntry;        { Linked list for multi-file }
  FileCount: Integer;
  PrivateFlag: Boolean;     { If true, don't use DHT }
  Announce: string;         { Primary tracker URL }
  Trackers: PTrackerInfo;   { Linked list of trackers }
  InfoHash: TSHA1Digest;    { SHA1 of info dictionary }
  IsSingleFile: Boolean;
  TotalLength: QWord;
end;

TMetainfoResult = record
  Success: Boolean;
  ErrorMsg: string;
end;
```

### Parsing Functions

#### `ParseTorrentFile`
```pascal
function ParseTorrentFile(const Filename: string; 
                          out Meta: PTorrentMeta): TMetainfoResult;
```
Parse a .torrent file from disk.

**Example:**
```pascal
var
  Result: TMetainfoResult;
  Meta: PTorrentMeta;
begin
  Result := ParseTorrentFile('example.torrent', Meta);
  if Result.Success then
  begin
    WriteLn('Name: ', Meta^.Name);
    WriteLn('Pieces: ', Meta^.PieceCount);
    FreeTorrentMeta(Meta);
  end
  else
    WriteLn('Error: ', Result.ErrorMsg);
end;
```

#### `ParseTorrentBuffer`
```pascal
function ParseTorrentBuffer(const Buffer: PChar; Len: Integer;
                            out Meta: PTorrentMeta): TMetainfoResult;
```
Parse torrent data from a buffer.

#### `FreeTorrentMeta`
```pascal
procedure FreeTorrentMeta(Meta: PTorrentMeta);
```
Free all memory associated with torrent metadata.

### Info Hash Functions

#### `GetInfoHashHex`
```pascal
function GetInfoHashHex(Meta: PTorrentMeta): string;
```
Get info hash as 40-character hex string.

#### `GetInfoHashURLEncoded`
```pascal
function GetInfoHashURLEncoded(Meta: PTorrentMeta): string;
```
Get info hash URL-encoded for tracker announces.

### Piece Functions

#### `GetPieceHash`
```pascal
function GetPieceHash(Meta: PTorrentMeta; PieceIndex: Integer): PSHA1Digest;
```
Get pointer to expected hash for a piece.

#### `GetPieceLength`
```pascal
function GetPieceLength(Meta: PTorrentMeta; PieceIndex: Integer): Cardinal;
```
Get the length of a specific piece (handles last piece correctly).

---

## File Manager Module

`src/filemgr.pas`

Manages piece-based file I/O with verification.

### Types

```pascal
PFileManager = ^TFileManager;

TFileManager = record
  Meta: PTorrentMeta;
  BasePath: string;
  BytesRead: QWord;
  BytesWritten: QWord;
  VerifiedPieces: PByteArray;  { Bitfield }
end;

TIOResult = record
  Success: Boolean;
  BytesTransferred: Integer;
  ErrorMsg: string;
end;

TPieceWriteResult = record
  Success: Boolean;
  Verified: Boolean;    { Whether piece passed hash verification }
  ErrorMsg: string;
end;
```

### Lifecycle Functions

#### `FileManagerCreate`
```pascal
function FileManagerCreate(Meta: PTorrentMeta; const BasePath: string;
                           out FM: PFileManager): Boolean;
```
Create a new file manager instance.

#### `FileManagerDestroy`
```pascal
procedure FileManagerDestroy(FM: PFileManager);
```
Destroy file manager and close all files.

#### `FileManagerInitialize`
```pascal
function FileManagerInitialize(FM: PFileManager): Boolean;
```
Initialize file manager (create directories, check files).

### Piece Operations

#### `FileManagerReadPiece`
```pascal
function FileManagerReadPiece(FM: PFileManager; PieceIndex: Integer;
                              Buffer: Pointer; BufferSize: Integer;
                              out IOReg: TIOResult): Boolean;
```
Read an entire piece from files.

#### `FileManagerWritePiece`
```pascal
function FileManagerWritePiece(FM: PFileManager; PieceIndex: Integer;
                               Buffer: Pointer; DataLen: Integer;
                               out PieceReg: TPieceWriteResult): Boolean;
```
Write an entire piece to files with automatic hash verification.

#### `FileManagerVerifyPiece`
```pascal
function FileManagerVerifyPiece(FM: PFileManager; PieceIndex: Integer;
                                out IOReg: TIOResult): Boolean;
```
Verify a piece against its expected hash.

### Status Functions

#### `FileManagerIsPieceVerified`
```pascal
function FileManagerIsPieceVerified(FM: PFileManager; PieceIndex: Integer): Boolean;
```
Check if a piece is verified (from cache).

#### `FileManagerGetCompletion`
```pascal
function FileManagerGetCompletion(FM: PFileManager): Double;
```
Get download completion as fraction (0.0 to 1.0).

---

## Protocol Module

`src/protocol.pas`

Implements BitTorrent Peer Wire Protocol message encoding/decoding.

### Message Types

```pascal
MSG_CHOKE = 0;
MSG_UNCHOKE = 1;
MSG_INTERESTED = 2;
MSG_NOT_INTERESTED = 3;
MSG_HAVE = 4;
MSG_BITFIELD = 5;
MSG_REQUEST = 6;
MSG_PIECE = 7;
MSG_CANCEL = 8;
MSG_PORT = 9;
```

### Types

```pascal
TWireMessage = record
  Length: Cardinal;
  MsgId: Byte;
  case Integer of
    0: (HaveIndex: Cardinal);
    1: (BitfieldData: PByteArray; BitfieldLen: Integer);
    2: (ReqIndex, ReqBegin, ReqLength: Cardinal);
    3: (PieceIndex, PieceBegin: Cardinal; PieceData: PByteArray; PieceDataLen: Integer);
end;

THandshake = record
  ProtocolLen: Byte;
  Protocol: array[0..18] of Char;
  Reserved: array[0..7] of Byte;
  InfoHash: array[0..19] of Byte;
  PeerId: array[0..19] of Byte;
end;
```

### Message Building Functions

All Build* functions return True on success and validate buffer size.

#### `BuildChoke`, `BuildUnchoke`, `BuildInterested`, `BuildNotInterested`
```pascal
function BuildChoke(Buffer: PByteArray; BufLen: Integer; out Len: Integer): Boolean;
```
Build simple messages (no payload).

#### `BuildHave`
```pascal
function BuildHave(PieceIndex: Integer; Buffer: PByteArray; 
                   BufLen: Integer; out Len: Integer): Boolean;
```
Build HAVE message.

#### `BuildRequest`
```pascal
function BuildRequest(Index, BeginOffset, Length: Integer;
                      Buffer: PByteArray; BufLen: Integer; out Len: Integer): Boolean;
```
Build REQUEST message.

#### `BuildCancel`
```pascal
function BuildCancel(Index, BeginOffset, Length: Integer;
                     Buffer: PByteArray; BufLen: Integer; out Len: Integer): Boolean;
```
Build CANCEL message.

### Message Decoding

#### `DecodeMessage`
```pascal
function DecodeMessage(Buffer: PByteArray; BufLen: Integer;
                       out Msg: TWireMessage;
                       out BytesConsumed: Integer): Boolean;
```
Decode a message from buffer.

#### `MessageComplete`
```pascal
function MessageComplete(Buffer: PByteArray; BufLen: Integer): Integer;
```
Check if complete message is available. Returns:
- `> 0`: Complete message size
- `0`: Incomplete
- `-1`: Invalid

### Handshake Functions

#### `EncodeHandshake`
```pascal
function EncodeHandshake(const InfoHash, PeerId: array of Byte;
                         Buffer: PByteArray; BufLen: Integer): Integer;
```
Encode handshake into buffer. Returns bytes written (68 on success).

#### `DecodeHandshake`
```pascal
function DecodeHandshake(Buffer: PByteArray; Len: Integer;
                         out HS: THandshake): Boolean;
```
Decode handshake from buffer.

---

## Socket Wrapper Module

`src/sockwrap.pas`

Cross-platform TCP socket abstraction.

### Types

```pascal
PSocketContext = ^TSocketContext;

TSocketContext = record
  Handle: TSocketHandle;
  State: Integer;           { SOCK_STATE_* constants }
  LastError: Integer;
  RemoteAddr: string;
  RemotePort: Word;
  LocalPort: Word;
  BytesSent: QWord;
  BytesReceived: QWord;
end;

{ States }
SOCK_STATE_NONE = 0;
SOCK_STATE_CONNECTING = 1;
SOCK_STATE_CONNECTED = 2;
SOCK_STATE_LISTENING = 3;
SOCK_STATE_ERROR = 4;
SOCK_STATE_CLOSED = 5;

{ Error codes }
SOCK_OK = 0;
SOCK_ERR_CREATE = -1;
SOCK_ERR_CONNECT = -2;
SOCK_ERR_BIND = -3;
SOCK_ERR_LISTEN = -4;
SOCK_ERR_ACCEPT = -5;
SOCK_ERR_SEND = -6;
SOCK_ERR_RECV = -7;
SOCK_ERR_CLOSED = -8;
SOCK_ERR_WOULDBLOCK = -9;
SOCK_ERR_INVALID = -10;
SOCK_ERR_RESOLVE = -11;
```

### Lifecycle Functions

#### `SocketInit`
```pascal
function SocketInit: Boolean;
```
Initialize socket subsystem (call once at startup).

#### `SocketCreate`
```pascal
function SocketCreate: PSocketContext;
```
Create a new socket context.

#### `SocketDestroy`
```pascal
procedure SocketDestroy(Context: PSocketContext);
```
Destroy socket context and close underlying socket.

### Client Operations

#### `SocketConnect`
```pascal
function SocketConnect(Context: PSocketContext; const Host: string;
                       Port: Word; Timeout: Integer): Integer;
```
Connect to a remote host. Returns `SOCK_OK` on success.

#### `SocketCheckConnect`
```pascal
function SocketCheckConnect(Context: PSocketContext): Integer;
```
Check if non-blocking connect completed.

### Server Operations

#### `SocketBind`
```pascal
function SocketBind(Context: PSocketContext; const Address: string;
                    Port: Word): Integer;
```
Bind socket to local address and port.

#### `SocketListen`
```pascal
function SocketListen(Context: PSocketContext; Backlog: Integer): Integer;
```
Start listening for connections.

#### `SocketAccept`
```pascal
function SocketAccept(Server: PSocketContext;
                      out Client: PSocketContext): Integer;
```
Accept an incoming connection.

### Data Transfer

#### `SocketSend`
```pascal
function SocketSend(Context: PSocketContext; Data: Pointer;
                    Len: Integer; out Sent: Integer): Integer;
```
Send data. Returns `SOCK_OK` on success.

#### `SocketReceive`
```pascal
function SocketReceive(Context: PSocketContext; Buffer: Pointer;
                       Len: Integer; out Received: Integer): Integer;
```
Receive data. Returns `SOCK_OK` on success.

#### `SocketSendAll`, `SocketReceiveAll`
```pascal
function SocketSendAll(Context: PSocketContext; Data: Pointer;
                       Len: Integer): Integer;
function SocketReceiveAll(Context: PSocketContext; Buffer: Pointer;
                          Len: Integer): Integer;
```
Send/receive exact amount (blocking until complete).

### Utility Functions

#### `SocketCanRead`, `SocketCanWrite`
```pascal
function SocketCanRead(Context: PSocketContext): Boolean;
function SocketCanWrite(Context: PSocketContext): Boolean;
```
Check if socket is ready for I/O (non-blocking).

#### `SocketResolveHost`
```pascal
function SocketResolveHost(const Hostname: string): string;
```
Resolve hostname to IP address.

---

## SHA1 Utils Module

`src/sha1utils.pas`

SHA1 hashing with progress callbacks.

### Types

```pascal
TSHA1Digest = array[0..19] of Byte;
PSHA1Digest = ^TSHA1Digest;
TSHA1Context = ...;  { Opaque context for incremental hashing }

TProgressCallback = procedure(Percent: Integer);
```

### One-Shot Hashing

#### `SHA1String`
```pascal
function SHA1String(const S: string): TSHA1Digest;
```
Hash a string.

#### `SHA1Buffer`
```pascal
function SHA1Buffer(const Buffer; Len: Integer): TSHA1Digest;
```
Hash a memory buffer.

#### `SHA1File`
```pascal
function SHA1File(const Filename: string; 
                  ProgressCallback: TProgressCallback = nil): TSHA1Digest;
```
Hash a file with optional progress callback.

### Incremental Hashing

```pascal
procedure SHA1Init(var Context: TSHA1Context);
procedure SHA1Update(var Context: TSHA1Context; const Buffer; Len: Integer);
procedure SHA1Final(var Context: TSHA1Context; out Digest: TSHA1Digest);
```

**Example:**
```pascal
var
  Context: TSHA1Context;
  Digest: TSHA1Digest;
begin
  SHA1Init(Context);
  SHA1Update(Context, Data1, Length(Data1));
  SHA1Update(Context, Data2, Length(Data2));
  SHA1Final(Context, Digest);
end;
```

### Utility Functions

#### `SHA1DigestToHex`
```pascal
function SHA1DigestToHex(const Digest: TSHA1Digest): string;
```
Convert digest to 40-character hex string.

#### `SHA1Equal`
```pascal
function SHA1Equal(const A, B: TSHA1Digest): Boolean;
```
Compare two digests for equality.

---

## Utils Module

`src/utils.pas`

General utility functions and data structures.

### Linked List Functions

```pascal
procedure ListAddHead(var Head: PGenericNode; Node: PGenericNode);
procedure ListAddTail(var Head: PGenericNode; Node: PGenericNode);
function ListRemove(var Head: PGenericNode; Node: PGenericNode): Boolean;
function ListCount(Head: PGenericNode): Integer;
procedure ListFree(var Head: PGenericNode);
```

### Dynamic Buffer

```pascal
function DynBufferCreate(InitialCapacity: Integer = 256): PDynBuffer;
procedure DynBufferFree(Buffer: PDynBuffer);
function DynBufferAppend(Buffer: PDynBuffer; const Data; Len: Integer): Boolean;
```

### String Utilities

```pascal
function FormatBytes(Size: Int64): string;     { "1.23 MB" }
function FormatSpeed(Bps: Cardinal): string;   { "1.5 MB/s" }
function FormatDuration(Seconds: Cardinal): string;  { "2h 15m" }
function TrimStr(const S: string): string;
function SplitString(const S, Delim: string; out Parts: array of string): Integer;
function JoinStrings(const Parts: array of string; const Delim: string): string;
```

### Path Utilities

```pascal
function JoinPath(const Dir, Filename: string): string;
function ExtractExtension(const Filename: string): string;
function IsAbsolutePath(const Path: string): Boolean;
```

### Binary Utilities

```pascal
function BytesToHex(const Bytes; Len: Integer): string;
function HexToBytes(const Hex: string; var Bytes; MaxBytes: Integer): Integer;
procedure WriteBE32(Buffer: PByteArray; Offset: Integer; Value: Cardinal);
function ReadBE32(const Buffer; Offset: Integer): Cardinal;
```

---

## Logging Module

`src/logging.pas`

Thread-safe logging infrastructure.

### Types

```pascal
TLogLevel = (llDebug, llInfo, llWarning, llError, llFatal);
PLogger = ^TLogger;

TLogger = record
  Destinations: set of TLogDestination;
  MinLevel: TLogLevel;
  UseColors: Boolean;
  MaxQueueSize: Integer;
end;

TLogDestination = (ldConsole, ldFile);
```

### Global Logger

```pascal
var
  GlobalLogger: PLogger;
```

### Lifecycle Functions

```pascal
function InitLogging: Boolean;
procedure ShutdownLogging;
function LoggerCreate: PLogger;
procedure LoggerDestroy(Logger: PLogger);
```

### Configuration

```pascal
procedure LoggerSetLevel(Logger: PLogger; Level: TLogLevel);
procedure LoggerSetDestinations(Logger: PLogger; Destinations: TLogDestinations);
procedure LoggerSetColors(Logger: PLogger; UseColors: Boolean);
function LoggerSetFile(Logger: PLogger; const Filename: string): Boolean;
```

### Logging Functions

```pascal
procedure Log(Logger: PLogger; Level: TLogLevel; const Category, Msg: string);
procedure Log(Level: TLogLevel; const Category, Msg: string);
procedure LogDebug(const Category, Msg: string);
procedure LogInfo(const Category, Msg: string);
procedure LogWarning(const Category, Msg: string);
procedure LogError(const Category, Msg: string);
procedure LogFatal(const Category, Msg: string);
```

**Example:**
```pascal
begin
  InitLogging;
  LoggerSetLevel(GlobalLogger, llDebug);
  
  LogInfo('Main', 'Application started');
  LogDebug('Network', 'Connecting to peer %s:%d', ['192.168.1.1', 6881]);
  
  ShutdownLogging;
end;
```

---

## Error Handling Conventions

All PascalTorrent functions follow these conventions:

1. **Boolean Return Functions** - Return `True` on success, `False` on failure
2. **Integer Return Functions** - Return `0` or positive on success, negative error codes on failure
3. **Result Records** - Use `Success` boolean field and `ErrorMsg` string field
4. **Nil Pointers** - Functions that allocate return `nil` on failure

### Common Error Codes

```pascal
{ File I/O }
ERR_FILE_NOT_FOUND = -1;
ERR_PERMISSION_DENIED = -2;
ERR_DISK_FULL = -3;

{ Network }
ERR_CONNECTION_REFUSED = -100;
ERR_CONNECTION_TIMEOUT = -101;
ERR_NETWORK_UNREACHABLE = -102;
```

---

## Thread Safety

| Module | Thread-Safe | Notes |
|--------|-------------|-------|
| Bencode | No | Must synchronize access to shared values |
| Metainfo | No | Read-only after creation is safe |
| File Manager | No | Single-threaded access only |
| Protocol | Yes | Stateless encoding/decoding |
| Socket | No | One context per thread |
| SHA1 | Yes | Context is per-thread |
| Utils | Yes | Stateless functions |
| Logging | Yes | Uses internal locking |

---

*End of API Reference*
