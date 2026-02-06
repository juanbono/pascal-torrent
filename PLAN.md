# PascalTorrent - BitTorrent Client Implementation Plan

## Overview

**PascalTorrent** is a BitTorrent client implementation written in Free Pascal (FPC) using only imperative programming constructs. No object-oriented features (classes, objects, methods) will be used. The implementation follows the KISS principle with modular design using records, pointers, and procedural programming.

## Architecture Principles

### Programming Paradigm
- **Purely Imperative**: Records instead of classes, procedures instead of methods
- **Modular Design**: Units with clear separation of concerns
- **Explicit Memory Management**: Manual allocation/deallocation with pointers
- **State Machines**: For protocol handling and connection management
- **Callback-driven**: Event handling through procedure pointers

### Core Data Structures

```pascal
{ Peer connection state }
PPeerConnection = ^TPeerConnection;
TPeerConnection = record
    Socket: TSocket;
    PeerId: array[0..19] of Byte;
    State: TPeerState;           { Handshake, Choked, Interested, etc. }
    AmChoking: Boolean;
    AmInterested: Boolean;
    PeerChoking: Boolean;
    PeerInterested: Boolean;
    Bitfield: PByteArray;        { Pieces peer has }
    BitfieldSize: Integer;
    OutgoingBuffer: PByteArray;  { Pending messages }
    BufferLen: Integer;
    BufferCap: Integer;
    LastActive: TDateTime;
    DownloadRate: Cardinal;      { Bytes/sec }
    UploadRate: Cardinal;
    TotalDownloaded: QWord;
    TotalUploaded: QWord;
    Next: PPeerConnection;       { Linked list }
end;

{ Piece management }
PPiece = ^TPiece;
TPiece = record
    Index: Integer;
    Hash: array[0..19] of Byte;  { SHA1 hash }
    Length: Cardinal;
    State: TPieceState;          { Missing, Downloading, Complete, Verified }
    Blocks: PBlockArray;         { Subdivision of piece }
    BlockCount: Integer;
    BlocksComplete: Integer;
    Data: PByteArray;            { Temporary storage while downloading }
    DownloadedFrom: PPeerConnection; { Current downloader }
    Next: PPiece;
end;

{ Torrent metadata }
PTorrent = ^TTorrent;
TTorrent = record
    InfoHash: array[0..19] of Byte;
    PeerId: array[0..19] of Byte;
    Name: string;
    PieceLength: Cardinal;
    Pieces: PPiece;              { Linked list of pieces }
    PieceCount: Integer;
    TotalLength: QWord;
    Files: PFileInfo;            { For multi-file torrents }
    FileCount: Integer;
    Trackers: PTrackerInfo;      { Linked list of trackers }
    PrivateFlag: Boolean;
    
    { State }
    State: TTorrentState;
    Uploaded: QWord;
    Downloaded: QWord;
    Left: QWord;
    
    { Connections }
    Peers: PPeerConnection;
    MaxPeers: Integer;
    MaxConnections: Integer;
    
    { Configuration }
    DownloadPath: string;
    ListenPort: Word;
    
    Next: PTorrent;
end;

{ Tracker information }
PTrackerInfo = ^TTrackerInfo;
TTrackerInfo = record
    Url: string;
    Tier: Integer;
    Protocol: TTrackerProtocol;  { HTTP, UDP }
    LastAnnounce: TDateTime;
    Interval: Integer;
    MinInterval: Integer;
    Seeders: Integer;
    Leechers: Integer;
    State: TTrackerState;
    FailureCount: Integer;
    Next: PTrackerInfo;
end;
```

---

## Phase 1: Foundation (Weeks 1-3)

### Goal
Establish the core infrastructure: bencoding/decoding, SHA1 hashing, and basic data structures.

### Deliverables

#### 1.1 Bencoding Module (`bencode.pas`)
```pascal
unit bencode;

interface

type
    PBencodeValue = ^TBencodeValue;
    TBencodeType = (btString, btInteger, btList, btDict);
    
    TBencodeValue = record
        ValueType: TBencodeType;
        case Byte of
            0: (StrVal: PChar; StrLen: Integer);
            1: (IntVal: Int64);
            2: (ListHead: PBencodeValue);
            3: (DictHead: PBencodeValue);
    end;
    
    { Functions }
function BencodeDecode(const Data: PChar; Len: Integer; out Value: PBencodeValue): Integer;
function BencodeEncode(Value: PBencodeValue; out Buffer: PChar; out BufLen: Integer): Boolean;
procedure BencodeFree(Value: PBencodeValue);

{ Dictionary helpers }
function BencodeDictGet(Dict: PBencodeValue; const Key: string): PBencodeValue;
function BencodeDictGetStr(Dict: PBencodeValue; const Key: string; out Value: string): Boolean;
function BencodeDictGetInt(Dict: PBencodeValue; const Key: string; out Value: Int64): Boolean;

implementation
    ...
end.
```

**Requirements:**
- Full BEP 3 compliance
- Handle nested structures
- Robust error handling
- Memory leak-free

#### 1.2 SHA1 Module (`sha1.pas`)
```pascal
unit sha1;

interface

type
    TSHA1Digest = array[0..19] of Byte;
    TSHA1Context = record
        State: array[0..4] of Cardinal;
        Count: array[0..1] of Cardinal;
        Buffer: array[0..63] of Byte;
    end;

procedure SHA1Init(var Context: TSHA1Context);
procedure SHA1Update(var Context: TSHA1Context; const Data; Len: Cardinal);
procedure SHA1Final(var Context: TSHA1Context; out Digest: TSHA1Digest);
function SHA1Buffer(const Buffer; Len: Cardinal): TSHA1Digest;
function SHA1File(const Filename: string): TSHA1Digest;
function SHA1DigestToString(const Digest: TSHA1Digest): string;
function SHA1StringToDigest(const S: string): TSHA1Digest;

implementation
    ...
end.
```

#### 1.3 Utility Modules
- `utils.pas`: Common helpers, linked list operations, buffer management
- `config.pas`: Configuration management
- `logging.pas`: Logging infrastructure

### Testing Criteria
- [ ] Decode all bencoding types correctly
- [ ] Encode round-trip preserves data
- [ ] SHA1 produces correct hashes for test vectors
- [ ] Memory profiling shows no leaks

---

## Phase 2: Metainfo and Torrent Files (Weeks 4-5)

### Goal
Parse .torrent files and manage torrent metadata.

### Deliverables

#### 2.1 Metainfo Parser (`metainfo.pas`)
```pascal
unit metainfo;

interface

uses bencode;

type
    PFileEntry = ^TFileEntry;
    TFileEntry = record
        Path: string;
        Length: QWord;
        Md5Sum: string;
        Next: PFileEntry;
    end;
    
    PTorrentMeta = ^TTorrentMeta;
    TTorrentMeta = record
        { Required fields }
        PieceLength: Cardinal;
        Pieces: PByteArray;          { Concatenated SHA1 hashes }
        PieceCount: Integer;
        
        { Single file mode }
        Name: string;
        Length: QWord;
        Md5Sum: string;
        
        { Multi-file mode }
        Files: PFileEntry;
        FileCount: Integer;
        
        { Common info }
        PrivateFlag: Boolean;
        
        { Non-info fields }
        Announce: string;
        AnnounceList: PPCharArray;
        AnnounceListCount: Integer;
        Comment: string;
        CreatedBy: string;
        CreationDate: Int64;
        Encoding: string;
        
        { Computed }
        InfoHash: array[0..19] of Byte;
        IsSingleFile: Boolean;
    end;

function ParseTorrentFile(const Filename: string; out Meta: PTorrentMeta): Boolean;
function ParseTorrentBuffer(const Buffer: PChar; Len: Integer; out Meta: PTorrentMeta): Boolean;
procedure FreeTorrentMeta(Meta: PTorrentMeta);
function GetPieceHash(Meta: PTorrentMeta; PieceIndex: Integer): PByte;
function ComputeInfoHash(const Buffer: PChar; InfoStart, InfoEnd: Integer; 
                         out Hash: array of Byte): Boolean;

implementation
    ...
end.
```

#### 2.2 File Manager (`filemgr.pas`)
```pascal
unit filemgr;

interface

uses metainfo;

type
    PFileManager = ^TFileManager;
    TFileManager = record
        Meta: PTorrentMeta;
        BasePath: string;
        Handles: PFileHandle;
        HandleCount: Integer;
        PieceBuffer: PByteArray;
        PieceBufferSize: Integer;
    end;
    
    TFileHandle = record
        Filename: string;
        Handle: THandle;
        Offset: QWord;
        Length: QWord;
    end;

function FileManagerCreate(Meta: PTorrentMeta; const BasePath: string; 
                           out FM: PFileManager): Boolean;
procedure FileManagerDestroy(FM: PFileManager);

{ Read/write operations }
function FileManagerReadPiece(FM: PFileManager; PieceIndex: Integer; 
                              out Buffer: PByteArray; out Len: Integer): Boolean;
function FileManagerWritePiece(FM: PFileManager; PieceIndex: Integer; 
                               Buffer: PByteArray; Len: Integer): Boolean;
function FileManagerReadBlock(FM: PFileManager; Offset: QWord; Len: Integer; 
                              Buffer: Pointer): Boolean;
function FileManagerWriteBlock(FM: PFileManager; Offset: QWord; Len: Integer; 
                               Buffer: Pointer): Boolean;

{ Verification }
function FileManagerVerifyPiece(FM: PFileManager; PieceIndex: Integer): Boolean;
function FileManagerGetCompletion(FM: PFileManager): Double;

implementation
    ...
end.
```

### Testing Criteria
- [ ] Parse single-file torrents correctly
- [ ] Parse multi-file torrents correctly
- [ ] Compute correct info-hash
- [ ] Read/write data at arbitrary offsets
- [ ] Verify pieces against SHA1 hashes

---

## Phase 3: Networking Foundation (Weeks 6-7)

### Goal
Establish TCP networking, async I/O, and buffer management.

### Deliverables

#### 3.1 Socket Utilities (`sockets.pas`)
```pascal
unit sockets;

interface

uses Sockets;  { FPC's sockets unit }

type
    TSocketState = (ssNone, ssConnecting, ssConnected, ssError, ssClosed);
    
    PSocketContext = ^TSocketContext;
    TSocketContext = record
        Socket: TSocket;
        State: TSocketState;
        
        { Buffers }
        ReadBuffer: PByteArray;
        ReadLen: Integer;
        ReadCap: Integer;
        
        WriteBuffer: PByteArray;
        WriteLen: Integer;
        WriteCap: Integer;
        
        { Events }
        OnConnect: procedure(Context: PSocketContext);
        OnRead: procedure(Context: PSocketContext; Data: PByteArray; Len: Integer);
        OnWrite: procedure(Context: PSocketContext; BytesWritten: Integer);
        OnError: procedure(Context: PSocketContext; ErrorCode: Integer);
        OnClose: procedure(Context: PSocketContext);
        
        UserData: Pointer;
        Next: PSocketContext;
    end;

{ Socket operations }
function SocketCreate: PSocketContext;
procedure SocketDestroy(Context: PSocketContext);
function SocketConnect(Context: PSocketContext; const Addr: string; Port: Word): Boolean;
function SocketListen(Context: PSocketContext; Port: Word): Boolean;
function SocketAccept(Server: PSocketContext; out Client: PSocketContext): Boolean;
procedure SocketClose(Context: PSocketContext);

{ Non-blocking I/O }
function SocketWrite(Context: PSocketContext; Data: Pointer; Len: Integer): Boolean;
function SocketWriteString(Context: PSocketContext; const S: string): Boolean;
function SocketReadAvailable(Context: PSocketContext): Integer;
function SocketProcessRead(Context: PSocketContext): Integer;

{ Select/poll wrapper }
function SocketPoll(var Contexts: PSocketContext; Count: Integer; 
                    Timeout: Integer): Integer;

implementation
    ...
end.
```

#### 3.2 Connection Manager (`connmgr.pas`)
```pascal
unit connmgr;

interface

uses sockets;

type
    PConnectionPool = ^TConnectionPool;
    TConnectionPool = record
        Connections: PSocketContext;
        Count: Integer;
        MaxConnections: Integer;
        ListenSocket: PSocketContext;
        ListenPort: Word;
    end;
    
    TConnectionEvent = procedure(Context: PSocketContext; UserData: Pointer);

function ConnectionPoolCreate(MaxConn: Integer; ListenPort: Word;
                              OnConnect, OnAccept: TConnectionEvent;
                              UserData: Pointer): PConnectionPool;
procedure ConnectionPoolDestroy(Pool: PConnectionPool);
function ConnectionPoolAdd(Pool: PConnectionPool; Context: PSocketContext): Boolean;
procedure ConnectionPoolRemove(Pool: PConnectionPool; Context: PSocketContext);
function ConnectionPoolConnect(Pool: PConnectionPool; const Addr: string; 
                               Port: Word; out Context: PSocketContext): Boolean;
function ConnectionPoolPoll(Pool: PConnectionPool; Timeout: Integer): Integer;

implementation
    ...
end.
```

### Testing Criteria
- [ ] Connect to remote hosts
- [ ] Accept incoming connections
- [ ] Handle multiple simultaneous connections
- [ ] Non-blocking I/O works correctly
- [ ] Buffer management is robust

---

## Phase 4: Peer Wire Protocol (Weeks 8-10)

### Goal
Implement the core BitTorrent peer protocol (BEP 3).

### Deliverables

#### 4.1 Protocol Messages (`protocol.pas`)
```pascal
unit protocol;

interface

const
    { Message IDs }
    MSG_CHOKE          = 0;
    MSG_UNCHOKE        = 1;
    MSG_INTERESTED     = 2;
    MSG_NOT_INTERESTED = 3;
    MSG_HAVE           = 4;
    MSG_BITFIELD       = 5;
    MSG_REQUEST        = 6;
    MSG_PIECE          = 7;
    MSG_CANCEL         = 8;
    MSG_PORT           = 9;    { DHT extension }
    
    { Protocol constants }
    HANDSHAKE_LEN      = 68;
    PROTOCOL_STRING    = 'BitTorrent protocol';
    PROTOCOL_LEN       = 19;
    BLOCK_SIZE         = 16384;  { 16 KiB }
    MAX_REQUESTS       = 250;    { Pipeline depth }

type
    { Message structures }
    PWireMessage = ^TWireMessage;
    TWireMessage = record
        Length: Cardinal;        { 4 bytes big-endian }
        MsgId: Byte;
        case Byte of
            0: ( );                         { Keep-alive (no ID) }
            MSG_HAVE: (PieceIndex: Cardinal);
            MSG_BITFIELD: (Bitfield: PByteArray; BitfieldLen: Integer);
            MSG_REQUEST: (ReqIndex, ReqBegin, ReqLength: Cardinal);
            MSG_PIECE: (PieceIdx, PieceBegin: Cardinal; PieceData: PByteArray; 
                       PieceDataLen: Integer);
            MSG_CANCEL: (CanIndex, CanBegin, CanLength: Cardinal);
            MSG_PORT: (DhtPort: Word);
    end;
    
    { Handshake }
    PHandshake = ^THandshake;
    THandshake = record
        ProtocolLen: Byte;
        Protocol: array[0..18] of Char;
        Reserved: array[0..7] of Byte;
        InfoHash: array[0..19] of Byte;
        PeerId: array[0..19] of Byte;
    end;

{ Message encoding/decoding }
function EncodeHandshake(const InfoHash, PeerId: array of Byte; 
                         out Buffer: array of Byte): Integer;
function DecodeHandshake(Buffer: PByteArray; Len: Integer; 
                         out HS: THandshake): Boolean;
function EncodeMessage(Msg: PWireMessage; out Buffer: PByteArray; 
                       out BufLen: Integer): Boolean;
function DecodeMessage(Buffer: PByteArray; Len: Integer; 
                       out Msg: TWireMessage; out BytesConsumed: Integer): Boolean;

{ Message builders }
function BuildChoke(out Buf: array of Byte): Integer;
function BuildUnchoke(out Buf: array of Byte): Integer;
function BuildInterested(out Buf: array of Byte): Integer;
function BuildNotInterested(out Buf: array of Byte): Integer;
function BuildHave(PieceIndex: Integer; out Buf: array of Byte): Integer;
function BuildBitfield(const Bits: PByteArray; BitLen: Integer; 
                       out Buf: PByteArray; out BufLen: Integer): Boolean;
function BuildRequest(Index, Begin, Length: Integer; out Buf: array of Byte): Integer;
function BuildPiece(Index, Begin: Integer; Data: Pointer; DataLen: Integer; 
                    out Buf: PByteArray; out BufLen: Integer): Boolean;
function BuildCancel(Index, Begin, Length: Integer; out Buf: array of Byte): Integer;

implementation
    ...
end.
```

#### 4.2 Peer Connection Handler (`peerconn.pas`)
```pascal
unit peerconn;

interface

uses sockets, protocol, metainfo;

type
    TPeerState = (psNone, psConnecting, psHandshakeSent, psHandshakeRecv,
                  psConnected, psChoked, psUnchoked, psInterested, 
                  psDownloading, psUploading, psError, psClosed);
    
    PPeerConn = ^TPeerConn;
    TPeerConn = record
        { Socket }
        Socket: PSocketContext;
        
        { Identity }
        PeerId: array[0..19] of Byte;
        PeerAddr: string;
        PeerPort: Word;
        HasPeerId: Boolean;
        
        { State }
        State: TPeerState;
        AmChoking: Boolean;
        AmInterested: Boolean;
        PeerChoking: Boolean;
        PeerInterested: Boolean;
        
        { Bitfield }
        HaveBitfield: PByteArray;
        BitfieldSize: Integer;
        
        { Protocol }
        InfoHash: array[0..19] of Byte;
        OurPeerId: array[0..19] of Byte;
        
        { Requests }
        OutstandingRequests: Integer;
        RequestQueue: PRequestEntry;
        RequestQueueLen: Integer;
        
        { Statistics }
        Downloaded: QWord;
        Uploaded: QWord;
        DownloadRate: Cardinal;
        UploadRate: Cardinal;
        ConnectedAt: TDateTime;
        LastActivity: TDateTime;
        
        { Buffers }
        HandshakeBuf: array[0..67] of Byte;
        HandshakeReceived: Integer;
        
        { Parser state }
        MsgLength: Cardinal;
        MsgId: Byte;
        MsgBuffer: PByteArray;
        MsgReceived: Integer;
        
        { Parent torrent }
        Torrent: Pointer;
        
        { Linked list }
        Next: PPeerConn;
    end;
    
    PRequestEntry = ^TRequestEntry;
    TRequestEntry = record
        PieceIndex: Integer;
        BlockBegin: Integer;
        BlockLength: Integer;
        RequestTime: TDateTime;
        Next: PRequestEntry;
    end;

{ Lifecycle }
function PeerCreate(Socket: PSocketContext; const InfoHash, OurPeerId: array of Byte;
                    out Peer: PPeerConn): Boolean;
procedure PeerDestroy(Peer: PPeerConn);
function PeerConnect(const Addr: string; Port: Word; 
                     const InfoHash, OurPeerId: array of Byte;
                     out Peer: PPeerConn): Boolean;
procedure PeerDisconnect(Peer: PPeerConn);

{ State management }
procedure PeerSetChoke(Peer: PPeerConn; Choke: Boolean);
procedure PeerSetInterested(Peer: PPeerConn; Interested: Boolean);

{ Data exchange }
function PeerSendHave(Peer: PPeerConn; PieceIndex: Integer): Boolean;
function PeerSendBitfield(Peer: PPeerConn; const Bitfield: PByteArray; 
                          Size: Integer): Boolean;
function PeerSendRequest(Peer: PPeerConn; PieceIndex, Begin, Length: Integer): Boolean;
function PeerSendPiece(Peer: PPeerConn; PieceIndex, Begin: Integer; 
                       Data: Pointer; DataLen: Integer): Boolean;
function PeerSendCancel(Peer: PPeerConn; PieceIndex, Begin, Length: Integer): Boolean;

{ Event handling }
procedure PeerProcessIncoming(Peer: PPeerConn);
procedure PeerProcessOutgoing(Peer: PPeerConn);
function PeerHandleRead(Peer: PPeerConn; Data: PByteArray; Len: Integer): Boolean;

{ Callbacks }
var
    PeerOnHave: procedure(Peer: PPeerConn; PieceIndex: Integer);
    PeerOnBitfield: procedure(Peer: PPeerConn; Bitfield: PByteArray; Size: Integer);
    PeerOnRequest: procedure(Peer: PPeerConn; PieceIndex, Begin, Length: Integer);
    PeerOnPiece: procedure(Peer: PPeerConn; PieceIndex, Begin: Integer; 
                           Data: Pointer; DataLen: Integer);
    PeerOnCancel: procedure(Peer: PPeerConn; PieceIndex, Begin, Length: Integer);
    PeerOnChoke: procedure(Peer: PPeerConn);
    PeerOnUnchoke: procedure(Peer: PPeerConn);
    PeerOnInterested: procedure(Peer: PPeerConn);
    PeerOnNotInterested: procedure(Peer: PPeerConn);
    PeerOnError: procedure(Peer: PPeerConn; const ErrorMsg: string);

implementation
    ...
end.
```

### Testing Criteria
- [ ] Successful handshake with remote peers
- [ ] Correct message encoding/decoding
- [ ] State machine transitions work correctly
- [ ] Handle all message types
- [ ] Proper error handling and recovery

---

## Phase 5: HTTP Tracker Protocol (Weeks 11-12)

### Goal
Implement HTTP/HTTPS tracker communication (BEP 3).

### Deliverables

#### 5.1 HTTP Tracker Module (`tracker_http.pas`)
```pascal
unit tracker_http;

interface

uses metainfo;

type
    TTrackerEvent = (teNone, teStarted, teStopped, teCompleted);
    
    PTrackerResponse = ^TTrackerResponse;
    TTrackerResponse = record
        Interval: Integer;
        MinInterval: Integer;
        Complete: Integer;      { Seeders }
        Incomplete: Integer;    { Leechers }
        PeersCompact: PByteArray;   { BEP 23 compact format }
        PeersCompactLen: Integer;
        PeersDict: PBencodeValue;   { Original dictionary format }
        FailureReason: string;
        WarningMessage: string;
        TrackerId: string;
    end;
    
    PTrackerAnnounce = ^TTrackerAnnounce;
    TTrackerAnnounce = record
        { Required params }
        InfoHash: array[0..19] of Byte;
        PeerId: array[0..19] of Byte;
        Port: Word;
        Uploaded: QWord;
        Downloaded: QWord;
        Left: QWord;
        
        { Optional params }
        Event: TTrackerEvent;
        IP: string;             { Optional client IP }
        NumWant: Integer;       { Desired peer count }
        Key: string;            { Client key }
        TrackerId: string;
        Compact: Boolean;       { BEP 23 support }
        NoPeerId: Boolean;
    end;

{ Synchronous (blocking) interface }
function TrackerAnnounceHTTP(const TrackerUrl: string; 
                             const Request: TTrackerAnnounce;
                             out Response: PTrackerResponse): Boolean;
function TrackerScrapeHTTP(const TrackerUrl: string;
                           const InfoHashes: array of TSHA1Digest;
                           out Response: PBencodeValue): Boolean;

{ Asynchronous interface }
type
    TTrackerCallback = procedure(Success: Boolean; Response: PTrackerResponse; 
                                 UserData: Pointer);
    
function TrackerAnnounceAsync(const TrackerUrl: string;
                              const Request: TTrackerAnnounce;
                              Callback: TTrackerCallback; UserData: Pointer): Boolean;
procedure TrackerCancelAsync(Handle: Pointer);

{ URL utilities }
function BuildAnnounceUrl(const BaseUrl: string; const Req: TTrackerAnnounce): string;
procedure FreeTrackerResponse(Response: PTrackerResponse);

implementation
    ...
end.
```

### Testing Criteria
- [ ] Successful announces to HTTP trackers
- [ ] Parse compact peer format (BEP 23)
- [ ] Handle tracker errors gracefully
- [ ] Respect announce intervals
- [ ] URL encoding of binary data (info_hash, peer_id)

---

## Phase 6: Piece Management (Weeks 13-14)

### Goal
Implement piece selection, downloading, and verification.

### Deliverables

#### 6.1 Piece Manager (`piecemgr.pas`)
```pascal
unit piecemgr;

interface

uses metainfo, peerconn;

type
    TBlockState = (bsMissing, bsRequested, bsDownloading, bsComplete);
    TPieceState = (psMissing, psPartial, psComplete, psVerified);
    
    PBlock = ^TBlock;
    TBlock = record
        BeginOffset: Integer;
        Length: Integer;
        State: TBlockState;
        RequestedFrom: PPeerConn;
        RequestTime: TDateTime;
        Data: PByteArray;
        Next: PBlock;
    end;
    
    PPieceInfo = ^TPieceInfo;
    TPieceInfo = record
        Index: Integer;
        Length: Integer;           { May differ from piece length for last piece }
        Hash: array[0..19] of Byte;
        State: TPieceState;
        Blocks: PBlock;
        BlockCount: Integer;
        BlocksComplete: Integer;
        Data: PByteArray;          { Full piece buffer when downloading }
        Priority: Integer;         { Download priority }
        RefCount: Integer;         { Number of peers having this piece }
        Next: PPieceInfo;
    end;
    
    PPieceManager = ^TPieceManager;
    TPieceManager = record
        Meta: PTorrentMeta;
        Pieces: PPieceInfo;
        PieceCount: Integer;
        
        { State tracking }
        CompletedPieces: Integer;
        PartialPieces: Integer;
        
        { Bitfield }
        OurBitfield: PByteArray;
        BitfieldSize: Integer;
        
        { File manager reference }
        FileMgr: Pointer;
        
        { Selection strategy }
        Strategy: TSelectionStrategy;
        RarestFirstMap: PRarityMap;
    end;
    
    TSelectionStrategy = (ssRarestFirst, ssSequential, ssRandom);
    PRarityMap = ^TRarityMap;
    TRarityMap = record
        PieceIndex: Integer;
        Rarity: Integer;
        Next: PRarityMap;
    end;

{ Lifecycle }
function PieceManagerCreate(Meta: PTorrentMeta; FileMgr: Pointer): PPieceManager;
procedure PieceManagerDestroy(PM: PPieceManager);

{ State queries }
function PieceManagerIsComplete(PM: PPieceManager; PieceIndex: Integer): Boolean;
function PieceManagerIsInterested(PM: PPieceManager; PeerBitfield: PByteArray; 
                                  Size: Integer): Boolean;
function PieceManagerGetCompletion(PM: PPieceManager): Double;

{ Selection }
function PieceManagerSelectPiece(PM: PPieceManager; Peer: PPeerConn; 
                                 out PieceIndex: Integer): Boolean;
function PieceManagerSelectBlocks(PM: PPieceManager; PieceIndex: Integer; 
                                  MaxBlocks: Integer; out Blocks: PBlock): Integer;
function PieceManagerGetBlockToRequest(PM: PPieceManager; Peer: PPeerConn;
                                       out PieceIndex, BlockBegin, 
                                       BlockLength: Integer): Boolean;

{ Download handling }
function PieceManagerOnBlockReceived(PM: PPieceManager; PieceIndex, Begin: Integer;
                                     Data: Pointer; DataLen: Integer): Boolean;
procedure PieceManagerOnBlockRequested(PM: PPieceManager; PieceIndex, Begin, 
                                       Length: Integer; Peer: PPeerConn);
procedure PieceManagerOnRequestCancelled(PM: PPieceManager; PieceIndex, 
                                         Begin, Length: Integer);

{ Verification }
function PieceManagerVerifyPiece(PM: PPieceManager; PieceIndex: Integer): Boolean;
procedure PieceManagerMarkPieceComplete(PM: PPieceManager; PieceIndex: Integer);

{ Rarity tracking }
procedure PieceManagerUpdateRarity(PM: PPieceManager; PeerBitfield: PByteArray; 
                                   Size: Integer; Increment: Boolean);

{ Upload handling }
function PieceManagerGetBlockForUpload(PM: PPieceManager; PieceIndex, Begin, 
                                       Length: Integer; out Data: Pointer): Boolean;

implementation
    ...
end.
```

#### 6.2 Choking Algorithm (`choker.pas`)
```pascal
unit choker;

interface

uses peerconn;

type
    PChoker = ^TChoker;
    TChoker = record
        Peers: PPeerConn;
        PeerCount: Integer;
        
        { Configuration }
        MaxUnchoked: Integer;
        UploadSlots: Integer;
        
        { Optimistic unchoke }
        OptimisticUnchokeIndex: Integer;
        LastOptimisticRotate: TDateTime;
        OptimisticInterval: Integer;  { Seconds }
        
        { Rate tracking }
        DecayingWindow: Double;
    end;

function ChokerCreate(MaxUnchoked: Integer): PChoker;
procedure ChokerDestroy(Choker: PChoker);

procedure ChokerAddPeer(Choker: PChoker; Peer: PPeerConn);
procedure ChokerRemovePeer(Choker: PChoker; Peer: PPeerConn);

{ Called periodically to update choking decisions }
procedure ChokerRecompute(Choker: PChoker);

{ Reciprocation algorithm }
function ChokerShouldUnchoke(Choker: PChoker; Peer: PPeerConn): Boolean;

implementation
    ...
end.
```

### Testing Criteria
- [ ] Rarest-first piece selection works
- [ ] End-game mode (request same block from multiple peers)
- [ ] Piece verification with SHA1
- [ ] Choking algorithm prioritizes uploaders
- [ ] Handle partial pieces correctly

---

## Phase 7: Main Torrent Engine (Weeks 15-16)

### Goal
Integrate all components into a working torrent engine.

### Deliverables

#### 7.1 Torrent Engine (`torrent.pas` - Main Unit)
```pascal
unit torrent;

interface

uses metainfo, filemgr, piecemgr, peerconn, tracker_http, connmgr;

type
    TTorrentState = (tsStopped, tsChecking, tsDownloading, tsSeeding, tsError);
    
    PTorrentEngine = ^TTorrentEngine;
    TTorrentEngine = record
        { Metadata }
        Meta: PTorrentMeta;
        
        { Subsystems }
        FileMgr: PFileManager;
        PieceMgr: PPieceManager;
        ConnPool: PConnectionPool;
        
        { Peers }
        Peers: PPeerConn;
        PeerCount: Integer;
        MaxPeers: Integer;
        
        { Trackers }
        Trackers: PTrackerInfo;
        CurrentTracker: PTrackerInfo;
        LastAnnounce: TDateTime;
        AnnounceInterval: Integer;
        
        { State }
        State: TTorrentState;
        ErrorMessage: string;
        
        { Statistics }
        Uploaded: QWord;
        Downloaded: QWord;
        SessionUploaded: QWord;
        SessionDownloaded: QWord;
        UploadRate: Cardinal;
        DownloadRate: Cardinal;
        
        { Configuration }
        ListenPort: Word;
        MaxConnections: Integer;
        MaxUploads: Integer;
        
        { Callbacks }
        OnStateChange: procedure(Engine: PTorrentEngine; OldState, NewState: TTorrentState);
        OnProgress: procedure(Engine: PTorrentEngine; Progress: Double);
        OnPeerConnect: procedure(Engine: PTorrentEngine; Peer: PPeerConn);
        OnPeerDisconnect: procedure(Engine: PTorrentEngine; Peer: PPeerConn);
        OnError: procedure(Engine: PTorrentEngine; const ErrorMsg: string);
        
        { Threading }
        Running: Boolean;
        ShutdownFlag: Boolean;
    end;

{ Lifecycle }
function TorrentCreate(const TorrentFile, DownloadPath: string; 
                       out Engine: PTorrentEngine): Boolean;
function TorrentCreateFromMagnet(const MagnetLink, DownloadPath: string;
                                 out Engine: PTorrentEngine): Boolean;
procedure TorrentDestroy(Engine: PTorrentEngine);

{ Control }
function TorrentStart(Engine: PTorrentEngine): Boolean;
procedure TorrentStop(Engine: PTorrentEngine);
procedure TorrentPause(Engine: PTorrentEngine);
procedure TorrentResume(Engine: PTorrentEngine);

{ Status }
function TorrentGetState(Engine: PTorrentEngine): TTorrentState;
function TorrentGetProgress(Engine: PTorrentEngine): Double;
function TorrentGetStats(Engine: PTorrentEngine; out Stats: TTransferStats): Boolean;

{ Main event loop - called repeatedly }
procedure TorrentProcess(Engine: PTorrentEngine);

{ Configuration }
procedure TorrentSetMaxConnections(Engine: PTorrentEngine; MaxConn: Integer);
procedure TorrentSetMaxUploads(Engine: PTorrentEngine; MaxUploads: Integer);
procedure TorrentSetListenPort(Engine: PTorrentEngine; Port: Word);

implementation
    ...
end.
```

### Testing Criteria
- [ ] Download a complete torrent from multiple peers
- [ ] Seed to other peers
- [ ] Handle tracker failures gracefully
- [ ] Resume partial downloads
- [ ] Multiple torrents can run simultaneously

---

## Phase 8: UDP Tracker Protocol (Weeks 17-18)

### Goal
Implement UDP tracker support for efficiency (BEP 15).

### Deliverables

#### 8.1 UDP Tracker Module (`tracker_udp.pas`)
```pascal
unit tracker_udp;

interface

uses tracker_http;

const
    UDP_PROTOCOL_ID = $41727101980;
    UDP_ACTION_CONNECT = 0;
    UDP_ACTION_ANNOUNCE = 1;
    UDP_ACTION_SCRAPE = 2;
    UDP_ACTION_ERROR = 3;
    
    UDP_TIMEOUT_INITIAL = 15;      { seconds }
    UDP_TIMEOUT_MAX = 3840;        { seconds (15 * 2^8) }
    UDP_CONNECTION_ID_TTL = 60;    { seconds }

type
    PUDPTrackerState = ^TUDPTrackerState;
    TUDPTrackerState = (utsIdle, utsConnecting, utsConnected, 
                        utsAnnouncing, utsError);
    
    PUDPTrackerContext = ^TUDPTrackerContext;
    TUDPTrackerContext = record
        Host: string;
        Port: Word;
        Socket: TSocket;
        
        { Connection state }
        State: TUDPTrackerState;
        ConnectionId: Int64;
        ConnectionTime: TDateTime;
        TransactionId: Cardinal;
        
        { Retry logic }
        RetryCount: Integer;
        LastAttempt: TDateTime;
        
        { Request/Response }
        PendingRequest: TTrackerAnnounce;
        ResponseBuffer: array[0..2047] of Byte;
        ResponseLen: Integer;
        
        { Callback }
        Callback: TTrackerCallback;
        UserData: Pointer;
    end;

function TrackerAnnounceUDP(const TrackerUrl: string;
                            const Request: TTrackerAnnounce;
                            out Response: PTrackerResponse): Boolean;
function TrackerAnnounceUDPAsync(const TrackerUrl: string;
                                 const Request: TTrackerAnnounce;
                                 Callback: TTrackerCallback; 
                                 UserData: Pointer): Boolean;

{ Connection management }
function UDPTrackerConnect(Context: PUDPTrackerContext): Boolean;
function UDPTrackerSendAnnounce(Context: PUDPTrackerContext): Boolean;
function UDPTrackerProcessResponse(Context: PUDPTrackerContext): Boolean;

implementation
    ...
end.
```

### Testing Criteria
- [ ] Connect to UDP trackers
- [ ] Handle connection ID expiration
- [ ] Retry with exponential backoff
- [ ] Parse both IPv4 and IPv6 responses
- [ ] Fall back to HTTP if UDP fails

---

## Phase 9: DHT Support (Weeks 19-21)

### Goal
Implement Distributed Hash Table for trackerless torrents (BEP 5).

### Deliverables

#### 9.1 DHT Module (`dht.pas`)
```pascal
unit dht;

interface

const
    DHT_K = 8;                      { Bucket size }
    DHT_BUCKET_REFRESH = 900;       { 15 minutes }
    DHT_TOKEN_TIMEOUT = 600;        { 10 minutes }
    DHT_BOOTSTRAP_NODES: array[0..2] of string = (
        'router.bittorrent.com:6881',
        'dht.transmissionbt.com:6881',
        'dht.aelitis.com:6881'
    );

type
    TNodeId = array[0..19] of Byte;
    
    PNodeInfo = ^TNodeInfo;
    TNodeInfo = record
        Id: TNodeId;
        IP: Cardinal;              { IPv4 for now }
        Port: Word;
        LastSeen: TDateTime;
        Good: Boolean;
        Next: PNodeInfo;
    end;
    
    PKBucket = ^TKBucket;
    TKBucket = record
        MinId: TNodeId;
        MaxId: TNodeId;
        Nodes: PNodeInfo;
        NodeCount: Integer;
        LastChanged: TDateTime;
        Next: PKBucket;
    end;
    
    PDHT = ^TDHT;
    TDHT = record
        OurId: TNodeId;
        Socket: TSocket;
        Port: Word;
        
        { Routing table }
        Buckets: PKBucket;
        
        { Pending queries }
        Queries: PDHTQuery;
        
        { Bootstrap state }
        Bootstrapped: Boolean;
        BootstrapNodes: PPCharArray;
    end;
    
    PDHTQuery = ^TDHTQuery;
    TDHTQuery = record
        TransactionId: string;
        QueryType: TDHTQueryType;
        Target: TNodeId;
        SentTo: PNodeInfo;
        SentTime: TDateTime;
        Callback: TDHTCallback;
        UserData: Pointer;
        Next: PDHTQuery;
    end;
    
    TDHTQueryType = (qtPing, qtFindNode, qtGetPeers, qtAnnouncePeer);
    TDHTCallback = procedure(Success: Boolean; const Nodes: PNodeInfo; 
                             const Peers: PPeerInfo; UserData: Pointer);

{ Lifecycle }
function DHTCreate(Port: Word; out DHT: PDHT): Boolean;
procedure DHTDestroy(DHT: PDHT);
function DHTBootstrap(DHT: PDHT): Boolean;

{ Operations }
function DHTGetPeers(DHT: PDHT; const InfoHash: TNodeId; 
                     Callback: TDHTCallback; UserData: Pointer): Boolean;
function DHTAnnouncePeer(DHT: PDHT; const InfoHash: TNodeId; Port: Word;
                         const Token: string): Boolean;

{ Message handlers }
procedure DHTProcessIncoming(DHT: PDHT);
function DHTSendPing(DHT: PDHT; Node: PNodeInfo): Boolean;
function DHTSendFindNode(DHT: PDHT; Node: PNodeInfo; const Target: TNodeId): Boolean;
function DHTSendGetPeers(DHT: PDHT; Node: PNodeInfo; const InfoHash: TNodeId): Boolean;

{ Routing table }
procedure DHTUpdateRoutingTable(DHT: PDHT; Node: PNodeInfo);
function DHTFindClosestNodes(DHT: PDHT; const Target: TNodeId; 
                             out Results: PNodeInfo): Integer;

implementation
    ...
end.
```

#### 9.2 KRPC Protocol (`krpc.pas`)
```pascal
unit krpc;

interface

uses dht, bencode;

{ Message encoding }
function KRPCEncodePing(const TransactionId: string; const NodeId: TNodeId;
                        out Buffer: PByteArray; out BufLen: Integer): Boolean;
function KRPCEncodeFindNode(const TransactionId: string; const NodeId, Target: TNodeId;
                            out Buffer: PByteArray; out BufLen: Integer): Boolean;
function KRPCEncodeGetPeers(const TransactionId: string; const NodeId, InfoHash: TNodeId;
                            out Buffer: PByteArray; out BufLen: Integer): Boolean;
function KRPCEncodeAnnouncePeer(const TransactionId: string; const NodeId, InfoHash: TNodeId;
                                Port: Word; const Token: string;
                                ImpliedPort: Boolean;
                                out Buffer: PByteArray; out BufLen: Integer): Boolean;

{ Message decoding }
function KRPCDecodeMessage(const Buffer: PByteArray; Len: Integer;
                           out MsgType: TKRPCMessageType;
                           out TransactionId: string;
                           out Dict: PBencodeValue): Boolean;

{ Response parsing }
function KRPCParseFindNodeResponse(Dict: PBencodeValue; out Nodes: PNodeInfo): Integer;
function KRPCParseGetPeersResponse(Dict: PBencodeValue; 
                                   out Token: string;
                                   out Peers: PPeerInfo; 
                                   out Nodes: PNodeInfo): Boolean;

{ Compact node info }
procedure EncodeCompactNode(const Id: TNodeId; IP: Cardinal; Port: Word; 
                            out Buf: array of Byte);
function DecodeCompactNode(const Buf: array of Byte; out Node: PNodeInfo): Boolean;

implementation
    ...
end.
```

### Testing Criteria
- [ ] Bootstrap into DHT network
- [ ] Respond to ping queries
- [ ] Handle find_node queries
- [ ] Retrieve peers via get_peers
- [ ] Announce to DHT successfully
- [ ] Routing table splits correctly

---

## Phase 10: Extension Protocol (Weeks 22-23)

### Goal
Implement BEP 10 for extensibility and add PEX/metadata exchange.

### Deliverables

#### 10.1 Extension Protocol (`extprotocol.pas`)
```pascal
unit extprotocol;

interface

uses peerconn, bencode;

const
    EXT_MSG_HANDSHAKE = 0;
    
    { Extended message IDs - assigned per connection }
    EXT_MSG_UT_METADATA = 1;
    EXT_MSG_UT_PEX = 2;

type
    PExtensionState = ^TExtensionState;
    TExtensionState = record
        Supported: Boolean;
        OurMsgId: Byte;
        PeerMsgId: Byte;
        Next: PExtensionState;
    end;
    
    PExtendedHandshake = ^TExtendedHandshake;
    TExtendedHandshake = record
        M: PBencodeValue;            { Extension message mapping }
        P: Word;                     { Listen port }
        V: string;                   { Client version }
        Yourip: string;              { Remote view of our IP }
        Ipv4: Cardinal;
        Ipv6: array[0..15] of Byte;
        Reqq: Integer;               { Max outstanding requests }
        MetadataSize: Integer;       { For magnet links }
    end;
    
    PPeerExtensions = ^TPeerExtensions;
    TPeerExtensions = record
        Peer: PPeerConn;
        
        { Negotiated extensions }
        SupportsMetadata: Boolean;
        MetadataMsgId: Byte;
        SupportsPex: Boolean;
        PexMsgId: Byte;
        
        { Our info }
        OurExtensions: Byte;
        PeerExtensions: Byte;
        
        { Metadata exchange }
        MetadataSize: Integer;
        MetadataPieces: Integer;
        MetadataPieceSize: Integer;
        Metadata: PByteArray;
    end;

{ Handshake }
function ExtSendHandshake(Peer: PPeerConn; const OurHandshake: TExtendedHandshake): Boolean;
function ExtParseHandshake(Data: PByteArray; Len: Integer; 
                           out HS: TExtendedHandshake): Boolean;

{ Metadata exchange (BEP 9) }
function ExtRequestMetadataPiece(PeerExt: PPeerExtensions; Piece: Integer): Boolean;
function ExtSendMetadataPiece(PeerExt: PPeerExtensions; Piece: Integer;
                              Data: Pointer; DataLen: Integer): Boolean;
function ExtRejectMetadata(PeerExt: PPeerExtensions; Piece: Integer): Boolean;

{ Peer exchange (PEX) }
function ExtSendPex(PeerExt: PPeerExtensions; Added, Dropped: PNodeInfo): Boolean;
function ExtParsePex(Data: PByteArray; Len: Integer; 
                     out Added, Dropped: PNodeInfo): Boolean;

implementation
    ...
end.
```

### Testing Criteria
- [ ] Negotiate extensions via handshake
- [ ] Send/receive extended messages
- [ ] Exchange peer lists via PEX
- [ ] Download metadata from magnet links

---

## Phase 11: Command-Line Interface (Weeks 24-25)

### Goal
Create a usable command-line torrent client.

### Deliverables

#### 11.1 Main Program (`pascaltorrent.pas`)
```pascal
program pascaltorrent;

uses
    SysUtils, torrent, metainfo;

const
    VERSION = '0.1.0';
    DEFAULT_PORT = 6881;

type
    TProgramOptions = record
        TorrentFile: string;
        DownloadPath: string;
        Port: Word;
        MaxConnections: Integer;
        MaxUploads: Integer;
        Verbose: Boolean;
        SeedOnly: Boolean;
        Help: Boolean;
    end;

procedure PrintUsage;
begin
    WriteLn('PascalTorrent v', VERSION);
    WriteLn;
    WriteLn('Usage: pascaltorrent [options] <torrent-file>');
    WriteLn;
    WriteLn('Options:');
    WriteLn('  -h, --help              Show this help message');
    WriteLn('  -p, --port <port>       Listen port (default: ', DEFAULT_PORT, ')');
    WriteLn('  -d, --dir <path>        Download directory (default: current)');
    WriteLn('  -c, --max-conn <n>      Maximum connections (default: 50)');
    WriteLn('  -u, --max-upload <n>    Maximum uploads (default: 4)');
    WriteLn('  -v, --verbose           Verbose output');
    WriteLn('  -s, --seed              Seed only (don''t download)');
end;

function ParseOptions(var Args: array of string; out Opts: TProgramOptions): Boolean;
procedure PrintProgress(Engine: PTorrentEngine; Progress: Double);
procedure PrintStats(Engine: PTorrentEngine);
procedure RunEventLoop(Engine: PTorrentEngine);

begin
    { Main program logic }
end.
```

#### 11.2 Configuration File Support (`configfile.pas`)
```pascal
unit configfile;

interface

type
    PConfig = ^TConfig;
    TConfig = record
        DownloadPath: string;
        ListenPortMin: Word;
        ListenPortMax: Word;
        MaxConnections: Integer;
        MaxUploads: Integer;
        MaxDownloadRate: Cardinal;    { 0 = unlimited }
        MaxUploadRate: Cardinal;
        DhtEnabled: Boolean;
        PexEnabled: Boolean;
        Encryption: TEncryptionMode;
    end;
    
    TEncryptionMode = (emDisabled, emEnabled, emRequired);

function ConfigLoad(const Filename: string; out Config: PConfig): Boolean;
function ConfigSave(const Filename: string; Config: PConfig): Boolean;
procedure ConfigFree(Config: PConfig);

implementation
    ...
end.
```

### Testing Criteria
- [ ] Parse command-line arguments correctly
- [ ] Display progress and statistics
- [ ] Handle signals (Ctrl+C gracefully)
- [ ] Configuration file loading/saving
- [ ] Log to file option

---

## Phase 12: Optimization and Polish (Weeks 26-28)

### Goal
Performance optimization, memory management, and bug fixes.

### Deliverables

#### 12.1 Performance Enhancements
- **Disk I/O optimization**: Async writes, read-ahead caching
- **Memory pools**: Reuse buffers instead of allocating
- **Rate limiting**: Token bucket algorithm for bandwidth control

#### 12.2 Rate Limiter (`ratelimit.pas`)
```pascal
unit ratelimit;

interface

type
    PRateLimiter = ^TRateLimiter;
    TRateLimiter = record
        Rate: Cardinal;              { Bytes per second }
        Tokens: Double;
        LastUpdate: TDateTime;
        BucketSize: Cardinal;
    end;

function RateLimiterCreate(Rate: Cardinal): PRateLimiter;
procedure RateLimiterDestroy(RL: PRateLimiter);
function RateLimiterCanTransfer(RL: PRateLimiter; Bytes: Cardinal): Boolean;
function RateLimiterConsume(RL: PRateLimiter; Bytes: Cardinal): Cardinal;
procedure RateLimiterSetRate(RL: PRateLimiter; Rate: Cardinal);

implementation
    ...
end.
```

#### 12.3 Memory Pool (`mempool.pas`)
```pascal
unit mempool;

interface

type
    PBlockPool = ^TBlockPool;
    TBlockPool = record
        BlockSize: Integer;
        BlocksPerChunk: Integer;
        
        { Free list }
        FreeList: PFreeBlock;
        
        { Chunks for cleanup }
        Chunks: PChunk;
    end;
    
    PFreeBlock = ^TFreeBlock;
    TFreeBlock = record
        Next: PFreeBlock;
    end;
    
    PChunk = ^TChunk;
    TChunk = record
        Memory: PByteArray;
        Next: PChunk;
    end;

function BlockPoolCreate(BlockSize, BlocksPerChunk: Integer): PBlockPool;
procedure BlockPoolDestroy(Pool: PBlockPool);
function BlockPoolAlloc(Pool: PBlockPool): Pointer;
procedure BlockPoolFree(Pool: PBlockPool; Block: Pointer);

implementation
    ...
end.
```

### Testing Criteria
- [ ] Memory usage is stable over long runs
- [ ] CPU usage is reasonable (<10% at idle)
- [ ] Rate limiting is accurate
- [ ] Handle torrents with 1000+ peers
- [ ] Download speeds comparable to reference clients

---

## Project Structure

```
pascaltorrent/
├── PLAN.md                    # This file
├── README.md                  # User documentation
├── Makefile                   # Build automation
├── docs/                      # BitTorrent specifications
│   ├── BEP_003.md            # BitTorrent Protocol
│   ├── BEP_005.md            # DHT Protocol
│   ├── BEP_010.md            # Extension Protocol
│   ├── BEP_012.md            # Multitracker Extension
│   ├── BEP_015.md            # UDP Tracker Protocol
│   ├── BEP_020.md            # Peer ID Conventions
│   └── BEP_023.md            # Compact Peer Lists
├── src/
│   ├── pascaltorrent.pas     # Main program
│   ├── bencode.pas           # Bencode encoding/decoding
│   ├── sha1.pas              # SHA1 hashing
│   ├── utils.pas             # Utility functions
│   ├── config.pas            # Configuration
│   ├── logging.pas           # Logging infrastructure
│   ├── metainfo.pas          # .torrent file parser
│   ├── filemgr.pas           # File I/O management
│   ├── sockets.pas           # Socket wrappers
│   ├── connmgr.pas           # Connection management
│   ├── protocol.pas          # Peer wire protocol
│   ├── peerconn.pas          # Peer connection handler
│   ├── tracker_http.pas      # HTTP tracker protocol
│   ├── tracker_udp.pas       # UDP tracker protocol (BEP 15)
│   ├── piecemgr.pas          # Piece management
│   ├── choker.pas            # Choking algorithm
│   ├── torrent.pas           # Main torrent engine
│   ├── dht.pas               # DHT implementation (BEP 5)
│   ├── krpc.pas              # KRPC protocol
│   ├── extprotocol.pas       # Extension protocol (BEP 10)
│   ├── ratelimit.pas         # Rate limiting
│   ├── mempool.pas           # Memory pools
│   └── configfile.pas        # Config file handling
├── tests/
│   ├── test_bencode.pas
│   ├── test_sha1.pas
│   ├── test_metainfo.pas
│   ├── test_protocol.pas
│   └── test_piecemgr.pas
└── examples/
    └── sample.torrent
```

---

## Build Instructions

### Prerequisites
- Free Pascal Compiler (FPC) 3.2.0 or later
- Make utility

### Compilation
```bash
# Debug build
make debug

# Release build (optimized)
make release

# Run tests
make test

# Clean build artifacts
make clean
```

### Platform Notes
- **Linux**: Native socket support
- **Windows**: Requires Winsock2, link with `-lws2_32`
- **macOS**: Standard BSD sockets

---

## Testing Strategy

### Unit Tests
Each module has corresponding tests in `tests/`:
- Property-based testing for bencode
- Known answer tests for SHA1
- Mock sockets for protocol testing

### Integration Tests
- Download well-known torrents (e.g., Ubuntu ISO)
- Test against reference clients (Transmission, qBittorrent)
- Long-running stability tests

### Compliance Tests
- Protocol analyzer verification
- Tracker compatibility matrix
- DHT conformance testing

---

## References

- [BitTorrent Specification (BEP 3)](https://www.bittorrent.org/beps/bep_0003.html)
- [DHT Protocol (BEP 5)](https://www.bittorrent.org/beps/bep_0005.html)
- [Extension Protocol (BEP 10)](https://www.bittorrent.org/beps/bep_0010.html)
- [UDP Tracker (BEP 15)](https://www.bittorrent.org/beps/bep_0015.html)
- [Free Pascal Documentation](https://www.freepascal.org/docs.html)
