{
  protocol.pas - BitTorrent Peer Wire Protocol
  
  Implements message encoding/decoding for the BitTorrent peer protocol (BEP 3).
  This unit has no network dependencies - it only handles message serialization.
  
  All functions are procedural with explicit outputs for testability.
}

unit protocol;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, sha1utils, utils;

{ ============================================================================ }
{ Protocol Constants                                                           }
{ ============================================================================ }

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
  BLOCK_SIZE         = 16384;  { 16 KiB - standard block size }
  MAX_REQUESTS       = 250;    { Maximum pipelined requests }
  
  { Message length prefixes (4 bytes) + message ID (1 byte) + payload }
  LEN_KEEPALIVE      = 0;      { 0 bytes - no message ID }
  LEN_CHOKE          = 1;      { 1 byte - message ID only }
  LEN_UNCHOKE        = 1;      { 1 byte - message ID only }
  LEN_INTERESTED     = 1;      { 1 byte - message ID only }
  LEN_NOT_INTERESTED = 1;      { 1 byte - message ID only }
  LEN_HAVE           = 5;      { 1 byte ID + 4 bytes piece index }
  LEN_REQUEST        = 13;     { 1 byte ID + 4 bytes index + 4 bytes begin + 4 bytes length }
  LEN_CANCEL         = 13;     { Same structure as request }
  LEN_PIECE_HEADER   = 9;      { 1 byte ID + 4 bytes index + 4 bytes begin (not including data) }
  
  { Reserved byte flags (BEP extensions) }
  EXTENSION_PROTOCOL = $10;  { Bit 4 - Extension protocol (BEP 10) }
  FAST_EXTENSION     = $04;  { Bit 2 - Fast extension (BEP 6) }
  DHT_EXTENSION      = $01;  { Bit 0 - DHT support (BEP 5) }

{ ============================================================================ }
{ Data Types                                                                   }
{ ============================================================================ }

type
  { Message structure - used for both encoding and decoding }
  PWireMessage = ^TWireMessage;
  TWireMessage = record
    Length: Cardinal;        { 4 bytes big-endian, 0 = keep-alive }
    MsgId: Byte;             { Message type (0-9) }
    
    { Union of payload data based on message type }
    case Integer of
      0: ( );                                    { Keep-alive }
      1: (HaveIndex: Cardinal);                  { MSG_HAVE }
      2: (BitfieldData: PByteArray;
           BitfieldLen: Integer);                { MSG_BITFIELD }
      3: (ReqIndex: Cardinal;
           ReqBegin: Cardinal;
           ReqLength: Cardinal);                 { MSG_REQUEST }
      4: (PieceIndex: Cardinal;
           PieceBegin: Cardinal;
           PieceData: PByteArray;
           PieceDataLen: Integer);               { MSG_PIECE }
      5: (CancelIndex: Cardinal;
           CancelBegin: Cardinal;
           CancelLength: Cardinal);              { MSG_CANCEL }
      6: (DhtPort: Word);                        { MSG_PORT }
  end;
  
  { Handshake structure }
  PHandshake = ^THandshake;
  THandshake = record
    ProtocolLen: Byte;                        { Always 19 }
    Protocol: array[0..18] of Char;           { "BitTorrent protocol" }
    Reserved: array[0..7] of Byte;            { Extension flags }
    InfoHash: array[0..SHA1_HASH_SIZE - 1] of Byte;  { 20 bytes }
    PeerId: array[0..SHA1_HASH_SIZE - 1] of Byte;    { 20 bytes }
  end;

{ ============================================================================ }
{ Handshake Functions                                                          }
{ ============================================================================ }

{ Encode a handshake into a buffer
  Returns: Number of bytes written (HANDSHAKE_LEN on success, 0 on failure) }
function EncodeHandshake(const InfoHash, PeerId: array of Byte;
                         Buffer: PByteArray; BufLen: Integer): Integer;

{ Decode a handshake from a buffer
  Returns: True if valid handshake decoded, False otherwise }
function DecodeHandshake(Buffer: PByteArray; Len: Integer;
                         out HS: THandshake): Boolean;

{ Validate a handshake structure
  Returns: True if handshake is valid, False otherwise }
function ValidateHandshake(const HS: THandshake): Boolean;

{ ============================================================================ }
{ Message Encoding                                                             }
{ ============================================================================ }

{ Get the size needed to encode a message (excluding length prefix) }
function GetMessagePayloadSize(MsgId: Byte): Integer;

{ Encode a wire message into a buffer
  Buffer must be large enough to hold the message (Length prefix + payload)
  Returns: True on success, False if buffer too small }
function EncodeMessage(const Msg: TWireMessage; 
                       Buffer: PByteArray; BufLen: Integer;
                       out BytesWritten: Integer): Boolean;

{ ============================================================================ }
{ Message Decoding                                                             }
{ ============================================================================ }

{ Decode a message from a buffer
  Returns: True if message decoded, False if incomplete or invalid
  BytesConsumed: Number of bytes consumed from buffer (0 if incomplete) }
function DecodeMessage(Buffer: PByteArray; BufLen: Integer;
                       out Msg: TWireMessage;
                       out BytesConsumed: Integer): Boolean;

{ Check if a complete message is available in buffer
  Returns: >0 = complete message size, 0 = incomplete, -1 = invalid }
function MessageComplete(Buffer: PByteArray; BufLen: Integer): Integer;

{ ============================================================================ }
{ Simple Message Builders (for common messages)                                }
{ ============================================================================ }

{ Build a choke message (5 bytes)
  BufLen must be at least 5 bytes
  Returns: True on success, False if buffer too small }
function BuildChoke(Buffer: PByteArray; BufLen: Integer; out Len: Integer): Boolean;

{ Build an unchoke message (5 bytes)
  BufLen must be at least 5 bytes
  Returns: True on success, False if buffer too small }
function BuildUnchoke(Buffer: PByteArray; BufLen: Integer; out Len: Integer): Boolean;

{ Build an interested message (5 bytes)
  BufLen must be at least 5 bytes
  Returns: True on success, False if buffer too small }
function BuildInterested(Buffer: PByteArray; BufLen: Integer; out Len: Integer): Boolean;

{ Build a not interested message (5 bytes)
  BufLen must be at least 5 bytes
  Returns: True on success, False if buffer too small }
function BuildNotInterested(Buffer: PByteArray; BufLen: Integer; out Len: Integer): Boolean;

{ Build a have message (9 bytes)
  BufLen must be at least 9 bytes
  Returns: True on success, False if buffer too small }
function BuildHave(PieceIndex: Integer; Buffer: PByteArray; BufLen: Integer; out Len: Integer): Boolean;

{ Build a request message (17 bytes)
  BufLen must be at least 17 bytes
  Returns: True on success, False if buffer too small }
function BuildRequest(Index, BeginOffset, Length: Integer;
                      Buffer: PByteArray; BufLen: Integer; out Len: Integer): Boolean;

{ Build a cancel message (17 bytes)
  BufLen must be at least 17 bytes
  Returns: True on success, False if buffer too small }
function BuildCancel(Index, BeginOffset, Length: Integer;
                     Buffer: PByteArray; BufLen: Integer; out Len: Integer): Boolean;

{ Build a port message (7 bytes)
  BufLen must be at least 7 bytes
  Returns: True on success, False if buffer too small }
function BuildPort(Port: Word; Buffer: PByteArray; BufLen: Integer; out Len: Integer): Boolean;

{ Build a bitfield message (variable size)
  Bitfield must point to valid data, it is NOT copied
  Returns: True on success }
function BuildBitfield(Bitfield: PByteArray; BitfieldLen: Integer;
                       Buffer: PByteArray; BufLen: Integer;
                       out MsgLen: Integer): Boolean;

{ Build a piece message (variable size)
  Data must point to valid data, it is NOT copied
  Returns: True on success }
function BuildPiece(Index, BeginOffset: Integer;
                    Data: PByteArray; DataLen: Integer;
                    Buffer: PByteArray; BufLen: Integer;
                    out MsgLen: Integer): Boolean;

{ ============================================================================ }
{ Utility Functions                                                            }
{ ============================================================================ }

{ Get message name for debugging }
function MessageName(MsgId: Byte): string;

{ Check if message ID is valid }
function IsValidMessageId(MsgId: Byte): Boolean;

implementation

{ ============================================================================ }
{ Handshake Implementation                                                     }
{ ============================================================================ }

function EncodeHandshake(const InfoHash, PeerId: array of Byte;
                         Buffer: PByteArray; BufLen: Integer): Integer;
var
  I: Integer;
  Offset: Integer;
begin
  Result := 0;
  
  { Validate parameters }
  if (Buffer = nil) or (BufLen < HANDSHAKE_LEN) then Exit;
  if Length(InfoHash) <> SHA1_HASH_SIZE then Exit;
  if Length(PeerId) <> SHA1_HASH_SIZE then Exit;
  
  Offset := 0;
  
  { Protocol length }
  Buffer^[Offset] := PROTOCOL_LEN;
  Inc(Offset);
  
  { Protocol string }
  for I := 0 to PROTOCOL_LEN - 1 do
    Buffer^[Offset + I] := Ord(PROTOCOL_STRING[I + 1]);
  Inc(Offset, PROTOCOL_LEN);
  
  { Reserved bytes (8 zeros) }
  for I := 0 to 7 do
    Buffer^[Offset + I] := 0;
  Inc(Offset, 8);
  
  { Info hash (20 bytes) }
  for I := 0 to SHA1_HASH_SIZE - 1 do
    Buffer^[Offset + I] := InfoHash[I];
  Inc(Offset, SHA1_HASH_SIZE);
  
  { Peer ID (20 bytes) }
  for I := 0 to SHA1_HASH_SIZE - 1 do
    Buffer^[Offset + I] := PeerId[I];
  Inc(Offset, SHA1_HASH_SIZE);
  
  Result := Offset;  { Should be HANDSHAKE_LEN (68) }
end;

function DecodeHandshake(Buffer: PByteArray; Len: Integer;
                         out HS: THandshake): Boolean;
var
  I: Integer;
  Offset: Integer;
begin
  Result := False;
  
  { Clear output }
  FillChar(HS, SizeOf(HS), 0);
  
  { Check buffer size }
  if (Buffer = nil) or (Len < HANDSHAKE_LEN) then Exit;
  
  Offset := 0;
  
  { Protocol length }
  HS.ProtocolLen := Buffer^[Offset];
  Inc(Offset);
  
  { Protocol string }
  for I := 0 to PROTOCOL_LEN - 1 do
    HS.Protocol[I] := Chr(Buffer^[Offset + I]);
  Inc(Offset, PROTOCOL_LEN);
  
  { Reserved bytes }
  for I := 0 to 7 do
    HS.Reserved[I] := Buffer^[Offset + I];
  Inc(Offset, 8);
  
  { Info hash }
  for I := 0 to SHA1_HASH_SIZE - 1 do
    HS.InfoHash[I] := Buffer^[Offset + I];
  Inc(Offset, SHA1_HASH_SIZE);
  
  { Peer ID }
  for I := 0 to SHA1_HASH_SIZE - 1 do
    HS.PeerId[I] := Buffer^[Offset + I];
  
  Result := True;
end;

function ValidateHandshake(const HS: THandshake): Boolean;
var
  I: Integer;
  ProtocolValid: Boolean;
begin
  Result := False;
  
  { Check protocol length }
  if HS.ProtocolLen <> PROTOCOL_LEN then Exit;
  
  { Check protocol string }
  ProtocolValid := True;
  for I := 0 to PROTOCOL_LEN - 1 do
    if HS.Protocol[I] <> PROTOCOL_STRING[I + 1] then
    begin
      ProtocolValid := False;
      Break;
    end;
  if not ProtocolValid then Exit;
  
  { Check info hash is not all zeros }
  ProtocolValid := False;
  for I := 0 to SHA1_HASH_SIZE - 1 do
    if HS.InfoHash[I] <> 0 then
    begin
      ProtocolValid := True;
      Break;
    end;
  if not ProtocolValid then Exit;
  
  Result := True;
end;

{ ============================================================================ }
{ Message Encoding Implementation                                              }
{ ============================================================================ }

function GetMessagePayloadSize(MsgId: Byte): Integer;
begin
  case MsgId of
    MSG_CHOKE, MSG_UNCHOKE, MSG_INTERESTED, MSG_NOT_INTERESTED:
      Result := 1;  { Just message ID }
    MSG_HAVE:
      Result := 5;  { ID + 4-byte piece index }
    MSG_BITFIELD:
      Result := -1; { Variable - caller must specify }
    MSG_REQUEST, MSG_CANCEL:
      Result := 13; { ID + index + begin + length }
    MSG_PIECE:
      Result := -1; { Variable - ID + index + begin + data }
    MSG_PORT:
      Result := 3;  { ID + 2-byte port }
  else
    Result := -1;   { Invalid or keep-alive (0) }
  end;
end;

procedure WriteBE32(Buffer: PByteArray; Offset: Integer; Value: Cardinal);
begin
  Buffer^[Offset]     := (Value shr 24) and $FF;
  Buffer^[Offset + 1] := (Value shr 16) and $FF;
  Buffer^[Offset + 2] := (Value shr 8)  and $FF;
  Buffer^[Offset + 3] := Value and $FF;
end;

function EncodeMessage(const Msg: TWireMessage;
                       Buffer: PByteArray; BufLen: Integer;
                       out BytesWritten: Integer): Boolean;
var
  Offset: Integer;
  PayloadSize: Integer;
begin
  Result := False;
  BytesWritten := 0;
  
  if Buffer = nil then Exit;
  
  { Keep-alive is special case (length 0, no ID) }
  if Msg.Length = 0 then
  begin
    if BufLen < 4 then Exit;
    WriteBE32(Buffer, 0, 0);
    BytesWritten := 4;
    Result := True;
    Exit;
  end;
  
  { Calculate payload size based on message type }
  case Msg.MsgId of
    MSG_CHOKE, MSG_UNCHOKE, MSG_INTERESTED, MSG_NOT_INTERESTED:
      PayloadSize := 1;
    MSG_HAVE:
      PayloadSize := 5;
    MSG_REQUEST, MSG_CANCEL:
      PayloadSize := 13;
    MSG_PORT:
      PayloadSize := 3;
    MSG_BITFIELD:
      PayloadSize := 1 + Msg.BitfieldLen;
    MSG_PIECE:
      PayloadSize := 9 + Msg.PieceDataLen;
  else
    Exit;  { Invalid message type }
  end;
  
  { Check buffer size }
  if BufLen < (4 + PayloadSize) then Exit;
  
  { Write length prefix (big-endian) }
  WriteBE32(Buffer, 0, PayloadSize);
  Offset := 4;
  
  { Write message ID }
  Buffer^[Offset] := Msg.MsgId;
  Inc(Offset);
  
  { Write payload based on message type }
  case Msg.MsgId of
    MSG_HAVE:
      WriteBE32(Buffer, Offset, Msg.HaveIndex);
      
    MSG_BITFIELD:
      if (Msg.BitfieldData <> nil) and (Msg.BitfieldLen > 0) then
        Move(Msg.BitfieldData^, Buffer^[Offset], Msg.BitfieldLen);
        
    MSG_REQUEST, MSG_CANCEL:
      begin
        if Msg.MsgId = MSG_REQUEST then
        begin
          WriteBE32(Buffer, Offset, Msg.ReqIndex);
          WriteBE32(Buffer, Offset + 4, Msg.ReqBegin);
          WriteBE32(Buffer, Offset + 8, Msg.ReqLength);
        end
        else
        begin
          WriteBE32(Buffer, Offset, Msg.CancelIndex);
          WriteBE32(Buffer, Offset + 4, Msg.CancelBegin);
          WriteBE32(Buffer, Offset + 8, Msg.CancelLength);
        end;
      end;
      
    MSG_PIECE:
      begin
        WriteBE32(Buffer, Offset, Msg.PieceIndex);
        WriteBE32(Buffer, Offset + 4, Msg.PieceBegin);
        if (Msg.PieceData <> nil) and (Msg.PieceDataLen > 0) then
          Move(Msg.PieceData^, Buffer^[Offset + 8], Msg.PieceDataLen);
      end;
      
    MSG_PORT:
      begin
        Buffer^[Offset] := (Msg.DhtPort shr 8) and $FF;
        Buffer^[Offset + 1] := Msg.DhtPort and $FF;
      end;
  end;
  
  BytesWritten := 4 + PayloadSize;
  Result := True;
end;

{ ============================================================================ }
{ Message Decoding Implementation                                              }
{ ============================================================================ }

function ReadBE32(Buffer: PByteArray; Offset: Integer): Cardinal;
begin
  Result := (Cardinal(Buffer^[Offset]) shl 24) or
            (Cardinal(Buffer^[Offset + 1]) shl 16) or
            (Cardinal(Buffer^[Offset + 2]) shl 8) or
            Cardinal(Buffer^[Offset + 3]);
end;

function MessageComplete(Buffer: PByteArray; BufLen: Integer): Integer;
var
  LengthPrefix: Cardinal;
begin
  Result := -1;  { Invalid }
  
  if (Buffer = nil) or (BufLen < 4) then
  begin
    Result := 0;  { Incomplete }
    Exit;
  end;
  
  { Read length prefix }
  LengthPrefix := ReadBE32(Buffer, 0);
  
  { Keep-alive (length 0) }
  if LengthPrefix = 0 then
  begin
    Result := 4;
    Exit;
  end;
  
  { Check if we have the full message }
  if BufLen < Integer(4 + LengthPrefix) then
  begin
    Result := 0;  { Incomplete }
    Exit;
  end;
  
  Result := Integer(4 + LengthPrefix);
end;

function DecodeMessage(Buffer: PByteArray; BufLen: Integer;
                       out Msg: TWireMessage;
                       out BytesConsumed: Integer): Boolean;
var
  LengthPrefix: Cardinal;
  Offset: Integer;
  MsgId: Byte;
begin
  Result := False;
  BytesConsumed := 0;
  
  { Clear output }
  FillChar(Msg, SizeOf(Msg), 0);
  
  { Validate inputs }
  if Buffer = nil then Exit;
  
  { Check for complete message }
  if BufLen < 4 then Exit;
  
  LengthPrefix := ReadBE32(Buffer, 0);
  
  { Keep-alive }
  if LengthPrefix = 0 then
  begin
    Msg.Length := 0;
    Msg.MsgId := 255;  { Invalid ID for keep-alive }
    BytesConsumed := 4;
    Result := True;
    Exit;
  end;
  
  { Check if complete message is available }
  if BufLen < Integer(4 + LengthPrefix) then Exit;
  
  Offset := 4;
  MsgId := Buffer^[Offset];
  Inc(Offset);
  
  { Validate message ID }
  if not IsValidMessageId(MsgId) then Exit;
  
  Msg.Length := LengthPrefix;
  Msg.MsgId := MsgId;
  
  { Parse payload based on message type }
  case MsgId of
    MSG_CHOKE, MSG_UNCHOKE, MSG_INTERESTED, MSG_NOT_INTERESTED:
      begin
        { No payload }
      end;
      
    MSG_HAVE:
      Msg.HaveIndex := ReadBE32(Buffer, Offset);
      
    MSG_BITFIELD:
      begin
        { Bitfield data starts at Offset, length is LengthPrefix - 1 }
        Msg.BitfieldLen := Integer(LengthPrefix) - 1;
        if Msg.BitfieldLen > 0 then
          Msg.BitfieldData := @Buffer^[Offset]
        else
          Msg.BitfieldLen := 0;
      end;
      
    MSG_REQUEST:
      begin
        Msg.ReqIndex := ReadBE32(Buffer, Offset);
        Msg.ReqBegin := ReadBE32(Buffer, Offset + 4);
        Msg.ReqLength := ReadBE32(Buffer, Offset + 8);
      end;
      
    MSG_PIECE:
      begin
        Msg.PieceIndex := ReadBE32(Buffer, Offset);
        Msg.PieceBegin := ReadBE32(Buffer, Offset + 4);
        Msg.PieceDataLen := Integer(LengthPrefix) - 9;
        if Msg.PieceDataLen > 0 then
          Msg.PieceData := @Buffer^[Offset + 8]
        else
          Msg.PieceDataLen := 0;
      end;
      
    MSG_CANCEL:
      begin
        Msg.CancelIndex := ReadBE32(Buffer, Offset);
        Msg.CancelBegin := ReadBE32(Buffer, Offset + 4);
        Msg.CancelLength := ReadBE32(Buffer, Offset + 8);
      end;
      
    MSG_PORT:
      Msg.DhtPort := (Buffer^[Offset] shl 8) or Buffer^[Offset + 1];
  end;
  
  BytesConsumed := Integer(4 + LengthPrefix);
  Result := True;
end;

{ ============================================================================ }
{ Simple Message Builders                                                      }
{ ============================================================================ }

function BuildChoke(Buffer: PByteArray; BufLen: Integer; out Len: Integer): Boolean;
begin
  Result := False;
  Len := 0;
  
  if (Buffer = nil) or (BufLen < 5) then Exit;
  
  WriteBE32(Buffer, 0, LEN_CHOKE);
  Buffer^[4] := MSG_CHOKE;
  Len := 5;
  Result := True;
end;

function BuildUnchoke(Buffer: PByteArray; BufLen: Integer; out Len: Integer): Boolean;
begin
  Result := False;
  Len := 0;
  
  if (Buffer = nil) or (BufLen < 5) then Exit;
  
  WriteBE32(Buffer, 0, LEN_UNCHOKE);
  Buffer^[4] := MSG_UNCHOKE;
  Len := 5;
  Result := True;
end;

function BuildInterested(Buffer: PByteArray; BufLen: Integer; out Len: Integer): Boolean;
begin
  Result := False;
  Len := 0;
  
  if (Buffer = nil) or (BufLen < 5) then Exit;
  
  WriteBE32(Buffer, 0, LEN_INTERESTED);
  Buffer^[4] := MSG_INTERESTED;
  Len := 5;
  Result := True;
end;

function BuildNotInterested(Buffer: PByteArray; BufLen: Integer; out Len: Integer): Boolean;
begin
  Result := False;
  Len := 0;
  
  if (Buffer = nil) or (BufLen < 5) then Exit;
  
  WriteBE32(Buffer, 0, LEN_NOT_INTERESTED);
  Buffer^[4] := MSG_NOT_INTERESTED;
  Len := 5;
  Result := True;
end;

function BuildHave(PieceIndex: Integer; Buffer: PByteArray; BufLen: Integer; out Len: Integer): Boolean;
begin
  Result := False;
  Len := 0;
  
  if (Buffer = nil) or (BufLen < 9) then Exit;
  
  WriteBE32(Buffer, 0, LEN_HAVE);
  Buffer^[4] := MSG_HAVE;
  WriteBE32(Buffer, 5, Cardinal(PieceIndex));
  Len := 9;
  Result := True;
end;

function BuildRequest(Index, BeginOffset, Length: Integer;
                      Buffer: PByteArray; BufLen: Integer; out Len: Integer): Boolean;
begin
  Result := False;
  Len := 0;
  
  if (Buffer = nil) or (BufLen < 17) then Exit;
  
  WriteBE32(Buffer, 0, LEN_REQUEST);
  Buffer^[4] := MSG_REQUEST;
  WriteBE32(Buffer, 5, Cardinal(Index));
  WriteBE32(Buffer, 9, Cardinal(BeginOffset));
  WriteBE32(Buffer, 13, Cardinal(Length));
  Len := 17;
  Result := True;
end;

function BuildCancel(Index, BeginOffset, Length: Integer;
                     Buffer: PByteArray; BufLen: Integer; out Len: Integer): Boolean;
begin
  Result := False;
  Len := 0;
  
  if (Buffer = nil) or (BufLen < 17) then Exit;
  
  WriteBE32(Buffer, 0, LEN_CANCEL);
  Buffer^[4] := MSG_CANCEL;
  WriteBE32(Buffer, 5, Cardinal(Index));
  WriteBE32(Buffer, 9, Cardinal(BeginOffset));
  WriteBE32(Buffer, 13, Cardinal(Length));
  Len := 17;
  Result := True;
end;

function BuildPort(Port: Word; Buffer: PByteArray; BufLen: Integer; out Len: Integer): Boolean;
begin
  Result := False;
  Len := 0;
  
  if (Buffer = nil) or (BufLen < 7) then Exit;
  
  WriteBE32(Buffer, 0, 3);  { LEN_PORT = 3: 1 byte ID + 2 bytes port number }
  Buffer^[4] := MSG_PORT;
  Buffer^[5] := (Port shr 8) and $FF;
  Buffer^[6] := Port and $FF;
  Len := 7;
  Result := True;
end;

function BuildBitfield(Bitfield: PByteArray; BitfieldLen: Integer;
                       Buffer: PByteArray; BufLen: Integer;
                       out MsgLen: Integer): Boolean;
begin
  Result := False;
  MsgLen := 0;
  
  { Safe check: prevent integer overflow in addition }
  if (Buffer = nil) or (BitfieldLen < 0) or (BufLen < 5) or 
     (BufLen - 5 < BitfieldLen) then Exit;
  
  WriteBE32(Buffer, 0, Cardinal(1 + BitfieldLen));
  Buffer^[4] := MSG_BITFIELD;
  
  if (Bitfield <> nil) and (BitfieldLen > 0) then
    Move(Bitfield^, Buffer^[5], BitfieldLen);
  
  MsgLen := 5 + BitfieldLen;
  Result := True;
end;

function BuildPiece(Index, BeginOffset: Integer;
                    Data: PByteArray; DataLen: Integer;
                    Buffer: PByteArray; BufLen: Integer;
                    out MsgLen: Integer): Boolean;
begin
  Result := False;
  MsgLen := 0;
  
  { Safe check: prevent integer overflow in addition }
  if (Buffer = nil) or (DataLen < 0) or (BufLen < 13) or 
     (BufLen - 13 < DataLen) then Exit;
  
  WriteBE32(Buffer, 0, Cardinal(9 + DataLen));
  Buffer^[4] := MSG_PIECE;
  WriteBE32(Buffer, 5, Cardinal(Index));
  WriteBE32(Buffer, 9, Cardinal(BeginOffset));
  
  if (Data <> nil) and (DataLen > 0) then
    Move(Data^, Buffer^[13], DataLen);
  
  MsgLen := 13 + DataLen;
  Result := True;
end;

{ ============================================================================ }
{ Utility Functions                                                            }
{ ============================================================================ }

function MessageName(MsgId: Byte): string;
begin
  case MsgId of
    255:      Result := 'Keep-Alive';
    MSG_CHOKE:          Result := 'Choke';
    MSG_UNCHOKE:        Result := 'Unchoke';
    MSG_INTERESTED:     Result := 'Interested';
    MSG_NOT_INTERESTED: Result := 'Not Interested';
    MSG_HAVE:           Result := 'Have';
    MSG_BITFIELD:       Result := 'Bitfield';
    MSG_REQUEST:        Result := 'Request';
    MSG_PIECE:          Result := 'Piece';
    MSG_CANCEL:         Result := 'Cancel';
    MSG_PORT:           Result := 'Port';
  else
    Result := 'Unknown';
  end;
end;

function IsValidMessageId(MsgId: Byte): Boolean;
begin
  Result := MsgId <= MSG_PORT;
end;

end.
