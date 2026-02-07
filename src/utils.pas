{
  utils.pas - General utility functions for PascalTorrent
  
  Common helper functions and data structures used throughout the client.
  Uses only imperative/procedural programming.
}

unit utils;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, StrUtils;

const
  { Buffer constants }
  MIN_BUFFER_CAPACITY = 16;
  MAX_BUFFER_CAPACITY = MaxInt div 4;  { Prevent integer overflow }
  
  { File constants }
  MAX_TORRENT_FILE_SIZE = 10 * 1024 * 1024;  { 10MB max for .torrent files }
  
  { Hex character tables }
  HEX_CHARS_LOWER: array[0..15] of Char = '0123456789abcdef';
  HEX_CHARS_UPPER: array[0..15] of Char = '0123456789ABCDEF';

type
  { Generic linked list node (can be used for various purposes) }
  PGenericNode = ^TGenericNode;
  TGenericNode = record
    Data: Pointer;
    Next: PGenericNode;
  end;
  
  { Buffer for dynamic byte arrays }
  PByteArray = ^TByteArray;
  TByteArray = array[0..MaxInt - 1] of Byte;
  
  { Dynamic buffer with automatic growth }
  PDynBuffer = ^TDynBuffer;
  TDynBuffer = record
    Data: PByteArray;
    Size: Integer;
    Capacity: Integer;
  end;
  
  { IP address record }
  TIPAddress = record
    IPv4: Cardinal;      { Network byte order }
    IPv6: array[0..15] of Byte;
    IsIPv6: Boolean;
  end;
  
  { Procedure types }
  TFreeProc = procedure(Data: Pointer);

{ ============================================================================ }
{ Linked List Utilities                                                       }
{ ============================================================================ }

{ Add node to head of list }
procedure ListAddHead(var Head: PGenericNode; Node: PGenericNode);

{ Add node to tail of list }
procedure ListAddTail(var Head: PGenericNode; Node: PGenericNode);

{ Remove node from list }
function ListRemove(var Head: PGenericNode; Node: PGenericNode): Boolean;

{ Find node in list }
function ListFind(Head: PGenericNode; Data: Pointer): PGenericNode;

{ Count nodes in list }
function ListCount(Head: PGenericNode): Integer;

{ Free all nodes in list (doesn't free Data pointers) }
procedure ListFree(var Head: PGenericNode);

{ Free all nodes and their data (using provided free proc) }
procedure ListFreeAll(var Head: PGenericNode; FreeProc: TFreeProc);

{ ============================================================================ }
{ Dynamic Buffer                                                              }
{ ============================================================================ }

{ Create a new dynamic buffer }
function DynBufferCreate(InitialCapacity: Integer = 256): PDynBuffer;

{ Free a dynamic buffer }
procedure DynBufferFree(Buffer: PDynBuffer);

{ Clear buffer (set size to 0) }
procedure DynBufferClear(Buffer: PDynBuffer);

{ Ensure capacity is at least Required }
function DynBufferReserve(Buffer: PDynBuffer; Required: Integer): Boolean;

{ Append data to buffer }
function DynBufferAppend(Buffer: PDynBuffer; const Data; Len: Integer): Boolean;

{ Append a single byte }
function DynBufferAppendByte(Buffer: PDynBuffer; B: Byte): Boolean;

{ Get pointer to data at offset }
function DynBufferAt(Buffer: PDynBuffer; Offset: Integer): Pointer;

{ ============================================================================ }
{ String Utilities                                                            }
{ ============================================================================ }

{ Convert integer to string with minimum width (zero-padded) }
function IntToStrPad(V: Integer; MinWidth: Integer): string;

{ Format bytes as human-readable string (B, KB, MB, GB) }
function FormatBytes(Size: Int64): string;

{ Format speed as human-readable string }
function FormatSpeed(Bps: Cardinal): string;

{ Format time duration }
function FormatDuration(Seconds: Cardinal): string;

{ Trim whitespace from both ends }
function TrimStr(const S: string): string;

{ Split string by delimiter }
function SplitString(const S, Delim: string; out Parts: array of string): Integer;

{ Join strings with delimiter }
function JoinStrings(const Parts: array of string; const Delim: string): string;

{ Convert hex string to bytes }
function HexToBytes(const Hex: string; var Bytes; MaxBytes: Integer): Integer;

{ Convert bytes to hex string }
function BytesToHex(const Bytes; Len: Integer): string;

{ URL-encode a string }
function URLEncode(const S: string): string;

{ URL-decode a string }
function URLDecode(const S: string): string;

{ Check if string starts with prefix }
function StartsWith(const S, Prefix: string): Boolean;

{ Check if string ends with suffix }
function EndsWith(const S, Suffix: string): Boolean;

{ ============================================================================ }
{ Binary Data Utilities                                                       }
{ ============================================================================ }

{ Swap byte order (big-endian <-> little-endian) for 16-bit }
function Swap16(V: Word): Word;

{ Swap byte order for 32-bit }
function Swap32(V: Cardinal): Cardinal;

{ Swap byte order for 64-bit }
function Swap64(V: QWord): QWord;

{ Convert host to network byte order (16-bit) }
function HTONS(V: Word): Word;

{ Convert host to network byte order (32-bit) }
function HTONL(V: Cardinal): Cardinal;

{ Convert network to host byte order (16-bit) }
function NTOHS(V: Word): Word;

{ Convert network to host byte order (32-bit) }
function NTOHL(V: Cardinal): Cardinal;

{ Read 16-bit big-endian from buffer }
function ReadBE16(const Buffer): Word;

{ Read 32-bit big-endian from buffer }
function ReadBE32(const Buffer): Cardinal;

{ Write 16-bit big-endian to buffer }
procedure WriteBE16(var Buffer; V: Word);

{ Write 32-bit big-endian to buffer }
procedure WriteBE32(var Buffer; V: Cardinal);

{ Copy memory with overlap protection (like memmove) }
procedure MemoryMove(const Source; var Dest; Len: Integer);

{ Compare memory blocks }
function MemoryEqual(const A, B; Len: Integer): Boolean;

{ Search for pattern in memory }
function MemoryFind(const Buffer; BufLen: Integer; 
                    const Pattern; PatLen: Integer): Integer;

{ ============================================================================ }
{ Random Utilities                                                            }
{ ============================================================================ }

{ Generate random bytes }
procedure RandomBytes(var Buffer; Len: Integer);

{ Generate random 32-bit integer }
function RandomInt: Cardinal;

{ Generate random integer in range [Min, Max] }
function RandomRange(Min, Max: Integer): Integer;

{ Generate random peer ID (20 bytes) }
procedure GeneratePeerID(out PeerID: array of Byte; const ClientPrefix: string);

{ ============================================================================ }
{ Time Utilities                                                              }
{ ============================================================================ }

{ Get current timestamp in milliseconds }
function GetTickMS: Int64;

{ Get monotonic time in seconds (for intervals) }
function GetMonoTime: Double;

{ Sleep for milliseconds }
procedure SleepMS(MS: Cardinal);

{ Calculate time difference in milliseconds }
function TimeDiffMS(StartTime, EndTime: Int64): Int64;

{ ============================================================================ }
{ File/Path Utilities                                                         }
{ ============================================================================ }

{ Join path components }
function JoinPath(const A, B: string): string;

{ Get filename from path }
function ExtractFile(const Path: string): string;

{ Get directory from path }
function ExtractDir(const Path: string): string;

{ Get file extension }
function ExtractExt(const Path: string): string;

{ Check if path is absolute }
function IsAbsolutePath(const Path: string): Boolean;

{ Expand relative path to absolute }
function ExpandPath(const Path: string): string;

{ Create directory (with parents if needed) }
function MakeDir(const Path: string): Boolean;

{ Check if file exists }
function FileExists(const Path: string): Boolean;

{ Check if directory exists }
function DirExists(const Path: string): Boolean;

{ Get file size }
function GetFileSize(const Path: string; out Size: Int64): Boolean;

{ Delete file }
function DeleteFile(const Path: string): Boolean;

{ Rename file }
function RenameFile(const OldPath, NewPath: string): Boolean;

{ Calculate bitfield size in bytes for given piece count }
function BitfieldBytes(PieceCount: Integer): Integer;

{ Create directory and all parent directories as needed }
function EnsureDirectories(const Path: string): Boolean;

implementation

{ ============================================================================ }
{ Linked List Utilities                                                       }
{ ============================================================================ }

procedure ListAddHead(var Head: PGenericNode; Node: PGenericNode);
begin
  if Node = nil then Exit;
  Node^.Next := Head;
  Head := Node;
end;

procedure ListAddTail(var Head: PGenericNode; Node: PGenericNode);
var
  Current: PGenericNode;
begin
  if Node = nil then Exit;
  Node^.Next := nil;
  
  if Head = nil then
  begin
    Head := Node;
  end
  else
  begin
    Current := Head;
    while Current^.Next <> nil do
      Current := Current^.Next;
    Current^.Next := Node;
  end;
end;

function ListRemove(var Head: PGenericNode; Node: PGenericNode): Boolean;
var
  Current, Prev: PGenericNode;
begin
  Result := False;
  if (Head = nil) or (Node = nil) then Exit;
  
  if Head = Node then
  begin
    Head := Node^.Next;
    Result := True;
    Exit;
  end;
  
  Prev := Head;
  Current := Head^.Next;
  while Current <> nil do
  begin
    if Current = Node then
    begin
      Prev^.Next := Node^.Next;
      Result := True;
      Exit;
    end;
    Prev := Current;
    Current := Current^.Next;
  end;
end;

function ListFind(Head: PGenericNode; Data: Pointer): PGenericNode;
begin
  Result := Head;
  while Result <> nil do
  begin
    if Result^.Data = Data then Exit;
    Result := Result^.Next;
  end;
end;

function ListCount(Head: PGenericNode): Integer;
var
  Node: PGenericNode;
begin
  Result := 0;
  Node := Head;
  while Node <> nil do
  begin
    Inc(Result);
    Node := Node^.Next;
  end;
end;

procedure ListFree(var Head: PGenericNode);
var
  Node, Next: PGenericNode;
begin
  Node := Head;
  while Node <> nil do
  begin
    Next := Node^.Next;
    Dispose(Node);
    Node := Next;
  end;
  Head := nil;
end;

procedure ListFreeAll(var Head: PGenericNode; FreeProc: TFreeProc);
var
  Node, Next: PGenericNode;
begin
  Node := Head;
  while Node <> nil do
  begin
    Next := Node^.Next;
    if (Node^.Data <> nil) and (TFreeProc(FreeProc) <> nil) then
      FreeProc(Node^.Data);
    Dispose(Node);
    Node := Next;
  end;
  Head := nil;
end;

{ ============================================================================ }
{ Dynamic Buffer                                                              }
{ ============================================================================ }

function DynBufferCreate(InitialCapacity: Integer): PDynBuffer;
begin
  if InitialCapacity < MIN_BUFFER_CAPACITY then 
    InitialCapacity := MIN_BUFFER_CAPACITY;
  
  New(Result);
  if Result = nil then Exit;  { Allocation failed }
  
  Result^.Size := 0;
  Result^.Capacity := InitialCapacity;
  GetMem(Result^.Data, InitialCapacity);
  
  { Check allocation failure }
  if Result^.Data = nil then
  begin
    Dispose(Result);
    Result := nil;
  end;
end;

procedure DynBufferFree(Buffer: PDynBuffer);
begin
  if Buffer = nil then Exit;
  if Buffer^.Data <> nil then
    FreeMem(Buffer^.Data);
  Dispose(Buffer);
end;

procedure DynBufferClear(Buffer: PDynBuffer);
begin
  if Buffer = nil then Exit;
  Buffer^.Size := 0;
end;

function DynBufferReserve(Buffer: PDynBuffer; Required: Integer): Boolean;
var
  NewCapacity: Integer;
  NewData: PByteArray;
begin
  Result := False;
  if Buffer = nil then Exit;
  if Buffer^.Capacity >= Required then
  begin
    Result := True;
    Exit;
  end;
  
  { Check for unreasonable size }
  if Required > MAX_BUFFER_CAPACITY then Exit;
  
  { Double capacity until sufficient, with overflow protection }
  NewCapacity := Buffer^.Capacity;
  while (NewCapacity < Required) and (NewCapacity < MAX_BUFFER_CAPACITY div 2) do
    NewCapacity := NewCapacity * 2;
  
  { If still not enough, use Required directly }
  if NewCapacity < Required then
    NewCapacity := Required;
  
  { Reallocate }
  GetMem(NewData, NewCapacity);
  if NewData = nil then Exit;  { Allocation failed }
  
  if Buffer^.Size > 0 then
    Move(Buffer^.Data^[0], NewData^[0], Buffer^.Size);
  FreeMem(Buffer^.Data);
  
  Buffer^.Data := NewData;
  Buffer^.Capacity := NewCapacity;
  Result := True;
end;

function DynBufferAppend(Buffer: PDynBuffer; const Data; Len: Integer): Boolean;
begin
  Result := False;
  if (Buffer = nil) or (Len < 0) then Exit;
  if Len = 0 then
  begin
    Result := True;
    Exit;
  end;
  
  { Check for potential overflow }
  if (Buffer^.Size > MaxInt - Len) then Exit;
  
  if not DynBufferReserve(Buffer, Buffer^.Size + Len) then Exit;
  
  Move(Data, Buffer^.Data^[Buffer^.Size], Len);
  Inc(Buffer^.Size, Len);
  Result := True;
end;

function DynBufferAppendByte(Buffer: PDynBuffer; B: Byte): Boolean;
begin
  Result := DynBufferAppend(Buffer, B, 1);
end;

function DynBufferAt(Buffer: PDynBuffer; Offset: Integer): Pointer;
begin
  if (Buffer = nil) or (Offset < 0) or (Offset >= Buffer^.Size) then
    Result := nil
  else
    Result := @Buffer^.Data^[Offset];
end;

{ ============================================================================ }
{ String Utilities                                                            }
{ ============================================================================ }

function IntToStrPad(V: Integer; MinWidth: Integer): string;
begin
  Result := IntToStr(V);
  while Length(Result) < MinWidth do
    Result := '0' + Result;
end;

function FormatBytes(Size: Int64): string;
const
  KB = 1024;
  MB = 1024 * 1024;
  GB = 1024 * 1024 * 1024;
  TB = Int64(1024) * 1024 * 1024 * 1024;
begin
  if Size < KB then
    Result := IntToStr(Size) + ' B'
  else if Size < MB then
    Result := Format('%.2f KB', [Size / KB])
  else if Size < GB then
    Result := Format('%.2f MB', [Size / MB])
  else if Size < TB then
    Result := Format('%.2f GB', [Size / GB])
  else
    Result := Format('%.2f TB', [Size / TB]);
end;

function FormatSpeed(Bps: Cardinal): string;
begin
  Result := FormatBytes(Bps) + '/s';
end;

function FormatDuration(Seconds: Cardinal): string;
var
  H, M, S: Cardinal;
begin
  H := Seconds div 3600;
  M := (Seconds mod 3600) div 60;
  S := Seconds mod 60;
  
  if H > 0 then
    Result := Format('%d:%.2d:%.2d', [H, M, S])
  else
    Result := Format('%d:%.2d', [M, S]);
end;

function TrimStr(const S: string): string;
var
  Start, Finish: Integer;
  Len: Integer;
begin
  Len := Length(S);
  Start := 1;
  while (Start <= Len) and (S[Start] <= ' ') do
    Inc(Start);
  
  Finish := Len;
  while (Finish >= 1) and (S[Finish] <= ' ') do
    Dec(Finish);
  
  if Start > Finish then
    Result := ''
  else
    Result := Copy(S, Start, Finish - Start + 1);
end;

function SplitString(const S, Delim: string; out Parts: array of string): Integer;
var
  Start, Pos: Integer;
  DelimLen: Integer;
begin
  Result := 0;
  DelimLen := Length(Delim);
  if DelimLen = 0 then Exit;
  
  Start := 1;
  Pos := PosEx(Delim, S, Start);
  
  while (Pos > 0) and (Result < Length(Parts)) do
  begin
    Parts[Result] := Copy(S, Start, Pos - Start);
    Inc(Result);
    Start := Pos + DelimLen;
    Pos := PosEx(Delim, S, Start);
  end;
  
  if (Start <= Length(S)) and (Result < Length(Parts)) then
  begin
    Parts[Result] := Copy(S, Start, MaxInt);
    Inc(Result);
  end;
end;

function JoinStrings(const Parts: array of string; const Delim: string): string;
var
  I: Integer;
  Len: Integer;
  Pos: Integer;
begin
  if Length(Parts) = 0 then
  begin
    Result := '';
    Exit;
  end;
  
  { Calculate total length }
  Len := 0;
  for I := 0 to High(Parts) do
    Len := Len + Length(Parts[I]);
  Len := Len + Length(Delim) * (Length(Parts) - 1);
  
  { Build result }
  SetLength(Result, Len);
  Pos := 1;
  
  for I := 0 to High(Parts) do
  begin
    if I > 0 then
    begin
      Move(PChar(Delim)^, Result[Pos], Length(Delim));
      Inc(Pos, Length(Delim));
    end;
    Move(PChar(Parts[I])^, Result[Pos], Length(Parts[I]));
    Inc(Pos, Length(Parts[I]));
  end;
end;

function HexToBytes(const Hex: string; var Bytes; MaxBytes: Integer): Integer;
var
  I: Integer;
  B: Byte;
  HighNib, LowNib: Byte;
  HexLen: Integer;
  OutIdx: Integer;
  P: PByte;

  function CharToNibble(C: Char; var N: Byte): Boolean;
  begin
    case C of
      '0'..'9': N := Ord(C) - Ord('0');
      'a'..'f': N := Ord(C) - Ord('a') + 10;
      'A'..'F': N := Ord(C) - Ord('A') + 10;
    else
      Result := False; Exit;
    end;
    Result := True;
  end;

begin
  Result := 0;
  HexLen := Length(Hex);
  if HexLen < 2 then Exit;
  
  P := PByte(@Bytes);
  OutIdx := 0;
  I := 1;
  
  while (I < HexLen) and (OutIdx < MaxBytes) do
  begin
    if not CharToNibble(Hex[I], HighNib) then
    begin
      Inc(I);
      Continue;
    end;
    
    if I + 1 > HexLen then Exit;
    if not CharToNibble(Hex[I + 1], LowNib) then Exit;
    
    B := (HighNib shl 4) or LowNib;
    (P + OutIdx)^ := B;
    Inc(OutIdx);
    Inc(I, 2);
  end;
  
  Result := OutIdx;
end;

function BytesToHex(const Bytes; Len: Integer): string;
var
  I: Integer;
  P: PByte;
begin
  SetLength(Result, Len * 2);
  P := PByte(@Bytes);
  for I := 0 to Len - 1 do
  begin
    Result[I * 2 + 1] := HEX_CHARS_LOWER[(P + I)^ shr 4];
    Result[I * 2 + 2] := HEX_CHARS_LOWER[(P + I)^ and $0F];
  end;
end;

function URLEncode(const S: string): string;
var
  I: Integer;
  C: Char;
  Pos: Integer;
  Len: Integer;
  ResultLen: Integer;
begin
  Len := Length(S);
  
  { Calculate result size first }
  ResultLen := 0;
  for I := 1 to Len do
  begin
    C := S[I];
    if (C >= 'A') and (C <= 'Z') or
       (C >= 'a') and (C <= 'z') or
       (C >= '0') and (C <= '9') or
       (C = '-') or (C = '_') or (C = '.') or (C = '~') then
      Inc(ResultLen)
    else
      Inc(ResultLen, 3);
  end;
  
  SetLength(Result, ResultLen);
  Pos := 1;
  
  for I := 1 to Len do
  begin
    C := S[I];
    if (C >= 'A') and (C <= 'Z') or
       (C >= 'a') and (C <= 'z') or
       (C >= '0') and (C <= '9') or
       (C = '-') or (C = '_') or (C = '.') or (C = '~') then
    begin
      Result[Pos] := C;
      Inc(Pos);
    end
    else
    begin
      Result[Pos] := '%';
      Result[Pos + 1] := HEX_CHARS_UPPER[Ord(C) shr 4];
      Result[Pos + 2] := HEX_CHARS_UPPER[Ord(C) and $0F];
      Inc(Pos, 3);
    end;
  end;
end;

function URLDecode(const S: string): string;
var
  I: Integer;
  Pos: Integer;
  Len: Integer;
  B: Byte;
  ResultLen: Integer;

  function HexValue(C: Char): Integer;
  begin
    case C of
      '0'..'9': Result := Ord(C) - Ord('0');
      'a'..'f': Result := Ord(C) - Ord('a') + 10;
      'A'..'F': Result := Ord(C) - Ord('A') + 10;
    else
      Result := -1;
    end;
  end;

begin
  Len := Length(S);
  
  { Calculate result length }
  ResultLen := 0;
  I := 1;
  while I <= Len do
  begin
    if (S[I] = '%') and (I + 2 <= Len) and (HexValue(S[I + 1]) >= 0) and (HexValue(S[I + 2]) >= 0) then
    begin
      Inc(ResultLen);
      Inc(I, 3);
    end
    else if S[I] = '+' then
    begin
      Inc(ResultLen);
      Inc(I);
    end
    else
    begin
      Inc(ResultLen);
      Inc(I);
    end;
  end;
  
  SetLength(Result, ResultLen);
  Pos := 1;
  I := 1;
  
  while I <= Len do
  begin
    if (S[I] = '%') and (I + 2 <= Len) then
    begin
      B := (HexValue(S[I + 1]) shl 4) or HexValue(S[I + 2]);
      Result[Pos] := Chr(B);
      Inc(Pos);
      Inc(I, 3);
      Continue;
    end
    else if S[I] = '+' then
    begin
      Result[Pos] := ' ';
      Inc(Pos);
      Inc(I);
      Continue;
    end;
    
    Result[Pos] := S[I];
    Inc(Pos);
    Inc(I);
  end;
end;

function StartsWith(const S, Prefix: string): Boolean;
var
  I: Integer;
begin
  if Length(S) < Length(Prefix) then
  begin
    Result := False;
    Exit;
  end;
  
  for I := 1 to Length(Prefix) do
    if S[I] <> Prefix[I] then
    begin
      Result := False;
      Exit;
    end;
  
  Result := True;
end;

function EndsWith(const S, Suffix: string): Boolean;
var
  Offset, I: Integer;
begin
  if Length(S) < Length(Suffix) then
  begin
    Result := False;
    Exit;
  end;
  
  Offset := Length(S) - Length(Suffix);
  for I := 1 to Length(Suffix) do
    if S[Offset + I] <> Suffix[I] then
    begin
      Result := False;
      Exit;
    end;
  
  Result := True;
end;

{ ============================================================================ }
{ Binary Data Utilities                                                       }
{ ============================================================================ }

function Swap16(V: Word): Word;
begin
  Result := ((V and $FF) shl 8) or ((V shr 8) and $FF);
end;

function Swap32(V: Cardinal): Cardinal;
begin
  Result := ((V and $000000FF) shl 24) or
            ((V and $0000FF00) shl 8) or
            ((V and $00FF0000) shr 8) or
            ((V and $FF000000) shr 24);
end;

function Swap64(V: QWord): QWord;
begin
  Result := ((V and $00000000000000FF) shl 56) or
            ((V and $000000000000FF00) shl 40) or
            ((V and $0000000000FF0000) shl 24) or
            ((V and $00000000FF000000) shl 8) or
            ((V and $000000FF00000000) shr 8) or
            ((V and $0000FF0000000000) shr 24) or
            ((V and $00FF000000000000) shr 40) or
            ((V and $FF00000000000000) shr 56);
end;

function HTONS(V: Word): Word;
{$IFDEF ENDIAN_BIG}
begin
  Result := V;
end;
{$ELSE}
begin
  Result := Swap16(V);
end;
{$ENDIF}

function HTONL(V: Cardinal): Cardinal;
{$IFDEF ENDIAN_BIG}
begin
  Result := V;
end;
{$ELSE}
begin
  Result := Swap32(V);
end;
{$ENDIF}

function NTOHS(V: Word): Word;
begin
  Result := HTONS(V);
end;

function NTOHL(V: Cardinal): Cardinal;
begin
  Result := HTONL(V);
end;

function ReadBE16(const Buffer): Word;
var
  P: PByte;
begin
  P := PByte(@Buffer);
  Result := (P[0] shl 8) or P[1];
end;

function ReadBE32(const Buffer): Cardinal;
var
  P: PByte;
begin
  P := PByte(@Buffer);
  Result := (Cardinal(P[0]) shl 24) or
            (Cardinal(P[1]) shl 16) or
            (Cardinal(P[2]) shl 8) or
            Cardinal(P[3]);
end;

procedure WriteBE16(var Buffer; V: Word);
var
  P: PByte;
begin
  P := PByte(@Buffer);
  P[0] := (V shr 8) and $FF;
  P[1] := V and $FF;
end;

procedure WriteBE32(var Buffer; V: Cardinal);
var
  P: PByte;
begin
  P := PByte(@Buffer);
  P[0] := (V shr 24) and $FF;
  P[1] := (V shr 16) and $FF;
  P[2] := (V shr 8) and $FF;
  P[3] := V and $FF;
end;

procedure MemoryMove(const Source; var Dest; Len: Integer);
begin
  Move(Source, Dest, Len);
end;

function MemoryEqual(const A, B; Len: Integer): Boolean;
var
  PA, PB: PByte;
  I: Integer;
begin
  PA := PByte(@A);
  PB := PByte(@B);
  
  for I := 0 to Len - 1 do
    if PA[I] <> PB[I] then
    begin
      Result := False;
      Exit;
    end;
  
  Result := True;
end;

function MemoryFind(const Buffer; BufLen: Integer; 
                    const Pattern; PatLen: Integer): Integer;
var
  PBuf: PByte;
  PPat: PByte;
  I, J: Integer;
begin
  Result := -1;
  if (BufLen < PatLen) or (PatLen <= 0) then Exit;
  
  PBuf := PByte(@Buffer);
  PPat := PByte(@Pattern);
  
  for I := 0 to BufLen - PatLen do
  begin
    J := 0;
    while (J < PatLen) and (PBuf[I + J] = PPat[J]) do
      Inc(J);
    if J = PatLen then
    begin
      Result := I;
      Exit;
    end;
  end;
end;

{ ============================================================================ }
{ Random Utilities                                                            }
{ ============================================================================ }

procedure RandomBytes(var Buffer; Len: Integer);
var
  P: PByte;
  I: Integer;
begin
  P := PByte(@Buffer);
  for I := 0 to Len - 1 do
    P[I] := Random(256);
end;

function RandomInt: Cardinal;
begin
  Result := Cardinal(Random($7FFFFFFF)) or (Cardinal(Random($7FFFFFFF)) shl 15);
end;

function RandomRange(Min, Max: Integer): Integer;
begin
  if Min >= Max then
    Result := Min
  else
    Result := Min + Random(Max - Min);
end;

procedure GeneratePeerID(out PeerID: array of Byte; const ClientPrefix: string);
var
  I: Integer;
  PrefixLen: Integer;
begin
  PrefixLen := Length(ClientPrefix);
  if PrefixLen > 20 then PrefixLen := 20;
  
  { Copy prefix }
  for I := 0 to PrefixLen - 1 do
    PeerID[I] := Ord(ClientPrefix[I + 1]);
  
  { Fill rest with random bytes }
  for I := PrefixLen to 19 do
    PeerID[I] := Random(256);
end;

{ ============================================================================ }
{ Time Utilities                                                              }
{ ============================================================================ }

function GetTickMS: Int64;
{$IFDEF MSWINDOWS}
var
  Tick: Cardinal;
begin
  Tick := GetTickCount;
  Result := Tick;
end;
{$ELSE}
begin
  Result := Int64(Trunc(Now * 24 * 60 * 60 * 1000));
end;
{$ENDIF}

function GetMonoTime: Double;
begin
  { Returns seconds with fractional part }
  Result := Now * 24 * 60 * 60;
end;

procedure SleepMS(MS: Cardinal);
begin
  Sleep(MS);
end;

function TimeDiffMS(StartTime, EndTime: Int64): Int64;
begin
  Result := EndTime - StartTime;
  if Result < 0 then
    Result := Result + $100000000;  { Handle 32-bit overflow }
end;

{ ============================================================================ }
{ File/Path Utilities                                                         }
{ ============================================================================ }

function JoinPath(const A, B: string): string;
var
  EndsWithSep, StartsWithSep: Boolean;
  Sep: Char;
begin
  {$IFDEF MSWINDOWS}
  Sep := '\';
  {$ELSE}
  Sep := '/';
  {$ENDIF}
  
  if A = '' then
  begin
    Result := B;
    Exit;
  end;
  
  if B = '' then
  begin
    Result := A;
    Exit;
  end;
  
  EndsWithSep := (A[Length(A)] = '/') or (A[Length(A)] = '\');
  StartsWithSep := (B[1] = '/') or (B[1] = '\');
  
  if EndsWithSep and StartsWithSep then
    Result := A + Copy(B, 2, MaxInt)
  else if EndsWithSep or StartsWithSep then
    Result := A + B
  else
    Result := A + Sep + B;
end;

function ExtractFile(const Path: string): string;
var
  I: Integer;
begin
  Result := Path;
  for I := Length(Path) downto 1 do
    if (Path[I] = '/') or (Path[I] = '\') then
    begin
      Result := Copy(Path, I + 1, MaxInt);
      Exit;
    end;
end;

function ExtractDir(const Path: string): string;
var
  I: Integer;
begin
  Result := Path;
  for I := Length(Path) downto 1 do
    if (Path[I] = '/') or (Path[I] = '\') then
    begin
      Result := Copy(Path, 1, I - 1);
      Exit;
    end;
end;

function ExtractExt(const Path: string): string;
var
  I: Integer;
begin
  Result := '';
  for I := Length(Path) downto 1 do
  begin
    if Path[I] = '.' then
    begin
      Result := Copy(Path, I, MaxInt);
      Exit;
    end
    else if (Path[I] = '/') or (Path[I] = '\') then
      Exit;
  end;
end;

function IsAbsolutePath(const Path: string): Boolean;
begin
  {$IFDEF MSWINDOWS}
  Result := (Length(Path) >= 2) and (Path[2] = ':');
  {$ELSE}
  Result := (Length(Path) > 0) and (Path[1] = '/');
  {$ENDIF}
end;

function ExpandPath(const Path: string): string;
begin
  { Simple version - just return as-is for now }
  { Full implementation would use realpath/GetFullPathName }
  Result := Path;
end;

function MakeDir(const Path: string): Boolean;
{$IFDEF MSWINDOWS}
begin
  Result := CreateDir(Path);
end;
{$ELSE}
begin
  {$I-}
  MkDir(Path);
  Result := IOResult = 0;
  {$I+}
end;
{$ENDIF}

function EnsureDirectories(const Path: string): Boolean;
var
  Parent: string;
  CurrentPath: string;
begin
  Result := False;
  if Path = '' then Exit;
  
  { Remove trailing separator }
  CurrentPath := Path;
  {$IFDEF MSWINDOWS}
  while (Length(CurrentPath) > 0) and 
        (CurrentPath[Length(CurrentPath)] in ['\', '/']) do
    Delete(CurrentPath, Length(CurrentPath), 1);
  {$ELSE}
  while (Length(CurrentPath) > 0) and 
        (CurrentPath[Length(CurrentPath)] = '/') do
    Delete(CurrentPath, Length(CurrentPath), 1);
  {$ENDIF}
  
  if CurrentPath = '' then Exit;
  
  { If already exists, we're done }
  if DirExists(CurrentPath) then
  begin
    Result := True;
    Exit;
  end;
  
  { Try to create directly first }
  if MakeDir(CurrentPath) then
  begin
    Result := True;
    Exit;
  end;
  
  { Need to create parent first }
  Parent := ExtractDir(CurrentPath);
  if (Parent = '') or (Parent = CurrentPath) then
  begin
    Result := MakeDir(CurrentPath);
    Exit;
  end;
  
  { Recursively create parent }
  if not EnsureDirectories(Parent) then Exit;
  
  { Now create this directory }
  Result := MakeDir(CurrentPath);
end;

function FileExists(const Path: string): Boolean;
{$IFDEF FPC}
begin
  Result := SysUtils.FileExists(Path);
end;
{$ELSE}
var
  F: File;
begin
  Assign(F, Path);
  {$I-}
  Reset(F, 1);
  Result := IOResult = 0;
  if Result then Close(F);
  {$I+}
end;
{$ENDIF}

function DirExists(const Path: string): Boolean;
{$IFDEF FPC}
begin
  Result := SysUtils.DirectoryExists(Path);
end;
{$ELSE}
begin
  { Simplified check }
  Result := FileExists(Path);
end;
{$ENDIF}

function GetFileSize(const Path: string; out Size: Int64): Boolean;
var
  F: File;
begin
  Result := False;
  Size := 0;
  
  Assign(F, Path);
  {$I-}
  Reset(F, 1);
  if IOResult = 0 then
  begin
    Size := System.FileSize(F);
    Close(F);
    Result := True;
  end;
  {$I+}
end;

function DeleteFile(const Path: string): Boolean;
{$IFDEF FPC}
begin
  Result := SysUtils.DeleteFile(Path);
end;
{$ELSE}
var
  F: File;
begin
  Assign(F, Path);
  {$I-}
  Erase(F);
  Result := IOResult = 0;
  {$I+}
end;
{$ENDIF}

function RenameFile(const OldPath, NewPath: string): Boolean;
{$IFDEF FPC}
begin
  Result := SysUtils.RenameFile(OldPath, NewPath);
end;
{$ELSE}
var
  F: File;
begin
  Assign(F, OldPath);
  {$I-}
  Rename(F, NewPath);
  Result := IOResult = 0;
  {$I+}
end;
{$ENDIF}

{ Calculate bitfield size in bytes for given piece count }
function BitfieldBytes(PieceCount: Integer): Integer;
begin
  if PieceCount <= 0 then
    Result := 0
  else
    Result := (PieceCount + 7) div 8;
end;

end.
