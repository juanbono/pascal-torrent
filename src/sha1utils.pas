{
  sha1utils.pas - SHA1 hashing utilities for BitTorrent
  
  This unit provides SHA1 hashing functionality using Free Pascal's
  built-in SHA1 unit from the hash package.
}

unit sha1utils;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, sha1;

type
  { 20-byte SHA1 digest }
  TSHA1Digest = sha1.TSHA1Digest;
  PSHA1Digest = ^TSHA1Digest;
  
  { Context for incremental hashing }
  TSHA1Context = sha1.TSHA1Context;
  
  { Progress callback type }
  TProgressCallback = procedure(Percent: Integer);

{ ============================================================================ }
{ One-Shot Hashing                                                            }
{ ============================================================================ }

{ Compute SHA1 hash of a buffer }
function SHA1Buffer(const Buffer; Len: Cardinal): TSHA1Digest;

{ Compute SHA1 hash of a string }
function SHA1String(const S: string): TSHA1Digest;

{ Compute SHA1 hash of a file }
function SHA1File(const Filename: string): TSHA1Digest;

{ Compute SHA1 hash of a file, with optional progress callback }
function SHA1FileProgress(const Filename: string; 
                          ProgressCallback: TProgressCallback;
                          ChunkSize: Cardinal = 65536): TSHA1Digest;

{ ============================================================================ }
{ Incremental Hashing                                                         }
{ ============================================================================ }

{ Initialize a new SHA1 context }
procedure SHA1Init(var Context: TSHA1Context);

{ Update hash with more data }
procedure SHA1Update(var Context: TSHA1Context; const Buffer; Len: Cardinal);

{ Finalize and get the digest }
procedure SHA1Final(var Context: TSHA1Context; var Digest: TSHA1Digest);

{ ============================================================================ }
{ Digest Utilities                                                            }
{ ============================================================================ }

{ Convert digest to hex string (lowercase) }
function SHA1DigestToHex(const Digest: TSHA1Digest): string;

{ Convert hex string to digest }
function SHA1HexToDigest(const Hex: string; var Digest: TSHA1Digest): Boolean;

{ Convert digest to base32 string }
function SHA1DigestToBase32(const Digest: TSHA1Digest): string;

{ Compare two digests }
function SHA1Equal(const A, B: TSHA1Digest): Boolean;

{ Check if digest is all zeros (empty) }
function SHA1IsEmpty(const Digest: TSHA1Digest): Boolean;

{ Clear digest to zeros }
procedure SHA1Clear(var Digest: TSHA1Digest);

{ Copy digest }
procedure SHA1Copy(const Source: TSHA1Digest; var Dest: TSHA1Digest);

{ ============================================================================ }
{ BitTorrent Specific Utilities                                               }
{ ============================================================================ }

{ Compute info-hash from torrent file data }
{ Extracts the 'info' dictionary and hashes it }
function ComputeInfoHash(const TorrentData: PChar; Len: Integer; 
                         var Digest: TSHA1Digest): Boolean;

{ Verify a piece against its expected hash }
function VerifyPiece(const Data: Pointer; Len: Integer; 
                     const ExpectedHash: TSHA1Digest): Boolean;

implementation

{ ============================================================================ }
{ One-Shot Hashing                                                            }
{ ============================================================================ }

function SHA1Buffer(const Buffer; Len: Cardinal): TSHA1Digest;
var
  Context: TSHA1Context;
begin
  SHA1Init(Context);
  SHA1Update(Context, Buffer, Len);
  SHA1Final(Context, Result);
end;

function SHA1String(const S: string): TSHA1Digest;
begin
  if Length(S) = 0 then
    Result := SHA1Buffer(PChar('')^, 0)
  else
    Result := SHA1Buffer(PChar(S)^, Length(S));
end;

function SHA1File(const Filename: string): TSHA1Digest;
begin
  Result := SHA1FileProgress(Filename, nil, 65536);
end;

function SHA1FileProgress(const Filename: string; 
                          ProgressCallback: TProgressCallback;
                          ChunkSize: Cardinal): TSHA1Digest;
var
  F: File;
  Context: TSHA1Context;
  Buffer: Pointer;
  BytesRead: LongInt;
  FileSize, TotalRead: Int64;
  LastPercent, CurrentPercent: Integer;
begin
  SHA1Clear(Result);
  
  { Open file }
  Assign(F, Filename);
  {$I-}
  Reset(F, 1);
  {$I+}
  
  if IOResult <> 0 then
    Exit;
  
  FileSize := System.FileSize(F);
  if FileSize <= 0 then
  begin
    Close(F);
    SHA1Clear(Result);
    Exit;
  end;
  
  { Allocate buffer }
  GetMem(Buffer, ChunkSize);
  
  { Initialize hash }
  SHA1Init(Context);
  
  TotalRead := 0;
  LastPercent := -1;
  
  { Read and hash in chunks }
  repeat
    BlockRead(F, Buffer^, ChunkSize, BytesRead);
    if BytesRead > 0 then
    begin
      SHA1Update(Context, Buffer^, BytesRead);
      Inc(TotalRead, BytesRead);
      
      { Report progress }
      if (ProgressCallback <> nil) and (FileSize > 0) then
      begin
        CurrentPercent := (TotalRead * 100) div FileSize;
        if CurrentPercent <> LastPercent then
        begin
          ProgressCallback(CurrentPercent);
          LastPercent := CurrentPercent;
        end;
      end;
    end;
  until BytesRead < ChunkSize;
  
  Close(F);
  FreeMem(Buffer);
  
  SHA1Final(Context, Result);
  
  { Final progress update }
  if ProgressCallback <> nil then
    ProgressCallback(100);
end;

{ ============================================================================ }
{ Incremental Hashing                                                         }
{ ============================================================================ }

procedure SHA1Init(var Context: TSHA1Context);
begin
  sha1.SHA1Init(Context);
end;

procedure SHA1Update(var Context: TSHA1Context; const Buffer; Len: Cardinal);
begin
  sha1.SHA1Update(Context, Buffer, Len);
end;

procedure SHA1Final(var Context: TSHA1Context; var Digest: TSHA1Digest);
begin
  sha1.SHA1Final(Context, Digest);
end;

{ ============================================================================ }
{ Digest Utilities                                                            }
{ ============================================================================ }

function SHA1DigestToHex(const Digest: TSHA1Digest): string;
const
  HexChars: array[0..15] of Char = '0123456789abcdef';
var
  I: Integer;
begin
  SetLength(Result, 40);
  for I := 0 to 19 do
  begin
    Result[I * 2 + 1] := HexChars[Digest[I] shr 4];
    Result[I * 2 + 2] := HexChars[Digest[I] and $0F];
  end;
end;

function SHA1HexToDigest(const Hex: string; var Digest: TSHA1Digest): Boolean;

  function HexCharToByte(C: Char; var B: Byte): Boolean;
  begin
    Result := True;
    case C of
      '0'..'9': B := Ord(C) - Ord('0');
      'a'..'f': B := Ord(C) - Ord('a') + 10;
      'A'..'F': B := Ord(C) - Ord('A') + 10;
    else
      Result := False;
    end;
  end;

var
  I: Integer;
  HighNib, LowNib: Byte;
begin
  Result := False;
  
  if Length(Hex) <> 40 then Exit;
  
  for I := 0 to 19 do
  begin
    if not HexCharToByte(Hex[I * 2 + 1], HighNib) then Exit;
    if not HexCharToByte(Hex[I * 2 + 2], LowNib) then Exit;
    Digest[I] := (HighNib shl 4) or LowNib;
  end;
  
  Result := True;
end;

function SHA1DigestToBase32(const Digest: TSHA1Digest): string;
const
  Base32Chars = 'abcdefghijklmnopqrstuvwxyz234567';
var
  I, Bits: Integer;
  Val: Cardinal;
  OutputLen: Integer;
  J: Integer;
begin
  { 20 bytes = 160 bits = 32 base32 chars }
  OutputLen := 32;
  SetLength(Result, OutputLen);
  
  Val := 0;
  Bits := 0;
  J := 1;
  
  for I := 0 to 19 do
  begin
    Val := (Val shl 8) or Digest[I];
    Inc(Bits, 8);
    
    while Bits >= 5 do
    begin
      Result[J] := Base32Chars[((Val shr (Bits - 5)) and $1F) + 1];
      Inc(J);
      Dec(Bits, 5);
      Val := Val and ((1 shl Bits) - 1);
    end;
  end;
  
  { Handle remaining bits (shouldn't happen with 160 bits) }
  if Bits > 0 then
  begin
    Val := Val shl (5 - Bits);
    Result[J] := Base32Chars[(Val and $1F) + 1];
  end;
end;

function SHA1Equal(const A, B: TSHA1Digest): Boolean;
var
  I: Integer;
begin
  Result := True;
  for I := 0 to 19 do
    if A[I] <> B[I] then
    begin
      Result := False;
      Exit;
    end;
end;

function SHA1IsEmpty(const Digest: TSHA1Digest): Boolean;
var
  I: Integer;
begin
  Result := True;
  for I := 0 to 19 do
    if Digest[I] <> 0 then
    begin
      Result := False;
      Exit;
    end;
end;

procedure SHA1Clear(var Digest: TSHA1Digest);
var
  I: Integer;
begin
  for I := 0 to 19 do
    Digest[I] := 0;
end;

procedure SHA1Copy(const Source: TSHA1Digest; var Dest: TSHA1Digest);
var
  I: Integer;
begin
  for I := 0 to 19 do
    Dest[I] := Source[I];
end;

{ ============================================================================ }
{ BitTorrent Specific Utilities                                               }
{ ============================================================================ }

function ComputeInfoHash(const TorrentData: PChar; Len: Integer; 
                         var Digest: TSHA1Digest): Boolean;
{ Simplified implementation - just finds "4:info" and hashes until matching 'e' }
var
  Pos: Integer;
  Depth: Integer;
  InString: Boolean;
  StringLen: Int64;
  I: Integer;

  function ParseInt(const Data: PChar; DataLen: Integer; 
                    var Value: Int64; var Chars: Integer): Boolean;
  var
    J: Integer;
    Neg: Boolean;
  begin
    Result := False;
    Value := 0;
    Chars := 0;
    
    if DataLen = 0 then Exit;
    
    J := 0;
    Neg := False;
    
    if Data[0] = '-' then
    begin
      Neg := True;
      J := 1;
    end;
    
    while J < DataLen do
    begin
      if (Data[J] < '0') or (Data[J] > '9') then Break;
      Value := Value * 10 + (Ord(Data[J]) - Ord('0'));
      Inc(J);
    end;
    
    if Neg then Value := -Value;
    Chars := J;
    Result := J > 0;
  end;
  
begin
  Result := False;
  SHA1Clear(Digest);
  
  if (TorrentData = nil) or (Len <= 0) then Exit;
  
  { Find the 'info' key }
  Pos := 0;
  while Pos < Len - 6 do
  begin
    if (TorrentData[Pos] = '4') and 
       (TorrentData[Pos + 1] = ':') and
       (TorrentData[Pos + 2] = 'i') and
       (TorrentData[Pos + 3] = 'n') and
       (TorrentData[Pos + 4] = 'f') and
       (TorrentData[Pos + 5] = 'o') then
    begin
      Pos := Pos + 6;
      
      if Pos >= Len then Exit;
      
      { Find the end of the value by tracking nesting }
      Depth := 0;
      InString := False;
      I := Pos;
      
      while I < Len do
      begin
        if InString then
        begin
          if I >= Pos + StringLen then
            InString := False;
        end
        else
        begin
          case TorrentData[I] of
            'd', 'l':
              Inc(Depth);
            'e':
              begin
                Dec(Depth);
                if Depth = 0 then
                begin
                  Digest := SHA1Buffer(TorrentData[Pos], I - Pos + 1);
                  Result := True;
                  Exit;
                end;
              end;
            'i':
              begin
                Inc(I);
                while (I < Len) and (TorrentData[I] <> 'e') do
                  Inc(I);
                if I >= Len then Exit;
              end;
            '0'..'9':
              begin
                StringLen := 0;
                while (I < Len) and (TorrentData[I] >= '0') and (TorrentData[I] <= '9') do
                begin
                  StringLen := StringLen * 10 + (Ord(TorrentData[I]) - Ord('0'));
                  Inc(I);
                end;
                if (I >= Len) or (TorrentData[I] <> ':') then Exit;
                Inc(I);  { Skip ':' }
                InString := True;
                I := I + StringLen - 1;
                if I >= Len then Exit;
                InString := False;
              end;
          end;
        end;
        Inc(I);
      end;
    end;
    Inc(Pos);
  end;
end;

function VerifyPiece(const Data: Pointer; Len: Integer; 
                     const ExpectedHash: TSHA1Digest): Boolean;
var
  ActualHash: TSHA1Digest;
begin
  if (Data = nil) or (Len <= 0) then
  begin
    Result := False;
    Exit;
  end;
  
  ActualHash := SHA1Buffer(Data^, Len);
  Result := SHA1Equal(ActualHash, ExpectedHash);
end;

end.
