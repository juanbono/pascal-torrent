{
  bencode.pas - Bencoding implementation for BitTorrent
  
  This unit provides encoding and decoding of bencoded data as used in
  the BitTorrent protocol (BEP 3). Bencoding supports:
  - Byte strings
  - Integers
  - Lists
  - Dictionaries
  
  Uses only imperative/procedural programming - no OOP.
}

unit bencode;

{$mode objfpc}{$H+}

interface

uses
  SysUtils;

const
  { Maximum file size for .torrent files (10MB) }
  MAX_TORRENT_FILE_SIZE = 10 * 1024 * 1024;

type
  { Forward declarations }
  PBencodeValue = ^TBencodeValue;
  
  { Types of bencoded values }
  TBencodeType = (btString, btInteger, btList, btDict);
  
  { Linked list node for dictionary entries (key-value pairs) }
  PBencodeDictEntry = ^TBencodeDictEntry;
  TBencodeDictEntry = record
    Key: PChar;
    KeyLen: Integer;
    Value: PBencodeValue;
    Next: PBencodeDictEntry;
  end;
  
  { Linked list node for list entries }
  PBencodeListEntry = ^TBencodeListEntry;
  TBencodeListEntry = record
    Value: PBencodeValue;
    Next: PBencodeListEntry;
  end;
  
  { Bencode value structure }
  TBencodeValue = record
    ValueType: TBencodeType;
    case Tag: Byte of
      0: (StrVal: PChar;      { String value }
          StrLen: Integer);
      1: (IntVal: Int64);     { Integer value }
      2: (ListHead: PBencodeListEntry);  { List head pointer }
      3: (DictHead: PBencodeDictEntry);  { Dictionary head pointer }
  end;
  
  { Parser state }
  TParseResult = record
    Success: Boolean;
    BytesConsumed: Integer;
    ErrorMsg: string;
  end;

{ ============================================================================ }
{ Decoding Functions                                                           }
{ ============================================================================ }

{ Decode bencoded data from a buffer. Returns nil on error. }
function BencodeDecode(const Data: PChar; Len: Integer; 
                       var Value: PBencodeValue): TParseResult;

{ Decode from a string }
function BencodeDecodeString(const S: string; var Value: PBencodeValue): TParseResult;

{ Decode from a file }
function BencodeDecodeFile(const Filename: string; 
                           var Value: PBencodeValue): TParseResult;

{ ============================================================================ }
{ Encoding Functions                                                           }
{ ============================================================================ }

{ Encode a bencode value to a newly allocated buffer. Caller must free buffer. }
function BencodeEncode(Value: PBencodeValue; var Buffer: PChar; 
                       var BufLen: Integer): Boolean;

{ Encode to a string }
function BencodeEncodeString(Value: PBencodeValue; var S: string): Boolean;

{ Calculate the size needed for encoding (without actually encoding) }
function BencodeCalcSize(Value: PBencodeValue; var Size: Integer): Boolean;

{ ============================================================================ }
{ Memory Management                                                            }
{ ============================================================================ }

{ Free a bencode value and all its children }
procedure BencodeFree(Value: PBencodeValue);

{ Create a new string value }
function BencodeNewString(const S: string): PBencodeValue;
function BencodeNewStringBuf(Buf: PChar; Len: Integer): PBencodeValue;

{ Create a new integer value }
function BencodeNewInteger(V: Int64): PBencodeValue;

{ Create a new empty list }
function BencodeNewList: PBencodeValue;

{ Create a new empty dictionary }
function BencodeNewDict: PBencodeValue;

{ ============================================================================ }
{ List Operations                                                              }
{ ============================================================================ }

{ Add a value to the end of a list }
function BencodeListAdd(List: PBencodeValue; Value: PBencodeValue): Boolean;

{ Get the number of elements in a list }
function BencodeListCount(List: PBencodeValue): Integer;

{ Get an element by index (0-based) }
function BencodeListGet(List: PBencodeValue; Index: Integer): PBencodeValue;

{ ============================================================================ }
{ Dictionary Operations                                                        }
{ ============================================================================ }

{ Add a key-value pair to a dictionary }
function BencodeDictAdd(Dict: PBencodeValue; const Key: string; 
                        Value: PBencodeValue): Boolean;

{ Look up a value by key }
function BencodeDictGet(Dict: PBencodeValue; const Key: string): PBencodeValue;

{ Check if a key exists }
function BencodeDictHasKey(Dict: PBencodeValue; const Key: string): Boolean;

{ Get the number of key-value pairs }
function BencodeDictCount(Dict: PBencodeValue): Integer;

{ Get a string value from dictionary }
function BencodeDictGetStr(Dict: PBencodeValue; const Key: string; 
                           var Value: string): Boolean;

{ Get an integer value from dictionary }
function BencodeDictGetInt(Dict: PBencodeValue; const Key: string; 
                           var Value: Int64): Boolean;

{ Get a list value from dictionary }
function BencodeDictGetList(Dict: PBencodeValue; const Key: string; 
                            var Value: PBencodeValue): Boolean;

{ Get a dict value from dictionary }
function BencodeDictGetDict(Dict: PBencodeValue; const Key: string; 
                            var Value: PBencodeValue): Boolean;

{ ============================================================================ }
{ Utility Functions                                                            }
{ ============================================================================ }

{ Compare two bencode values for equality }
function BencodeEqual(A, B: PBencodeValue): Boolean;

{ Clone a bencode value (deep copy) }
function BencodeClone(Value: PBencodeValue): PBencodeValue;

{ Print a bencode value for debugging (allocates string) }
function BencodeToDebugString(Value: PBencodeValue): string;

implementation

{ ============================================================================ }
{ Helper Functions                                                             }
{ ============================================================================ }

{ Find the position of a character in a buffer }
function FindChar(const Data: PChar; Len: Integer; C: Char; 
                  StartPos: Integer = 0): Integer;
var
  I: Integer;
begin
  for I := StartPos to Len - 1 do
  begin
    if Data[I] = C then
    begin
      Result := I;
      Exit;
    end;
  end;
  Result := -1;
end;

{ Compare two strings (case-sensitive) }
function StrEqual(const A: PChar; ALen: Integer; const B: string): Boolean; overload; forward;
function StrEqualImpl(const A: PChar; ALen: Integer; const B: string): Boolean;
var
  I: Integer;
begin
  if ALen <> Length(B) then
  begin
    Result := False;
    Exit;
  end;
  
  for I := 0 to ALen - 1 do
  begin
    if A[I] <> B[I + 1] then
    begin
      Result := False;
      Exit;
    end;
  end;
  Result := True;
end;

function StrEqual(const A: PChar; ALen: Integer; const B: string): Boolean; overload;
begin
  Result := StrEqualImpl(A, ALen, B);
end;

function StrEqual(const A: PChar; ALen: Integer; const B: PChar; BLen: Integer): Boolean;
var
  I: Integer;
begin
  if ALen <> BLen then
  begin
    Result := False;
    Exit;
  end;
  
  for I := 0 to ALen - 1 do
  begin
    if A[I] <> B[I] then
    begin
      Result := False;
      Exit;
    end;
  end;
  Result := True;
end;

{ Parse an integer from a string buffer }
function ParseInteger(const Data: PChar; Len: Integer; 
                      var Value: Int64; var CharsRead: Integer): Boolean;
var
  I: Integer;
  Negative: Boolean;
  Digit: Int64;
begin
  Result := False;
  Value := 0;
  CharsRead := 0;
  
  if Len = 0 then Exit;
  
  I := 0;
  Negative := False;
  
  if Data[0] = '-' then
  begin
    Negative := True;
    I := 1;
    if Len = 1 then Exit;  { Just "-" is invalid }
  end;
  
  { Check for leading zeros (except "0" itself) }
  if (I < Len) and (Data[I] = '0') then
  begin
    if (I + 1 < Len) and (Data[I + 1] >= '0') and (Data[I + 1] <= '9') then
      Exit;  { Leading zero not allowed }
  end;
  
  while I < Len do
  begin
    if (Data[I] < '0') or (Data[I] > '9') then
      Break;
    
    Digit := Ord(Data[I]) - Ord('0');
    
    { Check for overflow before multiplication }
    if Value > (High(Int64) - Digit) div 10 then
    begin
      { Integer overflow }
      Result := False;
      Exit;
    end;
    
    Value := Value * 10 + Digit;
    Inc(I);
  end;
  
  CharsRead := I;
  
  if Negative then
    Value := -Value;
  
  Result := True;
end;

{ ============================================================================ }
{ Forward Declarations for Recursive Functions                                 }
{ ============================================================================ }

function DecodeValue(const Data: PChar; Len: Integer; 
                     var Value: PBencodeValue; var BytesConsumed: Integer;
                     out ErrorMsg: string): Boolean; forward;

function EncodeValue(Value: PBencodeValue; Buffer: PChar; 
                     BufSize: Integer; out BytesWritten: Integer): Boolean; forward;

function CalcValueSize(Value: PBencodeValue; var Size: Integer): Boolean; forward;

function ValuesEqual(A, B: PBencodeValue): Boolean; forward;

function CloneValue(Value: PBencodeValue): PBencodeValue; forward;

procedure ValueToDebug(Value: PBencodeValue; var S: string; Indent: Integer); forward;

{ ============================================================================ }
{ Decoding Implementation                                                      }
{ ============================================================================ }

function DecodeString(const Data: PChar; Len: Integer; 
                      var Value: PBencodeValue; var BytesConsumed: Integer;
                      out ErrorMsg: string): Boolean;
var
  ColonPos: Integer = -1;
  StrLen: Int64 = 0;
  CharsRead: Integer = 0;
  NewValue: PBencodeValue = nil;
begin
  Result := False;
  BytesConsumed := 0;
  ErrorMsg := '';
  Value := nil;
  
  { Find colon }
  ColonPos := FindChar(Data, Len, ':');
  if ColonPos < 0 then
  begin
    ErrorMsg := 'String: missing colon';
    Exit;
  end;
  
  { Parse length }
  if not ParseInteger(Data, ColonPos, StrLen, CharsRead) then
  begin
    ErrorMsg := 'String: invalid length';
    Exit;
  end;
  
  if CharsRead <> ColonPos then
  begin
    ErrorMsg := 'String: invalid length format';
    Exit;
  end;
  
  if StrLen < 0 then
  begin
    ErrorMsg := 'String: negative length';
    Exit;
  end;
  
  { Check we have enough data - use Int64 to prevent overflow }
  if (Int64(ColonPos) + 1 + StrLen > Len) then
  begin
    ErrorMsg := 'String: not enough data';
    Exit;
  end;
  
  { Allocate and fill }
  New(NewValue);
  if NewValue = nil then
  begin
    ErrorMsg := 'String: memory allocation failed';
    Exit;
  end;
  
  NewValue^.ValueType := btString;
  NewValue^.StrLen := StrLen;
  
  if StrLen > 0 then
  begin
    GetMem(NewValue^.StrVal, StrLen);
    if NewValue^.StrVal = nil then
    begin
      Dispose(NewValue);
      ErrorMsg := 'String: memory allocation failed';
      Exit;
    end;
    Move(Data[ColonPos + 1], NewValue^.StrVal^, StrLen);
  end
  else
  begin
    NewValue^.StrVal := nil;
  end;
  
  Value := NewValue;
  BytesConsumed := ColonPos + 1 + StrLen;
  Result := True;
end;

function DecodeInteger(const Data: PChar; Len: Integer; 
                       var Value: PBencodeValue; var BytesConsumed: Integer;
                       out ErrorMsg: string): Boolean;
var
  IntVal: Int64 = 0;
  CharsRead: Integer = 0;
  NewValue: PBencodeValue = nil;
  I: Integer = 0;
begin
  Result := False;
  BytesConsumed := 0;
  ErrorMsg := '';
  Value := nil;
  
  { Must start with 'i' }
  if (Len < 1) or (Data[0] <> 'i') then
  begin
    ErrorMsg := 'Integer: must start with i';
    Exit;
  end;
  
  { Find 'e' }
  I := 1;
  while (I < Len) and (Data[I] <> 'e') do
    Inc(I);
  
  if I >= Len then
  begin
    ErrorMsg := 'Integer: missing terminating e';
    Exit;
  end;
  
  { Check for "i-0e" which is invalid }
  if (I >= 2) and (Data[1] = '-') and (Data[2] = '0') then
  begin
    ErrorMsg := 'Integer: i-0e is invalid';
    Exit;
  end;
  
  { Parse the integer }
  if not ParseInteger(Data + 1, I - 1, IntVal, CharsRead) then
  begin
    ErrorMsg := 'Integer: invalid format';
    Exit;
  end;
  
  if CharsRead <> I - 1 then
  begin
    ErrorMsg := 'Integer: extra characters';
    Exit;
  end;
  
  New(NewValue);
  NewValue^.ValueType := btInteger;
  NewValue^.IntVal := IntVal;
  
  Value := NewValue;
  BytesConsumed := I + 1;  { Include the 'e' }
  Result := True;
end;

function DecodeList(const Data: PChar; Len: Integer; 
                    var Value: PBencodeValue; var BytesConsumed: Integer;
                    out ErrorMsg: string): Boolean;
var
  Pos: Integer = 0;
  SubValue: PBencodeValue = nil;
  SubConsumed: Integer = 0;
  LastEntry: PBencodeListEntry = nil;
  NewEntry: PBencodeListEntry = nil;
  NewValue: PBencodeValue = nil;
begin
  Result := False;
  BytesConsumed := 0;
  ErrorMsg := '';
  Value := nil;
  
  { Must start with 'l' }
  if (Len < 1) or (Data[0] <> 'l') then
  begin
    ErrorMsg := 'List: must start with l';
    Exit;
  end;
  
  New(NewValue);
  NewValue^.ValueType := btList;
  NewValue^.ListHead := nil;
  LastEntry := nil;
  
  Pos := 1;
  while (Pos < Len) and (Data[Pos] <> 'e') do
  begin
    if not DecodeValue(Data + Pos, Len - Pos, SubValue, SubConsumed, ErrorMsg) then
    begin
      BencodeFree(NewValue);
      Exit;
    end;
    
    { Create list entry }
    New(NewEntry);
    if NewEntry = nil then
    begin
      ErrorMsg := 'List: memory allocation failed';
      BencodeFree(SubValue);
      BencodeFree(NewValue);
      Exit;
    end;
    NewEntry^.Value := SubValue;
    NewEntry^.Next := nil;
    
    if LastEntry = nil then
      NewValue^.ListHead := NewEntry
    else
      LastEntry^.Next := NewEntry;
    LastEntry := NewEntry;
    
    Inc(Pos, SubConsumed);
  end;
  
  if Pos >= Len then
  begin
    ErrorMsg := 'List: missing terminating e';
    BencodeFree(NewValue);
    Exit;
  end;
  
  Value := NewValue;
  BytesConsumed := Pos + 1;  { Include the 'e' }
  Result := True;
end;

function DecodeDict(const Data: PChar; Len: Integer; 
                    var Value: PBencodeValue; var BytesConsumed: Integer;
                    out ErrorMsg: string): Boolean;
var
  Pos: Integer = 0;
  KeyValue: PBencodeValue = nil;
  ValValue: PBencodeValue = nil;
  SubConsumed: Integer = 0;
  LastEntry: PBencodeDictEntry = nil;
  NewEntry: PBencodeDictEntry = nil;
  NewValue: PBencodeValue = nil;
begin
  Result := False;
  BytesConsumed := 0;
  ErrorMsg := '';
  Value := nil;
  
  { Must start with 'd' }
  if (Len < 1) or (Data[0] <> 'd') then
  begin
    ErrorMsg := 'Dict: must start with d';
    Exit;
  end;
  
  New(NewValue);
  NewValue^.ValueType := btDict;
  NewValue^.DictHead := nil;
  LastEntry := nil;
  
  Pos := 1;
  while (Pos < Len) and (Data[Pos] <> 'e') do
  begin
    { Decode key (must be a string) }
    if not DecodeValue(Data + Pos, Len - Pos, KeyValue, SubConsumed, ErrorMsg) then
    begin
      BencodeFree(NewValue);
      Exit;
    end;
    
    if KeyValue^.ValueType <> btString then
    begin
      ErrorMsg := 'Dict: key must be a string';
      BencodeFree(KeyValue);
      BencodeFree(NewValue);
      Exit;
    end;
    
    Inc(Pos, SubConsumed);
    
    if Pos >= Len then
    begin
      ErrorMsg := 'Dict: missing value for key';
      BencodeFree(KeyValue);
      BencodeFree(NewValue);
      Exit;
    end;
    
    { Decode value }
    if not DecodeValue(Data + Pos, Len - Pos, ValValue, SubConsumed, ErrorMsg) then
    begin
      BencodeFree(KeyValue);
      BencodeFree(NewValue);
      Exit;
    end;
    
    Inc(Pos, SubConsumed);
    
    { Create dict entry }
    New(NewEntry);
    if NewEntry = nil then
    begin
      ErrorMsg := 'Dict: memory allocation failed';
      BencodeFree(KeyValue);
      BencodeFree(ValValue);
      BencodeFree(NewValue);
      Exit;
    end;
    
    if KeyValue^.StrLen > 0 then
    begin
      GetMem(NewEntry^.Key, KeyValue^.StrLen);
      if NewEntry^.Key = nil then
      begin
        ErrorMsg := 'Dict: memory allocation failed';
        Dispose(NewEntry);
        BencodeFree(KeyValue);
        BencodeFree(ValValue);
        BencodeFree(NewValue);
        Exit;
      end;
      Move(KeyValue^.StrVal^, NewEntry^.Key^, KeyValue^.StrLen);
      NewEntry^.KeyLen := KeyValue^.StrLen;
    end
    else
    begin
      NewEntry^.Key := nil;
      NewEntry^.KeyLen := 0;
    end;
    NewEntry^.Value := ValValue;
    NewEntry^.Next := nil;
    
    if LastEntry = nil then
      NewValue^.DictHead := NewEntry
    else
      LastEntry^.Next := NewEntry;
    LastEntry := NewEntry;
    
    BencodeFree(KeyValue);  { Free the key value but keep the copied key }
  end;
  
  if Pos >= Len then
  begin
    ErrorMsg := 'Dict: missing terminating e';
    BencodeFree(NewValue);
    Exit;
  end;
  
  Value := NewValue;
  BytesConsumed := Pos + 1;  { Include the 'e' }
  Result := True;
end;

function DecodeValue(const Data: PChar; Len: Integer; 
                     var Value: PBencodeValue; var BytesConsumed: Integer;
                     out ErrorMsg: string): Boolean;
begin
  Result := False;
  BytesConsumed := 0;
  ErrorMsg := '';
  Value := nil;
  
  if Len <= 0 then
  begin
    ErrorMsg := 'Empty data';
    Exit;
  end;
  
  case Data[0] of
    '0'..'9': Result := DecodeString(Data, Len, Value, BytesConsumed, ErrorMsg);
    'i':      Result := DecodeInteger(Data, Len, Value, BytesConsumed, ErrorMsg);
    'l':      Result := DecodeList(Data, Len, Value, BytesConsumed, ErrorMsg);
    'd':      Result := DecodeDict(Data, Len, Value, BytesConsumed, ErrorMsg);
  else
    ErrorMsg := 'Unknown type: ' + Data[0];
  end;
end;

function BencodeDecode(const Data: PChar; Len: Integer; 
                       var Value: PBencodeValue): TParseResult;
var
  ErrorMsg: string;
begin
  { Initialize result }
  Result.Success := False;
  Result.ErrorMsg := '';
  Result.BytesConsumed := 0;
  
  Result.Success := DecodeValue(Data, Len, Value, Result.BytesConsumed, ErrorMsg);
  Result.ErrorMsg := ErrorMsg;
  
  if Result.Success then
  begin
    { Check for trailing data }
    if Result.BytesConsumed < Len then
    begin
      { This is a warning, not necessarily an error }
      { But we'll mark it as error for strictness }
      Result.Success := False;
      Result.ErrorMsg := 'Trailing data after valid bencode';
      BencodeFree(Value);
      Value := nil;
    end;
  end;
end;

function BencodeDecodeString(const S: string; var Value: PBencodeValue): TParseResult;
begin
  if Length(S) = 0 then
  begin
    Result.Success := False;
    Result.BytesConsumed := 0;
    Result.ErrorMsg := 'Empty string';
    Value := nil;
    Exit;
  end;
  Result := BencodeDecode(PChar(S), Length(S), Value);
end;

function BencodeDecodeFile(const Filename: string; 
                           var Value: PBencodeValue): TParseResult;
var
  F: File;
  Buffer: PChar;
  FileSize: LongInt;
  BytesRead: LongInt;
const
  MAX_FILE_SIZE = 10 * 1024 * 1024;  { 10MB limit for .torrent files }
begin
  Result.Success := False;
  Result.BytesConsumed := 0;
  Result.ErrorMsg := '';
  Value := nil;
  
  Assign(F, Filename);
  {$I-}
  Reset(F, 1);
  {$I+}
  
  if IOResult <> 0 then
  begin
    Result.ErrorMsg := 'Cannot open file: ' + Filename;
    Exit;
  end;
  
  FileSize := System.FileSize(F);
  if FileSize <= 0 then
  begin
    Result.ErrorMsg := 'Empty file: ' + Filename;
    Close(F);
    Exit;
  end;
  
  if FileSize > MAX_FILE_SIZE then
  begin
    Result.ErrorMsg := 'File too large: ' + Filename;
    Close(F);
    Exit;
  end;
  
  GetMem(Buffer, FileSize);
  if Buffer = nil then
  begin
    Result.ErrorMsg := 'Memory allocation failed';
    Close(F);
    Exit;
  end;
  
  BlockRead(F, Buffer^, FileSize, BytesRead);
  Close(F);
  
  if BytesRead <> FileSize then
  begin
    Result.ErrorMsg := 'Failed to read entire file';
    FreeMem(Buffer);
    Exit;
  end;
  
  Result := BencodeDecode(Buffer, FileSize, Value);
  FreeMem(Buffer);
end;

{ ============================================================================ }
{ Encoding Implementation                                                      }
{ ============================================================================ }

function EncodeString(Value: PBencodeValue; Buffer: PChar; 
                      BufSize: Integer; out BytesWritten: Integer): Boolean;
var
  LenStr: string;
  TotalLen: Integer;
  I: Integer;
begin
  Result := False;
  BytesWritten := 0;
  
  LenStr := IntToStr(Value^.StrLen);
  TotalLen := Length(LenStr) + 1 + Value^.StrLen;  { len + ':' + data }
  
  if BufSize < TotalLen then Exit;
  
  { Write length }
  for I := 1 to Length(LenStr) do
    Buffer[BytesWritten + I - 1] := LenStr[I];
  Inc(BytesWritten, Length(LenStr));
  
  { Write colon }
  Buffer[BytesWritten] := ':';
  Inc(BytesWritten);
  
  { Write string data }
  if Value^.StrLen > 0 then
  begin
    Move(Value^.StrVal^, Buffer[BytesWritten], Value^.StrLen);
    Inc(BytesWritten, Value^.StrLen);
  end;
  
  Result := True;
end;

function EncodeInteger(Value: PBencodeValue; Buffer: PChar; 
                       BufSize: Integer; out BytesWritten: Integer): Boolean;
var
  IntStr: string;
  TotalLen: Integer;
  I: Integer;
begin
  Result := False;
  BytesWritten := 0;
  
  IntStr := IntToStr(Value^.IntVal);
  TotalLen := 1 + Length(IntStr) + 1;  { 'i' + number + 'e' }
  
  if BufSize < TotalLen then Exit;
  
  Buffer[0] := 'i';
  BytesWritten := 1;
  
  for I := 1 to Length(IntStr) do
  begin
    Buffer[BytesWritten] := IntStr[I];
    Inc(BytesWritten);
  end;
  
  Buffer[BytesWritten] := 'e';
  Inc(BytesWritten);
  
  Result := True;
end;

function EncodeList(Value: PBencodeValue; Buffer: PChar; 
                    BufSize: Integer; out BytesWritten: Integer): Boolean;
var
  Entry: PBencodeListEntry;
  SubWritten: Integer;
begin
  Result := False;
  BytesWritten := 0;
  
  if BufSize < 1 then Exit;
  
  Buffer[0] := 'l';
  BytesWritten := 1;
  
  Entry := Value^.ListHead;
  while Entry <> nil do
  begin
    if not EncodeValue(Entry^.Value, Buffer + BytesWritten, 
                       BufSize - BytesWritten, SubWritten) then
      Exit;
    Inc(BytesWritten, SubWritten);
    Entry := Entry^.Next;
  end;
  
  if BufSize - BytesWritten < 1 then Exit;
  Buffer[BytesWritten] := 'e';
  Inc(BytesWritten);
  
  Result := True;
end;

function EncodeDict(Value: PBencodeValue; Buffer: PChar; 
                    BufSize: Integer; out BytesWritten: Integer): Boolean;
var
  Entry: PBencodeDictEntry;
  SubWritten: Integer;
  I: Integer;
  LenStr: string;
begin
  Result := False;
  BytesWritten := 0;
  
  if BufSize < 1 then Exit;
  
  Buffer[0] := 'd';
  BytesWritten := 1;
  
  Entry := Value^.DictHead;
  while Entry <> nil do
  begin
    { Encode key as string }
    LenStr := IntToStr(Entry^.KeyLen);
    if BufSize - BytesWritten < Length(LenStr) + 1 + Entry^.KeyLen then Exit;
    
    for I := 1 to Length(LenStr) do
      Buffer[BytesWritten + I - 1] := LenStr[I];
    Inc(BytesWritten, Length(LenStr));
    
    Buffer[BytesWritten] := ':';
    Inc(BytesWritten);
    
    if Entry^.KeyLen > 0 then
    begin
      Move(Entry^.Key^, Buffer[BytesWritten], Entry^.KeyLen);
      Inc(BytesWritten, Entry^.KeyLen);
    end;
    
    { Encode value }
    if not EncodeValue(Entry^.Value, Buffer + BytesWritten, 
                       BufSize - BytesWritten, SubWritten) then
      Exit;
    Inc(BytesWritten, SubWritten);
    
    Entry := Entry^.Next;
  end;
  
  if BufSize - BytesWritten < 1 then Exit;
  Buffer[BytesWritten] := 'e';
  Inc(BytesWritten);
  
  Result := True;
end;

function EncodeValue(Value: PBencodeValue; Buffer: PChar; 
                     BufSize: Integer; out BytesWritten: Integer): Boolean;
begin
  Result := False;
  BytesWritten := 0;
  
  if Value = nil then Exit;
  
  case Value^.ValueType of
    btString:  Result := EncodeString(Value, Buffer, BufSize, BytesWritten);
    btInteger: Result := EncodeInteger(Value, Buffer, BufSize, BytesWritten);
    btList:    Result := EncodeList(Value, Buffer, BufSize, BytesWritten);
    btDict:    Result := EncodeDict(Value, Buffer, BufSize, BytesWritten);
  end;
end;

function BencodeEncode(Value: PBencodeValue; var Buffer: PChar; 
                       var BufLen: Integer): Boolean;
var
  BytesWritten: Integer;
begin
  Result := False;
  Buffer := nil;
  BufLen := 0;
  
  if Value = nil then Exit;
  
  if not BencodeCalcSize(Value, BufLen) then Exit;
  
  GetMem(Buffer, BufLen);
  
  if EncodeValue(Value, Buffer, BufLen, BytesWritten) and (BytesWritten = BufLen) then
    Result := True
  else
  begin
    FreeMem(Buffer);
    Buffer := nil;
    BufLen := 0;
  end;
end;

function BencodeEncodeString(Value: PBencodeValue; var S: string): Boolean;
var
  Buffer: PChar;
  Len: Integer;
  I: Integer;
begin
  Result := False;
  S := '';
  
  if not BencodeEncode(Value, Buffer, Len) then Exit;
  
  SetLength(S, Len);
  for I := 1 to Len do
    S[I] := Buffer[I - 1];
  
  FreeMem(Buffer);
  Result := True;
end;

function CalcStringSize(Value: PBencodeValue; var Size: Integer): Boolean;
var
  LenStr: string;
begin
  LenStr := IntToStr(Value^.StrLen);
  Size := Length(LenStr) + 1 + Value^.StrLen;
  Result := True;
end;

function CalcIntegerSize(Value: PBencodeValue; var Size: Integer): Boolean;
var
  IntStr: string;
begin
  IntStr := IntToStr(Value^.IntVal);
  Size := 1 + Length(IntStr) + 1;  { 'i' + number + 'e' }
  Result := True;
end;

function CalcListSize(Value: PBencodeValue; var Size: Integer): Boolean;
var
  Entry: PBencodeListEntry;
  SubSize: Integer;
begin
  Size := 2;  { 'l' and 'e' }
  Entry := Value^.ListHead;
  while Entry <> nil do
  begin
    if not CalcValueSize(Entry^.Value, SubSize) then
    begin
      Result := False;
      Exit;
    end;
    Inc(Size, SubSize);
    Entry := Entry^.Next;
  end;
  Result := True;
end;

function CalcDictSize(Value: PBencodeValue; var Size: Integer): Boolean;
var
  Entry: PBencodeDictEntry;
  SubSize: Integer;
  LenStr: string;
begin
  Size := 2;  { 'd' and 'e' }
  Entry := Value^.DictHead;
  while Entry <> nil do
  begin
    { Key size }
    LenStr := IntToStr(Entry^.KeyLen);
    Inc(Size, Length(LenStr) + 1 + Entry^.KeyLen);
    
    { Value size }
    if not CalcValueSize(Entry^.Value, SubSize) then
    begin
      Result := False;
      Exit;
    end;
    Inc(Size, SubSize);
    
    Entry := Entry^.Next;
  end;
  Result := True;
end;

function CalcValueSize(Value: PBencodeValue; var Size: Integer): Boolean;
begin
  Result := False;
  Size := 0;
  
  if Value = nil then Exit;
  
  case Value^.ValueType of
    btString:  Result := CalcStringSize(Value, Size);
    btInteger: Result := CalcIntegerSize(Value, Size);
    btList:    Result := CalcListSize(Value, Size);
    btDict:    Result := CalcDictSize(Value, Size);
  end;
end;

function BencodeCalcSize(Value: PBencodeValue; var Size: Integer): Boolean;
begin
  Result := CalcValueSize(Value, Size);
end;

{ ============================================================================ }
{ Memory Management                                                            }
{ ============================================================================ }

procedure BencodeFree(Value: PBencodeValue);
var
  Entry, NextEntry: PBencodeListEntry;
  DictEntry, NextDictEntry: PBencodeDictEntry;
begin
  if Value = nil then Exit;
  
  case Value^.ValueType of
    btString:
      begin
        if Value^.StrVal <> nil then
          FreeMem(Value^.StrVal);
      end;
      
    btList:
      begin
        Entry := Value^.ListHead;
        while Entry <> nil do
        begin
          NextEntry := Entry^.Next;
          BencodeFree(Entry^.Value);
          Dispose(Entry);
          Entry := NextEntry;
        end;
      end;
      
    btDict:
      begin
        DictEntry := Value^.DictHead;
        while DictEntry <> nil do
        begin
          NextDictEntry := DictEntry^.Next;
          if DictEntry^.Key <> nil then
            FreeMem(DictEntry^.Key);
          BencodeFree(DictEntry^.Value);
          Dispose(DictEntry);
          DictEntry := NextDictEntry;
        end;
      end;
  end;
  
  Dispose(Value);
end;

function BencodeNewString(const S: string): PBencodeValue;
begin
  Result := BencodeNewStringBuf(PChar(S), Length(S));
end;

function BencodeNewStringBuf(Buf: PChar; Len: Integer): PBencodeValue;
begin
  Result := nil;
  
  New(Result);
  if Result = nil then Exit;  { Memory allocation failed }
  
  Result^.ValueType := btString;
  Result^.StrLen := Len;
  if Len > 0 then
  begin
    GetMem(Result^.StrVal, Len);
    if Result^.StrVal = nil then
    begin
      { String data allocation failed - clean up and return nil }
      Dispose(Result);
      Result := nil;
      Exit;
    end;
    Move(Buf^, Result^.StrVal^, Len);
  end
  else
  begin
    Result^.StrVal := nil;
  end;
end;

function BencodeNewInteger(V: Int64): PBencodeValue;
begin
  Result := nil;
  
  New(Result);
  if Result = nil then Exit;  { Memory allocation failed }
  
  Result^.ValueType := btInteger;
  Result^.IntVal := V;
end;

function BencodeNewList: PBencodeValue;
begin
  Result := nil;
  
  New(Result);
  if Result = nil then Exit;  { Memory allocation failed }
  
  Result^.ValueType := btList;
  Result^.ListHead := nil;
end;

function BencodeNewDict: PBencodeValue;
begin
  Result := nil;
  
  New(Result);
  if Result = nil then Exit;  { Memory allocation failed }
  
  Result^.ValueType := btDict;
  Result^.DictHead := nil;
end;

{ ============================================================================ }
{ List Operations                                                              }
{ ============================================================================ }

function BencodeListAdd(List: PBencodeValue; Value: PBencodeValue): Boolean;
var
  Entry, NewEntry: PBencodeListEntry;
begin
  Result := False;
  if (List = nil) or (List^.ValueType <> btList) or (Value = nil) then Exit;
  
  New(NewEntry);
  NewEntry^.Value := Value;
  NewEntry^.Next := nil;
  
  if List^.ListHead = nil then
  begin
    List^.ListHead := NewEntry;
  end
  else
  begin
    Entry := List^.ListHead;
    while Entry^.Next <> nil do
      Entry := Entry^.Next;
    Entry^.Next := NewEntry;
  end;
  
  Result := True;
end;

function BencodeListCount(List: PBencodeValue): Integer;
var
  Entry: PBencodeListEntry;
begin
  Result := 0;
  if (List = nil) or (List^.ValueType <> btList) then Exit;
  
  Entry := List^.ListHead;
  while Entry <> nil do
  begin
    Inc(Result);
    Entry := Entry^.Next;
  end;
end;

function BencodeListGet(List: PBencodeValue; Index: Integer): PBencodeValue;
var
  Entry: PBencodeListEntry;
  I: Integer;
begin
  Result := nil;
  if (List = nil) or (List^.ValueType <> btList) or (Index < 0) then Exit;
  
  Entry := List^.ListHead;
  I := 0;
  while Entry <> nil do
  begin
    if I = Index then
    begin
      Result := Entry^.Value;
      Exit;
    end;
    Inc(I);
    Entry := Entry^.Next;
  end;
end;

{ ============================================================================ }
{ Dictionary Operations                                                        }
{ ============================================================================ }

function BencodeDictAdd(Dict: PBencodeValue; const Key: string; 
                        Value: PBencodeValue): Boolean;
var
  Entry, NewEntry: PBencodeDictEntry;
  KeyLen: Integer;
begin
  Result := False;
  if (Dict = nil) or (Dict^.ValueType <> btDict) or (Value = nil) then Exit;
  
  KeyLen := Length(Key);
  
  New(NewEntry);
  if KeyLen > 0 then
  begin
    GetMem(NewEntry^.Key, KeyLen);
    Move(PChar(Key)^, NewEntry^.Key^, KeyLen);
  end
  else
  begin
    NewEntry^.Key := nil;
  end;
  NewEntry^.KeyLen := KeyLen;
  NewEntry^.Value := Value;
  NewEntry^.Next := nil;
  
  if Dict^.DictHead = nil then
  begin
    Dict^.DictHead := NewEntry;
  end
  else
  begin
    Entry := Dict^.DictHead;
    while Entry^.Next <> nil do
      Entry := Entry^.Next;
    Entry^.Next := NewEntry;
  end;
  
  Result := True;
end;

function BencodeDictGet(Dict: PBencodeValue; const Key: string): PBencodeValue;
var
  Entry: PBencodeDictEntry;
begin
  Result := nil;
  if (Dict = nil) or (Dict^.ValueType <> btDict) then Exit;
  
  Entry := Dict^.DictHead;
  while Entry <> nil do
  begin
    if StrEqual(Entry^.Key, Entry^.KeyLen, Key) then
    begin
      Result := Entry^.Value;
      Exit;
    end;
    Entry := Entry^.Next;
  end;
end;

function BencodeDictHasKey(Dict: PBencodeValue; const Key: string): Boolean;
begin
  Result := BencodeDictGet(Dict, Key) <> nil;
end;

function BencodeDictCount(Dict: PBencodeValue): Integer;
var
  Entry: PBencodeDictEntry;
begin
  Result := 0;
  if (Dict = nil) or (Dict^.ValueType <> btDict) then Exit;
  
  Entry := Dict^.DictHead;
  while Entry <> nil do
  begin
    Inc(Result);
    Entry := Entry^.Next;
  end;
end;

function BencodeDictGetStr(Dict: PBencodeValue; const Key: string; 
                           var Value: string): Boolean;
var
  V: PBencodeValue;
  I: Integer;
begin
  Result := False;
  Value := '';
  
  V := BencodeDictGet(Dict, Key);
  if (V = nil) or (V^.ValueType <> btString) then Exit;
  
  SetLength(Value, V^.StrLen);
  for I := 1 to V^.StrLen do
    Value[I] := V^.StrVal[I - 1];
  
  Result := True;
end;

function BencodeDictGetInt(Dict: PBencodeValue; const Key: string; 
                           var Value: Int64): Boolean;
var
  V: PBencodeValue;
begin
  Result := False;
  Value := 0;
  
  V := BencodeDictGet(Dict, Key);
  if (V = nil) or (V^.ValueType <> btInteger) then Exit;
  
  Value := V^.IntVal;
  Result := True;
end;

function BencodeDictGetList(Dict: PBencodeValue; const Key: string; 
                            var Value: PBencodeValue): Boolean;
begin
  Value := BencodeDictGet(Dict, Key);
  Result := (Value <> nil) and (Value^.ValueType = btList);
end;

function BencodeDictGetDict(Dict: PBencodeValue; const Key: string; 
                            var Value: PBencodeValue): Boolean;
begin
  Value := BencodeDictGet(Dict, Key);
  Result := (Value <> nil) and (Value^.ValueType = btDict);
end;

{ ============================================================================ }
{ Utility Functions                                                            }
{ ============================================================================ }

function ValuesEqual(A, B: PBencodeValue): Boolean;
var
  EntryA, EntryB: PBencodeListEntry;
  DictA, DictB: PBencodeDictEntry;
  Count: Integer;
begin
  Result := False;
  
  if A = B then
  begin
    Result := True;
    Exit;
  end;
  
  if (A = nil) or (B = nil) then Exit;
  if A^.ValueType <> B^.ValueType then Exit;
  
  case A^.ValueType of
    btString:
      begin
        if A^.StrLen <> B^.StrLen then Exit;
        if A^.StrLen > 0 then
        begin
          Result := CompareMem(A^.StrVal, B^.StrVal, A^.StrLen);
        end
        else
          Result := True;
      end;
      
    btInteger:
      Result := A^.IntVal = B^.IntVal;
      
    btList:
      begin
        Count := BencodeListCount(A);
        if Count <> BencodeListCount(B) then Exit;
        
        EntryA := A^.ListHead;
        EntryB := B^.ListHead;
        while EntryA <> nil do
        begin
          if not ValuesEqual(EntryA^.Value, EntryB^.Value) then Exit;
          EntryA := EntryA^.Next;
          EntryB := EntryB^.Next;
        end;
        Result := True;
      end;
      
    btDict:
      begin
        { This is a simplified check - doesn't verify same keys }
        { Full check would require sorting keys }
        if BencodeDictCount(A) <> BencodeDictCount(B) then Exit;
        
        DictA := A^.DictHead;
        while DictA <> nil do
        begin
          { Find matching key in B }
          DictB := B^.DictHead;
          while DictB <> nil do
          begin
            if StrEqual(DictA^.Key, DictA^.KeyLen, 
                       DictB^.Key, DictB^.KeyLen) then
              Break;
            DictB := DictB^.Next;
          end;
          
          if DictB = nil then Exit;  { Key not found }
          if not ValuesEqual(DictA^.Value, DictB^.Value) then Exit;
          
          DictA := DictA^.Next;
        end;
        Result := True;
      end;
  end;
end;

function BencodeEqual(A, B: PBencodeValue): Boolean;
begin
  Result := ValuesEqual(A, B);
end;

function CloneValue(Value: PBencodeValue): PBencodeValue;
var
  Entry: PBencodeListEntry;
  DictEntry: PBencodeDictEntry;
  KeyStr: string;
  I: Integer;
begin
  Result := nil;
  if Value = nil then Exit;
  
  case Value^.ValueType of
    btString:
      begin
        Result := BencodeNewStringBuf(Value^.StrVal, Value^.StrLen);
      end;
      
    btInteger:
      begin
        Result := BencodeNewInteger(Value^.IntVal);
      end;
      
    btList:
      begin
        Result := BencodeNewList;
        Entry := Value^.ListHead;
        while Entry <> nil do
        begin
          BencodeListAdd(Result, CloneValue(Entry^.Value));
          Entry := Entry^.Next;
        end;
      end;
      
    btDict:
      begin
        Result := BencodeNewDict;
        DictEntry := Value^.DictHead;
        while DictEntry <> nil do
        begin
          SetLength(KeyStr, DictEntry^.KeyLen);
          for I := 1 to DictEntry^.KeyLen do
            KeyStr[I] := DictEntry^.Key[I - 1];
          BencodeDictAdd(Result, KeyStr, CloneValue(DictEntry^.Value));
          DictEntry := DictEntry^.Next;
        end;
      end;
  end;
end;

function BencodeClone(Value: PBencodeValue): PBencodeValue;
begin
  Result := CloneValue(Value);
end;

procedure ValueToDebug(Value: PBencodeValue; var S: string; Indent: Integer);
var
  Entry: PBencodeListEntry;
  DictEntry: PBencodeDictEntry;
  Prefix: string;
  I: Integer;
  First: Boolean;
begin
  if Value = nil then
  begin
    S := S + 'null';
    Exit;
  end;
  
  Prefix := StringOfChar(' ', Indent);
  
  case Value^.ValueType of
    btString:
      begin
        S := S + '"';
        for I := 0 to Value^.StrLen - 1 do
        begin
          case Value^.StrVal[I] of
            #0..#31, #127..#255: S := S + '\x' + IntToHex(Ord(Value^.StrVal[I]), 2);
            '\': S := S + '\\';
            '"': S := S + '\"';
          else
            S := S + Value^.StrVal[I];
          end;
        end;
        S := S + '"';
      end;
      
    btInteger:
      S := S + IntToStr(Value^.IntVal);
      
    btList:
      begin
        S := S + '[';
        Entry := Value^.ListHead;
        First := True;
        while Entry <> nil do
        begin
          if not First then S := S + ',';
          S := S + #10 + Prefix + '  ';
          ValueToDebug(Entry^.Value, S, Indent + 2);
          First := False;
          Entry := Entry^.Next;
        end;
        if not First then S := S + #10 + Prefix;
        S := S + ']';
      end;
      
    btDict:
      begin
        S := S + '{';
        DictEntry := Value^.DictHead;
        First := True;
        while DictEntry <> nil do
        begin
          if not First then S := S + ',';
          S := S + #10 + Prefix + '  "';
          for I := 0 to DictEntry^.KeyLen - 1 do
            S := S + DictEntry^.Key[I];
          S := S + '": ';
          ValueToDebug(DictEntry^.Value, S, Indent + 2);
          First := False;
          DictEntry := DictEntry^.Next;
        end;
        if not First then S := S + #10 + Prefix;
        S := S + '}';
      end;
  end;
end;

function BencodeToDebugString(Value: PBencodeValue): string;
begin
  Result := '';
  ValueToDebug(Value, Result, 0);
end;

end.
