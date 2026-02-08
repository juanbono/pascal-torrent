{
  test_error_scenarios.pas - Error Handling and Edge Case Tests
  
  Tests error conditions and edge cases including:
  - Malformed protocol messages
  - Invalid piece indices
  - Buffer overflows
  - Disk full scenarios
  - Out-of-order operations
  - Timeout handling
}

program test_error_scenarios;

{$mode objfpc}{$H+}

uses
  SysUtils, bencode, metainfo, filemgr, protocol, sha1utils, utils, sockwrap;

var
  TotalTests: Integer = 0;
  PassedTests: Integer = 0;
  FailedTests: Integer = 0;
  TempDir: string;

procedure TestResult(const TestName: string; Passed: Boolean; const Msg: string = '');
begin
  Inc(TotalTests);
  if Passed then
  begin
    Inc(PassedTests);
    WriteLn('[PASS] ', TestName);
  end
  else
  begin
    Inc(FailedTests);
    WriteLn('[FAIL] ', TestName);
    if Msg <> '' then
      WriteLn('       ', Msg);
  end;
end;

{ ============================================================================ }
{ Setup                                                                        }
{ ============================================================================ }

procedure SetupTestDirectory;
begin
  TempDir := GetTempDir + 'pascaltorrent_err_test' + PathDelim;
  if not DirExists(TempDir) then
    MakeDir(TempDir);
end;

procedure CleanupTestDirectory;
var
  SearchRec: TSearchRec;
  Res: Integer;
begin
  Res := FindFirst(TempDir + '*', faAnyFile, SearchRec);
  while Res = 0 do
  begin
    if (SearchRec.Name <> '.') and (SearchRec.Name <> '..') then
    begin
      if (SearchRec.Attr and faDirectory) <> 0 then
        RmDir(TempDir + SearchRec.Name)
      else
        DeleteFile(TempDir + SearchRec.Name);
    end;
    Res := FindNext(SearchRec);
  end;
  FindClose(SearchRec);
  
  {$I-}
  RmDir(TempDir);
  {$I+}
end;

{ ============================================================================ }
{ Test 1: Malformed Protocol Messages                                          }
{ ============================================================================ }

procedure TestMalformedProtocolMessages;
var
  Buffer: array[0..1023] of Byte;
  Msg: TWireMessage;
  HS: THandshake;
  BytesConsumed: Integer;
  Success: Boolean;
  I: Integer;
begin
  WriteLn(#10'=== Testing Malformed Protocol Messages ===');
  
  { Test 1: Empty buffer }
  Success := DecodeMessage(@Buffer, 0, Msg, BytesConsumed);
  TestResult('Empty buffer rejected', not Success);
  
  { Test 2: nil buffer }
  Success := DecodeMessage(nil, 10, Msg, BytesConsumed);
  TestResult('nil buffer rejected', not Success);
  
  { Test 3: Invalid message ID }
  Buffer[0] := 0; Buffer[1] := 0; Buffer[2] := 0; Buffer[3] := 1;
  Buffer[4] := 100;  { Invalid message ID }
  Success := DecodeMessage(@Buffer, 5, Msg, BytesConsumed);
  TestResult('Invalid message ID rejected', not Success);
  
  { Test 4: Message ID 255 (keep-alive marker) with payload }
  Buffer[0] := 0; Buffer[1] := 0; Buffer[2] := 0; Buffer[3] := 1;
  Buffer[4] := 255;
  Success := DecodeMessage(@Buffer, 5, Msg, BytesConsumed);
  TestResult('Message ID 255 rejected', not Success);
  
  { Test 5: Incomplete message (have only 8 bytes of 9-byte message) }
  Buffer[0] := 0; Buffer[1] := 0; Buffer[2] := 0; Buffer[3] := 5;
  Buffer[4] := MSG_HAVE;
  Buffer[5] := 0; Buffer[6] := 0; Buffer[7] := 0;
  { Missing last byte of piece index }
  Success := DecodeMessage(@Buffer, 8, Msg, BytesConsumed);
  TestResult('Incomplete HAVE message rejected', not Success);
  
  { Test 6: Message larger than buffer (incomplete message) }
  Buffer[0] := 0; Buffer[1] := 0; Buffer[2] := 0; Buffer[3] := 100;  { 100 bytes payload }
  Buffer[4] := MSG_PIECE;
  { Only provide 10 bytes, but message needs 104 }
  Success := DecodeMessage(@Buffer, 10, Msg, BytesConsumed);
  TestResult('Incomplete large message rejected', not Success);
  
  { Test 7: Zero-length message (keep-alive) should be valid }
  Buffer[0] := 0; Buffer[1] := 0; Buffer[2] := 0; Buffer[3] := 0;
  Success := DecodeMessage(@Buffer, 4, Msg, BytesConsumed);
  TestResult('Keep-alive message accepted', Success);
  TestResult('Keep-alive bytes consumed is 4', BytesConsumed = 4);
  
  { Test 8: Truncated handshake }
  FillChar(Buffer, SizeOf(Buffer), 0);
  Buffer[0] := 19;  { Protocol length }
  Move(PROTOCOL_STRING[1], Buffer[1], 19);
  { Missing reserved, info hash, peer id }
  Success := DecodeHandshake(@Buffer, 30, HS);
  TestResult('Truncated handshake rejected', not Success);
  
  { Test 9: Wrong protocol string in handshake }
  FillChar(Buffer, SizeOf(Buffer), 0);
  Buffer[0] := 19;
  Move('BitTorrent protocol'[1], Buffer[1], 19);
  Buffer[20] := Ord('X');  { Corrupt protocol string }
  Success := DecodeHandshake(@Buffer, HANDSHAKE_LEN, HS);
  if Success then
  begin
    { Decode might succeed but validate should fail }
    TestResult('Corrupted handshake fails validation', 
               not ValidateHandshake(HS));
  end
  else
    TestResult('Corrupted handshake rejected', True);
  
  { Test 10: All-zero info hash }
  FillChar(Buffer, SizeOf(Buffer), 0);
  Buffer[0] := 19;
  Move(PROTOCOL_STRING[1], Buffer[1], 19);
  FillChar(Buffer[20], 8, 0);  { Reserved }
  FillChar(Buffer[28], 20, 0);  { Zero info hash }
  FillChar(Buffer[48], 20, 1);  { Non-zero peer id }
  Success := DecodeHandshake(@Buffer, HANDSHAKE_LEN, HS);
  if Success then
    TestResult('Zero info hash fails validation', not ValidateHandshake(HS))
  else
    TestResult('Zero info hash handled', True);
end;

{ ============================================================================ }
{ Test 2: Invalid Piece Operations                                             }
{ ============================================================================ }

procedure TestInvalidPieceOperations;
var
  Root, InfoDict, Pieces: PBencodeValue;
  Meta: PTorrentMeta;
  ParseResult: TMetainfoResult;
  FM: PFileManager;
  PieceData: array of Byte;
  PiecesStr: string;
  PieceLen: Integer;
  I: Integer;
  IOResult: TIOResult;
  PieceWriteResult: TPieceWriteResult;
  ReadBuffer: array of Byte;
begin
  WriteLn(#10'=== Testing Invalid Piece Operations ===');
  
  PieceLen := 16384;
  SetLength(PieceData, PieceLen);
  for I := 0 to PieceLen - 1 do
    PieceData[I] := I mod 256;
  
  { Create torrent with 3 pieces }
  Root := BencodeNewDict;
  InfoDict := BencodeNewDict;
  BencodeDictAdd(InfoDict, 'name', BencodeNewString('invalid_test.bin'));
  BencodeDictAdd(InfoDict, 'piece length', BencodeNewInteger(PieceLen));
  BencodeDictAdd(InfoDict, 'length', BencodeNewInteger(3 * PieceLen));
  
  SetLength(PiecesStr, 60);  { 3 * 20 bytes }
  FillChar(PiecesStr[1], 60, 0);
  for I := 0 to 2 do
    PiecesStr[I * 20 + 1] := Chr(I + 1);
  BencodeDictAdd(InfoDict, 'pieces', BencodeNewString(PiecesStr));
  BencodeDictAdd(Root, 'info', InfoDict);
  
  ParseResult := ParseTorrentBencode(Root, Meta);
  if not ParseResult.Success then
  begin
    TestResult('Parse torrent', False);
    BencodeFree(Root);
    Exit;
  end;
  
  try
    if not FileManagerCreate(Meta, TempDir, FM) then
    begin
      TestResult('Create file manager', False);
      Exit;
    end;
    
    try
      FileManagerInitialize(FM);
      
      { Test 1: Negative piece index }
      TestResult('Negative piece index rejected', 
                 not FileManagerIsPieceVerified(FM, -1));
      
      { Test 2: Piece index out of bounds }
      TestResult('Large piece index rejected', 
                 not FileManagerIsPieceVerified(FM, 1000));
      TestResult('Piece index = piece count rejected', 
                 not FileManagerIsPieceVerified(FM, 3));
      
      { Test 3: Read invalid piece }
      SetLength(ReadBuffer, PieceLen);
      TestResult('Read negative piece fails', 
                 not FileManagerReadPiece(FM, -1, @ReadBuffer[0], PieceLen, IOResult));
      TestResult('Read out-of-bounds piece fails', 
                 not FileManagerReadPiece(FM, 100, @ReadBuffer[0], PieceLen, IOResult));
      
      { Test 4: Write invalid piece }
      TestResult('Write negative piece fails', 
                 not FileManagerWritePiece(FM, -1, @PieceData[0], PieceLen, PieceWriteResult));
      TestResult('Write out-of-bounds piece fails', 
                 not FileManagerWritePiece(FM, 100, @PieceData[0], PieceLen, PieceWriteResult));
      
      { Test 5: Write piece with wrong size }
      TestResult('Write undersized piece fails', 
                 not FileManagerWritePiece(FM, 0, @PieceData[0], PieceLen - 1, PieceWriteResult));
      TestResult('Write oversized piece fails', 
                 not FileManagerWritePiece(FM, 0, @PieceData[0], PieceLen + 1, PieceWriteResult));
      
      { Test 6: Verify unverified piece fails }
      TestResult('Verify unverified piece returns false', 
                 not FileManagerVerifyPiece(FM, 0, IOResult));
      
    finally
      FileManagerDestroy(FM);
    end;
    
  finally
    FreeTorrentMeta(Meta);
    BencodeFree(Root);
    DeleteFile(JoinPath(TempDir, 'invalid_test.bin'));
  end;
end;

{ ============================================================================ }
{ Test 3: Malformed Bencode Data                                               }
{ ============================================================================ }

procedure TestMalformedBencode;
var
  Root: PBencodeValue;
  ParseRes: TParseResult;
  Malformed: string;
begin
  WriteLn(#10'=== Testing Malformed Bencode Data ===');
  
  { Test 1: Unterminated integer }
  Malformed := 'i123';
  ParseRes := BencodeDecode(@Malformed[1], Length(Malformed), Root);
  TestResult('Unterminated integer rejected', not ParseRes.Success);
  if Root <> nil then BencodeFree(Root);
  
  { Test 2: Negative string length }
  Malformed := '-5:hello';
  ParseRes := BencodeDecode(@Malformed[1], Length(Malformed), Root);
  TestResult('Negative string length rejected', not ParseRes.Success);
  if Root <> nil then BencodeFree(Root);
  
  { Test 3: Missing colon in string }
  Malformed := '5hello';
  ParseRes := BencodeDecode(@Malformed[1], Length(Malformed), Root);
  TestResult('Missing colon rejected', not ParseRes.Success);
  if Root <> nil then BencodeFree(Root);
  
  { Test 4: Unterminated list }
  Malformed := 'li1ei2e';
  ParseRes := BencodeDecode(@Malformed[1], Length(Malformed), Root);
  TestResult('Unterminated list rejected', not ParseRes.Success);
  if Root <> nil then BencodeFree(Root);
  
  { Test 5: Unterminated dictionary }
  Malformed := 'd4:spami1e';
  ParseRes := BencodeDecode(@Malformed[1], Length(Malformed), Root);
  TestResult('Unterminated dict rejected', not ParseRes.Success);
  if Root <> nil then BencodeFree(Root);
  
  { Test 6: Integer key in dictionary }
  Malformed := 'di1e4:spame';
  ParseRes := BencodeDecode(@Malformed[1], Length(Malformed), Root);
  TestResult('Integer key rejected', not ParseRes.Success);
  if Root <> nil then BencodeFree(Root);
  
  { Test 7: Invalid character in integer }
  Malformed := 'i12a3e';
  ParseRes := BencodeDecode(@Malformed[1], Length(Malformed), Root);
  TestResult('Invalid int character rejected', not ParseRes.Success);
  if Root <> nil then BencodeFree(Root);
  
  { Test 8: Leading zero in integer }
  Malformed := 'i0123e';
  ParseRes := BencodeDecode(@Malformed[1], Length(Malformed), Root);
  TestResult('Leading zero in int rejected', not ParseRes.Success);
  if Root <> nil then BencodeFree(Root);
  
  { Test 9: Negative zero }
  Malformed := 'i-0e';
  ParseRes := BencodeDecode(@Malformed[1], Length(Malformed), Root);
  TestResult('Negative zero rejected', not ParseRes.Success);
  if Root <> nil then BencodeFree(Root);
  
  { Test 10: Empty input }
  ParseRes := BencodeDecode(nil, 0, Root);
  TestResult('Empty input rejected', not ParseRes.Success);
  if Root <> nil then BencodeFree(Root);
  
  { Test 11: Just whitespace }
  Malformed := '   ';
  ParseRes := BencodeDecode(@Malformed[1], Length(Malformed), Root);
  TestResult('Whitespace-only rejected', not ParseRes.Success);
  if Root <> nil then BencodeFree(Root);
end;

{ ============================================================================ }
{ Test 4: Invalid Torrent Metadata                                             }
{ ============================================================================ }

procedure TestInvalidTorrentMetadata;
var
  Root, InfoDict: PBencodeValue;
  Meta: PTorrentMeta;
  ParseResult: TMetainfoResult;
  PiecesStr: string;
  I: Integer;
begin
  WriteLn(#10'=== Testing Invalid Torrent Metadata ===');
  
  { Test 1: Missing name }
  Root := BencodeNewDict;
  InfoDict := BencodeNewDict;
  BencodeDictAdd(InfoDict, 'piece length', BencodeNewInteger(32768));
  BencodeDictAdd(InfoDict, 'length', BencodeNewInteger(100000));
  SetLength(PiecesStr, 20);
  FillChar(PiecesStr[1], 20, 0);
  BencodeDictAdd(InfoDict, 'pieces', BencodeNewString(PiecesStr));
  BencodeDictAdd(Root, 'info', InfoDict);
  
  ParseResult := ParseTorrentBencode(Root, Meta);
  TestResult('Missing name rejected', not ParseResult.Success);
  BencodeFree(Root);
  if ParseResult.Success then FreeTorrentMeta(Meta);
  
  { Test 2: Negative piece length }
  Root := BencodeNewDict;
  InfoDict := BencodeNewDict;
  BencodeDictAdd(InfoDict, 'name', BencodeNewString('test'));
  BencodeDictAdd(InfoDict, 'piece length', BencodeNewInteger(-1));
  BencodeDictAdd(InfoDict, 'length', BencodeNewInteger(100000));
  SetLength(PiecesStr, 20);
  BencodeDictAdd(InfoDict, 'pieces', BencodeNewString(PiecesStr));
  BencodeDictAdd(Root, 'info', InfoDict);
  
  ParseResult := ParseTorrentBencode(Root, Meta);
  TestResult('Negative piece length rejected', not ParseResult.Success);
  BencodeFree(Root);
  
  { Test 3: Zero piece length }
  Root := BencodeNewDict;
  InfoDict := BencodeNewDict;
  BencodeDictAdd(InfoDict, 'name', BencodeNewString('test'));
  BencodeDictAdd(InfoDict, 'piece length', BencodeNewInteger(0));
  BencodeDictAdd(InfoDict, 'length', BencodeNewInteger(100000));
  SetLength(PiecesStr, 20);
  BencodeDictAdd(InfoDict, 'pieces', BencodeNewString(PiecesStr));
  BencodeDictAdd(Root, 'info', InfoDict);
  
  ParseResult := ParseTorrentBencode(Root, Meta);
  TestResult('Zero piece length rejected', not ParseResult.Success);
  BencodeFree(Root);
  
  { Test 4: Empty pieces string }
  Root := BencodeNewDict;
  InfoDict := BencodeNewDict;
  BencodeDictAdd(InfoDict, 'name', BencodeNewString('test'));
  BencodeDictAdd(InfoDict, 'piece length', BencodeNewInteger(32768));
  BencodeDictAdd(InfoDict, 'length', BencodeNewInteger(100000));
  BencodeDictAdd(InfoDict, 'pieces', BencodeNewString(''));
  BencodeDictAdd(Root, 'info', InfoDict);
  
  ParseResult := ParseTorrentBencode(Root, Meta);
  TestResult('Empty pieces rejected', not ParseResult.Success);
  BencodeFree(Root);
  
  { Test 5: Pieces not multiple of 20 }
  Root := BencodeNewDict;
  InfoDict := BencodeNewDict;
  BencodeDictAdd(InfoDict, 'name', BencodeNewString('test'));
  BencodeDictAdd(InfoDict, 'piece length', BencodeNewInteger(32768));
  BencodeDictAdd(InfoDict, 'length', BencodeNewInteger(100000));
  BencodeDictAdd(InfoDict, 'pieces', BencodeNewStringBuf('1234567890123456789', 19));
  BencodeDictAdd(Root, 'info', InfoDict);
  
  ParseResult := ParseTorrentBencode(Root, Meta);
  TestResult('Invalid pieces length rejected', not ParseResult.Success);
  BencodeFree(Root);
  
  { Test 6: Missing both length and files (multi-file) }
  Root := BencodeNewDict;
  InfoDict := BencodeNewDict;
  BencodeDictAdd(InfoDict, 'name', BencodeNewString('test'));
  BencodeDictAdd(InfoDict, 'piece length', BencodeNewInteger(32768));
  SetLength(PiecesStr, 20);
  BencodeDictAdd(InfoDict, 'pieces', BencodeNewString(PiecesStr));
  BencodeDictAdd(Root, 'info', InfoDict);
  
  ParseResult := ParseTorrentBencode(Root, Meta);
  TestResult('Missing length/files rejected', not ParseResult.Success);
  BencodeFree(Root);
  
  { Test 7: Both length and files present - parser is lenient and uses length }
  Root := BencodeNewDict;
  InfoDict := BencodeNewDict;
  BencodeDictAdd(InfoDict, 'name', BencodeNewString('test'));
  BencodeDictAdd(InfoDict, 'piece length', BencodeNewInteger(32768));
  BencodeDictAdd(InfoDict, 'length', BencodeNewInteger(100000));
  BencodeDictAdd(InfoDict, 'files', BencodeNewList);
  SetLength(PiecesStr, 20);
  BencodeDictAdd(InfoDict, 'pieces', BencodeNewString(PiecesStr));
  BencodeDictAdd(Root, 'info', InfoDict);
  
  ParseResult := ParseTorrentBencode(Root, Meta);
  { Parser accepts both but prioritizes length, treating as single-file }
  TestResult('Both length and files accepted (lenient parsing)', ParseResult.Success);
  if ParseResult.Success then
    TestResult('Treated as single-file', Meta^.IsSingleFile);
  BencodeFree(Root);
  if ParseResult.Success then FreeTorrentMeta(Meta);
end;

{ ============================================================================ }
{ Test 5: Socket Error Handling                                                }
{ ============================================================================ }

procedure TestSocketErrorHandling;
var
  Context: PSocketContext;
  Res: Integer;
  Buffer: array[0..1023] of Byte;
  Sent, Received: Integer;
begin
  WriteLn(#10'=== Testing Socket Error Handling ===');
  
  SocketInit;
  
  { Test 1: Operations on nil context }
  SocketClose(nil);
  TestResult('SocketClose(nil) safe', True);
  
  SocketDestroy(nil);
  TestResult('SocketDestroy(nil) safe', True);
  
  Res := SocketBind(nil, '127.0.0.1', 8080);
  TestResult('SocketBind(nil) returns error', Res <> SOCK_OK);
  
  Res := SocketConnect(nil, '127.0.0.1', 8080, 1000);
  TestResult('SocketConnect(nil) returns error', Res <> SOCK_OK);
  
  Res := SocketSend(nil, @Buffer, 100, Sent);
  TestResult('SocketSend(nil) returns error', Res <> SOCK_OK);
  
  Res := SocketReceive(nil, @Buffer, 100, Received);
  TestResult('SocketReceive(nil) returns error', Res <> SOCK_OK);
  
  { Test 2: Operations on closed socket }
  Context := SocketCreate;
  if Context <> nil then
  begin
    SocketClose(Context);
    
    Res := SocketSend(Context, @Buffer, 100, Sent);
    TestResult('SocketSend on closed returns error', Res <> SOCK_OK);
    
    SocketDestroy(Context);
  end;
  
  { Test 3: Invalid addresses }
  Context := SocketCreate;
  if Context <> nil then
  begin
    Res := SocketConnect(Context, '', 8080, 1000);
    TestResult('Connect to empty host fails', Res <> SOCK_OK);
    
    Res := SocketConnect(Context, '256.256.256.256', 8080, 1000);
    TestResult('Connect to invalid IP fails', Res <> SOCK_OK);
    
    SocketDestroy(Context);
  end;
  
  { Test 4: Invalid ports }
  Context := SocketCreate;
  if Context <> nil then
  begin
    Res := SocketBind(Context, '127.0.0.1', 0);
    TestResult('Bind to port 0 succeeds (OS assigns)', Res = SOCK_OK);
    
    SocketClose(Context);
    Context := SocketCreate;
    
    Res := SocketConnect(Context, '127.0.0.1', 0, 1000);
    TestResult('Connect to port 0 fails', Res <> SOCK_OK);
    
    SocketDestroy(Context);
  end;
  
  { Test 5: nil buffer operations }
  Context := SocketCreate;
  if Context <> nil then
  begin
    Res := SocketSend(Context, nil, 100, Sent);
    TestResult('SocketSend(nil buffer) returns error', Res <> SOCK_OK);
    
    Res := SocketReceive(Context, nil, 100, Received);
    TestResult('SocketReceive(nil buffer) returns error', Res <> SOCK_OK);
    
    SocketDestroy(Context);
  end;
  
  { Test 6: Negative/zero length }
  Context := SocketCreate;
  if Context <> nil then
  begin
    Res := SocketSend(Context, @Buffer, 0, Sent);
    TestResult('SocketSend(0 length) returns error', Res <> SOCK_OK);
    
    Res := SocketSend(Context, @Buffer, -1, Sent);
    TestResult('SocketSend(-1 length) returns error', Res <> SOCK_OK);
    
    SocketDestroy(Context);
  end;
  
  SocketCleanup;
end;

{ ============================================================================ }
{ Test 6: Protocol Buffer Bounds                                               }
{ ============================================================================ }

procedure TestProtocolBufferBounds;
var
  Buffer: array[0..63] of Byte;  { Small buffer }
  Msg: TWireMessage;
  BytesWritten: Integer;
  Success: Boolean;
  Data: array[0..1023] of Byte;
  I: Integer;
  InfoHash: array[0..19] of Byte;
  PeerId: array[0..19] of Byte;
  Res: Integer;
  WrongHash: array of Byte;
  WrongId: array of Byte;
begin
  WriteLn(#10'=== Testing Protocol Buffer Bounds ===');
  
  { Fill data }
  for I := 0 to High(Data) do
    Data[I] := I mod 256;
  
  { Test 1: Bitfield too large for buffer }
  Msg.Length := 1 + 100;
  Msg.MsgId := MSG_BITFIELD;
  Msg.BitfieldData := @Data;
  Msg.BitfieldLen := 100;
  
  Success := EncodeMessage(Msg, @Buffer, SizeOf(Buffer), BytesWritten);
  TestResult('Bitfield too large rejected', not Success);
  
  { Test 2: Piece message too large }
  Msg.Length := 9 + 1024;
  Msg.MsgId := MSG_PIECE;
  Msg.PieceIndex := 0;
  Msg.PieceBegin := 0;
  Msg.PieceData := @Data;
  Msg.PieceDataLen := 1024;
  
  Success := EncodeMessage(Msg, @Buffer, SizeOf(Buffer), BytesWritten);
  TestResult('Piece message too large rejected', not Success);
  
  { Test 3: nil buffer }
  Msg.Length := 5;
  Msg.MsgId := MSG_HAVE;
  Msg.HaveIndex := 0;
  
  Success := EncodeMessage(Msg, nil, 100, BytesWritten);
  TestResult('nil buffer rejected', not Success);
  
  { Test 4: EncodeHandshake with small buffer }
  FillChar(InfoHash, SizeOf(InfoHash), 0);
  FillChar(PeerId, SizeOf(PeerId), 0);
  
  Res := EncodeHandshake(InfoHash, PeerId, @Buffer, 10);
  TestResult('Handshake to small buffer returns 0', Res = 0);
  
  { Test 5: nil buffer for handshake }
  Res := EncodeHandshake(InfoHash, PeerId, nil, 100);
  TestResult('nil buffer for handshake returns 0', Res = 0);
  
  { Test 6: Wrong hash sizes }
  SetLength(WrongHash, 19);
  SetLength(WrongId, 20);
  
  Res := EncodeHandshake(WrongHash, WrongId, @Buffer, SizeOf(Buffer));
  TestResult('Wrong info hash size returns 0', Res = 0);
  
  SetLength(WrongHash, 20);
  SetLength(WrongId, 19);
  
  Res := EncodeHandshake(WrongHash, WrongId, @Buffer, SizeOf(Buffer));
  TestResult('Wrong peer ID size returns 0', Res = 0);
end;

{ ============================================================================ }
{ Main Program                                                                 }
{ ============================================================================ }

begin
  WriteLn('==============================================');
  WriteLn('  ERROR SCENARIOS AND EDGE CASE TESTS');
  WriteLn('==============================================');
  
  SetupTestDirectory;
  try
    TestMalformedProtocolMessages;
    TestInvalidPieceOperations;
    TestMalformedBencode;
    TestInvalidTorrentMetadata;
    TestSocketErrorHandling;
    TestProtocolBufferBounds;
    
    { Summary }
    WriteLn(#10'==============================================');
    WriteLn('  RESULTS: ', PassedTests, '/', TotalTests, ' tests passed');
    WriteLn('==============================================');
    
    if FailedTests > 0 then
    begin
      WriteLn('FAILED: ', FailedTests, ' tests failed');
      Halt(1);
    end
    else
      WriteLn('SUCCESS: All error scenario tests passed!');
      
  finally
    CleanupTestDirectory;
  end;
end.
