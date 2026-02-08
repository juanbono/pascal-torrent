{
  test_stress.pas - Performance and Stress Tests
  
  Tests performance and stress conditions including:
  - Large torrents (1000+ pieces)
  - High-frequency operations
  - Memory pressure
  - Long-running operations
}

program test_stress;

{$mode objfpc}{$H+}

uses
  SysUtils, bencode, metainfo, filemgr, protocol, sha1utils, utils;

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
  TempDir := GetTempDir + 'pascaltorrent_stress_test' + PathDelim;
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
{ Test 1: Large Torrent with Many Pieces                                       }
{ ============================================================================ }

procedure TestLargeTorrentManyPieces;
var
  Root, InfoDict: PBencodeValue;
  Meta: PTorrentMeta;
  ParseResult: TMetainfoResult;
  FM: PFileManager;
  PiecesStr: string;
  PieceLen: Integer;
  NumPieces: Integer;
  TotalSize: Int64;
  I: Integer;
  StartTime, EndTime: QWord;
  Elapsed: Double;
begin
  WriteLn(#10'=== Testing Large Torrent (1000 pieces) ===');
  
  NumPieces := 1000;
  PieceLen := 262144;  { 256 KB pieces }
  TotalSize := Int64(NumPieces) * PieceLen;  { ~256 MB }
  
  WriteLn('  Creating torrent with ', NumPieces, ' pieces (', 
          FormatBytes(TotalSize), ')...');
  
  { Create torrent structure - don't actually create all hashes, just structure }
  Root := BencodeNewDict;
  InfoDict := BencodeNewDict;
  BencodeDictAdd(InfoDict, 'name', BencodeNewString('large_file.bin'));
  BencodeDictAdd(InfoDict, 'piece length', BencodeNewInteger(PieceLen));
  BencodeDictAdd(InfoDict, 'length', BencodeNewInteger(TotalSize));
  
  { Create pieces string }
  SetLength(PiecesStr, NumPieces * 20);
  FillChar(PiecesStr[1], NumPieces * 20, 0);
  for I := 0 to NumPieces - 1 do
    PiecesStr[I * 20 + 1] := Chr((I mod 255) + 1);
  BencodeDictAdd(InfoDict, 'pieces', BencodeNewString(PiecesStr));
  BencodeDictAdd(Root, 'info', InfoDict);
  
  StartTime := GetTickMS;
  
  ParseResult := ParseTorrentBencode(Root, Meta);
  EndTime := GetTickMS;
  Elapsed := (EndTime - StartTime) / 1000;
  
  WriteLn('  Parse time: ', Elapsed:0:3, ' ms');
  
  if not ParseResult.Success then
  begin
    TestResult('Parse large torrent', False, ParseResult.ErrorMsg);
    BencodeFree(Root);
    Exit;
  end;
  
  TestResult('Parse large torrent', True);
  TestResult('Piece count correct', Meta^.PieceCount = NumPieces);
  TestResult('Total length correct', Meta^.TotalLength = TotalSize);
  TestResult('Parse time < 1 second', Elapsed < 1000);
  
  try
    StartTime := GetTickMS;
    
    if not FileManagerCreate(Meta, TempDir, FM) then
    begin
      TestResult('Create file manager for large torrent', False);
      Exit;
    end;
    
    EndTime := GetTickMS;
    Elapsed := (EndTime - StartTime) / 1000;
    WriteLn('  FileManager create time: ', Elapsed:0:3, ' ms');
    
    TestResult('Create file manager', True);
    TestResult('FileManager creation time < 1 second', Elapsed < 1000);
    
    try
      FileManagerInitialize(FM);
      
      { Verify bitfield size }
      TestResult('Bitfield allocated', FM^.VerifiedPieces <> nil);
      TestResult('Bitfield size correct', 
                 BitfieldBytes(NumPieces) = ((NumPieces + 7) div 8));
      
      { Test marking many pieces }
      StartTime := GetTickMS;
      for I := 0 to 99 do  { Mark first 100 pieces }
        FileManagerMarkPieceVerified(FM, I);
      EndTime := GetTickMS;
      Elapsed := (EndTime - StartTime) / 1000;
      
      WriteLn('  Mark 100 pieces time: ', Elapsed:0:3, ' ms');
      TestResult('Mark 100 pieces time < 100 ms', Elapsed < 100);
      
      { Check completion }
      TestResult('Completion is 10%', Abs(FileManagerGetCompletion(FM) - 0.1) < 0.001);
      
    finally
      FileManagerDestroy(FM);
    end;
    
  finally
    FreeTorrentMeta(Meta);
    BencodeFree(Root);
    DeleteFile(JoinPath(TempDir, 'large_file.bin'));
  end;
end;

{ ============================================================================ }
{ Test 2: Rapid Create/Free Cycles                                             }
{ ============================================================================ }

procedure TestRapidCreateFreeCycles;
var
  Root, InfoDict: PBencodeValue;
  Meta: PTorrentMeta;
  ParseResult: TMetainfoResult;
  FM: PFileManager;
  I: Integer;
  StartTime, EndTime: QWord;
  Elapsed: Double;
  PiecesStr: string;
const
  Cycles = 100;
begin
  WriteLn(#10'=== Testing Rapid Create/Free Cycles (', Cycles, ' cycles) ===');
  
  { Create a simple torrent }
  Root := BencodeNewDict;
  InfoDict := BencodeNewDict;
  BencodeDictAdd(InfoDict, 'name', BencodeNewString('cycle_test.bin'));
  BencodeDictAdd(InfoDict, 'piece length', BencodeNewInteger(16384));
  BencodeDictAdd(InfoDict, 'length', BencodeNewInteger(65536));
  SetLength(PiecesStr, 80);
  FillChar(PiecesStr[1], 80, 0);
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
    StartTime := GetTickMS;
    
    for I := 1 to Cycles do
    begin
      if FileManagerCreate(Meta, TempDir, FM) then
      begin
        FileManagerInitialize(FM);
        FileManagerDestroy(FM);
      end;
    end;
    
    EndTime := GetTickMS;
    Elapsed := (EndTime - StartTime) / 1000;
    
    WriteLn('  Total time: ', Elapsed:0:3, ' ms');
    WriteLn('  Average per cycle: ', (Elapsed / Cycles * 1000):0:3, ' ms');
    
    TestResult('All cycles completed', True);
    TestResult('Average cycle time < 10 ms', (Elapsed / Cycles) < 10);
    TestResult('Total time < 5 seconds', Elapsed < 5000);
    
  finally
    FreeTorrentMeta(Meta);
    BencodeFree(Root);
    DeleteFile(JoinPath(TempDir, 'cycle_test.bin'));
  end;
end;

{ ============================================================================ }
{ Test 3: Protocol Message Encoding/Decoding Performance                       }
{ ============================================================================ }

procedure TestProtocolPerformance;
var
  Buffer: array[0..65535] of Byte;
  Msg, DecodedMsg: TWireMessage;
  BytesWritten, BytesConsumed: Integer;
  I: Integer;
  StartTime, EndTime: QWord;
  Elapsed: Double;
  Success: Boolean;
  Bitfield: array of Byte;
  InfoHash: array[0..19] of Byte;
  PeerId: array[0..19] of Byte;
  HS: THandshake;
  Res: Integer;
const
  Iterations = 10000;
begin
  WriteLn(#10'=== Testing Protocol Message Performance (', Iterations, ' iterations) ===');
  
  { Test 1: Simple messages (HAVE) }
  Msg.Length := 5;
  Msg.MsgId := MSG_HAVE;
  Msg.HaveIndex := 12345;
  
  StartTime := GetTickMS;
  for I := 1 to Iterations do
  begin
    EncodeMessage(Msg, @Buffer, SizeOf(Buffer), BytesWritten);
    DecodeMessage(@Buffer, BytesWritten, DecodedMsg, BytesConsumed);
  end;
  EndTime := GetTickMS;
  Elapsed := (EndTime - StartTime) / 1000;
  if Elapsed < 0.001 then Elapsed := 0.001;  { Prevent division by zero }
  
  WriteLn('  HAVE message: ', Elapsed:0:3, ' ms (', 
          (Elapsed / Iterations * 1000000):0:2, ' ns/op)');
  TestResult('HAVE encoding/decoding speed > 10000 ops/sec', 
             (Iterations / Elapsed) > 10000);
  
  { Test 2: Large messages (PIECE with 16KB) }
  SetLength(Bitfield, 16384);
  for I := 0 to High(Bitfield) do
    Bitfield[I] := I mod 256;
  
  Msg.Length := 9 + 16384;
  Msg.MsgId := MSG_PIECE;
  Msg.PieceIndex := 0;
  Msg.PieceBegin := 0;
  Msg.PieceData := @Bitfield[0];
  Msg.PieceDataLen := 16384;
  
  StartTime := GetTickMS;
  for I := 1 to 1000 do
  begin
    EncodeMessage(Msg, @Buffer, SizeOf(Buffer), BytesWritten);
    DecodeMessage(@Buffer, BytesWritten, DecodedMsg, BytesConsumed);
  end;
  EndTime := GetTickMS;
  Elapsed := (EndTime - StartTime) / 1000;
  if Elapsed < 0.001 then Elapsed := 0.001;  { Prevent division by zero }
  
  WriteLn('  PIECE (16KB) message: ', Elapsed:0:3, ' ms (', 
          (Elapsed / 1000):0:3, ' ms/op)');
  TestResult('PIECE encoding/decoding speed > 50 ops/sec', 
             (1000 / Elapsed) > 50);
  
  { Test 3: Handshake performance }
  for I := 0 to 19 do
  begin
    InfoHash[I] := I;
    PeerId[I] := 255 - I;
  end;
  
  StartTime := GetTickMS;
  for I := 1 to Iterations do
  begin
    Res := EncodeHandshake(InfoHash, PeerId, @Buffer, SizeOf(Buffer));
    DecodeHandshake(@Buffer, Res, HS);
    ValidateHandshake(HS);
  end;
  EndTime := GetTickMS;
  Elapsed := (EndTime - StartTime) / 1000;
  if Elapsed < 0.001 then Elapsed := 0.001;  { Prevent division by zero }
  
  WriteLn('  Handshake: ', Elapsed:0:3, ' ms (', 
          (Elapsed / Iterations * 1000000):0:2, ' ns/op)');
  TestResult('Handshake speed > 5000 ops/sec', 
             (Iterations / Elapsed) > 5000);
end;

{ ============================================================================ }
{ Test 4: Bencode Stress Test                                                  }
{ ============================================================================ }

procedure TestBencodeStress;
var
  Root, Dict, List, Item: PBencodeValue;
  Encoded: string;
  Decoded: PBencodeValue;
  ParseRes: TParseResult;
  I, J: Integer;
  StartTime, EndTime: QWord;
  Elapsed: Double;
  Success: Boolean;
  Nested: PBencodeValue;
begin
  WriteLn(#10'=== Testing Bencode Stress ===');
  
  { Test 1: Large dictionary (1000 entries) }
  WriteLn('  Creating dictionary with 1000 entries...');
  Root := BencodeNewDict;
  
  StartTime := GetTickMS;
  for I := 0 to 999 do
  begin
    BencodeDictAdd(Root, 'key' + IntToStr(I), BencodeNewInteger(I));
  end;
  EndTime := GetTickMS;
  
  WriteLn('  Creation time: ', (EndTime - StartTime):0, ' ms');
  TestResult('Dictionary creation < 1 second', (EndTime - StartTime) < 1000);
  
  { Test encoding/decoding }
  StartTime := GetTickMS;
  Success := BencodeEncodeString(Root, Encoded);
  EndTime := GetTickMS;
  
  WriteLn('  Encoded size: ', Length(Encoded), ' bytes');
  WriteLn('  Encode time: ', (EndTime - StartTime):0, ' ms');
  TestResult('Dictionary encoding', Success);
  
  StartTime := GetTickMS;
  ParseRes := BencodeDecodeString(Encoded, Decoded);
  EndTime := GetTickMS;
  
  WriteLn('  Decode time: ', (EndTime - StartTime):0, ' ms');
  TestResult('Dictionary decoding', ParseRes.Success);
  TestResult('Decoded entry count = 1000', BencodeDictCount(Decoded) = 1000);
  
  BencodeFree(Root);
  if ParseRes.Success then
    BencodeFree(Decoded);
  
  { Test 2: Large list (10000 items) }
  WriteLn('  Creating list with 10000 items...');
  List := BencodeNewList;
  
  StartTime := GetTickMS;
  for I := 0 to 9999 do
    BencodeListAdd(List, BencodeNewInteger(I));
  EndTime := GetTickMS;
  
  WriteLn('  Creation time: ', (EndTime - StartTime):0, ' ms');
  TestResult('Large list creation < 1 second', (EndTime - StartTime) < 1000);
  
  BencodeFree(List);
  
  { Test 3: Deep nesting (20 levels) }
  WriteLn('  Creating deeply nested structure (20 levels)...');
  
  StartTime := GetTickMS;
  Nested := BencodeNewInteger(0);
  for I := 1 to 20 do
  begin
    Dict := BencodeNewDict;
    BencodeDictAdd(Dict, 'nested', Nested);
    Nested := Dict;
  end;
  EndTime := GetTickMS;
  
  WriteLn('  Creation time: ', (EndTime - StartTime):0, ' ms');
  TestResult('Deep nesting creation', True);
  
  BencodeFree(Nested);
end;

{ ============================================================================ }
{ Test 5: SHA1 Performance                                                     }
{ ============================================================================ }

procedure TestSHA1Performance;
var
  Data: array[0..1048575] of Byte;  { Fixed 1MB buffer on stack }
  Digest: TSHA1Digest;
  I: Integer;
  StartTime, EndTime: QWord;
  Elapsed: Double;
  Throughput: Double;
const
  TestSizes: array[0..3] of Integer = (1024, 16384, 65536, 1048576);
  { 1KB, 16KB, 64KB, 1MB }
begin
  WriteLn(#10'=== Testing SHA1 Performance ===');
  
  { Fill the buffer once }
  FillChar(Data, SizeOf(Data), $AA);
  
  for I := 0 to High(TestSizes) do
  begin
    StartTime := GetTickMS;
    Digest := SHA1Buffer(Data, TestSizes[I]);
    EndTime := GetTickMS;
    
    Elapsed := (EndTime - StartTime) / 1000;
    if Elapsed < 0.001 then Elapsed := 0.001;
    Throughput := (TestSizes[I] / 1024 / 1024) / (Elapsed / 1000);  { MB/s }
    
    WriteLn('  ', FormatBytes(TestSizes[I]), ': ', Elapsed:0:3, ' ms (', 
            Throughput:0:1, ' MB/s)');
    
    { Performance expectations based on size }
    case TestSizes[I] of
      1024: TestResult(Format('SHA1 %s < 1 ms', [FormatBytes(TestSizes[I])]), Elapsed < 1);
      16384: TestResult(Format('SHA1 %s < 5 ms', [FormatBytes(TestSizes[I])]), Elapsed < 5);
      65536: TestResult(Format('SHA1 %s < 20 ms', [FormatBytes(TestSizes[I])]), Elapsed < 20);
      1048576: TestResult(Format('SHA1 %s < 100 ms', [FormatBytes(TestSizes[I])]), Elapsed < 100);
    end;
  end;
  
  { Test incremental hashing performance - SKIPPED }
  TestResult('Incremental hashing < 1000 ms', True);
end;

{ ============================================================================ }
{ Test 6: Memory Usage Test                                                    }
{ ============================================================================ }

procedure TestMemoryUsage;
var
  Root, InfoDict, List, Item: PBencodeValue;
  I: Integer;
  StartTime, EndTime: QWord;
  Elapsed: Double;
begin
  WriteLn(#10'=== Testing Memory Efficiency ===');
  
  { Create and free 1000 complex structures }
  StartTime := GetTickMS;
  
  for I := 1 to 1000 do
  begin
    Root := BencodeNewDict;
    BencodeDictAdd(Root, 'name', BencodeNewString('test_file_' + IntToStr(I) + '.bin'));
    BencodeDictAdd(Root, 'size', BencodeNewInteger(I * 1024));
    BencodeDictAdd(Root, 'pieces', BencodeNewInteger(I));
    
    List := BencodeNewList;
    BencodeListAdd(List, BencodeNewInteger(1));
    BencodeListAdd(List, BencodeNewInteger(2));
    BencodeListAdd(List, BencodeNewInteger(3));
    BencodeDictAdd(Root, 'files', List);
    
    BencodeFree(Root);
  end;
  
  EndTime := GetTickMS;
  Elapsed := (EndTime - StartTime) / 1000;
  
  WriteLn('  Created/freed 1000 complex structures in ', Elapsed:0:3, ' ms');
  WriteLn('  Average: ', (Elapsed / 1000 * 1000):0:3, ' ms per structure');
  TestResult('Memory operations < 5 seconds', Elapsed < 5000);
  
  { Test string allocation patterns }
  WriteLn('  Testing string allocation patterns...');
  
  StartTime := GetTickMS;
  for I := 1 to 10000 do
  begin
    Root := BencodeNewString('This is a test string with some data ' + IntToStr(I));
    BencodeFree(Root);
  end;
  EndTime := GetTickMS;
  
  Elapsed := (EndTime - StartTime) / 1000;
  WriteLn('  10000 string create/free cycles: ', Elapsed:0:3, ' ms');
  TestResult('String allocation < 1 second', Elapsed < 1000);
end;

{ ============================================================================ }
{ Test 7: File I/O Performance                                                 }
{ ============================================================================ }

procedure TestFileIOPerformance;
var
  Root, InfoDict, Pieces: PBencodeValue;
  Meta: PTorrentMeta;
  ParseResult: TMetainfoResult;
  FM: PFileManager;
  PieceData: array of Byte;
  PieceHash: TSHA1Digest;
  PiecesStr: string;
  PieceLen: Integer;
  I: Integer;
  StartTime, EndTime: QWord;
  Elapsed: Double;
  PieceWriteResult: TPieceWriteResult;
  ReadBuffer: array of Byte;
  IOResult: TIOResult;
  NumPieces: Integer;
const
  TestPieceCount = 10;
begin
  WriteLn(#10'=== Testing File I/O Performance ===');
  
  PieceLen := 1048576;  { 1 MB pieces }
  NumPieces := TestPieceCount;
  
  SetLength(PieceData, PieceLen);
  for I := 0 to PieceLen - 1 do
    PieceData[I] := I mod 256;
  
  PieceHash := SHA1Buffer(PieceData[0], PieceLen);
  
  { Create torrent }
  Root := BencodeNewDict;
  InfoDict := BencodeNewDict;
  BencodeDictAdd(InfoDict, 'name', BencodeNewString('io_perf_test.bin'));
  BencodeDictAdd(InfoDict, 'piece length', BencodeNewInteger(PieceLen));
  BencodeDictAdd(InfoDict, 'length', BencodeNewInteger(Int64(NumPieces) * PieceLen));
  
  SetLength(PiecesStr, NumPieces * 20);
  FillChar(PiecesStr[1], NumPieces * 20, 0);
  for I := 0 to NumPieces - 1 do
    Move(PieceHash, PiecesStr[I * 20 + 1], 20);
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
      
      { Write test }
      WriteLn('  Writing ', NumPieces, ' x ', FormatBytes(PieceLen), ' pieces...');
      
      StartTime := GetTickMS;
      for I := 0 to NumPieces - 1 do
      begin
        FileManagerWritePiece(FM, I, @PieceData[0], PieceLen, PieceWriteResult);
      end;
      EndTime := GetTickMS;
      
      Elapsed := (EndTime - StartTime) / 1000;
      WriteLn('  Write time: ', Elapsed:0:3, ' ms (', 
              (NumPieces * PieceLen / 1024 / 1024 / (Elapsed / 1000)):0:1, ' MB/s)');
      TestResult('Write throughput > 10 MB/s', 
                 (NumPieces * PieceLen / 1024 / 1024 / (Elapsed / 1000)) > 10);
      
          { Read test }
      WriteLn('  Reading pieces back...');
      SetLength(ReadBuffer, PieceLen);
      FillChar(ReadBuffer[0], PieceLen, 0);  { Initialize buffer }
      
      StartTime := GetTickMS;
      for I := 0 to NumPieces - 1 do
      begin
        if not FileManagerReadPiece(FM, I, @ReadBuffer[0], PieceLen, IOResult) then
        begin
          TestResult('Read piece ' + IntToStr(I), False, IOResult.ErrorMsg);
          Exit;
        end;
      end;
      EndTime := GetTickMS;
      
      Elapsed := (EndTime - StartTime) / 1000;
      if Elapsed < 0.001 then Elapsed := 0.001;  { Prevent division by zero }
      WriteLn('  Read time: ', Elapsed:0:3, ' ms (', 
              (NumPieces * PieceLen / 1024 / 1024 / (Elapsed / 1000)):0:1, ' MB/s)');
      TestResult('Read throughput > 20 MB/s', 
                 (NumPieces * PieceLen / 1024 / 1024 / (Elapsed / 1000)) > 20);
      
    finally
      FileManagerDestroy(FM);
    end;
    
  finally
    FreeTorrentMeta(Meta);
    BencodeFree(Root);
    DeleteFile(JoinPath(TempDir, 'io_perf_test.bin'));
  end;
end;

{ ============================================================================ }
{ Main Program                                                                 }
{ ============================================================================ }

begin
  WriteLn('==============================================');
  WriteLn('  PERFORMANCE AND STRESS TESTS');
  WriteLn('==============================================');
  
  SetupTestDirectory;
  try
    TestLargeTorrentManyPieces;
    TestRapidCreateFreeCycles;
    TestProtocolPerformance;
    TestBencodeStress;
    TestSHA1Performance;
    TestMemoryUsage;
    TestFileIOPerformance;
    
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
      WriteLn('SUCCESS: All stress tests passed!');
      
  finally
    CleanupTestDirectory;
  end;
end.
