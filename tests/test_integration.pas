{
  test_integration.pas - Integration tests for PascalTorrent
  
  Tests multiple units working together in realistic scenarios.
}

program test_integration;

{$mode objfpc}{$H+}

uses
  SysUtils, 
  bencode, metainfo, filemgr, protocol, sha1utils, utils;

var
  TotalTests: Integer = 0;
  PassedTests: Integer = 0;
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
    WriteLn('[FAIL] ', TestName);
    if Msg <> '' then
      WriteLn('       ', Msg);
  end;
end;

{ ============================================================================ }
{ Integration Test 1: Complete Torrent Loading and Validation                 }
{ ============================================================================ }

procedure TestTorrentLoadingWorkflow;
var
  Root: PBencodeValue;
  InfoDict: PBencodeValue;
  Pieces: PBencodeValue;
  PiecesStr: string;
  FilesList: PBencodeValue;
  File1: PBencodeValue;
  File2: PBencodeValue;
  Path1: PBencodeValue;
  Path2: PBencodeValue;
  Meta: PTorrentMeta;
  ParseResult: TMetainfoResult;
  FM: PFileManager;
  InfoHash: TSHA1Digest;
  InfoHashHex: string;
  Valid: Boolean;
  ErrorMsg: string;
  I: Integer;
  FilesPath: string;
  DataPath: string;
begin
  WriteLn(#10'=== Integration Test: Torrent Loading Workflow ===');
  
  { Step 1: Create a realistic multi-file torrent structure }
  Root := BencodeNewDict;
  BencodeDictAdd(Root, 'announce', BencodeNewString('http://tracker.example.com:8080/announce'));
  
  InfoDict := BencodeNewDict;
  { Use unique name to avoid conflicts with other tests }
  BencodeDictAdd(InfoDict, 'name', BencodeNewString('TestContentMulti'));
  BencodeDictAdd(InfoDict, 'piece length', BencodeNewInteger(32768));
  { Note: For multi-file torrents, 'length' should NOT be present - only 'files' list }
  
  { Create realistic pieces - build string properly }
  SetLength(PiecesStr, 4 * 20);
  FillChar(PiecesStr[1], 4 * 20, 0);
  for I := 0 to 3 do
    PiecesStr[I * 20 + 1] := Chr(I + 1);
  Pieces := BencodeNewString(PiecesStr);
  BencodeDictAdd(InfoDict, 'pieces', Pieces);
  
  { Add files list }
  FilesList := BencodeNewList;
  
  File1 := BencodeNewDict;
  BencodeDictAdd(File1, 'length', BencodeNewInteger(50000));
  Path1 := BencodeNewList;
  BencodeListAdd(Path1, BencodeNewString('readme.txt'));
  BencodeDictAdd(File1, 'path', Path1);
  BencodeListAdd(FilesList, File1);
  
  File2 := BencodeNewDict;
  BencodeDictAdd(File2, 'length', BencodeNewInteger(50000));
  Path2 := BencodeNewList;
  BencodeListAdd(Path2, BencodeNewString('data'));
  BencodeListAdd(Path2, BencodeNewString('content.bin'));
  BencodeDictAdd(File2, 'path', Path2);
  BencodeListAdd(FilesList, File2);
  
  BencodeDictAdd(InfoDict, 'files', FilesList);
  BencodeDictAdd(Root, 'info', InfoDict);
  
  { Step 2: Parse the torrent }
  ParseResult := ParseTorrentBencode(Root, Meta);
  TestResult('Parse multi-file torrent succeeds', 
             ParseResult.Success, ParseResult.ErrorMsg);
  
  if not ParseResult.Success then
  begin
    BencodeFree(Root);
    Exit;
  end;
  
  try
    { Step 3: Verify parsed metadata }
    TestResult('Torrent name is "TestContentMulti"', 
               Meta^.Name = 'TestContentMulti');
    TestResult('Piece length is 32768', 
               Meta^.PieceLength = 32768);
    TestResult('Piece count is 4', 
               Meta^.PieceCount = 4);
    TestResult('Is multi-file torrent', 
               not Meta^.IsSingleFile);
    TestResult('File count is 2', 
               Meta^.FileCount = 2);
    TestResult('Total length is 100000', 
               Meta^.TotalLength = 100000);
    TestResult('Tracker URL parsed correctly', 
               Meta^.Announce = 'http://tracker.example.com:8080/announce');
    
    { Step 4: Validate the torrent }
    Valid := ValidateTorrentMeta(Meta, ErrorMsg);
    TestResult('Torrent validation passes', 
               Valid, ErrorMsg);
    
    { Step 5: Compute info hash from the original bencode }
    if not ComputeInfoHashFromBencode(InfoDict, InfoHash) then
    begin
      TestResult('Compute info hash from bencode', False);
      Exit;
    end;
    InfoHashHex := SHA1DigestToHex(InfoHash);
    TestResult('Info hash computed (40 hex chars)', 
               Length(InfoHashHex) = 40);
    TestResult('Info hash is not empty', 
               InfoHashHex <> StringOfChar('0', 40));
    
    { Step 6: Create file manager }
    if FileManagerCreate(Meta, TempDir, FM) then
    begin
      try
        TestResult('FileManager created successfully', True);
        TestResult('FileManager initialized', FM^.Initialized);
        TestResult('VerifiedPieces allocated', FM^.VerifiedPieces <> nil);
        TestResult('Bitfield size correct', 
                   BitfieldBytes(Meta^.PieceCount) = 1);
        
        { Step 7: Preallocate files }
        if FileManagerPreallocateFiles(FM) then
        begin
          TestResult('Files preallocated successfully', True);
          TestResult('Files exist after preallocation', 
                     FileManagerFilesExist(FM));
        end
        else
          TestResult('Files preallocated successfully', False);
        
        FileManagerDestroy(FM);
      except
        TestResult('FileManager operations failed', False);
        FileManagerDestroy(FM);
      end;
    end
    else
      TestResult('FileManager created successfully', False);
    
  finally
    FreeTorrentMeta(Meta);
    BencodeFree(Root);
    
    { Cleanup created files }
    FilesPath := JoinPath(TempDir, 'TestContentMulti');
    DataPath := JoinPath(FilesPath, 'data');
    DeleteFile(JoinPath(FilesPath, 'readme.txt'));
    DeleteFile(JoinPath(DataPath, 'content.bin'));
    {$I-}
    RmDir(DataPath);
    RmDir(FilesPath);
    {$I+}
  end;
end;

{ ============================================================================ }
{ Integration Test 2: Piece Writing and Verification Flow                     }
{ ============================================================================ }

procedure TestPieceVerificationWorkflow;
var
  Root: PBencodeValue;
  InfoDict: PBencodeValue;
  Pieces: PBencodeValue;
  Meta: PTorrentMeta;
  ParseResult: TMetainfoResult;
  FM: PFileManager;
  PieceData: array of Byte;
  ReadBuffer: array of Byte;
  PieceHash: TSHA1Digest;
  IOResult: TIOResult;
  PieceWriteResult: TPieceWriteResult;
  PieceLen: Integer;
  PiecesStr: string;
  I: Integer;
begin
  WriteLn(#10'=== Integration Test: Piece Verification Workflow ===');
  
  { Create a single-piece torrent }
  Root := BencodeNewDict;
  BencodeDictAdd(Root, 'announce', BencodeNewString('http://tracker.test/announce'));
  
  InfoDict := BencodeNewDict;
  BencodeDictAdd(InfoDict, 'name', BencodeNewString('single.bin'));
  PieceLen := 16384;
  BencodeDictAdd(InfoDict, 'piece length', BencodeNewInteger(PieceLen));
  BencodeDictAdd(InfoDict, 'length', BencodeNewInteger(PieceLen));
  
  { Generate piece data and compute real hash }
  SetLength(PieceData, PieceLen);
  for I := 0 to PieceLen - 1 do
    PieceData[I] := I mod 256;
  PieceHash := SHA1Buffer(PieceData[0], PieceLen);
  
  { Create pieces string with actual hash }
  SetLength(PiecesStr, 20);
  Move(PieceHash, PiecesStr[1], 20);
  Pieces := BencodeNewString(PiecesStr);
  BencodeDictAdd(InfoDict, 'pieces', Pieces);
  BencodeDictAdd(Root, 'info', InfoDict);
  
  { Parse and create file manager }
  ParseResult := ParseTorrentBencode(Root, Meta);
  if not ParseResult.Success then
  begin
    TestResult('Parse torrent for piece test', False);
    BencodeFree(Root);
    Exit;
  end;
  
  try
    { Update with computed hash }
    Move(PieceHash, Meta^.Pieces^[0], 20);
    
    if not FileManagerCreate(Meta, TempDir, FM) then
    begin
      TestResult('Create file manager for piece test', False);
      Exit;
    end;
    
    try
      FileManagerInitialize(FM);
      
      { Step 1: Verify piece is not verified initially }
      TestResult('Piece 0 not verified initially', 
                 not FileManagerIsPieceVerified(FM, 0));
      TestResult('Completion is 0%', 
                 FileManagerGetCompletion(FM) = 0.0);
      
      { Step 2: Write piece with correct hash }
      TestResult('Write piece with correct hash succeeds', 
                 FileManagerWritePiece(FM, 0, @PieceData[0], PieceLen, PieceWriteResult) and
                 PieceWriteResult.Success and PieceWriteResult.Verified);
      
      { Step 3: Verify piece is now marked verified }
      TestResult('Piece 0 is verified after write', 
                 FileManagerIsPieceVerified(FM, 0));
      TestResult('Completion is 100%', 
                 FileManagerGetCompletion(FM) = 1.0);
      TestResult('Verified bytes equals total', 
                 FileManagerGetVerifiedBytes(FM) = PieceLen);
      
      { Step 4: Read piece back and verify data }
      SetLength(ReadBuffer, PieceLen);
      if FileManagerReadPiece(FM, 0, @ReadBuffer[0], PieceLen, IOResult) then
      begin
        TestResult('Read piece succeeds', IOResult.Success);
        TestResult('Read data matches written', 
                   CompareMem(@PieceData[0], @ReadBuffer[0], PieceLen));
      end
      else
      begin
        TestResult('Read piece succeeds', False);
      end;
      
      { Step 5: Write piece with wrong hash should fail }
      PieceData[0] := PieceData[0] xor $FF;
      TestResult('Write piece with wrong hash fails', 
                 not FileManagerWritePiece(FM, 0, @PieceData[0], PieceLen, PieceWriteResult));
      
    finally
      FileManagerDestroy(FM);
    end;
    
  finally
    FreeTorrentMeta(Meta);
    BencodeFree(Root);
    DeleteFile(JoinPath(TempDir, 'single.bin'));
  end;
end;

{ ============================================================================ }
{ Integration Test 3: Protocol Message Round-Trip                             }
{ ============================================================================ }

procedure TestProtocolMessageRoundTrip;
var
  Buffer: array[0..65535] of Byte;
  Bitfield: array of Byte;
  PieceData: array of Byte;
  Encoded: TWireMessage;
  Decoded: TWireMessage;
  BytesWritten: Integer;
  BytesConsumed: Integer;
  I: Integer;
begin
  WriteLn(#10'=== Integration Test: Protocol Message Round-Trip ===');
  
  { Test 1: Have message with realistic piece index }
  FillChar(Encoded, SizeOf(Encoded), 0);
  Encoded.Length := 5;
  Encoded.MsgId := MSG_HAVE;
  Encoded.HaveIndex := 12345;
  
  TestResult('Encode Have(12345) succeeds', 
             EncodeMessage(Encoded, @Buffer, SizeOf(Buffer), BytesWritten));
  TestResult('Decode Have succeeds', 
             DecodeMessage(@Buffer, BytesWritten, Decoded, BytesConsumed));
  TestResult('Have piece index preserved', 
             Decoded.HaveIndex = 12345);
  
  { Test 2: Request message with realistic block parameters }
  FillChar(Encoded, SizeOf(Encoded), 0);
  Encoded.Length := 13;
  Encoded.MsgId := MSG_REQUEST;
  Encoded.ReqIndex := 42;
  Encoded.ReqBegin := 16384;
  Encoded.ReqLength := 16384;
  
  TestResult('Encode Request(42, 16384, 16384) succeeds', 
             EncodeMessage(Encoded, @Buffer, SizeOf(Buffer), BytesWritten));
  TestResult('Decode Request succeeds', 
             DecodeMessage(@Buffer, BytesWritten, Decoded, BytesConsumed));
  TestResult('Request index preserved', Decoded.ReqIndex = 42);
  TestResult('Request begin preserved', Decoded.ReqBegin = 16384);
  TestResult('Request length preserved', Decoded.ReqLength = 16384);
  
  { Test 3: Bitfield with realistic size }
  SetLength(Bitfield, 13);
  for I := 0 to 12 do
    Bitfield[I] := I * 17 mod 256;
  
  FillChar(Encoded, SizeOf(Encoded), 0);
  Encoded.Length := 1 + 13;
  Encoded.MsgId := MSG_BITFIELD;
  Encoded.BitfieldData := @Bitfield[0];
  Encoded.BitfieldLen := 13;
  
  TestResult('Encode Bitfield(100 pieces) succeeds', 
             EncodeMessage(Encoded, @Buffer, SizeOf(Buffer), BytesWritten));
  TestResult('Decode Bitfield succeeds', 
             DecodeMessage(@Buffer, BytesWritten, Decoded, BytesConsumed));
  TestResult('Bitfield length preserved', Decoded.BitfieldLen = 13);
  TestResult('Bitfield data matches', 
             CompareMem(@Bitfield[0], Decoded.BitfieldData, 13));
  
  { Test 4: Piece message with realistic 16KB block }
  SetLength(PieceData, 16384);
  for I := 0 to 16383 do
    PieceData[I] := I mod 256;
  
  FillChar(Encoded, SizeOf(Encoded), 0);
  Encoded.Length := 9 + 16384;
  Encoded.MsgId := MSG_PIECE;
  Encoded.PieceIndex := 99;
  Encoded.PieceBegin := 0;
  Encoded.PieceData := @PieceData[0];
  Encoded.PieceDataLen := 16384;
  
  TestResult('Encode Piece(99, 0, 16384 bytes) succeeds', 
             EncodeMessage(Encoded, @Buffer, SizeOf(Buffer), BytesWritten));
  TestResult('Decode Piece succeeds', 
             DecodeMessage(@Buffer, BytesWritten, Decoded, BytesConsumed));
  TestResult('Piece index preserved', Decoded.PieceIndex = 99);
  TestResult('Piece begin preserved', Decoded.PieceBegin = 0);
  TestResult('Piece data length preserved', Decoded.PieceDataLen = 16384);
  TestResult('Piece data matches', 
             CompareMem(@PieceData[0], Decoded.PieceData, 16384));
end;

{ ============================================================================ }
{ Integration Test 4: Error Handling and Edge Cases                           }
{ ============================================================================ }

procedure TestErrorHandlingAndEdgeCases;
var
  Root: PBencodeValue;
  InfoDict: PBencodeValue;
  Pieces: PBencodeValue;
  Meta: PTorrentMeta;
  ParseResult: TMetainfoResult;
  FM: PFileManager;
  IOResult: TIOResult;
  Buffer: array[0..1023] of Byte;
  Valid: Boolean;
  ErrorMsg: string;
  PiecesStr: string;
  I: Integer;
begin
  WriteLn(#10'=== Integration Test: Error Handling and Edge Cases ===');
  
  { Test 1: Malformed torrent (negative piece length) }
  Root := BencodeNewDict;
  InfoDict := BencodeNewDict;
  BencodeDictAdd(InfoDict, 'name', BencodeNewString('bad.torrent'));
  BencodeDictAdd(InfoDict, 'piece length', BencodeNewInteger(-1));
  BencodeDictAdd(InfoDict, 'length', BencodeNewInteger(100));
  BencodeDictAdd(InfoDict, 'pieces', BencodeNewStringBuf('12345678901234567890', 20));
  BencodeDictAdd(Root, 'info', InfoDict);
  
  ParseResult := ParseTorrentBencode(Root, Meta);
  TestResult('Negative piece length rejected', 
             not ParseResult.Success);
  BencodeFree(Root);
  
  { Test 2: Empty pieces }
  Root := BencodeNewDict;
  InfoDict := BencodeNewDict;
  BencodeDictAdd(InfoDict, 'name', BencodeNewString('bad.torrent'));
  BencodeDictAdd(InfoDict, 'piece length', BencodeNewInteger(32768));
  BencodeDictAdd(InfoDict, 'length', BencodeNewInteger(100));
  BencodeDictAdd(InfoDict, 'pieces', BencodeNewString(''));
  BencodeDictAdd(Root, 'info', InfoDict);
  
  ParseResult := ParseTorrentBencode(Root, Meta);
  TestResult('Empty pieces rejected', 
             not ParseResult.Success);
  BencodeFree(Root);
  
  { Test 3: Invalid piece index operations }
  Root := BencodeNewDict;
  InfoDict := BencodeNewDict;
  BencodeDictAdd(InfoDict, 'name', BencodeNewString('test.txt'));
  BencodeDictAdd(InfoDict, 'piece length', BencodeNewInteger(32768));
  BencodeDictAdd(InfoDict, 'length', BencodeNewInteger(65536));
  SetLength(PiecesStr, 40);
  FillChar(PiecesStr[1], 40, 0);
  for I := 0 to 1 do
    PiecesStr[I * 20 + 1] := Chr(I + 1);
  Pieces := BencodeNewString(PiecesStr);
  BencodeDictAdd(InfoDict, 'pieces', Pieces);
  BencodeDictAdd(Root, 'info', InfoDict);
  
  ParseResult := ParseTorrentBencode(Root, Meta);
  if ParseResult.Success then
  begin
    if FileManagerCreate(Meta, TempDir, FM) then
    begin
      try
        TestResult('Negative piece index rejected', 
                   not FileManagerIsPieceVerified(FM, -1));
        TestResult('Large piece index rejected', 
                   not FileManagerIsPieceVerified(FM, 1000));
        TestResult('Read invalid piece fails', 
                   not FileManagerReadPiece(FM, 100, @Buffer[0], 1024, IOResult));
      finally
        FileManagerDestroy(FM);
      end;
    end;
    FreeTorrentMeta(Meta);
  end;
  BencodeFree(Root);
end;

{ ============================================================================ }
{ Integration Test 5: Statistics and Progress Tracking                         }
{ ============================================================================ }

procedure TestStatisticsAndProgress;
var
  Root: PBencodeValue;
  InfoDict: PBencodeValue;
  Pieces: PBencodeValue;
  Meta: PTorrentMeta;
  ParseResult: TMetainfoResult;
  FM: PFileManager;
  I: Integer;
  PiecesStr: string;
  Completion: Double;
  ReadBytes: QWord;
  WrittenBytes: QWord;
  ReadOps: Cardinal;
  WriteOps: Cardinal;
  Errors: Cardinal;
begin
  WriteLn(#10'=== Integration Test: Statistics and Progress ===');
  
  { Create torrent with 10 pieces }
  Root := BencodeNewDict;
  InfoDict := BencodeNewDict;
  BencodeDictAdd(InfoDict, 'name', BencodeNewString('progress_test.bin'));
  BencodeDictAdd(InfoDict, 'piece length', BencodeNewInteger(16384));
  BencodeDictAdd(InfoDict, 'length', BencodeNewInteger(163840));
  
  SetLength(PiecesStr, 10 * 20);
  FillChar(PiecesStr[1], 10 * 20, 0);
  for I := 0 to 9 do
    PiecesStr[I * 20 + 1] := Chr(I + 1);
  Pieces := BencodeNewString(PiecesStr);
  BencodeDictAdd(InfoDict, 'pieces', Pieces);
  BencodeDictAdd(Root, 'info', InfoDict);
  
  ParseResult := ParseTorrentBencode(Root, Meta);
  if not ParseResult.Success then
  begin
    TestResult('Parse torrent for progress test', False);
    BencodeFree(Root);
    Exit;
  end;
  
  try
    if not FileManagerCreate(Meta, TempDir, FM) then
    begin
      TestResult('Create file manager for progress test', False);
      Exit;
    end;
    
    try
      FileManagerInitialize(FM);
      
      { Test initial state }
      TestResult('Initial completion is 0%', 
                 FileManagerGetCompletion(FM) = 0.0);
      TestResult('Initial verified bytes is 0', 
                 FileManagerGetVerifiedBytes(FM) = 0);
      
      { Mark pieces as verified and check progress }
      for I := 0 to 9 do
      begin
        FileManagerMarkPieceVerified(FM, I);
        Completion := FileManagerGetCompletion(FM);
        TestResult(Format('After piece %d, completion is %d%%', [I, (I + 1) * 10]),
                   Abs(Completion - ((I + 1) / 10)) < 0.01);
      end;
      
      TestResult('Final completion is 100%', 
                 FileManagerGetCompletion(FM) = 1.0);
      TestResult('Final verified bytes equals total', 
                 FileManagerGetVerifiedBytes(FM) = Meta^.TotalLength);
      
      { Test statistics counters }
      FileManagerGetStats(FM, ReadBytes, WrittenBytes, ReadOps, WriteOps, Errors);
      TestResult('Stats ReadBytes is 0 (no reads)', ReadBytes = 0);
      TestResult('Stats ReadOps is 0', ReadOps = 0);
      TestResult('Stats Errors is 0', Errors = 0);
      
    finally
      FileManagerDestroy(FM);
    end;
    
  finally
    FreeTorrentMeta(Meta);
    BencodeFree(Root);
    DeleteFile(JoinPath(TempDir, 'progress_test.bin'));
  end;
end;

{ ============================================================================ }
{ Main Program                                                                 }
{ ============================================================================ }

begin
  WriteLn('==============================================');
  WriteLn('  PASCALTORRENT INTEGRATION TESTS');
  WriteLn('==============================================');
  
  { Setup temp directory }
  TempDir := GetTempDir + 'pascaltorrent_int_test' + PathDelim;
  if not DirExists(TempDir) then
    MakeDir(TempDir);
  
  try
    { Run integration tests }
    TestTorrentLoadingWorkflow;
    TestPieceVerificationWorkflow;
    TestProtocolMessageRoundTrip;
    TestErrorHandlingAndEdgeCases;
    TestStatisticsAndProgress;
    
    { Summary }
    WriteLn(#10'==============================================');
    WriteLn('  RESULTS: ', PassedTests, '/', TotalTests, ' tests passed');
    WriteLn('==============================================');
    
    if PassedTests < TotalTests then
    begin
      WriteLn('FAILED: ', TotalTests - PassedTests, ' tests failed');
      Halt(1);
    end
    else
      WriteLn('SUCCESS: All integration tests passed!');
      
  finally
    { Cleanup temp directory }
    {$I-}
    RmDir(TempDir);
    {$I+}
  end;
end.
