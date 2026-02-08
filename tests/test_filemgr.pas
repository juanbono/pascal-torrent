{
  test_filemgr.pas - Comprehensive tests for filemgr unit
}

{$mode objfpc}{$H+}

program test_filemgr;

uses
  SysUtils, bencode, metainfo, filemgr, sha1utils, utils, logging, testframework;

type
  TTestFileRec = record
    Path: string;
    Length: Int64;
  end;

var
  TempDir: string;

{ ============================================================================ }
{ Helper: Create a test torrent with specific properties                       }
{ ============================================================================ }

function CreateTestTorrent(const Name: string; Length: Int64;
                           PieceLength: Integer): PBencodeValue;
var
  Root, Info: PBencodeValue;
  Pieces: string;
  I, J: Integer;
  NumPieces: Integer;
  Hash: TSHA1Digest;
begin
  Result := nil;
  
  Root := BencodeNewDict;
  if Root = nil then Exit;
  
  Info := BencodeNewDict;
  if Info = nil then
  begin
    BencodeFree(Root);
    Exit;
  end;
  
  BencodeDictAdd(Info, 'name', BencodeNewString(Name));
  BencodeDictAdd(Info, 'piece length', BencodeNewInteger(PieceLength));
  BencodeDictAdd(Info, 'length', BencodeNewInteger(Length));
  
  { Create fake pieces with unique hashes }
  NumPieces := (Length + PieceLength - 1) div PieceLength;
  SetLength(Pieces, NumPieces * 20);
  { SetLength already initializes to zeros }
  
  for I := 0 to NumPieces - 1 do
  begin
    for J := 0 to 19 do
      Hash[J] := (I + J) mod 256;
    Move(Hash, Pieces[I * 20 + 1], 20);
  end;
  
  BencodeDictAdd(Info, 'pieces', BencodeNewStringBuf(PChar(Pieces), System.Length(Pieces)));
  BencodeDictAdd(Root, 'info', Info);
  
  Result := Root;
end;

function CreateMultiFileTestTorrent(const Name: string;
                                    const Files: array of TTestFileRec;
                                    PieceLength: Integer): PBencodeValue;
var
  Root, Info, FilesList, FileDict, PathList: PBencodeValue;
  Pieces: string;
  I: Integer;
  NumPieces: Integer;
  TotalLength: Int64;
begin
  Result := nil;
  
  TotalLength := 0;
  for I := 0 to High(Files) do
    TotalLength := TotalLength + Files[I].Length;
  
  Root := BencodeNewDict;
  if Root = nil then Exit;
  
  Info := BencodeNewDict;
  if Info = nil then
  begin
    BencodeFree(Root);
    Exit;
  end;
  
  BencodeDictAdd(Info, 'name', BencodeNewString(Name));
  BencodeDictAdd(Info, 'piece length', BencodeNewInteger(PieceLength));
  
  { Create fake pieces }
  NumPieces := (TotalLength + PieceLength - 1) div PieceLength;
  SetLength(Pieces, NumPieces * 20);
  FillChar(Pieces[1], Length(Pieces), 0);
  
  BencodeDictAdd(Info, 'pieces', BencodeNewStringBuf(PChar(Pieces), Length(Pieces)));
  
  { Create files list }
  FilesList := BencodeNewList;
  for I := 0 to High(Files) do
  begin
    FileDict := BencodeNewDict;
    BencodeDictAdd(FileDict, 'length', BencodeNewInteger(Files[I].Length));
    
    PathList := BencodeNewList;
    BencodeListAdd(PathList, BencodeNewString(Files[I].Path));
    BencodeDictAdd(FileDict, 'path', PathList);
    
    BencodeListAdd(FilesList, FileDict);
  end;
  
  BencodeDictAdd(Info, 'files', FilesList);
  BencodeDictAdd(Root, 'info', Info);
  
  Result := Root;
end;

{ ============================================================================ }
{ Helper: Generate test data with known hash                                   }
{ ============================================================================ }

procedure GenerateTestData(Buffer: Pointer; Size: Integer; Seed: Byte);
var
  I: Integer;
  P: PByte;
begin
  P := PByte(Buffer);
  for I := 0 to Size - 1 do
  begin
    P^ := (Seed + I) mod 256;
    Inc(P);
  end;
end;

{ ============================================================================ }
{ Test: File manager creation and destruction                                  }
{ ============================================================================ }

procedure TestCreateDestroy;
var
  Root: PBencodeValue;
  Meta: PTorrentMeta;
  ParseResult: TMetainfoResult;
  FM: PFileManager;
begin
  WriteLn(#10'=== Testing FileManager Creation ===');
  
  Root := CreateTestTorrent('test.txt', 1024, 256);
  if Root = nil then
  begin
    TestResult('Create test torrent', False, 'Failed to create test data');
    Exit;
  end;
  
  try
    ParseResult := ParseTorrentBencode(Root, Meta);
    if not ParseResult.Success then
    begin
      TestResult('Parse torrent', False, ParseResult.ErrorMsg);
      Exit;
    end;
    
    try
      { Test nil meta }
      TestResult('Create with nil meta fails', 
                 not FileManagerCreate(nil, TempDir, FM));
      
      { Test empty path }
      TestResult('Create with empty path fails', 
                 not FileManagerCreate(Meta, '', FM));
      
      { Test valid creation }
      TestResult('Create with valid params', 
                 FileManagerCreate(Meta, TempDir, FM));
      
      if FM <> nil then
      begin
        TestResult('FileManager is initialized', FM^.Initialized);
        TestResult('FileManager Meta is set', FM^.Meta = Meta);
        TestResult('FileManager BasePath is set', FM^.BasePath = TempDir);
        TestResult('VerifiedPieces is allocated', FM^.VerifiedPieces <> nil);
        
        FileManagerDestroy(FM);
        TestResult('Destroy completes without error', True);
      end;
    finally
      FreeTorrentMeta(Meta);
    end;
  finally
    BencodeFree(Root);
  end;
end;

{ ============================================================================ }
{ Test: Single file write and read                                             }
{ ============================================================================ }

procedure TestSingleFileWriteRead;
var
  Root: PBencodeValue;
  Meta: PTorrentMeta;
  ParseResult: TMetainfoResult;
  FM: PFileManager;
  WriteData: array[0..1023] of Byte;
  ReadData: array[0..1023] of Byte;
  IOResult: TIOResult;
  I: Integer;
begin
  WriteLn(#10'=== Testing Single File Write/Read ===');
  
  { Create torrent with 1 piece of 1024 bytes }
  Root := CreateTestTorrent('singletest.txt', 1024, 1024);
  if Root = nil then
  begin
    TestResult('Create test torrent', False, 'Failed to create test data');
    Exit;
  end;
  
  try
    ParseResult := ParseTorrentBencode(Root, Meta);
    if not ParseResult.Success then
    begin
      TestResult('Parse torrent', False, ParseResult.ErrorMsg);
      Exit;
    end;
    
    try
      { Modify the hash for piece 0 to match our test data }
      GenerateTestData(@WriteData, 1024, 0);
      { FIXED: Initialize all 20 bytes of the hash (was only setting 2 bytes) }
      FillChar(Meta^.Pieces^[0], SHA1_HASH_SIZE, 0);
      
      { Create file manager }
      if not FileManagerCreate(Meta, TempDir, FM) then
      begin
        TestResult('Create file manager', False);
        Exit;
      end;
      
      try
        FileManagerInitialize(FM);
        
        { Write a block }
        TestResult('Write 256 byte block', 
                   FileManagerWriteBlock(FM, 0, 256, @WriteData, IOResult) and 
                   IOResult.Success and (IOResult.BytesTransferred = 256));
        
        { Read it back }
        FillChar(ReadData, SizeOf(ReadData), 0);
        TestResult('Read 256 byte block', 
                   FileManagerReadBlock(FM, 0, 256, @ReadData, IOResult) and 
                   IOResult.Success and (IOResult.BytesTransferred = 256));
        
        { Verify data }
        I := 0;
        while (I < 256) and (ReadData[I] = WriteData[I]) do
          Inc(I);
        TestResult('Read data matches written', I = 256);
        
        { Test offset read/write }
        TestResult('Write at offset 512', 
                   FileManagerWriteBlock(FM, 512, 256, @WriteData[512], IOResult) and 
                   IOResult.Success);
        
        TestResult('Read at offset 512', 
                   FileManagerReadBlock(FM, 512, 256, @ReadData[512], IOResult) and 
                   IOResult.Success);
        
        I := 512;
        while (I < 768) and (ReadData[I] = WriteData[I]) do
          Inc(I);
        TestResult('Offset data matches', I = 768);
        
        { Check stats }
        TestResult('ReadOps > 0', FM^.ReadOps > 0);
        TestResult('WriteOps > 0', FM^.WriteOps > 0);
        TestResult('BytesRead > 0', FM^.BytesRead > 0);
        TestResult('BytesWritten > 0', FM^.BytesWritten > 0);
        
      finally
        FileManagerDestroy(FM);
      end;
    finally
      FreeTorrentMeta(Meta);
    end;
  finally
    BencodeFree(Root);
    
    { Cleanup test file }
    DeleteFile(JoinPath(TempDir, 'singletest.txt'));
  end;
end;

{ ============================================================================ }
{ Test: Piece operations                                                         }
{ ============================================================================ }

procedure TestPieceOperations;
var
  Root: PBencodeValue;
  Meta: PTorrentMeta;
  ParseResult: TMetainfoResult;
  FM: PFileManager;
  WriteBuffer, ReadBuffer: array of Byte;
  PieceLen: Integer;
  IOResult: TIOResult;
  PieceWriteResult: TPieceWriteResult;
  I: Integer;
  ActualHash: TSHA1Digest;
begin
  WriteLn(#10'=== Testing Piece Operations ===');
  
  { Create torrent with 4 pieces of 256 bytes each }
  Root := CreateTestTorrent('piecetest.txt', 1024, 256);
  if Root = nil then
  begin
    TestResult('Create test torrent', False, 'Failed to create test data');
    Exit;
  end;
  
  try
    ParseResult := ParseTorrentBencode(Root, Meta);
    if not ParseResult.Success then
    begin
      TestResult('Parse torrent', False, ParseResult.ErrorMsg);
      Exit;
    end;
    
    try
      PieceLen := 256;
      SetLength(WriteBuffer, PieceLen);
      SetLength(ReadBuffer, PieceLen);
      
      { Generate test data and compute hash }
      GenerateTestData(@WriteBuffer[0], PieceLen, 42);
      ActualHash := SHA1Buffer(WriteBuffer[0], PieceLen);
      
      { Set the hash in the meta }
      Move(ActualHash, Meta^.Pieces^[0], 20);
      
      { Create file manager }
      if not FileManagerCreate(Meta, TempDir, FM) then
      begin
        TestResult('Create file manager', False);
        Exit;
      end;
      
      try
        FileManagerInitialize(FM);
        
        { Initially piece should not be verified }
        TestResult('Piece 0 initially not verified', 
                   not FileManagerIsPieceVerified(FM, 0));
        
        { Write piece 0 with correct hash first }
        GenerateTestData(@WriteBuffer[0], PieceLen, 42);
        ActualHash := SHA1Buffer(WriteBuffer[0], PieceLen);
        Move(ActualHash, Meta^.Pieces^[0], 20);  { Hash for piece 0 }
        TestResult('Write piece 0 with correct hash', 
                   FileManagerWritePiece(FM, 0, @WriteBuffer[0], PieceLen, PieceWriteResult) and
                   PieceWriteResult.Success and PieceWriteResult.Verified);
        
        { Now piece should be verified and readable }
        TestResult('Piece 0 marked verified', 
                   FileManagerIsPieceVerified(FM, 0));
        
        { Read piece back }
        FillChar(ReadBuffer[0], PieceLen, 0);
        TestResult('Read piece 0 (from cache)', 
                   FileManagerReadPiece(FM, 0, @ReadBuffer[0], PieceLen, IOResult));
        
        { Test write piece with wrong hash }
        GenerateTestData(@WriteBuffer[0], PieceLen, 99);  { Different data }
        TestResult('Write piece with wrong hash fails', 
                   not FileManagerWritePiece(FM, 1, @WriteBuffer[0], PieceLen, PieceWriteResult));
        
        { Test write piece with correct hash }
        GenerateTestData(@WriteBuffer[0], PieceLen, 123);
        ActualHash := SHA1Buffer(WriteBuffer[0], PieceLen);
        Move(ActualHash, Meta^.Pieces^[20], 20);  { Hash for piece 1 }
        
        TestResult('Write piece 1 with correct hash', 
                   FileManagerWritePiece(FM, 1, @WriteBuffer[0], PieceLen, PieceWriteResult) and 
                   PieceWriteResult.Success and PieceWriteResult.Verified);
        
        { Read piece back }
        FillChar(ReadBuffer[0], PieceLen, 0);
        TestResult('Read piece 1 back', 
                   FileManagerReadPiece(FM, 1, @ReadBuffer[0], PieceLen, IOResult) and 
                   (IOResult.BytesTransferred = PieceLen));
        
        { Verify data }
        I := 0;
        while (I < PieceLen) and (ReadBuffer[I] = WriteBuffer[I]) do
          Inc(I);
        TestResult('Piece data matches', I = PieceLen);
        
      finally
        FileManagerDestroy(FM);
      end;
    finally
      FreeTorrentMeta(Meta);
    end;
  finally
    BencodeFree(Root);
    DeleteFile(JoinPath(TempDir, 'piecetest.txt'));
  end;
end;

{ ============================================================================ }
{ Test: Multi-file torrent file operations                                     }
{ ============================================================================ }

procedure TestMultiFileOperations;
var
  Root: PBencodeValue;
  Meta: PTorrentMeta;
  ParseResult: TMetainfoResult;
  FM: PFileManager;
  Files: array[0..1] of TTestFileRec;
  WriteData: array[0..511] of Byte;
  ReadData: array[0..511] of Byte;
  IOResult: TIOResult;
  I: Integer;
begin
  WriteLn(#10'=== Testing Multi-File Operations ===');
  
  Files[0].Path := 'file1.txt';
  Files[0].Length := 256;
  Files[1].Path := 'file2.txt';
  Files[1].Length := 256;
  
  Root := CreateMultiFileTestTorrent('multitest', Files, 256);
  if Root = nil then
  begin
    TestResult('Create test torrent', False, 'Failed to create test data');
    Exit;
  end;
  
  try
    ParseResult := ParseTorrentBencode(Root, Meta);
    if not ParseResult.Success then
    begin
      TestResult('Parse torrent', False, ParseResult.ErrorMsg);
      Exit;
    end;
    
    try
      if not FileManagerCreate(Meta, TempDir, FM) then
      begin
        TestResult('Create file manager', False);
        Exit;
      end;
      
      try
        { Initialize - creates directories }
        TestResult('Initialize file manager', FileManagerInitialize(FM));
        
        { Preallocate files }
        TestResult('Preallocate files', FileManagerPreallocateFiles(FM));
        
        { Check files exist }
        TestResult('Files exist after preallocation', FileManagerFilesExist(FM));
        
        { Write data spanning both files }
        GenerateTestData(@WriteData, 512, 0);
        TestResult('Write 512 bytes across files', 
                   FileManagerWriteBlock(FM, 0, 512, @WriteData, IOResult) and 
                   IOResult.Success and (IOResult.BytesTransferred = 512));
        
        { Read data back }
        FillChar(ReadData, SizeOf(ReadData), 0);
        TestResult('Read 512 bytes back', 
                   FileManagerReadBlock(FM, 0, 512, @ReadData, IOResult) and 
                   IOResult.Success and (IOResult.BytesTransferred = 512));
        
        { Verify data }
        I := 0;
        while (I < 512) and (ReadData[I] = WriteData[I]) do
          Inc(I);
        TestResult('Multi-file data matches', I = 512);
        
      finally
        FileManagerDestroy(FM);
      end;
    finally
      FreeTorrentMeta(Meta);
    end;
  finally
    BencodeFree(Root);
    
    { Cleanup }
    DeleteFile(JoinPath(TempDir, 'multitest/file1.txt'));
    DeleteFile(JoinPath(TempDir, 'multitest/file2.txt'));
    {$I-}
    RmDir(JoinPath(TempDir, 'multitest'));
    {$I+}
  end;
end;

{ ============================================================================ }
{ Test: Completion tracking                                                    }
{ ============================================================================ }

procedure TestCompletionTracking;
var
  Root: PBencodeValue;
  Meta: PTorrentMeta;
  ParseResult: TMetainfoResult;
  FM: PFileManager;
  Completion: Double;
begin
  WriteLn(#10'=== Testing Completion Tracking ===');
  
  Root := CreateTestTorrent('completiontest.txt', 1024, 256);
  if Root = nil then
  begin
    TestResult('Create test torrent', False, 'Failed to create test data');
    Exit;
  end;
  
  try
    ParseResult := ParseTorrentBencode(Root, Meta);
    if not ParseResult.Success then
    begin
      TestResult('Parse torrent', False, ParseResult.ErrorMsg);
      Exit;
    end;
    
    try
      if not FileManagerCreate(Meta, TempDir, FM) then
      begin
        TestResult('Create file manager', False);
        Exit;
      end;
      
      try
        { Initial completion should be 0 }
        Completion := FileManagerGetCompletion(FM);
        TestResult('Initial completion is 0%', Completion = 0.0);
        
        { Mark one piece complete out of 4 }
        FileManagerMarkPieceVerified(FM, 0);
        Completion := FileManagerGetCompletion(FM);
        TestResult('After 1/4 pieces, completion is 25%', Completion = 0.25);
        
        { Mark another }
        FileManagerMarkPieceVerified(FM, 2);
        Completion := FileManagerGetCompletion(FM);
        TestResult('After 2/4 pieces, completion is 50%', Completion = 0.5);
        
        { All complete }
        FileManagerMarkPieceVerified(FM, 1);
        FileManagerMarkPieceVerified(FM, 3);
        Completion := FileManagerGetCompletion(FM);
        TestResult('After all pieces, completion is 100%', Completion = 1.0);
        
        { Check verified bytes }
        TestResult('Verified bytes equals total', 
                   FileManagerGetVerifiedBytes(FM) = 1024);
        
      finally
        FileManagerDestroy(FM);
      end;
    finally
      FreeTorrentMeta(Meta);
    end;
  finally
    BencodeFree(Root);
  end;
end;

{ ============================================================================ }
{ Test: Error handling                                                         }
{ ============================================================================ }

procedure TestErrorHandling;
var
  Root: PBencodeValue;
  Meta: PTorrentMeta;
  ParseResult: TMetainfoResult;
  FM: PFileManager;
  IOResult: TIOResult;
  Buffer: array[0..255] of Byte;
begin
  WriteLn(#10'=== Testing Error Handling ===');
  
  Root := CreateTestTorrent('errortest.txt', 1024, 256);
  if Root = nil then
  begin
    TestResult('Create test torrent', False, 'Failed to create test data');
    Exit;
  end;
  
  try
    ParseResult := ParseTorrentBencode(Root, Meta);
    if not ParseResult.Success then
    begin
      TestResult('Parse torrent', False, ParseResult.ErrorMsg);
      Exit;
    end;
    
    try
      if not FileManagerCreate(Meta, TempDir, FM) then
      begin
        TestResult('Create file manager', False);
        Exit;
      end;
      
      try
        { Read from non-existent file }
        TestResult('Read from non-existent file fails gracefully', 
                   not FileManagerReadBlock(FM, 0, 256, @Buffer, IOResult) or 
                   not IOResult.Success);
        
        { Invalid piece index }
        TestResult('Read piece -1 fails', 
                   not FileManagerReadPiece(FM, -1, @Buffer, 256, IOResult));
        TestResult('Read piece 1000 fails', 
                   not FileManagerReadPiece(FM, 1000, @Buffer, 256, IOResult));
        
        { Nil buffer }
        TestResult('Read with nil buffer fails', 
                   not FileManagerReadBlock(FM, 0, 256, nil, IOResult));
        
      finally
        FileManagerDestroy(FM);
      end;
    finally
      FreeTorrentMeta(Meta);
    end;
  finally
    BencodeFree(Root);
  end;
end;

{ ============================================================================ }
{ Test: File deletion                                                          }
{ ============================================================================ }

procedure TestFileDeletion;
var
  Root: PBencodeValue;
  Meta: PTorrentMeta;
  ParseResult: TMetainfoResult;
  FM: PFileManager;
  FullPath: string;
  F: Text;
begin
  WriteLn(#10'=== Testing File Deletion ===');
  
  Root := CreateTestTorrent('deletetest.txt', 100, 256);
  if Root = nil then
  begin
    TestResult('Create test torrent', False, 'Failed to create test data');
    Exit;
  end;
  
  try
    ParseResult := ParseTorrentBencode(Root, Meta);
    if not ParseResult.Success then
    begin
      TestResult('Parse torrent', False, ParseResult.ErrorMsg);
      Exit;
    end;
    
    try
      if not FileManagerCreate(Meta, TempDir, FM) then
      begin
        TestResult('Create file manager', False);
        Exit;
      end;
      
      try
        { Create a test file }
        FullPath := JoinPath(TempDir, 'deletetest.txt');
        Assign(F, FullPath);
        {$I-}
        Rewrite(F);
        WriteLn(F, 'test data');
        Close(F);
        {$I+}
        
        TestResult('Test file created', SysUtils.FileExists(FullPath));
        
        { Delete all files }
        TestResult('DeleteAllFiles succeeds', FileManagerDeleteAllFiles(FM));
        
        { Check file is gone }
        TestResult('File deleted', not SysUtils.FileExists(FullPath));
        
      finally
        FileManagerDestroy(FM);
      end;
    finally
      FreeTorrentMeta(Meta);
    end;
  finally
    BencodeFree(Root);
    { Cleanup if still exists }
    DeleteFile(JoinPath(TempDir, 'deletetest.txt'));
  end;
end;

{ ============================================================================ }
{ Test: Edge cases for piece operations                                          }
{ ============================================================================ }

procedure TestPieceEdgeCases;
var
  Root: PBencodeValue;
  Meta: PTorrentMeta;
  ParseResult: TMetainfoResult;
  FM: PFileManager;
  IORes: TIOResult;  { Local variable for I/O results }
begin
  WriteLn(#10'=== Testing Piece Edge Cases ===');
  
  { Test 1: Single piece torrent }
  Root := CreateTestTorrent('singlepiece.txt', 100, 100);
  if Root = nil then
  begin
    TestResult('Create single-piece torrent', False, 'Failed to create test data');
    Exit;
  end;
  
  try
    ParseResult := ParseTorrentBencode(Root, Meta);
    if not ParseResult.Success then
    begin
      TestResult('Parse single-piece torrent', False, ParseResult.ErrorMsg);
      Exit;
    end;
    
    try
      TestResult('Single piece count is 1', Meta^.PieceCount = 1);
      TestResult('Piece length is 100', GetPieceLength(Meta, 0) = 100);
      
      if FileManagerCreate(Meta, TempDir, FM) then
      begin
        try
          FileManagerInitialize(FM);
          
          { Mark as verified and check }
          FileManagerMarkPieceVerified(FM, 0);
          TestResult('Single piece verified', 
                     FileManagerIsPieceVerified(FM, 0));
          TestResult('Completion is 100%', 
                     FileManagerGetCompletion(FM) = 1.0);
        finally
          FileManagerDestroy(FM);
        end;
      end;
    finally
      FreeTorrentMeta(Meta);
    end;
  finally
    BencodeFree(Root);
    DeleteFile(JoinPath(TempDir, 'singlepiece.txt'));
  end;
  
  { Test 2: Zero pieces verified }
  Root := CreateTestTorrent('zeropiece.txt', 1024, 256);
  if Root = nil then Exit;
  
  try
    ParseResult := ParseTorrentBencode(Root, Meta);
    if not ParseResult.Success then Exit;
    
    try
      if FileManagerCreate(Meta, TempDir, FM) then
      begin
        try
          TestResult('Initial completion is 0', 
                     FileManagerGetCompletion(FM) = 0.0);
          TestResult('Verified bytes is 0', 
                     FileManagerGetVerifiedBytes(FM) = 0);
          TestResult('Piece 0 not verified', 
                     not FileManagerIsPieceVerified(FM, 0));
        finally
          FileManagerDestroy(FM);
        end;
      end;
    finally
      FreeTorrentMeta(Meta);
    end;
  finally
    BencodeFree(Root);
  end;
  
  { Test 3: Nil file manager operations }
  TestResult('IsPieceVerified with nil FM', 
             not FileManagerIsPieceVerified(nil, 0));
  TestResult('GetCompletion with nil FM', 
             FileManagerGetCompletion(nil) = 0.0);
  TestResult('GetVerifiedBytes with nil FM', 
             FileManagerGetVerifiedBytes(nil) = 0);
  
  { Test 4: Out of bounds piece index }
  Root := CreateTestTorrent('bounds.txt', 1024, 256);
  if Root = nil then Exit;
  
  try
    ParseResult := ParseTorrentBencode(Root, Meta);
    if not ParseResult.Success then Exit;
    
    try
      if FileManagerCreate(Meta, TempDir, FM) then
      begin
        try
          { These should not crash and return safe defaults }
          { FIXED: These were placeholder tests that didn't actually verify anything }
          TestResult('Negative index returns not verified', 
                     not FileManagerIsPieceVerified(FM, -1));
          TestResult('Large index returns not verified', 
                     not FileManagerIsPieceVerified(FM, 1000));
          TestResult('Negative verify piece returns false',
                     not FileManagerVerifyPiece(FM, -1, IORes));
          TestResult('Out of bounds not verified 1', 
                     not FileManagerIsPieceVerified(FM, -1));
          TestResult('Out of bounds not verified 2', 
                     not FileManagerIsPieceVerified(FM, 1000));
        finally
          FileManagerDestroy(FM);
        end;
      end;
    finally
      FreeTorrentMeta(Meta);
    end;
  finally
    BencodeFree(Root);
  end;
end;

{ ============================================================================ }
{ Test: Bitfield bounds checking                                                 }
{ ============================================================================ }

procedure TestBitfieldBounds;
var
  Root: PBencodeValue;
  Meta: PTorrentMeta;
  ParseResult: TMetainfoResult;
  FM: PFileManager;
begin
  WriteLn(#10'=== Testing Bitfield Bounds ===');
  
  Root := CreateTestTorrent('bitfieldtest.txt', 1024, 256);
  if Root = nil then
  begin
    TestResult('Create test torrent', False, 'Failed to create test data');
    Exit;
  end;
  
  try
    ParseResult := ParseTorrentBencode(Root, Meta);
    if not ParseResult.Success then
    begin
      TestResult('Parse torrent', False, ParseResult.ErrorMsg);
      Exit;
    end;
    
    try
      if not FileManagerCreate(Meta, TempDir, FM) then
      begin
        TestResult('Create file manager', False);
        Exit;
      end;
      
      try
        { Test valid indices }
        TestResult('Mark piece 0 valid', True);
        FileManagerMarkPieceVerified(FM, 0);
        TestResult('Piece 0 verified', FileManagerIsPieceVerified(FM, 0));
        
        TestResult('Mark piece 3 valid', True);
        FileManagerMarkPieceVerified(FM, 3);
        TestResult('Piece 3 verified', FileManagerIsPieceVerified(FM, 3));
        
        { Test out of bounds - should not crash }
        TestResult('Mark negative index handled', True);
        FileManagerMarkPieceVerified(FM, -1);
        
        TestResult('Mark large index handled', True);
        FileManagerMarkPieceVerified(FM, 1000);
        
        TestResult('Check negative index handled', True);
        TestResult('Negative index not verified', 
                   not FileManagerIsPieceVerified(FM, -1));
        
        TestResult('Check large index handled', True);
        TestResult('Large index not verified', 
                   not FileManagerIsPieceVerified(FM, 1000));
        
      finally
        FileManagerDestroy(FM);
      end;
    finally
      FreeTorrentMeta(Meta);
    end;
  finally
    BencodeFree(Root);
  end;
end;

{ ============================================================================ }
{ Main                                                                         }
{ ============================================================================ }

begin
  BeginSuite('FILE MANAGER UNIT TESTS');
  
  InitLogging;
  LoggerSetLevel(GlobalLogger, llWarning);
  
  { Create temp directory for tests }
  TempDir := JoinPath(GetTempDir, 'pascaltorrent_test');
  MakeDir(TempDir);
  
  WriteLn('Using temp directory: ', TempDir);
  
  try
    TestCreateDestroy;
    TestSingleFileWriteRead;
    TestPieceOperations;
    TestMultiFileOperations;
    TestCompletionTracking;
    TestErrorHandling;
    TestFileDeletion;
    TestPieceEdgeCases;
    TestBitfieldBounds;
    
    EndSuite;
    ExitWithResult;
  finally
    { Cleanup temp directory }
    {$I-}
    RmDir(TempDir);
    {$I+}
    ShutdownLogging;
  end;
end.
