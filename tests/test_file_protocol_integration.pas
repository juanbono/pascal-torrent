{
  test_file_protocol_integration.pas - File Manager + Protocol Integration Tests
  
  Tests integration between:
  - Protocol message handling and file operations
  - Piece writing via protocol and verification
  - Block-level operations
  - Concurrent piece operations
}

program test_file_protocol_integration;

{$mode objfpc}{$H+}

uses
  SysUtils, bencode, metainfo, filemgr, protocol, sha1utils, utils,
  testframework;

var
  TempDir: string;

{ ============================================================================ }
{ Setup/Teardown                                                               }
{ ============================================================================ }

procedure SetupTestDirectory;
begin
  TempDir := GetTempDir + 'pascaltorrent_fpi_test' + PathDelim;
  if not DirExists(TempDir) then
    MakeDir(TempDir);
end;

procedure CleanupTestDirectory;
var
  SearchRec: TSearchRec;
  Res: Integer;
begin
  { Remove all files in temp directory }
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
{ Test 1: Single Piece Write via Protocol                                      }
{ ============================================================================ }

procedure TestSinglePieceWriteViaProtocol;
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
  
  { Protocol message simulation }
  ProtocolBuffer: array[0..65535] of Byte;
  Msg: TWireMessage;
  EncodedLen: Integer;
  DecodedMsg: TWireMessage;
  BytesConsumed: Integer;
  
  { File reading }
  ReadBuffer: array of Byte;
  IOResult: TIOResult;
  PieceWriteResult: TPieceWriteResult;
begin
  WriteLn(#10'=== Testing Single Piece Write via Protocol ===');
  
  PieceLen := 16384;
  SetLength(PieceData, PieceLen);
  for I := 0 to PieceLen - 1 do
    PieceData[I] := I mod 256;
  
  PieceHash := SHA1Buffer(PieceData[0], PieceLen);
  
  { Create torrent structure }
  Root := BencodeNewDict;
  BencodeDictAdd(Root, 'announce', BencodeNewString('http://test/announce'));
  
  InfoDict := BencodeNewDict;
  BencodeDictAdd(InfoDict, 'name', BencodeNewString('protocol_test.bin'));
  BencodeDictAdd(InfoDict, 'piece length', BencodeNewInteger(PieceLen));
  BencodeDictAdd(InfoDict, 'length', BencodeNewInteger(PieceLen));
  
  SetLength(PiecesStr, 20);
  Move(PieceHash, PiecesStr[1], 20);
  Pieces := BencodeNewString(PiecesStr);
  BencodeDictAdd(InfoDict, 'pieces', Pieces);
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
      
      { Simulate receiving a piece message via protocol }
      { First, encode it as if it came from network }
      Msg.Length := 9 + PieceLen;
      Msg.MsgId := MSG_PIECE;
      Msg.PieceIndex := 0;
      Msg.PieceBegin := 0;
      Msg.PieceData := @PieceData[0];
      Msg.PieceDataLen := PieceLen;
      
      if not EncodeMessage(Msg, @ProtocolBuffer, SizeOf(ProtocolBuffer), EncodedLen) then
      begin
        TestResult('Encode PIECE message', False);
        Exit;
      end;
      TestResult('Encode PIECE message', EncodedLen = 13 + PieceLen);
      
      { Decode it as the receiver would }
      if not DecodeMessage(@ProtocolBuffer, EncodedLen, DecodedMsg, BytesConsumed) then
      begin
        TestResult('Decode PIECE message', False);
        Exit;
      end;
      TestResult('Decode PIECE message', True);
      TestResult('Decoded piece index is 0', DecodedMsg.PieceIndex = 0);
      TestResult('Decoded piece begin is 0', DecodedMsg.PieceBegin = 0);
      TestResult('Decoded piece data length is 16384', DecodedMsg.PieceDataLen = PieceLen);
      
      { Write the piece data to file via FileManager }
      if FileManagerWritePiece(FM, 0, DecodedMsg.PieceData, DecodedMsg.PieceDataLen, PieceWriteResult) then
      begin
        TestResult('Write piece via FileManager', PieceWriteResult.Success);
        TestResult('Piece verified correctly', PieceWriteResult.Verified);
        
        if PieceWriteResult.Verified then
        begin
          TestResult('Piece marked as verified', FileManagerIsPieceVerified(FM, 0));
          TestResult('Completion is 100%', FileManagerGetCompletion(FM) = 1.0);
          
          { Read back and verify }
          SetLength(ReadBuffer, PieceLen);
          if FileManagerReadPiece(FM, 0, @ReadBuffer[0], PieceLen, IOResult) then
          begin
            TestResult('Read piece back', IOResult.Success);
            TestResult('Read data matches protocol data', 
                       CompareMem(@PieceData[0], @ReadBuffer[0], PieceLen));
          end
          else
            TestResult('Read piece back', False);
        end;
      end
      else
        TestResult('Write piece via FileManager', False);
        
    finally
      FileManagerDestroy(FM);
    end;
    
  finally
    FreeTorrentMeta(Meta);
    BencodeFree(Root);
    DeleteFile(JoinPath(TempDir, 'protocol_test.bin'));
  end;
end;

{ ============================================================================ }
{ Test 2: Multi-Piece Download Simulation                                      }
{ ============================================================================ }

procedure TestMultiPieceDownloadSimulation;
var
  Root, InfoDict: PBencodeValue;
  Meta: PTorrentMeta;
  ParseResult: TMetainfoResult;
  FM: PFileManager;
  PieceData: array of Byte;
  PieceHashes: array of TSHA1Digest;
  PiecesStr: string;
  PieceLen: Integer;
  NumPieces: Integer;
  I, P: Integer;
  
  ProtocolBuffer: array[0..65535] of Byte;
  Msg: TWireMessage;
  EncodedLen: Integer;
  DecodedMsg: TWireMessage;
  BytesConsumed: Integer;
  PieceWriteResult: TPieceWriteResult;
  Completion: Double;
begin
  WriteLn(#10'=== Testing Multi-Piece Download Simulation ===');
  
  NumPieces := 4;
  PieceLen := 8192;
  SetLength(PieceData, PieceLen);
  SetLength(PieceHashes, NumPieces);
  
  { Create unique data and hashes for each piece }
  SetLength(PiecesStr, NumPieces * 20);
  for P := 0 to NumPieces - 1 do
  begin
    for I := 0 to PieceLen - 1 do
      PieceData[I] := (P * 17 + I) mod 256;  { Unique pattern per piece }
    
    PieceHashes[P] := SHA1Buffer(PieceData[0], PieceLen);
    Move(PieceHashes[P], PiecesStr[P * 20 + 1], 20);
  end;
  
  { Create torrent }
  Root := BencodeNewDict;
  InfoDict := BencodeNewDict;
  BencodeDictAdd(InfoDict, 'name', BencodeNewString('multi_piece.bin'));
  BencodeDictAdd(InfoDict, 'piece length', BencodeNewInteger(PieceLen));
  BencodeDictAdd(InfoDict, 'length', BencodeNewInteger(NumPieces * PieceLen));
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
    { Update Meta with our computed hashes - Pieces is PByteArray, so index by byte }
    for P := 0 to NumPieces - 1 do
    begin
      Move(PieceHashes[P], Meta^.Pieces^[P * 20], 20);
    end;
    
    if not FileManagerCreate(Meta, TempDir, FM) then
    begin
      TestResult('Create file manager', False);
      Exit;
    end;
    
    try
      FileManagerInitialize(FM);
      
      { Verify initial state }
      TestResult('Initial completion is 0%', FileManagerGetCompletion(FM) = 0.0);
      TestResult('Piece 0 not verified initially', not FileManagerIsPieceVerified(FM, 0));
      
      { Download pieces in random order (simulating real P2P) }
      for P := 0 to NumPieces - 1 do
      begin
        { Regenerate piece data }
        for I := 0 to PieceLen - 1 do
          PieceData[I] := (P * 17 + I) mod 256;
        
        { Simulate protocol receive }
        Msg.Length := 9 + PieceLen;
        Msg.MsgId := MSG_PIECE;
        Msg.PieceIndex := P;
        Msg.PieceBegin := 0;
        Msg.PieceData := @PieceData[0];
        Msg.PieceDataLen := PieceLen;
        
        EncodeMessage(Msg, @ProtocolBuffer, SizeOf(ProtocolBuffer), EncodedLen);
        DecodeMessage(@ProtocolBuffer, EncodedLen, DecodedMsg, BytesConsumed);
        
        { Write piece }
        if FileManagerWritePiece(FM, P, DecodedMsg.PieceData, DecodedMsg.PieceDataLen, PieceWriteResult) then
        begin
          TestResult(Format('Write piece %d succeeds', [P]), PieceWriteResult.Success);
          TestResult(Format('Piece %d verified', [P]), PieceWriteResult.Verified);
          TestResult(Format('Piece %d marked verified', [P]), FileManagerIsPieceVerified(FM, P));
        end
        else
        begin
          WriteLn('       Debug: PieceWriteResult.Success = ', PieceWriteResult.Success,
                  ', Verified = ', PieceWriteResult.Verified,
                  ', Error = ', PieceWriteResult.ErrorMsg);
          TestResult(Format('Write piece %d', [P]), False);
        end;
        
        { Check completion }
        Completion := FileManagerGetCompletion(FM);
        TestResult(Format('Completion after piece %d is %d%%', [P, (P + 1) * 25]),
                   Abs(Completion - ((P + 1) / NumPieces)) < 0.01);
      end;
      
      TestResult('Final completion is 100%', FileManagerGetCompletion(FM) = 1.0);
      TestResult('All pieces verified', FileManagerGetVerifiedBytes(FM) = NumPieces * PieceLen);
      
    finally
      FileManagerDestroy(FM);
    end;
    
  finally
    FreeTorrentMeta(Meta);
    BencodeFree(Root);
    DeleteFile(JoinPath(TempDir, 'multi_piece.bin'));
  end;
end;

{ ============================================================================ }
{ Test 3: Block-Level Operations Across Piece Boundaries                       }
{ ============================================================================ }

procedure TestBlockLevelOperations;
var
  Root, InfoDict: PBencodeValue;
  Meta: PTorrentMeta;
  ParseResult: TMetainfoResult;
  FM: PFileManager;
  PieceData: array of Byte;
  PieceHash: TSHA1Digest;
  PiecesStr: string;
  PieceLen: Integer;
  BlockSize: Integer;
  I: Integer;
  BlockData: array of Byte;
  ReadBuffer: array of Byte;
  IOResult: TIOResult;
  TotalBlocks: Integer;
  B: Integer;
begin
  WriteLn(#10'=== Testing Block-Level Operations ===');
  
  PieceLen := 32768;  { 32KB piece }
  BlockSize := 16384; { 16KB blocks (standard) }
  TotalBlocks := PieceLen div BlockSize; { 2 blocks }
  
  SetLength(PieceData, PieceLen);
  for I := 0 to PieceLen - 1 do
    PieceData[I] := I mod 256;
  
  PieceHash := SHA1Buffer(PieceData[0], PieceLen);
  
  { Create torrent }
  Root := BencodeNewDict;
  InfoDict := BencodeNewDict;
  BencodeDictAdd(InfoDict, 'name', BencodeNewString('block_test.bin'));
  BencodeDictAdd(InfoDict, 'piece length', BencodeNewInteger(PieceLen));
  BencodeDictAdd(InfoDict, 'length', BencodeNewInteger(PieceLen));
  
  SetLength(PiecesStr, 20);
  Move(PieceHash, PiecesStr[1], 20);
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
      
      SetLength(BlockData, BlockSize);
      
      { Write blocks in reverse order to test assembly }
      for B := TotalBlocks - 1 downto 0 do
      begin
        { Copy block from piece data }
        Move(PieceData[B * BlockSize], BlockData[0], BlockSize);
        
        if FileManagerWriteBlock(FM, B * BlockSize, BlockSize, @BlockData[0], IOResult) then
        begin
          TestResult(Format('Write block %d at offset %d', [B, B * BlockSize]), 
                     IOResult.Success);
        end
        else
          TestResult(Format('Write block %d', [B]), False);
      end;
      
      { Now verify the complete piece }
      SetLength(ReadBuffer, PieceLen);
      if FileManagerReadPiece(FM, 0, @ReadBuffer[0], PieceLen, IOResult) then
      begin
        TestResult('Read complete piece after blocks written', IOResult.Success);
        
        if IOResult.Success then
        begin
          TestResult('Reassembled piece matches original', 
                     CompareMem(@PieceData[0], @ReadBuffer[0], PieceLen));
        end;
      end
      else
        TestResult('Read complete piece', False);
      
      { Verify the piece after all blocks are written }
      FileManagerVerifyPiece(FM, 0, IOResult);
      TestResult('Piece verified after all blocks written', 
                 FileManagerIsPieceVerified(FM, 0));
      
    finally
      FileManagerDestroy(FM);
    end;
    
  finally
    FreeTorrentMeta(Meta);
    BencodeFree(Root);
    DeleteFile(JoinPath(TempDir, 'block_test.bin'));
  end;
end;

{ ============================================================================ }
{ Test 4: Corrupt Piece Rejection                                              }
{ ============================================================================ }

procedure TestCorruptPieceRejection;
var
  Root, InfoDict: PBencodeValue;
  Meta: PTorrentMeta;
  ParseResult: TMetainfoResult;
  FM: PFileManager;
  PieceData: array of Byte;
  PieceHash: TSHA1Digest;
  PiecesStr: string;
  PieceLen: Integer;
  I: Integer;
  PieceWriteResult: TPieceWriteResult;
begin
  WriteLn(#10'=== Testing Corrupt Piece Rejection ===');
  
  PieceLen := 16384;
  SetLength(PieceData, PieceLen);
  for I := 0 to PieceLen - 1 do
    PieceData[I] := I mod 256;
  
  PieceHash := SHA1Buffer(PieceData[0], PieceLen);
  
  { Create torrent }
  Root := BencodeNewDict;
  InfoDict := BencodeNewDict;
  BencodeDictAdd(InfoDict, 'name', BencodeNewString('corrupt_test.bin'));
  BencodeDictAdd(InfoDict, 'piece length', BencodeNewInteger(PieceLen));
  BencodeDictAdd(InfoDict, 'length', BencodeNewInteger(PieceLen));
  
  SetLength(PiecesStr, 20);
  Move(PieceHash, PiecesStr[1], 20);
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
      
      { Corrupt the data }
      PieceData[0] := PieceData[0] xor $FF;
      
      { Try to write corrupt piece }
      if FileManagerWritePiece(FM, 0, @PieceData[0], PieceLen, PieceWriteResult) then
      begin
        TestResult('Write corrupt piece fails', not PieceWriteResult.Success);
        TestResult('Corrupt piece not verified', not PieceWriteResult.Verified);
        TestResult('Piece not marked verified', not FileManagerIsPieceVerified(FM, 0));
        TestResult('Completion remains 0%', FileManagerGetCompletion(FM) = 0.0);
      end
      else
      begin
        { Write might fail completely, which is also valid }
        TestResult('Write corrupt piece rejected', True);
        TestResult('Piece not marked verified', not FileManagerIsPieceVerified(FM, 0));
      end;
      
    finally
      FileManagerDestroy(FM);
    end;
    
  finally
    FreeTorrentMeta(Meta);
    BencodeFree(Root);
    DeleteFile(JoinPath(TempDir, 'corrupt_test.bin'));
  end;
end;

{ ============================================================================ }
{ Test 5: Resume Partial Download                                              }
{ ============================================================================ }

procedure TestResumePartialDownload;
var
  Root, InfoDict: PBencodeValue;
  Meta: PTorrentMeta;
  ParseResult: TMetainfoResult;
  FM: PFileManager;
  PieceData: array of Byte;
  PieceHashes: array of TSHA1Digest;
  PiecesStr: string;
  PieceLen: Integer;
  NumPieces: Integer;
  I, P: Integer;
  PieceWriteResult: TPieceWriteResult;
  Completion: Double;
  VerifiedPieces: array of Boolean;
begin
  WriteLn(#10'=== Testing Resume Partial Download ===');
  
  NumPieces := 5;
  PieceLen := 8192;
  SetLength(VerifiedPieces, NumPieces);
  
  { Create piece data and hashes }
  SetLength(PieceData, PieceLen);
  SetLength(PieceHashes, NumPieces);
  SetLength(PiecesStr, NumPieces * 20);
  
  for P := 0 to NumPieces - 1 do
  begin
    for I := 0 to PieceLen - 1 do
      PieceData[I] := (P * 31 + I) mod 256;
    PieceHashes[P] := SHA1Buffer(PieceData[0], PieceLen);
    Move(PieceHashes[P], PiecesStr[P * 20 + 1], 20);
  end;
  
  { Create torrent }
  Root := BencodeNewDict;
  InfoDict := BencodeNewDict;
  BencodeDictAdd(InfoDict, 'name', BencodeNewString('resume_test.bin'));
  BencodeDictAdd(InfoDict, 'piece length', BencodeNewInteger(PieceLen));
  BencodeDictAdd(InfoDict, 'length', BencodeNewInteger(NumPieces * PieceLen));
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
    { Update Meta with hashes - Pieces is PByteArray, so index by byte }
    for P := 0 to NumPieces - 1 do
      Move(PieceHashes[P], Meta^.Pieces^[P * 20], 20);
    
    if not FileManagerCreate(Meta, TempDir, FM) then
    begin
      TestResult('Create file manager', False);
      Exit;
    end;
    
    try
      FileManagerInitialize(FM);
      
      { Download pieces 0 and 2 (simulate partial download) }
      for P in [0, 2] do
      begin
        for I := 0 to PieceLen - 1 do
          PieceData[I] := (P * 31 + I) mod 256;
        
        FileManagerWritePiece(FM, P, @PieceData[0], PieceLen, PieceWriteResult);
        VerifiedPieces[P] := PieceWriteResult.Verified;
      end;
      
      TestResult('Piece 0 verified', VerifiedPieces[0]);
      TestResult('Piece 2 verified', VerifiedPieces[2]);
      TestResult('Piece 1 not verified', not FileManagerIsPieceVerified(FM, 1));
      
      Completion := FileManagerGetCompletion(FM);
      TestResult('Completion is 40% (2/5)', Abs(Completion - 0.4) < 0.01);
      
      { Now "resume" and download remaining pieces }
      for P := 0 to NumPieces - 1 do
      begin
        if VerifiedPieces[P] then Continue;  { Skip already verified }
        
        for I := 0 to PieceLen - 1 do
          PieceData[I] := (P * 31 + I) mod 256;
        
        FileManagerWritePiece(FM, P, @PieceData[0], PieceLen, PieceWriteResult);
        VerifiedPieces[P] := PieceWriteResult.Verified;
      end;
      
      { Verify all pieces now complete }
      TestResult('All pieces now verified', FileManagerGetCompletion(FM) = 1.0);
      for P := 0 to NumPieces - 1 do
        TestResult(Format('Piece %d verified', [P]), FileManagerIsPieceVerified(FM, P));
        
    finally
      FileManagerDestroy(FM);
    end;
    
  finally
    FreeTorrentMeta(Meta);
    BencodeFree(Root);
    DeleteFile(JoinPath(TempDir, 'resume_test.bin'));
  end;
end;

{ ============================================================================ }
{ Test 6: Multi-File Torrent Protocol Integration                              }
{ ============================================================================ }

procedure TestMultiFileTorrentProtocol;
var
  Root, InfoDict, Pieces, FilesList, File1, File2, Path1, Path2: PBencodeValue;
  Meta: PTorrentMeta;
  ParseResult: TMetainfoResult;
  FM: PFileManager;
  PieceData: array of Byte;
  PiecesStr: string;
  PieceLen: Integer;
  I: Integer;
  File1Len, File2Len: Integer;
  TotalLen: Integer;
  PieceWriteResult: TPieceWriteResult;
  FilesPath, DataPath: string;
begin
  WriteLn(#10'=== Testing Multi-File Torrent Protocol Integration ===');
  
  PieceLen := 16384;
  File1Len := 10000;
  File2Len := 6384;  { Total = 16384 = 1 piece }
  TotalLen := File1Len + File2Len;
  
  SetLength(PieceData, TotalLen);
  for I := 0 to TotalLen - 1 do
    PieceData[I] := I mod 256;
  
  { Create multi-file torrent }
  Root := BencodeNewDict;
  InfoDict := BencodeNewDict;
  BencodeDictAdd(InfoDict, 'name', BencodeNewString('MultiFileContent'));
  BencodeDictAdd(InfoDict, 'piece length', BencodeNewInteger(PieceLen));
  
  { Single piece hash for simplicity }
  SetLength(PiecesStr, 20);
  FillChar(PiecesStr[1], 20, 0);
  PiecesStr[1] := #1;
  BencodeDictAdd(InfoDict, 'pieces', BencodeNewString(PiecesStr));
  
  { Files list }
  FilesList := BencodeNewList;
  
  File1 := BencodeNewDict;
  BencodeDictAdd(File1, 'length', BencodeNewInteger(File1Len));
  Path1 := BencodeNewList;
  BencodeListAdd(Path1, BencodeNewString('file1.txt'));
  BencodeDictAdd(File1, 'path', Path1);
  BencodeListAdd(FilesList, File1);
  
  File2 := BencodeNewDict;
  BencodeDictAdd(File2, 'length', BencodeNewInteger(File2Len));
  Path2 := BencodeNewList;
  BencodeListAdd(Path2, BencodeNewString('subdir'));
  BencodeListAdd(Path2, BencodeNewString('file2.bin'));
  BencodeDictAdd(File2, 'path', Path2);
  BencodeListAdd(FilesList, File2);
  
  BencodeDictAdd(InfoDict, 'files', FilesList);
  BencodeDictAdd(Root, 'info', InfoDict);
  
  ParseResult := ParseTorrentBencode(Root, Meta);
  if not ParseResult.Success then
  begin
    TestResult('Parse multi-file torrent', False, ParseResult.ErrorMsg);
    BencodeFree(Root);
    Exit;
  end;
  
  try
    TestResult('Torrent is multi-file', not Meta^.IsSingleFile);
    TestResult('File count is 2', Meta^.FileCount = 2);
    TestResult('Total length correct', Meta^.TotalLength = TotalLen);
    
    if not FileManagerCreate(Meta, TempDir, FM) then
    begin
      TestResult('Create file manager', False);
      Exit;
    end;
    
    try
      FileManagerInitialize(FM);
      
      { Preallocate files }
      if FileManagerPreallocateFiles(FM) then
      begin
        TestResult('Files preallocated', True);
        TestResult('Files exist', FileManagerFilesExist(FM));
      end
      else
        TestResult('Files preallocated', False);
      
      { Write the single piece (spans both files) }
      if FileManagerWritePiece(FM, 0, @PieceData[0], TotalLen, PieceWriteResult) then
      begin
        TestResult('Write piece spanning multiple files', PieceWriteResult.Success);
      end;
      
    finally
      FileManagerDestroy(FM);
    end;
    
  finally
    FreeTorrentMeta(Meta);
    BencodeFree(Root);
    
    { Cleanup }
    FilesPath := JoinPath(TempDir, 'MultiFileContent');
    DataPath := JoinPath(FilesPath, 'subdir');
    DeleteFile(JoinPath(FilesPath, 'file1.txt'));
    DeleteFile(JoinPath(DataPath, 'file2.bin'));
    {$I-}
    RmDir(DataPath);
    RmDir(FilesPath);
    {$I+}
  end;
end;

{ ============================================================================ }
{ Main Program                                                                 }
{ ============================================================================ }

begin
  BeginSuite('FILE + PROTOCOL INTEGRATION TESTS');
  
  SetupTestDirectory;
  try
    TestSinglePieceWriteViaProtocol;
    TestMultiPieceDownloadSimulation;
    TestBlockLevelOperations;
    TestCorruptPieceRejection;
    TestResumePartialDownload;
    TestMultiFileTorrentProtocol;
    
  finally
    CleanupTestDirectory;
  end;
  
  EndSuite;
  ExitWithResult;
end.
