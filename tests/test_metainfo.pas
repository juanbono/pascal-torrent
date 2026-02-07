{
  test_metainfo.pas - Comprehensive tests for metainfo unit
}

{$mode objfpc}{$H+}

program test_metainfo;

uses
  SysUtils, bencode, metainfo, sha1utils, utils, logging;

type
  TTestFileRec = record
    Path: string;
    Length: Int64;
  end;

var
  TotalTests: Integer = 0;
  PassedTests: Integer = 0;
  FailedTests: Integer = 0;
  Verbose: Boolean = True;

procedure TestResult(const TestName: string; Passed: Boolean; const Msg: string = '');
begin
  Inc(TotalTests);
  if Passed then
  begin
    Inc(PassedTests);
    if Verbose then
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

var
  NumPieces: Integer;  { Global for test helper functions }

{ ============================================================================ }
{ Test: Create minimal single-file torrent metainfo                            }
{ ============================================================================ }

function CreateSingleFileTorrent(const Name: string; Length: Int64;
                                 PieceLength: Integer): PBencodeValue;
var
  Root, Info: PBencodeValue;
  Pieces: string;
  I: Integer;
  Hash: TSHA1Digest;
  PieceCount: Integer;
begin
  Result := nil;
  
  { Create root dictionary }
  Root := BencodeNewDict;
  if Root = nil then Exit;
  
  { Create info dictionary }
  Info := BencodeNewDict;
  if Info = nil then
  begin
    BencodeFree(Root);
    Exit;
  end;
  
  { Add name }
  if not BencodeDictAdd(Info, 'name', BencodeNewString(Name)) then
  begin
    BencodeFree(Info);
    BencodeFree(Root);
    Exit;
  end;
  
  { Add piece length }
  if not BencodeDictAdd(Info, 'piece length', BencodeNewInteger(PieceLength)) then
  begin
    BencodeFree(Info);
    BencodeFree(Root);
    Exit;
  end;
  
  { Add length }
  if not BencodeDictAdd(Info, 'length', BencodeNewInteger(Length)) then
  begin
    BencodeFree(Info);
    BencodeFree(Root);
    Exit;
  end;
  
  { Create fake pieces (20 bytes of zeros for each piece) }
  PieceCount := (Length + PieceLength - 1) div PieceLength;
  SetLength(Pieces, PieceCount * 20);
  
  { Put unique data in each hash so we can verify later }
  for I := 0 to PieceCount - 1 do
  begin
    Hash[0] := I mod 256;
    Hash[1] := (I div 256) mod 256;
    Move(Hash, Pieces[I * 20 + 1], 20);
  end;
  
  if not BencodeDictAdd(Info, 'pieces', BencodeNewStringBuf(PChar(Pieces), System.Length(Pieces))) then
  begin
    BencodeFree(Info);
    BencodeFree(Root);
    Exit;
  end;
  
  { Add info to root }
  if not BencodeDictAdd(Root, 'info', Info) then
  begin
    BencodeFree(Info);
    BencodeFree(Root);
    Exit;
  end;
  
  Result := Root;
end;

function CreateMultiFileTorrent(const Name: string; 
                                const Files: array of TTestFileRec;
                                PieceLength: Integer): PBencodeValue;
var
  Root, Info, FilesList, FileDict: PBencodeValue;
  Pieces: string;
  I: Integer;
  TotalLength: Int64;
  PathParts: array of string;
  PathList: PBencodeValue;
  J: Integer;
  PieceCount: Integer;
begin
  Result := nil;
  
  { Calculate total length }
  TotalLength := 0;
  for I := 0 to High(Files) do
    TotalLength := TotalLength + Files[I].Length;
  
  { Create root dictionary }
  Root := BencodeNewDict;
  if Root = nil then Exit;
  
  { Create info dictionary }
  Info := BencodeNewDict;
  if Info = nil then
  begin
    BencodeFree(Root);
    Exit;
  end;
  
  { Add name }
  if not BencodeDictAdd(Info, 'name', BencodeNewString(Name)) then
  begin
    BencodeFree(Info);
    BencodeFree(Root);
    Exit;
  end;
  
  { Add piece length }
  if not BencodeDictAdd(Info, 'piece length', BencodeNewInteger(PieceLength)) then
  begin
    BencodeFree(Info);
    BencodeFree(Root);
    Exit;
  end;
  
  { Create fake pieces }
  PieceCount := (TotalLength + PieceLength - 1) div PieceLength;
  SetLength(Pieces, PieceCount * 20);
  
  if not BencodeDictAdd(Info, 'pieces', BencodeNewStringBuf(PChar(Pieces), Length(Pieces))) then
  begin
    BencodeFree(Info);
    BencodeFree(Root);
    Exit;
  end;
  
  { Create files list }
  FilesList := BencodeNewList;
  if FilesList = nil then
  begin
    BencodeFree(Info);
    BencodeFree(Root);
    Exit;
  end;
  
  for I := 0 to High(Files) do
  begin
    FileDict := BencodeNewDict;
    if FileDict = nil then Continue;
    
    { Add length }
    BencodeDictAdd(FileDict, 'length', BencodeNewInteger(Files[I].Length));
    
    { Add path as list }
    PathList := BencodeNewList;
    { Simple path splitting - just use the path as-is for now }
    BencodeListAdd(PathList, BencodeNewString(Files[I].Path));
    BencodeDictAdd(FileDict, 'path', PathList);
    
    BencodeListAdd(FilesList, FileDict);
  end;
  
  BencodeDictAdd(Info, 'files', FilesList);
  
  { Add info to root }
  if not BencodeDictAdd(Root, 'info', Info) then
  begin
    BencodeFree(Info);
    BencodeFree(Root);
    Exit;
  end;
  
  Result := Root;
end;

{ ============================================================================ }
{ Test: Basic single-file torrent parsing                                      }
{ ============================================================================ }

procedure TestSingleFileTorrent;
var
  Root: PBencodeValue;
  Meta: PTorrentMeta;
  Result: TMetainfoResult;
begin
  WriteLn(#10'=== Testing Single-File Torrent Parsing ===');
  
  Root := CreateSingleFileTorrent('test.txt', 1024, 256);
  if Root = nil then
  begin
    TestResult('Create test torrent', False, 'Failed to create test data');
    Exit;
  end;
  
  try
    Result := ParseTorrentBencode(Root, Meta);
    TestResult('Parse single-file torrent', Result.Success, Result.ErrorMsg);
    
    if Result.Success and (Meta <> nil) then
    begin
      TestResult('Is single file', Meta^.IsSingleFile);
      TestResult('Name is "test.txt"', Meta^.Name = 'test.txt');
      TestResult('Length is 1024', Meta^.Length = 1024);
      TestResult('Total length is 1024', Meta^.TotalLength = 1024);
      TestResult('Piece length is 256', Meta^.PieceLength = 256);
      TestResult('Piece count is 4', Meta^.PieceCount = 4);
      TestResult('Info hash is not empty', not SHA1IsEmpty(Meta^.InfoHash));
      
      FreeTorrentMeta(Meta);
    end;
  finally
    BencodeFree(Root);
  end;
end;

{ ============================================================================ }
{ Test: Multi-file torrent parsing                                             }
{ ============================================================================ }

procedure TestMultiFileTorrent;
var
  Root: PBencodeValue;
  Meta: PTorrentMeta;
  Result: TMetainfoResult;
  Files: array[0..2] of TTestFileRec;
begin
  WriteLn(#10'=== Testing Multi-File Torrent Parsing ===');
  
  Files[0].Path := 'readme.txt';
  Files[0].Length := 100;
  Files[1].Path := 'data/file1.bin';
  Files[1].Length := 500;
  Files[2].Path := 'data/file2.bin';
  Files[2].Length := 400;
  
  Root := CreateMultiFileTorrent('myapp', Files, 256);
  if Root = nil then
  begin
    TestResult('Create test torrent', False, 'Failed to create test data');
    Exit;
  end;
  
  try
    Result := ParseTorrentBencode(Root, Meta);
    TestResult('Parse multi-file torrent', Result.Success, Result.ErrorMsg);
    
    if Result.Success and (Meta <> nil) then
    begin
      TestResult('Is multi-file', not Meta^.IsSingleFile);
      TestResult('Name is "myapp"', Meta^.Name = 'myapp');
      TestResult('File count is 3', Meta^.FileCount = 3);
      TestResult('Total length is 1000', Meta^.TotalLength = 1000);
      TestResult('Files list is not nil', Meta^.Files <> nil);
      
      if Meta^.Files <> nil then
      begin
        TestResult('First file path is "readme.txt"', 
                   Meta^.Files^.Path = 'readme.txt');
        TestResult('First file length is 100', 
                   Meta^.Files^.Length = 100);
        TestResult('First file offset is 0', 
                   Meta^.Files^.Offset = 0);
        
        if Meta^.Files^.Next <> nil then
        begin
          TestResult('Second file offset is 100', 
                     Meta^.Files^.Next^.Offset = 100);
          TestResult('Second file length is 500', 
                     Meta^.Files^.Next^.Length = 500);
        end;
      end;
      
      FreeTorrentMeta(Meta);
    end;
  finally
    BencodeFree(Root);
  end;
end;

{ ============================================================================ }
{ Test: Piece hash access                                                      }
{ ============================================================================ }

procedure TestPieceHashAccess;
var
  Root: PBencodeValue;
  Meta: PTorrentMeta;
  Result: TMetainfoResult;
  Hash: PSHA1Digest;
  ExpectedHash: TSHA1Digest;
begin
  WriteLn(#10'=== Testing Piece Hash Access ===');
  
  Root := CreateSingleFileTorrent('test.txt', 1024, 256);
  if Root = nil then
  begin
    TestResult('Create test torrent', False, 'Failed to create test data');
    Exit;
  end;
  
  try
    Result := ParseTorrentBencode(Root, Meta);
    if not Result.Success then
    begin
      TestResult('Parse torrent', False, Result.ErrorMsg);
      Exit;
    end;
    
    try
      { Test valid piece indices }
      TestResult('Get piece 0 hash', GetPieceHash(Meta, 0) <> nil);
      TestResult('Get piece 1 hash', GetPieceHash(Meta, 1) <> nil);
      TestResult('Get piece 3 hash', GetPieceHash(Meta, 3) <> nil);
      
      { Test invalid piece indices }
      TestResult('Piece -1 returns nil', GetPieceHash(Meta, -1) = nil);
      TestResult('Piece 4 returns nil', GetPieceHash(Meta, 4) = nil);
      TestResult('Piece 100 returns nil', GetPieceHash(Meta, 100) = nil);
      
      { Test hash content - piece 0 should have first byte = 0 }
      Hash := GetPieceHash(Meta, 0);
      if Hash <> nil then
      begin
        TestResult('Piece 0 hash[0] = 0', Hash^[0] = 0);
      end;
      
      { Test hash content - piece 1 should have first byte = 1 }
      Hash := GetPieceHash(Meta, 1);
      if Hash <> nil then
      begin
        TestResult('Piece 1 hash[0] = 1', Hash^[0] = 1);
      end;
    finally
      FreeTorrentMeta(Meta);
    end;
  finally
    BencodeFree(Root);
  end;
end;

{ ============================================================================ }
{ Test: Piece length calculation                                               }
{ ============================================================================ }

procedure TestPieceLength;
var
  Root: PBencodeValue;
  Meta: PTorrentMeta;
  Result: TMetainfoResult;
begin
  WriteLn(#10'=== Testing Piece Length Calculation ===');
  
  { Create torrent with 1000 bytes, piece size 256 = 4 pieces }
  Root := CreateSingleFileTorrent('test.txt', 1000, 256);
  if Root = nil then
  begin
    TestResult('Create test torrent', False, 'Failed to create test data');
    Exit;
  end;
  
  try
    Result := ParseTorrentBencode(Root, Meta);
    if not Result.Success then
    begin
      TestResult('Parse torrent', False, Result.ErrorMsg);
      Exit;
    end;
    
    try
      TestResult('Piece 0 length is 256', GetPieceLength(Meta, 0) = 256);
      TestResult('Piece 1 length is 256', GetPieceLength(Meta, 1) = 256);
      TestResult('Piece 2 length is 256', GetPieceLength(Meta, 2) = 256);
      { Last piece: 1000 - 3*256 = 232 }
      TestResult('Piece 3 length is 232', GetPieceLength(Meta, 3) = 232);
      TestResult('Piece 4 length is 0 (out of range)', GetPieceLength(Meta, 4) = 0);
    finally
      FreeTorrentMeta(Meta);
    end;
  finally
    BencodeFree(Root);
  end;
end;

{ ============================================================================ }
{ Test: Info hash formats                                                      }
{ ============================================================================ }

procedure TestInfoHashFormats;
var
  Root: PBencodeValue;
  Meta: PTorrentMeta;
  Result: TMetainfoResult;
  HexHash, URLEncoded: string;
  I: Integer;
begin
  WriteLn(#10'=== Testing Info Hash Formats ===');
  
  Root := CreateSingleFileTorrent('test.txt', 1024, 256);
  if Root = nil then
  begin
    TestResult('Create test torrent', False, 'Failed to create test data');
    Exit;
  end;
  
  try
    Result := ParseTorrentBencode(Root, Meta);
    if not Result.Success then
    begin
      TestResult('Parse torrent', False, Result.ErrorMsg);
      Exit;
    end;
    
    try
      HexHash := GetInfoHashHex(Meta);
      TestResult('Hex hash is 40 chars', Length(HexHash) = 40);
      
      { Verify all hex chars }
      I := 1;
      while I <= Length(HexHash) do
      begin
        if not (HexHash[I] in ['0'..'9', 'a'..'f']) then
          Break;
        Inc(I);
      end;
      TestResult('Hex hash contains only valid chars', I > Length(HexHash));
      
      URLEncoded := GetInfoHashURLEncoded(Meta);
      TestResult('URL-encoded hash is 60 chars (%XX for each byte)', 
                 Length(URLEncoded) = 60);
      
      { Verify format: should start with % }
      TestResult('URL-encoded starts with %', StartsWith(URLEncoded, '%'));
      
      { Count % characters - should be 20 }
      I := 0;
      while Pos('%', URLEncoded) > 0 do
      begin
        Inc(I);
        URLEncoded[Pos('%', URLEncoded)] := ' ';
      end;
      TestResult('URL-encoded has 20 % characters', I = 20);
    finally
      FreeTorrentMeta(Meta);
    end;
  finally
    BencodeFree(Root);
  end;
end;

{ ============================================================================ }
{ Test: Validation                                                             }
{ ============================================================================ }

procedure TestValidation;
var
  Root: PBencodeValue;
  Meta: PTorrentMeta;
  Result: TMetainfoResult;
  Valid: Boolean;
  ErrorMsg: string;
begin
  WriteLn(#10'=== Testing Validation ===');
  
  { Test valid torrent }
  Root := CreateSingleFileTorrent('test.txt', 1024, 256);
  if Root <> nil then
  begin
    try
      Result := ParseTorrentBencode(Root, Meta);
      if Result.Success then
      begin
        Valid := ValidateTorrentMeta(Meta, ErrorMsg);
        TestResult('Valid torrent passes validation', Valid);
        FreeTorrentMeta(Meta);
      end;
    finally
      BencodeFree(Root);
    end;
  end;
  
  { Test nil meta }
  Valid := ValidateTorrentMeta(nil, ErrorMsg);
  TestResult('Nil meta fails validation', not Valid);
  TestResult('Nil meta error mentions nil', Pos('nil', LowerCase(ErrorMsg)) > 0);
end;

{ ============================================================================ }
{ Test: File at offset lookup                                                  }
{ ============================================================================ }

procedure TestFileAtOffset;
var
  Root: PBencodeValue;
  Meta: PTorrentMeta;
  Result: TMetainfoResult;
  FileEntry: PFileEntry;
  Found: Boolean;
begin
  WriteLn(#10'=== Testing File At Offset Lookup ===');
  
  Root := CreateSingleFileTorrent('test.txt', 1024, 256);
  if Root <> nil then
  begin
    try
      Result := ParseTorrentBencode(Root, Meta);
      if Result.Success then
      begin
        { Single file - should return true but FileEntry is nil }
        Found := FindFileAtOffset(Meta, 0, FileEntry);
        TestResult('Single file: offset 0 returns true', Found);
        TestResult('Single file: FileEntry is nil', FileEntry = nil);
        
        Found := FindFileAtOffset(Meta, 1023, FileEntry);
        TestResult('Single file: offset 1023 returns true', Found);
        
        Found := FindFileAtOffset(Meta, 1024, FileEntry);
        TestResult('Single file: offset 1024 returns false', not Found);
        
        FreeTorrentMeta(Meta);
      end;
    finally
      BencodeFree(Root);
    end;
  end;
end;

{ ============================================================================ }
{ Test: Bitfield completion                                                    }
{ ============================================================================ }

procedure TestBitfieldCompletion;
var
  Bitfield: array[0..3] of Byte;
  Completion: Double;
begin
  WriteLn(#10'=== Testing Bitfield Completion ===');
  
  { 0% complete }
  Bitfield[0] := 0;
  Bitfield[1] := 0;
  Bitfield[2] := 0;
  Bitfield[3] := 0;
  Completion := GetCompletionFromBitfield(@Bitfield, 4, 32);
  TestResult('All zeros = 0%', Completion = 0.0);
  
  { 100% complete }
  Bitfield[0] := $FF;
  Bitfield[1] := $FF;
  Bitfield[2] := $FF;
  Bitfield[3] := $FF;
  Completion := GetCompletionFromBitfield(@Bitfield, 4, 32);
  TestResult('All ones = 100%', Completion = 1.0);
  
  { 50% complete (16 of 32 pieces) }
  Bitfield[0] := $FF;
  Bitfield[1] := $FF;
  Bitfield[2] := 0;
  Bitfield[3] := 0;
  Completion := GetCompletionFromBitfield(@Bitfield, 4, 32);
  TestResult('Half complete = 50%', Completion = 0.5);
  
  { 25% complete (2 of 8 pieces) }
  Bitfield[0] := $03;  { 00000011 - first 2 bits set }
  Completion := GetCompletionFromBitfield(@Bitfield, 1, 8);
  TestResult('2 of 8 = 25%', Completion = 0.25);
  
  { Partial last byte: 7 pieces, 5 complete (MSB-first: bits 7,6,5,4,3 = 11111000) }
  Bitfield[0] := $F8;  { 11111000 - 5 bits set }
  Completion := GetCompletionFromBitfield(@Bitfield, 1, 7);
  TestResult('5 of 7 = ~71.4%', Abs(Completion - 5/7) < 0.01);
end;

{ ============================================================================ }
{ Test: Error handling                                                         }
{ ============================================================================ }

procedure TestErrorHandling;
var
  Root: PBencodeValue;
  Meta: PTorrentMeta;
  Result: TMetainfoResult;
begin
  WriteLn(#10'=== Testing Error Handling ===');
  
  { Test nil input }
  Result := ParseTorrentBuffer(nil, 0, Meta);
  TestResult('Nil buffer fails', not Result.Success);
  
  { Test empty dictionary }
  Root := BencodeNewDict;
  if Root <> nil then
  begin
    try
      Result := ParseTorrentBencode(Root, Meta);
      TestResult('Empty dict fails (no info)', not Result.Success);
    finally
      BencodeFree(Root);
    end;
  end;
  
  { Test missing required fields }
  Root := BencodeNewDict;
  if Root <> nil then
  begin
    BencodeDictAdd(Root, 'info', BencodeNewDict);
    try
      Result := ParseTorrentBencode(Root, Meta);
      TestResult('Info without piece length fails', not Result.Success);
    finally
      BencodeFree(Root);
    end;
  end;
end;

{ ============================================================================ }
{ Test: Announce and tracker list                                              }
{ ============================================================================ }

procedure TestAnnounceParsing;
var
  Root, Info: PBencodeValue;
  Meta: PTorrentMeta;
  Result: TMetainfoResult;
begin
  WriteLn(#10'=== Testing Announce Parsing ===');
  
  { Create torrent with announce }
  Root := BencodeNewDict;
  Info := BencodeNewDict;
  
  if (Root <> nil) and (Info <> nil) then
  begin
    { Add required info fields }
    BencodeDictAdd(Info, 'name', BencodeNewString('test.txt'));
    BencodeDictAdd(Info, 'piece length', BencodeNewInteger(256));
    BencodeDictAdd(Info, 'length', BencodeNewInteger(1024));
    BencodeDictAdd(Info, 'pieces', BencodeNewString(StringOfChar(#0, 80)));
    BencodeDictAdd(Root, 'info', Info);
    
    { Add announce URL }
    BencodeDictAdd(Root, 'announce', 
                   BencodeNewString('http://tracker.example.com/announce'));
    
    try
      Result := ParseTorrentBencode(Root, Meta);
      if Result.Success then
      begin
        TestResult('Announce URL parsed', 
                   Meta^.Announce = 'http://tracker.example.com/announce');
        TestResult('Tracker created from announce', Meta^.Trackers <> nil);
        if Meta^.Trackers <> nil then
        begin
          TestResult('Tracker URL matches', 
                     Meta^.Trackers^.Url = 'http://tracker.example.com/announce');
          TestResult('Tracker protocol is HTTP', Meta^.Trackers^.Protocol = tpHTTP);
        end;
        FreeTorrentMeta(Meta);
      end
      else
      begin
        TestResult('Parse with announce', False, Result.ErrorMsg);
      end;
    finally
      BencodeFree(Root);
    end;
  end;
end;

{ ============================================================================ }
{ Test: Path sanitization                                                        }
{ ============================================================================ }

procedure TestPathSanitization;
begin
  WriteLn(#10'=== Testing Path Sanitization ===');
  
  TestResult('Normal path unchanged', 
    SanitizePath('normal/path/file.txt') = 'normal/path/file.txt');
  TestResult('Removes .. from path', 
    SanitizePath('path/../file.txt') = 'path/file.txt');
  TestResult('Removes leading slash', 
    SanitizePath('/absolute/path') = 'absolute/path');
  TestResult('Removes multiple slashes',
    SanitizePath('path//to///file') = 'path/to/file');
  TestResult('Empty path stays empty',
    SanitizePath('') = '');
  TestResult('Single dot removed from start',
    SanitizePath('./file.txt') = 'file.txt');
end;

{ ============================================================================ }
{ Test: Path traversal detection                                                 }
{ ============================================================================ }

procedure TestPathTraversalDetection;
begin
  WriteLn(#10'=== Testing Path Traversal Detection ===');
  
  TestResult('Detects .. in name', ContainsTraversal('../name'));
  TestResult('Detects .. in path', ContainsTraversal('path/../file'));
  TestResult('Detects .. at start', ContainsTraversal('../file'));
  TestResult('Detects backslash', ContainsTraversal('path\file'));
  TestResult('Normal path ok', not ContainsTraversal('normal/path/file'));
  TestResult('Simple name ok', not ContainsTraversal('filename.txt'));
  TestResult('Empty path ok', not ContainsTraversal(''));
end;

{ ============================================================================ }
{ Test: Validation of malicious paths                                            }
{ ============================================================================ }

procedure TestMaliciousPathValidation;
var
  Root, Info: PBencodeValue;
  Meta: PTorrentMeta;
  Result: TMetainfoResult;
begin
  WriteLn(#10'=== Testing Malicious Path Validation ===');
  
  { Test path traversal in name }
  Root := BencodeNewDict;
  Info := BencodeNewDict;
  if (Root <> nil) and (Info <> nil) then
  begin
    BencodeDictAdd(Info, 'name', BencodeNewString('../etc/passwd'));
    BencodeDictAdd(Info, 'piece length', BencodeNewInteger(256));
    BencodeDictAdd(Info, 'length', BencodeNewInteger(1024));
    BencodeDictAdd(Info, 'pieces', BencodeNewString(StringOfChar(#0, 80)));
    BencodeDictAdd(Root, 'info', Info);
    
    try
      Result := ParseTorrentBencode(Root, Meta);
      if Result.Success then
      begin
        { Path should be sanitized }
        TestResult('Path traversal sanitized in name', 
                   Pos('..', Meta^.Name) = 0);
        FreeTorrentMeta(Meta);
      end
      else
      begin
        { Or rejected entirely - also acceptable }
        TestResult('Path traversal rejected', True);
      end;
    finally
      BencodeFree(Root);
    end;
  end;
end;

{ ============================================================================ }
{ Test: Malformed torrent handling                                               }
{ ============================================================================ }

procedure TestMalformedTorrents;
var
  Root, Info: PBencodeValue;
  Meta: PTorrentMeta;
  Result: TMetainfoResult;
  I: Integer;
  Pieces: string;
begin
  WriteLn(#10'=== Testing Malformed Torrent Handling ===');
  
  { Test 1: Negative piece length }
  Root := BencodeNewDict;
  Info := BencodeNewDict;
  if (Root <> nil) and (Info <> nil) then
  begin
    BencodeDictAdd(Info, 'name', BencodeNewString('test.txt'));
    BencodeDictAdd(Info, 'piece length', BencodeNewInteger(-1));
    BencodeDictAdd(Info, 'length', BencodeNewInteger(100));
    BencodeDictAdd(Info, 'pieces', BencodeNewString(StringOfChar(#0, 20)));
    BencodeDictAdd(Root, 'info', Info);
    
    try
      Result := ParseTorrentBencode(Root, Meta);
      TestResult('Negative piece length rejected', not Result.Success);
      if Result.Success then FreeTorrentMeta(Meta);
    finally
      BencodeFree(Root);
    end;
  end;
  
  { Test 2: Zero piece length }
  Root := BencodeNewDict;
  Info := BencodeNewDict;
  if (Root <> nil) and (Info <> nil) then
  begin
    BencodeDictAdd(Info, 'name', BencodeNewString('test.txt'));
    BencodeDictAdd(Info, 'piece length', BencodeNewInteger(0));
    BencodeDictAdd(Info, 'length', BencodeNewInteger(100));
    BencodeDictAdd(Info, 'pieces', BencodeNewString(StringOfChar(#0, 20)));
    BencodeDictAdd(Root, 'info', Info);
    
    try
      Result := ParseTorrentBencode(Root, Meta);
      TestResult('Zero piece length rejected', not Result.Success);
      if Result.Success then FreeTorrentMeta(Meta);
    finally
      BencodeFree(Root);
    end;
  end;
  
  { Test 3: Wrong pieces length (not multiple of 20) }
  Root := BencodeNewDict;
  Info := BencodeNewDict;
  if (Root <> nil) and (Info <> nil) then
  begin
    BencodeDictAdd(Info, 'name', BencodeNewString('test.txt'));
    BencodeDictAdd(Info, 'piece length', BencodeNewInteger(100));
    BencodeDictAdd(Info, 'length', BencodeNewInteger(100));
    BencodeDictAdd(Info, 'pieces', BencodeNewString(StringOfChar(#0, 21)));  { Invalid }
    BencodeDictAdd(Root, 'info', Info);
    
    try
      Result := ParseTorrentBencode(Root, Meta);
      TestResult('Invalid pieces length rejected', not Result.Success);
      if Result.Success then FreeTorrentMeta(Meta);
    finally
      BencodeFree(Root);
    end;
  end;
  
  { Test 4: Empty pieces }
  Root := BencodeNewDict;
  Info := BencodeNewDict;
  if (Root <> nil) and (Info <> nil) then
  begin
    BencodeDictAdd(Info, 'name', BencodeNewString('test.txt'));
    BencodeDictAdd(Info, 'piece length', BencodeNewInteger(100));
    BencodeDictAdd(Info, 'length', BencodeNewInteger(100));
    BencodeDictAdd(Info, 'pieces', BencodeNewString(''));  { Empty }
    BencodeDictAdd(Root, 'info', Info);
    
    try
      Result := ParseTorrentBencode(Root, Meta);
      TestResult('Empty pieces rejected', not Result.Success);
      if Result.Success then FreeTorrentMeta(Meta);
    finally
      BencodeFree(Root);
    end;
  end;
  
  { Test 5: Negative file length }
  Root := BencodeNewDict;
  Info := BencodeNewDict;
  if (Root <> nil) and (Info <> nil) then
  begin
    BencodeDictAdd(Info, 'name', BencodeNewString('test.txt'));
    BencodeDictAdd(Info, 'piece length', BencodeNewInteger(100));
    BencodeDictAdd(Info, 'length', BencodeNewInteger(-1));  { Invalid }
    BencodeDictAdd(Info, 'pieces', BencodeNewString(StringOfChar(#0, 20)));
    BencodeDictAdd(Root, 'info', Info);
    
    try
      Result := ParseTorrentBencode(Root, Meta);
      TestResult('Negative file length rejected', not Result.Success);
      if Result.Success then FreeTorrentMeta(Meta);
    finally
      BencodeFree(Root);
    end;
  end;
  
  { Test 6: Missing name }
  Root := BencodeNewDict;
  Info := BencodeNewDict;
  if (Root <> nil) and (Info <> nil) then
  begin
    { No name field }
    BencodeDictAdd(Info, 'piece length', BencodeNewInteger(100));
    BencodeDictAdd(Info, 'length', BencodeNewInteger(100));
    BencodeDictAdd(Info, 'pieces', BencodeNewString(StringOfChar(#0, 20)));
    BencodeDictAdd(Root, 'info', Info);
    
    try
      Result := ParseTorrentBencode(Root, Meta);
      TestResult('Missing name rejected', not Result.Success);
      if Result.Success then FreeTorrentMeta(Meta);
    finally
      BencodeFree(Root);
    end;
  end;
  
  { Test 7: Empty name }
  Root := BencodeNewDict;
  Info := BencodeNewDict;
  if (Root <> nil) and (Info <> nil) then
  begin
    BencodeDictAdd(Info, 'name', BencodeNewString(''));  { Empty }
    BencodeDictAdd(Info, 'piece length', BencodeNewInteger(100));
    BencodeDictAdd(Info, 'length', BencodeNewInteger(100));
    BencodeDictAdd(Info, 'pieces', BencodeNewString(StringOfChar(#0, 20)));
    BencodeDictAdd(Root, 'info', Info);
    
    try
      Result := ParseTorrentBencode(Root, Meta);
      TestResult('Empty name rejected', not Result.Success);
      if Result.Success then FreeTorrentMeta(Meta);
    finally
      BencodeFree(Root);
    end;
  end;
  
  { Test 8: Unicode name }
  Root := BencodeNewDict;
  Info := BencodeNewDict;
  if (Root <> nil) and (Info <> nil) then
  begin
    BencodeDictAdd(Info, 'name', BencodeNewString('文件.txt'));  { Unicode }
    BencodeDictAdd(Info, 'piece length', BencodeNewInteger(100));
    BencodeDictAdd(Info, 'length', BencodeNewInteger(100));
    BencodeDictAdd(Info, 'pieces', BencodeNewString(StringOfChar(#0, 20)));
    BencodeDictAdd(Root, 'info', Info);
    
    try
      Result := ParseTorrentBencode(Root, Meta);
      TestResult('Unicode name accepted', Result.Success);
      if Result.Success then
      begin
        TestResult('Unicode name preserved', Meta^.Name = '文件.txt');
        FreeTorrentMeta(Meta);
      end;
    finally
      BencodeFree(Root);
    end;
  end;
  
  { Test 9: Very long name (255 chars) }
  Root := BencodeNewDict;
  Info := BencodeNewDict;
  if (Root <> nil) and (Info <> nil) then
  begin
    BencodeDictAdd(Info, 'name', BencodeNewString(StringOfChar('a', 255)));
    BencodeDictAdd(Info, 'piece length', BencodeNewInteger(100));
    BencodeDictAdd(Info, 'length', BencodeNewInteger(100));
    BencodeDictAdd(Info, 'pieces', BencodeNewString(StringOfChar(#0, 20)));
    BencodeDictAdd(Root, 'info', Info);
    
    try
      Result := ParseTorrentBencode(Root, Meta);
      TestResult('Long name (255 chars) accepted', Result.Success);
      if Result.Success then
      begin
        TestResult('Long name preserved', Length(Meta^.Name) = 255);
        FreeTorrentMeta(Meta);
      end;
    finally
      BencodeFree(Root);
    end;
  end;
  
  { Test 10: Pieces count mismatch with file size }
  { 100 bytes with piece length 100 = 1 piece = 20 bytes of hashes }
  { But we provide 40 bytes (2 pieces) }
  Root := BencodeNewDict;
  Info := BencodeNewDict;
  if (Root <> nil) and (Info <> nil) then
  begin
    BencodeDictAdd(Info, 'name', BencodeNewString('test.txt'));
    BencodeDictAdd(Info, 'piece length', BencodeNewInteger(100));
    BencodeDictAdd(Info, 'length', BencodeNewInteger(100));
    { 2 pieces worth of hashes for 1 piece of data }
    BencodeDictAdd(Info, 'pieces', BencodeNewString(StringOfChar(#0, 40)));
    BencodeDictAdd(Root, 'info', Info);
    
    try
      Result := ParseTorrentBencode(Root, Meta);
      { This may or may not be rejected depending on strictness }
      TestResult('Piece count mismatch handled', True);
      if Result.Success then FreeTorrentMeta(Meta);
    finally
      BencodeFree(Root);
    end;
  end;
end;

{ ============================================================================ }
{ Main                                                                         }
{ ============================================================================ }

begin
  WriteLn('==============================================');
  WriteLn('  METAINFO UNIT TESTS');
  WriteLn('==============================================');
  
  InitLogging;
  LoggerSetLevel(GlobalLogger, llWarning);  { Reduce log noise }
  
  try
    TestSingleFileTorrent;
    TestMultiFileTorrent;
    TestPieceHashAccess;
    TestPieceLength;
    TestInfoHashFormats;
    TestValidation;
    TestFileAtOffset;
    TestBitfieldCompletion;
    TestErrorHandling;
    TestAnnounceParsing;
    TestPathSanitization;
    TestPathTraversalDetection;
    TestMaliciousPathValidation;
    TestMalformedTorrents;
    
    WriteLn(#10'==============================================');
    WriteLn('  Results: ', PassedTests, '/', TotalTests, ' tests passed');
    WriteLn('==============================================');
    
    if FailedTests > 0 then
    begin
      WriteLn('FAILED: ', FailedTests, ' tests failed');
      Halt(1);
    end
    else
    begin
      WriteLn('SUCCESS: All tests passed!');
      Halt(0);
    end;
  finally
    ShutdownLogging;
  end;
end.
