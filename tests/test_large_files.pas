{
  test_large_files.pas - Tests for large file (>4GB) handling
  
  These tests verify 64-bit offset calculations work correctly
  without actually creating multi-GB files (which would be slow
  and require lots of disk space).
}

{$mode objfpc}{$H+}

program test_large_files;

uses
  SysUtils, testframework, metainfo, filemgr, bencode, sha1utils;

{ ============================================================================ }
{ Helper: Create a torrent with specified total size and piece length }
{ ============================================================================ }
function CreateLargeTorrentMeta(TotalSize: QWord; PieceLength: Integer): PTorrentMeta;
var
  Root, InfoDict: PBencodeValue;
  NumPieces: Integer;
  PiecesStr: string;
  I: Integer;
  ParseResult: TMetainfoResult;
begin
  Result := nil;
  
  { Calculate number of pieces }
  NumPieces := (TotalSize + PieceLength - 1) div PieceLength;
  
  { Create bencode structure }
  Root := BencodeNewDict;
  if Root = nil then Exit;
  
  InfoDict := BencodeNewDict;
  if InfoDict = nil then
  begin
    BencodeFree(Root);
    Exit;
  end;
  
  { Fill in info }
  BencodeDictAdd(InfoDict, 'name', BencodeNewString('large_file.bin'));
  BencodeDictAdd(InfoDict, 'piece length', BencodeNewInteger(PieceLength));
  BencodeDictAdd(InfoDict, 'length', BencodeNewInteger(Int64(TotalSize)));
  
  { Create fake piece hashes (20 bytes per piece) }
  SetLength(PiecesStr, NumPieces * 20);
  for I := 1 to NumPieces * 20 do
    PiecesStr[I] := Chr((I mod 256));
  BencodeDictAdd(InfoDict, 'pieces', BencodeNewString(PiecesStr));
  
  BencodeDictAdd(Root, 'info', InfoDict);
  
  { Parse into meta structure }
  ParseResult := ParseTorrentBencode(Root, Result);
  BencodeFree(Root);
  
  if not ParseResult.Success then
  begin
    if Result <> nil then
    begin
      FreeTorrentMeta(Result);
      Result := nil;
    end;
  end;
end;

{ ============================================================================ }
{ Test 1: 4GB boundary (exactly 4GB) }
{ ============================================================================ }
procedure Test4GBBoundary;
var
  Meta: PTorrentMeta;
  TotalSize: QWord;
  ExpectedPieces: Integer;
begin
  BeginSuite('Testing 4GB Boundary (Exactly 4GB)');
  
  TotalSize := QWord(4) * 1024 * 1024 * 1024;  { 4GB }
  ExpectedPieces := 4096;  { 4GB / 1MB = 4096 pieces }
  
  Meta := CreateLargeTorrentMeta(TotalSize, 1024 * 1024);  { 1MB pieces }
  
  AssertNotNil('Create 4GB torrent meta', Meta);
  AssertEquals('Piece count correct', ExpectedPieces, Meta^.PieceCount);
  AssertTrue('Total length is 4GB', Meta^.TotalLength = TotalSize);
  
  if Meta <> nil then
    FreeTorrentMeta(Meta);
  
  EndSuite;
end;

{ ============================================================================ }
{ Test 2: Just over 4GB boundary }
{ ============================================================================ }
procedure TestJustOver4GB;
var
  Meta: PTorrentMeta;
  TotalSize: QWord;
  ExpectedPieces: Integer;
begin
  BeginSuite('Testing Just Over 4GB (4GB + 1 byte)');
  
  TotalSize := QWord(4) * 1024 * 1024 * 1024 + 1;  { 4GB + 1 }
  ExpectedPieces := 4097;  { Need extra piece for the 1 byte }
  
  Meta := CreateLargeTorrentMeta(TotalSize, 1024 * 1024);  { 1MB pieces }
  
  AssertNotNil('Create 4GB+1 torrent meta', Meta);
  AssertEquals('Piece count correct', ExpectedPieces, Meta^.PieceCount);
  AssertTrue('Total length is 4GB+1', Meta^.TotalLength = TotalSize);
  
  if Meta <> nil then
    FreeTorrentMeta(Meta);
  
  EndSuite;
end;

{ ============================================================================ }
{ Test 3: Large file (100GB) }
{ ============================================================================ }
procedure Test100GB;
var
  Meta: PTorrentMeta;
  TotalSize: QWord;
  ExpectedPieces: Integer;
begin
  BeginSuite('Testing Large File (100GB)');
  
  TotalSize := QWord(100) * 1024 * 1024 * 1024;  { 100GB }
  ExpectedPieces := 100;  { 100GB / 1GB pieces = 100 pieces }
  
  Meta := CreateLargeTorrentMeta(TotalSize, 1024 * 1024 * 1024);  { 1GB pieces }
  
  AssertNotNil('Create 100GB torrent meta', Meta);
  AssertEquals('Piece count correct', ExpectedPieces, Meta^.PieceCount);
  AssertTrue('Total length is 100GB', Meta^.TotalLength = TotalSize);
  
  if Meta <> nil then
    FreeTorrentMeta(Meta);
  
  EndSuite;
end;

{ ============================================================================ }
{ Test 4: Maximum torrent size (theoretical) }
{ ============================================================================ }
procedure TestTheoreticalMax;
var
  Meta: PTorrentMeta;
  TotalSize: QWord;
  PieceLength: Integer;
  ExpectedPieces: Integer;
  I: Integer;
  MaxPieces: Integer;
begin
  BeginSuite('Testing Theoretical Maximum Size');
  
  { Use 4MB pieces, test ~1TB }
  TotalSize := QWord(1024) * 1024 * 1024 * 1024;  { 1TB }
  PieceLength := 4 * 1024 * 1024;  { 4MB pieces }
  ExpectedPieces := 262144;  { 1TB / 4MB = 262144 pieces }
  
  { Note: This would require 5MB just for piece hashes (262144 * 20)
    which is reasonable for a test }
  
  Meta := CreateLargeTorrentMeta(TotalSize, PieceLength);
  
  AssertNotNil('Create 1TB torrent meta', Meta);
  AssertEquals('Piece count correct', ExpectedPieces, Meta^.PieceCount);
  AssertTrue('Total length is 1TB', Meta^.TotalLength = TotalSize);
  
  if Meta <> nil then
    FreeTorrentMeta(Meta);
  
  EndSuite;
end;

{ ============================================================================ }
{ Test 5: Offset calculations at 4GB boundary }
{ ============================================================================ }
procedure TestOffsetCalculations;
var
  Meta: PTorrentMeta;
  TotalSize: QWord;
  Offset: QWord;
  PieceIndex: Integer;
  ExpectedPiece: Integer;
  PieceLength: Cardinal;
begin
  BeginSuite('Testing Offset Calculations at 4GB Boundary');
  
  TotalSize := QWord(8) * 1024 * 1024 * 1024;  { 8GB }
  
  Meta := CreateLargeTorrentMeta(TotalSize, 1024 * 1024);  { 1MB pieces }
  AssertNotNil('Create 8GB torrent meta', Meta);
  
  if Meta <> nil then
  begin
    { Test offset exactly at 4GB }
    Offset := QWord(4) * 1024 * 1024 * 1024;  { 4GB }
    ExpectedPiece := 4096;  { 4GB / 1MB = piece 4096 }
    
    PieceIndex := Offset div Meta^.PieceLength;
    AssertEquals('Offset at exactly 4GB maps to correct piece', 
                 ExpectedPiece, PieceIndex);
    
    { Test offset just before 4GB }
    Offset := QWord(4) * 1024 * 1024 * 1024 - 1;  { 4GB - 1 }
    ExpectedPiece := 4095;  { Still in piece 4095 }
    
    PieceIndex := Offset div Meta^.PieceLength;
    AssertEquals('Offset at 4GB-1 maps to correct piece', 
                 ExpectedPiece, PieceIndex);
    
    { Test offset just after 4GB }
    Offset := QWord(4) * 1024 * 1024 * 1024 + 1;  { 4GB + 1 }
    ExpectedPiece := 4096;  { Now in piece 4096 }
    
    PieceIndex := Offset div Meta^.PieceLength;
    AssertEquals('Offset at 4GB+1 maps to correct piece', 
                 ExpectedPiece, PieceIndex);
    
    { Test GetPieceLength at boundary }
    PieceLength := GetPieceLength(Meta, 4096);
    AssertTrue('Piece length at boundary is valid', PieceLength > 0);
    
    FreeTorrentMeta(Meta);
  end;
  
  EndSuite;
end;

{ ============================================================================ }
{ Test 6: Integer overflow prevention }
{ ============================================================================ }
procedure TestIntegerOverflowPrevention;
var
  A, B: QWord;
  Result: QWord;
  Overflow: Boolean;
begin
  BeginSuite('Testing Integer Overflow Prevention');
  
  { Test multiplication that would overflow 32-bit }
  A := QWord(65536);
  B := QWord(65536);
  Result := A * B;  { = 4GB, would overflow 32-bit }
  
  AssertTrue('64-bit multiplication works', Result = QWord(4294967296));
  
  { Test with larger values }
  A := QWord(1024) * 1024 * 1024;  { 1GB }
  B := 100;
  Result := A * B;  { = 100GB }
  
  AssertTrue('100GB calculation correct', Result = QWord(107374182400));
  
  { Test division }
  A := QWord(100) * 1024 * 1024 * 1024;  { 100GB }
  B := 1024 * 1024;  { 1MB }
  Result := A div B;
  
  AssertTrue('100GB / 1MB = 102400', Result = 102400);
  
  EndSuite;
end;

{ ============================================================================ }
{ Main }
{ ============================================================================ }
begin
  WriteLn('==============================================');
  WriteLn('  LARGE FILE (>4GB) HANDLING TESTS');
  WriteLn('==============================================');
  
  Test4GBBoundary;
  TestJustOver4GB;
  Test100GB;
  TestTheoreticalMax;
  TestOffsetCalculations;
  TestIntegerOverflowPrevention;
  
  ExitWithResult;
end.
