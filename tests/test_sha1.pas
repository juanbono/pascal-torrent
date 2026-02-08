{
  test_sha1.pas - Comprehensive tests for SHA1 utilities
  
  Uses the shared test framework for consistent test reporting.
}

{$mode objfpc}{$H+}

program test_sha1;

uses
  SysUtils, StrUtils, sha1utils, sha1, testframework;

procedure TestNISTVectors;
{ NIST SHA1 test vectors from FIPS 180-2 }
begin
  WriteLn(#10'=== Testing NIST SHA1 Vectors ===');
  
  { Test 1: "abc" }
  TestResult('NIST vector "abc"',
             SHA1DigestToHex(SHA1String('abc')) = 
             'a9993e364706816aba3e25717850c26c9cd0d89d');
  
  { Test 2: "" (empty string) }
  TestResult('NIST vector "" (empty)',
             SHA1DigestToHex(SHA1String('')) = 
             'da39a3ee5e6b4b0d3255bfef95601890afd80709');
  
  { Test 3: "abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq" }
  TestResult('NIST vector long (abcdbcde...)',
             SHA1DigestToHex(SHA1String('abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq')) = 
             '84983e441c3bd26ebaae4aa1f95129e5e54670f1');
  
  { Test 4: "a" * 1000000 (million 'a' characters) }
  TestResult('NIST vector million "a"s',
             SHA1DigestToHex(SHA1String(StringOfChar('a', 1000000))) = 
             '34aa973cd4c4daa4f61eeb2bdbad27316534016f');
  
  { Test 5: "The quick brown fox jumps over the lazy dog" }
  TestResult('SHA1 "The quick brown fox..."',
             SHA1DigestToHex(SHA1String('The quick brown fox jumps over the lazy dog')) = 
             '2fd4e1c67a2d28fced849ee1bb76e7391b93eb12');
end;

procedure TestBufferHashing;
var
  Data: array[0..63] of Byte;
  BigData: array of Byte;
  I: Integer;
  Hash: TSHA1Digest;
begin
  WriteLn(#10'=== Testing Buffer Hashing ===');
  
  { Test 1: Single byte }
  Data[0] := Ord('A');
  Hash := SHA1Buffer(Data[0], 1);
  TestResult('Single byte "A"',
             SHA1DigestToHex(Hash) = '6dcd4ce23d88e2ee9568ba546c007c63d9131c1b');
  
  { Test 2: Multiple bytes }
  for I := 0 to 25 do
    Data[I] := Ord('a') + I;
  Hash := SHA1Buffer(Data, 26);
  TestResult('Alphabet (a-z)',
             SHA1DigestToHex(Hash) = '32d10c7b8cf96570ca04ce37f2a19d84240d3a89');
  
  { Test 3: Binary data - use allocated buffer }
  SetLength(BigData, 256);
  for I := 0 to 255 do
    BigData[I] := I;
  Hash := SHA1Buffer(BigData[0], 256);
  TestResult('Binary data (0-255)',
             SHA1DigestToHex(Hash) = '4916d6bdb7f78e6803698cab32d1586ea457dfc8');
end;

procedure TestIncrementalHashing;
var
  Context: TSHA1Context;
  Hash1, Hash2: TSHA1Digest;
begin
  WriteLn(#10'=== Testing Incremental Hashing ===');
  
  { Test 1: Incremental vs one-shot }
  SHA1Init(Context);
  SHA1Update(Context, PChar('ab')^, 2);
  SHA1Update(Context, PChar('c')^, 1);
  SHA1Final(Context, Hash1);
  
  Hash2 := SHA1String('abc');
  
  TestResult('Incremental matches one-shot',
             SHA1Equal(Hash1, Hash2));
  
  { Test 2: Many small updates }
  SHA1Init(Context);
  SHA1Update(Context, PChar('a')^, 1);
  SHA1Update(Context, PChar('b')^, 1);
  SHA1Update(Context, PChar('c')^, 1);
  SHA1Update(Context, PChar('d')^, 1);
  SHA1Update(Context, PChar('e')^, 1);
  SHA1Final(Context, Hash1);
  
  Hash2 := SHA1String('abcde');
  
  TestResult('Many small updates',
             SHA1Equal(Hash1, Hash2));
end;

procedure TestDigestUtilities;
var
  Digest: TSHA1Digest;
  HexStr: string;
  Recovered: TSHA1Digest;
begin
  WriteLn(#10'=== Testing Digest Utilities ===');
  
  { Test 1: Digest to hex }
  Digest := SHA1String('test');
  HexStr := SHA1DigestToHex(Digest);
  TestResult('Digest to hex conversion',
             Length(HexStr) = 40);
  
  { Test 2: Hex to digest }
  TestResult('Hex to digest conversion',
             SHA1HexToDigest(HexStr, Recovered) and SHA1Equal(Digest, Recovered));
  
  { Test 3: Invalid hex }
  TestResult('Invalid hex detection',
             not SHA1HexToDigest('not a hex string', Recovered));
  
  { Test 4: Wrong length hex }
  TestResult('Wrong length hex detection',
             not SHA1HexToDigest('abc123', Recovered));
  
  { Test 5: Empty digest check }
  SHA1Clear(Digest);
  TestResult('Empty digest detection',
             SHA1IsEmpty(Digest));
  
  { Test 6: Non-empty digest }
  Digest := SHA1String('test');
  TestResult('Non-empty digest',
             not SHA1IsEmpty(Digest));
  
  { Test 7: Copy digest }
  Digest := SHA1String('copy test');
  SHA1Copy(Digest, Recovered);
  TestResult('Copy digest',
             SHA1Equal(Digest, Recovered));
  
  { Test 8: Equal digests }
  Digest := SHA1String('same');
  Recovered := SHA1String('same');
  TestResult('Equal digests',
             SHA1Equal(Digest, Recovered));
  
  { Test 9: Different digests }
  Digest := SHA1String('one');
  Recovered := SHA1String('two');
  TestResult('Different digests',
             not SHA1Equal(Digest, Recovered));
  
  { Test 10: Hex case insensitivity }
  SHA1HexToDigest('A9993E364706816ABA3E25717850C26C9CD0D89D', Digest);
  SHA1HexToDigest('a9993e364706816aba3e25717850c26c9cd0d89d', Recovered);
  TestResult('Hex case insensitivity',
             SHA1Equal(Digest, Recovered));
end;

procedure TestBitTorrentSpecific;
var
  Digest: TSHA1Digest;
begin
  WriteLn(#10'=== Testing BitTorrent Specific ===');
  
  { Test 1: Verify piece with correct hash }
  Digest := SHA1String('piece data content');
  TestResult('Verify correct piece',
             VerifyPiece(PChar('piece data content'), 18, Digest));
  
  { Test 2: Verify piece with wrong hash }
  TestResult('Verify wrong piece fails',
             not VerifyPiece(PChar('different data'), 14, Digest));
  
  { Test 3: Verify with nil data }
  TestResult('Verify nil data fails',
             not VerifyPiece(nil, 10, Digest));
  
  { Test 4: Verify with zero length }
  TestResult('Verify zero length fails',
             not VerifyPiece(PChar('data'), 0, Digest));
end;

procedure TestLargeData;
const
  CHUNK_SIZE = 65536;
  TOTAL_SIZE = 1024 * 1024;  { 1 MB }
var
  Data: PByteArray;
  I: Integer;
  Hash1, Hash2: TSHA1Digest;
  Context: TSHA1Context;
  Pos: Integer;
begin
  WriteLn(#10'=== Testing Large Data ===');
  
  { Allocate large buffer }
  GetMem(Data, TOTAL_SIZE);
  
  { Fill with pattern }
  for I := 0 to TOTAL_SIZE - 1 do
    Data^[I] := I mod 256;
  
  { One-shot hash }
  Hash1 := SHA1Buffer(Data^, TOTAL_SIZE);
  
  { Incremental hash in chunks }
  SHA1Init(Context);
  Pos := 0;
  while Pos < TOTAL_SIZE do
  begin
    if Pos + CHUNK_SIZE <= TOTAL_SIZE then
    begin
      SHA1Update(Context, Data^[Pos], CHUNK_SIZE);
      Inc(Pos, CHUNK_SIZE);
    end
    else
    begin
      SHA1Update(Context, Data^[Pos], TOTAL_SIZE - Pos);
      Pos := TOTAL_SIZE;
    end;
  end;
  SHA1Final(Context, Hash2);
  
  TestResult('1MB incremental matches one-shot',
             SHA1Equal(Hash1, Hash2));
  
  FreeMem(Data);
end;

procedure TestEdgeCases;
var
  Hash: TSHA1Digest;
  I: Integer;
  Data: array of Byte;
begin
  WriteLn(#10'=== Testing Edge Cases ===');
  
  { Test 1: Very long string }
  TestResult('100KB string',
             Length(SHA1DigestToHex(SHA1String(StringOfChar('x', 100000)))) = 40);
  
  { Test 2: String with null bytes }
  SetLength(Data, 10);
  for I := 0 to 9 do
    Data[I] := I * 10;
  Hash := SHA1Buffer(Data[0], 10);
  TestResult('Binary data with nulls',
             not SHA1IsEmpty(Hash));
  
  { Test 3: Unicode data }
  Hash := SHA1String('Hello 世界 🌍');
  TestResult('Unicode string',
             Length(SHA1DigestToHex(Hash)) = 40);
  
  { Test 4: All null bytes }
  SetLength(Data, 100);
  for I := 0 to 99 do
    Data[I] := 0;
  Hash := SHA1Buffer(Data[0], 100);
  TestResult('All null bytes',
             not SHA1IsEmpty(Hash));
  
  { Test 5: All 0xFF bytes }
  for I := 0 to 99 do
    Data[I] := $FF;
  Hash := SHA1Buffer(Data[0], 100);
  TestResult('All 0xFF bytes',
             not SHA1IsEmpty(Hash));
end;

procedure TestBase32;
var
  Digest: TSHA1Digest;
  Base32: string;
begin
  WriteLn(#10'=== Testing Base32 Encoding ===');
  
  { Test 1: Known hash to base32 }
  SHA1HexToDigest('a9993e364706816aba3e25717850c26c9cd0d89d', Digest);
  Base32 := SHA1DigestToBase32(Digest);
  TestResult('Base32 encoding produces 32 chars',
             Length(Base32) = 32);
  
  { Test 2: Base32 is lowercase }
  TestResult('Base32 is lowercase',
             Base32 = LowerCase(Base32));
  
  { Test 3: Base32 uses valid characters }
  TestResult('Base32 uses valid characters',
             PosEx('0', Base32) = 0);  { 0 and 1 are not in base32 alphabet }
end;

procedure TestFileOperations;
var
  F: File;
  TestFilename: string;
  Content: string;
  Hash1, Hash2: TSHA1Digest;
  Size: Int64;
begin
  WriteLn(#10'=== Testing File Operations ===');
  
  TestFilename := 'test_sha1_tmp.txt';
  Content := 'This is test content for file hashing.';
  
  { Create test file }
  Assign(F, TestFilename);
  Rewrite(F, 1);
  BlockWrite(F, PChar(Content)^, Length(Content));
  Close(F);
  
  { Hash file }
  Hash1 := SHA1File(TestFilename);
  
  { Hash string }
  Hash2 := SHA1String(Content);
  
  TestResult('File hash matches string hash',
             SHA1Equal(Hash1, Hash2));
  
  { Check file size }
  Assign(F, TestFilename);
  Reset(F, 1);
  if IOResult = 0 then
  begin
    Size := FileSize(F);
    Close(F);
    TestResult('File size correct', Size = Length(Content));
  end
  else
    TestResult('File size retrieval', False);
  
  { Cleanup }
  Erase(F);
end;

procedure TestComputeInfoHash;
const
  { Simple torrent with info dict }
  TorrentData = 'd8:announce35:http://tracker.example.com/announce4:infod6:lengthi1234e4:name8:test.txteee';
var
  Digest: TSHA1Digest;
  HexHash: string;
  Result: Boolean;
begin
  WriteLn(#10'=== Testing ComputeInfoHash ===');
  
  { Test 1: Valid torrent with info dict }
  Result := ComputeInfoHash(PChar(TorrentData), Length(TorrentData), Digest);
  TestResult('ComputeInfoHash with valid torrent',
             Result);
  
  if Result then
  begin
    HexHash := SHA1DigestToHex(Digest);
    TestResult('Info hash is 40 hex chars',
               Length(HexHash) = 40);
    TestResult('Info hash is not empty',
               not SHA1IsEmpty(Digest));
  end;
  
  { Test 2: Nil data }
  Result := ComputeInfoHash(nil, 100, Digest);
  TestResult('ComputeInfoHash with nil data fails',
             not Result);
  
  { Test 3: Zero length }
  Result := ComputeInfoHash(PChar('test'), 0, Digest);
  TestResult('ComputeInfoHash with zero length fails',
             not Result);
  
  { Test 4: No info key }
  Result := ComputeInfoHash(PChar('d4:testi42ee'), 12, Digest);
  TestResult('ComputeInfoHash without info key fails',
             not Result);
end;

begin
  Randomize;
  
  WriteLn('==============================================');
  WriteLn('  SHA1 UNIT TEST SUITE');
  WriteLn('==============================================');
  
  TestNISTVectors;
  TestBufferHashing;
  TestIncrementalHashing;
  TestDigestUtilities;
  TestBitTorrentSpecific;
  TestLargeData;
  TestEdgeCases;
  TestBase32;
  TestFileOperations;
  TestComputeInfoHash;
  
  ExitWithResult;
end.
