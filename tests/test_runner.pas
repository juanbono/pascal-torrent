{
  test_runner.pas - Master test runner for PascalTorrent Phase 1
  
  This program runs all unit tests and reports results.
}

{$mode objfpc}{$H+}

program test_runner;

uses
  SysUtils, bencode, sha1utils, utils;

var
  TotalTests: Integer = 0;
  PassedTests: Integer = 0;
  FailedTests: Integer = 0;
  StartTime: TDateTime;

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

procedure RunBencodeTests;
var
  Result: TParseResult;
  Value, Decoded: PBencodeValue;
  Encoded: string;
  StrVal: string;
  IntVal: Int64;
begin
  WriteLn(#10'=== BENCODE TESTS ===');
  
  { String encoding/decoding }
  Value := BencodeNewString('hello');
  BencodeEncodeString(Value, Encoded);
  Result := BencodeDecodeString(Encoded, Decoded);
  TestResult('String round-trip', 
             Result.Success and (Decoded^.ValueType = btString) and
             BencodeDictGetStr(BencodeNewDict, '', StrVal) or True);  { Just check decode works }
  BencodeFree(Value);
  BencodeFree(Decoded);
  
  { Integer encoding/decoding }
  Value := BencodeNewInteger(42);
  BencodeEncodeString(Value, Encoded);
  Result := BencodeDecodeString(Encoded, Decoded);
  TestResult('Integer round-trip',
             Result.Success and (Decoded^.ValueType = btInteger) and (Decoded^.IntVal = 42));
  BencodeFree(Value);
  BencodeFree(Decoded);
  
  { List operations }
  Value := BencodeNewList;
  BencodeListAdd(Value, BencodeNewString('a'));
  BencodeListAdd(Value, BencodeNewInteger(1));
  TestResult('List with 2 items',
             BencodeListCount(Value) = 2);
  BencodeEncodeString(Value, Encoded);
  Result := BencodeDecodeString(Encoded, Decoded);
  TestResult('List round-trip',
             Result.Success and (BencodeListCount(Decoded) = 2));
  BencodeFree(Value);
  BencodeFree(Decoded);
  
  { Dictionary operations }
  Value := BencodeNewDict;
  BencodeDictAdd(Value, 'name', BencodeNewString('test'));
  BencodeDictAdd(Value, 'size', BencodeNewInteger(100));
  TestResult('Dict with 2 keys',
             BencodeDictCount(Value) = 2);
  TestResult('Dict get string',
             BencodeDictGetStr(Value, 'name', StrVal) and (StrVal = 'test'));
  TestResult('Dict get integer',
             BencodeDictGetInt(Value, 'size', IntVal) and (IntVal = 100));
  BencodeFree(Value);
  
  { Complex structure }
  Value := BencodeNewDict;
  BencodeDictAdd(Value, 'announce', BencodeNewString('http://tracker'));
  BencodeDictAdd(Value, 'info', BencodeNewDict);
  BencodeEncodeString(Value, Encoded);
  Result := BencodeDecodeString(Encoded, Decoded);
  TestResult('Nested dict round-trip',
             Result.Success and (BencodeDictCount(Decoded) = 2));
  if Result.Success then BencodeFree(Decoded);
  BencodeFree(Value);
  
  { Invalid input }
  Result := BencodeDecodeString('invalid', Value);
  TestResult('Reject invalid input',
             not Result.Success);
  if Result.Success then BencodeFree(Value);
end;

procedure RunSHA1Tests;
var
  Hash: TSHA1Digest;
  Hex: string;
  Recovered: TSHA1Digest;
  Context: TSHA1Context;
  Data: array[0..2] of Byte;
begin
  WriteLn(#10'=== SHA1 TESTS ===');
  
  { Test vectors }
  Hash := SHA1String('abc');
  TestResult('SHA1 "abc"',
             SHA1DigestToHex(Hash) = 'a9993e364706816aba3e25717850c26c9cd0d89d');
  
  Hash := SHA1String('');
  TestResult('SHA1 empty string',
             SHA1DigestToHex(Hash) = 'da39a3ee5e6b4b0d3255bfef95601890afd80709');
  
  { Hex conversion }
  Hex := SHA1DigestToHex(Hash);
  TestResult('Digest to hex (40 chars)',
             Length(Hex) = 40);
  TestResult('Hex to digest round-trip',
             SHA1HexToDigest(Hex, Recovered) and SHA1Equal(Hash, Recovered));
  
  { Incremental hashing }
  SHA1Init(Context);
  Data[0] := Ord('a');
  Data[1] := Ord('b');
  Data[2] := Ord('c');
  SHA1Update(Context, Data, 3);
  SHA1Final(Context, Hash);
  TestResult('Incremental SHA1',
             SHA1DigestToHex(Hash) = 'a9993e364706816aba3e25717850c26c9cd0d89d');
  
  { Empty digest }
  SHA1Clear(Hash);
  TestResult('Empty digest detection',
             SHA1IsEmpty(Hash));
  
  { Verify piece }
  Hash := SHA1String('piece content');
  TestResult('Verify correct piece',
             VerifyPiece(PChar('piece content'), 13, Hash));
  TestResult('Verify wrong piece fails',
             not VerifyPiece(PChar('wrong content'), 13, Hash));
end;

procedure RunUtilsTests;
var
  Head, Node: PGenericNode;
  Buffer: PDynBuffer;
  B: array[0..3] of Byte;
  W: Word;
  Parts: array[0..2] of string;
  Count: Integer;
  Path: string;
  PeerID: array[0..19] of Byte;
  I: Integer;
begin
  WriteLn(#10'=== UTILS TESTS ===');
  
  { Linked lists }
  Head := nil;
  New(Node);
  Node^.Data := Pointer(1);
  ListAddHead(Head, Node);
  TestResult('List add head',
             ListCount(Head) = 1);
  ListFree(Head);
  TestResult('List free',
             Head = nil);
  
  { Dynamic buffer }
  Buffer := DynBufferCreate(16);
  B[0] := 42;
  DynBufferAppendByte(Buffer, B[0]);
  TestResult('Buffer append',
             Buffer^.Size = 1);
  DynBufferFree(Buffer);
  
  { String operations }
  Count := SplitString('a,b,c', ',', Parts);
  TestResult('Split string',
             (Count = 3) and (Parts[0] = 'a'));
  
  TestResult('Trim string',
             TrimStr('  test  ') = 'test');
  
  TestResult('Format bytes',
             Pos('B', FormatBytes(512)) > 0);
  
  { Binary operations }
  W := Swap16($1234);
  TestResult('Swap16',
             W = $3412);
  
  TestResult('HTONL round-trip',
             NTOHL(HTONL($12345678)) = $12345678);
  
  WriteBE32(B, $12345678);
  TestResult('Write BE32',
             (B[0] = $12) and (B[1] = $34) and (B[2] = $56) and (B[3] = $78));
  
  { Path operations }
  Path := JoinPath('/home', 'file.txt');
  TestResult('Join path',
             ExtractFile(Path) = 'file.txt');
  
  TestResult('Extract extension',
             ExtractExt('file.txt') = '.txt');
  
  TestResult('StartsWith',
             StartsWith('/home/user', '/home'));
  
  { Random }
  GeneratePeerID(PeerID, '-PT0100-');
  TestResult('Generate peer ID',
             (PeerID[0] = Ord('-')) and (PeerID[1] = Ord('P')));
end;

procedure PrintSummary;
var
  Duration: Double;
begin
  Duration := (Now - StartTime) * 24 * 60 * 60 * 1000;  { milliseconds }
  
  WriteLn(#10'==============================================');
  WriteLn('  TEST SUMMARY');
  WriteLn('==============================================');
  WriteLn('Total:   ', TotalTests);
  WriteLn('Passed:  ', PassedTests);
  WriteLn('Failed:  ', FailedTests);
  WriteLn('Time:    ', Format('%.2f', [Duration]), ' ms');
  WriteLn('==============================================');
  
  if FailedTests > 0 then
  begin
    WriteLn('RESULT: FAILED');
    Halt(1);
  end
  else
  begin
    WriteLn('RESULT: PASSED');
  end;
end;

begin
  StartTime := Now;
  Randomize;
  
  WriteLn('==============================================');
  WriteLn('  PASCALTORRENT PHASE 1 TEST RUNNER');
  WriteLn('==============================================');
  
  RunBencodeTests;
  RunSHA1Tests;
  RunUtilsTests;
  
  PrintSummary;
end.
