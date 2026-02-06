{
  test_bencode_extended.pas - Additional critical tests for bencode unit
  
  This test suite covers:
  - Untested public API functions
  - Error conditions and edge cases
  - File I/O operations
  - Memory safety under stress
}

{$mode objfpc}{$H+}

program test_bencode_extended;

uses
  SysUtils, bencode;

var
  TotalTests: Integer = 0;
  PassedTests: Integer = 0;
  FailedTests: Integer = 0;

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
{ Test 1: Untested API Functions                                              }
{ ============================================================================ }

procedure TestBencodeCalcSize;
var
  Value: PBencodeValue;
  Size: Integer;
  Encoded: string;
begin
  WriteLn(#10'=== Testing BencodeCalcSize ===');
  
  { Test 1: String size }
  Value := BencodeNewString('spam');
  TestResult('CalcSize for string',
             BencodeCalcSize(Value, Size) and (Size = 6));  { '4:spam' = 6 }
  BencodeFree(Value);
  
  { Test 2: Integer size }
  Value := BencodeNewInteger(42);
  TestResult('CalcSize for integer',
             BencodeCalcSize(Value, Size) and (Size = 4));  { 'i42e' = 4 }
  BencodeFree(Value);
  
  { Test 3: Empty list size }
  Value := BencodeNewList;
  TestResult('CalcSize for empty list',
             BencodeCalcSize(Value, Size) and (Size = 2));  { 'le' = 2 }
  BencodeFree(Value);
  
  { Test 4: Verify calc size matches actual }
  Value := BencodeNewDict;
  BencodeDictAdd(Value, 'key', BencodeNewString('value'));
  BencodeCalcSize(Value, Size);
  BencodeEncodeString(Value, Encoded);
  TestResult('CalcSize matches actual encoded size',
             Size = Length(Encoded));
  BencodeFree(Value);
  
  { Test 5: Nil value }
  TestResult('CalcSize returns false for nil',
             not BencodeCalcSize(nil, Size));
end;

procedure TestBencodeDictGetTyped;
var
  Dict: PBencodeValue;
  ListVal, DictVal: PBencodeValue;
begin
  WriteLn(#10'=== Testing BencodeDictGetList/Dict ===');
  
  { Test 1: Get list from dict }
  Dict := BencodeNewDict;
  BencodeDictAdd(Dict, 'list', BencodeNewList);
  BencodeListAdd(BencodeDictGet(Dict, 'list'), BencodeNewInteger(1));
  
  TestResult('DictGetList returns correct type',
             BencodeDictGetList(Dict, 'list', ListVal) and 
             (ListVal <> nil) and (ListVal^.ValueType = btList));
  
  TestResult('DictGetList returns false for wrong type',
             not BencodeDictGetList(Dict, 'nonexistent', ListVal));
  BencodeFree(Dict);
  
  { Test 2: Get dict from dict }
  Dict := BencodeNewDict;
  BencodeDictAdd(Dict, 'nested', BencodeNewDict);
  BencodeDictAdd(BencodeDictGet(Dict, 'nested'), 'key', BencodeNewString('val'));
  
  TestResult('DictGetDict returns correct type',
             BencodeDictGetDict(Dict, 'nested', DictVal) and 
             (DictVal <> nil) and (DictVal^.ValueType = btDict));
  
  TestResult('DictGetDict returns false for wrong type',
             not BencodeDictGetDict(Dict, 'nonexistent', DictVal));
  BencodeFree(Dict);
end;

procedure TestBencodeToDebugString;
var
  Value: PBencodeValue;
  DebugStr: string;
begin
  WriteLn(#10'=== Testing BencodeToDebugString ===');
  
  { Test 1: String debug }
  Value := BencodeNewString('test');
  DebugStr := BencodeToDebugString(Value);
  TestResult('Debug string for string value',
             Pos('"test"', DebugStr) > 0);
  BencodeFree(Value);
  
  { Test 2: Integer debug }
  Value := BencodeNewInteger(42);
  DebugStr := BencodeToDebugString(Value);
  TestResult('Debug string for integer value',
             DebugStr = '42');
  BencodeFree(Value);
  
  { Test 3: List debug }
  Value := BencodeNewList;
  BencodeListAdd(Value, BencodeNewInteger(1));
  BencodeListAdd(Value, BencodeNewInteger(2));
  DebugStr := BencodeToDebugString(Value);
  TestResult('Debug string for list',
             (Pos('[', DebugStr) > 0) and (Pos('1', DebugStr) > 0));
  BencodeFree(Value);
  
  { Test 4: Dict debug }
  Value := BencodeNewDict;
  BencodeDictAdd(Value, 'key', BencodeNewString('val'));
  DebugStr := BencodeToDebugString(Value);
  TestResult('Debug string for dict',
             (Pos('{', DebugStr) > 0) and (Pos('key', DebugStr) > 0));
  BencodeFree(Value);
  
  { Test 5: Nil value }
  DebugStr := BencodeToDebugString(nil);
  TestResult('Debug string for nil value',
             Pos('null', DebugStr) > 0);
end;

{ ============================================================================ }
{ Test 2: Error Conditions & Nil Handling                                      }
{ ============================================================================ }

procedure TestNilHandling;
var
  Value: PBencodeValue;
  List: PBencodeValue;
  Dict: PBencodeValue;
  IntVal: Int64;
  StrVal: string;
  Size: Integer;
  Buffer: PChar;
  BufLen: Integer;
  S: string;
begin
  WriteLn(#10'=== Testing Nil Pointer Handling ===');
  
  { BencodeFree should not crash on nil }
  BencodeFree(nil);
  TestResult('BencodeFree(nil) is safe', True);
  
  { BencodeCalcSize should return false on nil }
  TestResult('BencodeCalcSize(nil) returns false',
             not BencodeCalcSize(nil, Size));
  
  { BencodeEncode should return false on nil }
  TestResult('BencodeEncode(nil) returns false',
             not BencodeEncode(nil, Buffer, BufLen));
  
  { BencodeEncodeString should return false on nil }
  TestResult('BencodeEncodeString(nil) returns false',
             not BencodeEncodeString(nil, S));
  
  { BencodeListAdd on nil list }
  Value := BencodeNewInteger(1);
  TestResult('BencodeListAdd(nil, value) returns false',
             not BencodeListAdd(nil, Value));
  BencodeFree(Value);
  
  { BencodeListAdd on non-list }
  List := BencodeNewList;
  Value := BencodeNewInteger(1);
  TestResult('BencodeListAdd(integer, value) returns false',
             not BencodeListAdd(Value, List));  { Wrong order - Value is integer }
  BencodeFree(Value);
  BencodeFree(List);
  
  { BencodeListCount on nil }
  TestResult('BencodeListCount(nil) returns 0',
             BencodeListCount(nil) = 0);
  
  { BencodeListCount on non-list }
  Value := BencodeNewInteger(1);
  TestResult('BencodeListCount(integer) returns 0',
             BencodeListCount(Value) = 0);
  BencodeFree(Value);
  
  { BencodeListGet on nil }
  TestResult('BencodeListGet(nil, 0) returns nil',
             BencodeListGet(nil, 0) = nil);
  
  { BencodeListGet with negative index }
  List := BencodeNewList;
  BencodeListAdd(List, BencodeNewInteger(1));
  TestResult('BencodeListGet(list, -1) returns nil',
             BencodeListGet(List, -1) = nil);
  TestResult('BencodeListGet(list, 999) returns nil (out of bounds)',
             BencodeListGet(List, 999) = nil);
  BencodeFree(List);
  
  { BencodeDictAdd on nil }
  Value := BencodeNewInteger(1);
  TestResult('BencodeDictAdd(nil, key, value) returns false',
             not BencodeDictAdd(nil, 'key', Value));
  BencodeFree(Value);
  
  { BencodeDictAdd on non-dict }
  Value := BencodeNewInteger(1);
  TestResult('BencodeDictAdd(integer, key, value) returns false',
             not BencodeDictAdd(Value, 'key', BencodeNewString('val')));
  BencodeFree(Value);
  
  { BencodeDictGet on nil }
  TestResult('BencodeDictGet(nil, key) returns nil',
             BencodeDictGet(nil, 'key') = nil);
  
  { BencodeDictGet on non-dict }
  Value := BencodeNewInteger(1);
  TestResult('BencodeDictGet(integer, key) returns nil',
             BencodeDictGet(Value, 'key') = nil);
  BencodeFree(Value);
  
  { BencodeDictHasKey on nil }
  TestResult('BencodeDictHasKey(nil, key) returns false',
             not BencodeDictHasKey(nil, 'key'));
  
  { BencodeDictCount on nil }
  TestResult('BencodeDictCount(nil) returns 0',
             BencodeDictCount(nil) = 0);
  
  { BencodeDictGetStr on nil }
  TestResult('BencodeDictGetStr(nil, key, s) returns false',
             not BencodeDictGetStr(nil, 'key', StrVal));
  
  { BencodeDictGetInt on nil }
  TestResult('BencodeDictGetInt(nil, key, i) returns false',
             not BencodeDictGetInt(nil, 'key', IntVal));
  
  { BencodeDictGetList on nil }
  List := nil;
  TestResult('BencodeDictGetList(nil, key, list) returns false',
             not BencodeDictGetList(nil, 'key', List));
  
  { BencodeDictGetDict on nil }
  Dict := nil;
  TestResult('BencodeDictGetDict(nil, key, dict) returns false',
             not BencodeDictGetDict(nil, 'key', Dict));
  
  { BencodeEqual with nil }
  Value := BencodeNewInteger(1);
  TestResult('BencodeEqual(nil, value) returns false',
             not BencodeEqual(nil, Value));
  TestResult('BencodeEqual(value, nil) returns false',
             not BencodeEqual(Value, nil));
  TestResult('BencodeEqual(nil, nil) returns true',
             BencodeEqual(nil, nil));
  BencodeFree(Value);
  
  { BencodeClone nil }
  TestResult('BencodeClone(nil) returns nil',
             BencodeClone(nil) = nil);
end;

{ ============================================================================ }
{ Test 3: File I/O Operations                                                   }
{ ============================================================================ }

procedure TestFileOperations;
var
  Result: TParseResult;
  Value: PBencodeValue;
  F: File;
  TestFilename: string;
  Content: string;
  BytesWritten: LongInt;
const
  ValidTorrent = 'd8:announce35:http://tracker.example.com/announce13:creation datei1609459200e4:infod6:lengthi12345e4:name8:test.txt12:piece lengthi262144e6:pieces20:01234567890123456789ee';
begin
  WriteLn(#10'=== Testing File Operations ===');
  
  TestFilename := 'test_bencode_temp.torrent';
  
  { Test 1: Non-existent file }
  Result := BencodeDecodeFile('/nonexistent/path/file.torrent', Value);
  TestResult('Non-existent file returns error',
             not Result.Success and (Pos('Cannot open', Result.ErrorMsg) > 0));
  
  { Test 2: Empty file }
  Assign(F, TestFilename);
  Rewrite(F, 1);
  Close(F);
  Result := BencodeDecodeFile(TestFilename, Value);
  TestResult('Empty file returns error',
             not Result.Success and (Pos('Empty', Result.ErrorMsg) > 0));
  Erase(F);
  
  { Test 3: Valid torrent file }
  Content := ValidTorrent;
  Assign(F, TestFilename);
  Rewrite(F, 1);
  BlockWrite(F, PChar(Content)^, Length(Content), BytesWritten);
  Close(F);
  
  Result := BencodeDecodeFile(TestFilename, Value);
  TestResult('Valid torrent file parses correctly',
             Result.Success and (Value <> nil));
  if Result.Success then
  begin
    TestResult('  Has announce key',
               BencodeDictHasKey(Value, 'announce'));
    TestResult('  Has info dict',
               BencodeDictHasKey(Value, 'info'));
    BencodeFree(Value);
  end;
  Erase(F);
  
  { Test 4: Invalid bencode in file }
  Content := 'this is not bencode';
  Assign(F, TestFilename);
  Rewrite(F, 1);
  BlockWrite(F, PChar(Content)^, Length(Content), BytesWritten);
  Close(F);
  
  Result := BencodeDecodeFile(TestFilename, Value);
  TestResult('Invalid bencode in file returns error',
             not Result.Success);
  if Result.Success then BencodeFree(Value);
  Erase(F);
  
  { Test 5: File with trailing data (should fail with strict parser) }
  Content := '4:testextra garbage';
  Assign(F, TestFilename);
  Rewrite(F, 1);
  BlockWrite(F, PChar(Content)^, Length(Content), BytesWritten);
  Close(F);
  
  Result := BencodeDecodeFile(TestFilename, Value);
  TestResult('File with trailing data returns error',
             not Result.Success and (Pos('Trailing', Result.ErrorMsg) > 0));
  if Result.Success then BencodeFree(Value);
  Erase(F);
end;

{ ============================================================================ }
{ Test 4: Edge Cases & Stress Tests                                             }
{ ============================================================================ }

procedure TestIntegerEdgeCases;
var
  Result: TParseResult;
  Value: PBencodeValue;
begin
  WriteLn(#10'=== Testing Integer Edge Cases ===');
  
  { Test 1: Zero }
  Result := BencodeDecodeString('i0e', Value);
  TestResult('Integer zero (i0e)',
             Result.Success and (Value^.IntVal = 0));
  if Result.Success then BencodeFree(Value);
  
  { Test 2: Maximum 32-bit integer }
  Result := BencodeDecodeString('i2147483647e', Value);
  TestResult('Max 32-bit integer (2147483647)',
             Result.Success and (Value^.IntVal = 2147483647));
  if Result.Success then BencodeFree(Value);
  
  { Test 3: Minimum 32-bit integer }
  Result := BencodeDecodeString('i-2147483648e', Value);
  TestResult('Min 32-bit integer (-2147483648)',
             Result.Success and (Value^.IntVal = -2147483648));
  if Result.Success then BencodeFree(Value);
  
  { Test 4: Large 64-bit integer }
  Result := BencodeDecodeString('i9223372036854775807e', Value);
  TestResult('Max 64-bit integer',
             Result.Success and (Value^.IntVal = 9223372036854775807));
  if Result.Success then BencodeFree(Value);
  
  { Test 5: Leading zeros should fail }
  Result := BencodeDecodeString('i01e', Value);
  TestResult('Leading zero (i01e) fails',
             not Result.Success);
  if Result.Success then BencodeFree(Value);
  
  { Test 6: Negative zero should fail }
  Result := BencodeDecodeString('i-0e', Value);
  TestResult('Negative zero (i-0e) fails',
             not Result.Success);
  if Result.Success then BencodeFree(Value);
  
  { Test 7: Empty integer should fail }
  Result := BencodeDecodeString('ie', Value);
  TestResult('Empty integer (ie) fails',
             not Result.Success);
  if Result.Success then BencodeFree(Value);
  
  { Test 8: Double negative should fail }
  Result := BencodeDecodeString('i--1e', Value);
  TestResult('Double negative (i--1e) fails',
             not Result.Success);
  if Result.Success then BencodeFree(Value);
  
  { Test 9: Plus sign should fail }
  Result := BencodeDecodeString('i+1e', Value);
  TestResult('Plus sign (i+1e) fails',
             not Result.Success);
  if Result.Success then BencodeFree(Value);
end;

procedure TestStringEdgeCases;
var
  Result: TParseResult;
  Value: PBencodeValue;
begin
  WriteLn(#10'=== Testing String Edge Cases ===');
  
  { Test 1: String with length 0 }
  Result := BencodeDecodeString('0:', Value);
  TestResult('Empty string (0:)',
             Result.Success and (Value^.StrLen = 0));
  if Result.Success then BencodeFree(Value);
  
  { Test 2: String length with leading zeros should fail }
  Result := BencodeDecodeString('01:x', Value);
  TestResult('Leading zero in length (01:x) fails',
             not Result.Success);
  if Result.Success then BencodeFree(Value);
  
  { Test 3: String with colon in data }
  Result := BencodeDecodeString('5:a:b:c', Value);
  TestResult('String with colons (5:a:b:c)',
             Result.Success and (Value^.StrLen = 5));
  if Result.Success then BencodeFree(Value);
  
  { Test 4: String containing 'e' (could be confused with terminator) }
  Result := BencodeDecodeString('5:hello', Value);
  TestResult('String containing letters',
             Result.Success and (StrLComp(Value^.StrVal, 'hello', 5) = 0));
  if Result.Success then BencodeFree(Value);
  
  { Test 5: Very long length that exceeds available data }
  Result := BencodeDecodeString('999999:x', Value);
  TestResult('Length exceeds data fails',
             not Result.Success);
  if Result.Success then BencodeFree(Value);
end;

procedure TestBinaryKeys;
var
  Result: TParseResult;
  Value: PBencodeValue;
  Key: array[0..3] of Byte;
  Encoded: string;
  FoundValue: PBencodeValue;
  I: Integer;
begin
  WriteLn(#10'=== Testing Binary Dictionary Keys ===');
  
  { Create a dict with binary key }
  Value := BencodeNewDict;
  
  { Add entry with binary key (null bytes) }
  Encoded := 'd4:'#0#1#2#3'i42ee';  { Key is 4 bytes: 0,1,2,3 }
  Result := BencodeDecodeString(Encoded, Value);
  TestResult('Dictionary with binary key parses',
             Result.Success and (BencodeDictCount(Value) = 1));
  if Result.Success then
    BencodeFree(Value);
end;

procedure TestDeepNesting;
var
  Result: TParseResult;
  Value: PBencodeValue;
  I: Integer;
  DeepList: string;
begin
  WriteLn(#10'=== Testing Deep Nesting ===');
  
  { Test 1: Moderately nested structure (20 levels) }
  DeepList := '';
  for I := 1 to 20 do
    DeepList := DeepList + 'l';
  DeepList := DeepList + 'i1e';  { Innermost value }
  for I := 1 to 20 do
    DeepList := DeepList + 'e';
  
  Result := BencodeDecodeString(DeepList, Value);
  TestResult('20-level nested list parses',
             Result.Success);
  if Result.Success then BencodeFree(Value);
  
  { Test 2: Nested dicts }
  Result := BencodeDecodeString('d1:ad1:bd1:ci1eeee', Value);
  TestResult('Nested dictionaries parse',
             Result.Success and (BencodeDictCount(Value) = 1));
  if Result.Success then BencodeFree(Value);
end;

procedure TestStressMemory;
var
  Dict: PBencodeValue;
  I: Integer;
begin
  WriteLn(#10'=== Testing Memory Stress ===');
  
  { Test 1: Dictionary with many entries }
  Dict := BencodeNewDict;
  for I := 1 to 1000 do
    BencodeDictAdd(Dict, 'key' + IntToStr(I), BencodeNewInteger(I));
  TestResult('Dictionary with 1000 entries',
             BencodeDictCount(Dict) = 1000);
  BencodeFree(Dict);
  
  { Test 2: List with many entries }
  Dict := BencodeNewList;
  for I := 1 to 1000 do
    BencodeListAdd(Dict, BencodeNewInteger(I));
  TestResult('List with 1000 entries',
             BencodeListCount(Dict) = 1000);
  BencodeFree(Dict);
  
  { Test 3: Rapid create/free cycles }
  for I := 1 to 10000 do
  begin
    Dict := BencodeNewDict;
    BencodeDictAdd(Dict, 'key', BencodeNewString('value'));
    BencodeFree(Dict);
  end;
  TestResult('10000 create/free cycles', True, 'Check for memory leaks');
end;

{ ============================================================================ }
{ Test 5: Complex Real-World Scenarios                                          }
{ ============================================================================ }

procedure TestComplexStructures;
var
  Result: TParseResult;
  Value: PBencodeValue;
  Info: PBencodeValue;
  Files: PBencodeValue;
  FileEntry: PBencodeValue;
  IntVal: Int64;
  StrVal: string;
  { Realistic torrent structure }
  ComplexTorrent: string;
begin
  WriteLn(#10'=== Testing Complex Real-World Structures ===');
  
  { Multi-file torrent structure - manually verified lengths }
  { announce URL: 'http://tracker.example.com/' = 27 chars }
  { files list: 2 file entries, each ending with 'e', then 'e' to end list }
  ComplexTorrent := 'd8:announce27:http://tracker.example.com/13:creation datei1609459200e4:infod5:filesld6:lengthi1024e4:pathl9:file1.txteed6:lengthi2048e4:pathl9:file2.txteee4:name4:test12:piece lengthi262144e6:pieces60:' + StringOfChar('x', 60) + 'ee';
  
  Result := BencodeDecodeString(ComplexTorrent, Value);
  TestResult('Complex multi-file torrent parses',
             Result.Success);
  
  if Result.Success then
  begin
    { Verify top-level fields }
    TestResult('  Has announce',
               BencodeDictGetStr(Value, 'announce', StrVal));
    TestResult('  Has creation_date',
               BencodeDictGetInt(Value, 'creation date', IntVal) and (IntVal = 1609459200));
    
    { Verify info dict }
    TestResult('  Has info dict',
               BencodeDictGetDict(Value, 'info', Info));
    
    if Info <> nil then
    begin
      TestResult('  Info has name',
                 BencodeDictGetStr(Info, 'name', StrVal) and (StrVal = 'test'));
      TestResult('  Info has piece length',
                 BencodeDictGetInt(Info, 'piece length', IntVal) and (IntVal = 262144));
      TestResult('  Info has files list',
                 BencodeDictGetList(Info, 'files', Files));
      
      if Files <> nil then
        TestResult('  Files list has 2 entries',
                   BencodeListCount(Files) = 2);
    end;
    
    BencodeFree(Value);
  end;
end;

{ ============================================================================ }
{ Main Test Runner                                                              }
{ ============================================================================ }

procedure RunAllTests;
begin
  WriteLn('==============================================');
  WriteLn('  BENCODE EXTENDED TEST SUITE');
  WriteLn('  Critical Path Testing');
  WriteLn('==============================================');
  
  TestBencodeCalcSize;
  TestBencodeDictGetTyped;
  TestBencodeToDebugString;
  TestNilHandling;
  TestFileOperations;
  TestIntegerEdgeCases;
  TestStringEdgeCases;
  TestBinaryKeys;
  TestDeepNesting;
  TestStressMemory;
  TestComplexStructures;
  
  WriteLn(#10'==============================================');
  WriteLn('  RESULTS: ', PassedTests, '/', TotalTests, ' tests passed');
  WriteLn('==============================================');
  
  if FailedTests > 0 then
  begin
    WriteLn('FAILED: ', FailedTests, ' test(s) failed');
    Halt(1);
  end
  else
  begin
    WriteLn('All tests passed!');
  end;
end;

begin
  Randomize;
  RunAllTests;
end.
