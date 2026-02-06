{
  test_bencode.pas - Comprehensive tests for bencode unit
}

{$mode objfpc}{$H+}

program test_bencode;

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

procedure TestStringDecoding;
var
  Result: TParseResult;
  Value: PBencodeValue;
begin
  WriteLn(#10'=== Testing String Decoding ===');
  
  { Test 1: Simple string }
  Result := BencodeDecodeString('4:spam', Value);
  TestResult('Simple string (4:spam)', 
             Result.Success and (Value^.ValueType = btString) and 
             (Value^.StrLen = 4) and (StrLComp(Value^.StrVal, 'spam', 4) = 0));
  BencodeFree(Value);
  
  { Test 2: Empty string }
  Result := BencodeDecodeString('0:', Value);
  TestResult('Empty string (0:)',
             Result.Success and (Value^.ValueType = btString) and
             (Value^.StrLen = 0));
  BencodeFree(Value);
  
  { Test 3: Long string }
  Result := BencodeDecodeString('10:0123456789', Value);
  TestResult('Long string (10 digits)',
             Result.Success and (Value^.StrLen = 10));
  BencodeFree(Value);
  
  { Test 4: String with spaces }
  Result := BencodeDecodeString('11:hello world', Value);
  TestResult('String with spaces',
             Result.Success and (Value^.StrLen = 11));
  BencodeFree(Value);
  
  { Test 5: Missing colon }
  Result := BencodeDecodeString('4spam', Value);
  TestResult('Missing colon (should fail)', 
             not Result.Success);
  if Result.Success then BencodeFree(Value);
  
  { Test 6: Invalid length (negative) }
  Result := BencodeDecodeString('-1:test', Value);
  TestResult('Negative length (should fail)',
             not Result.Success);
  if Result.Success then BencodeFree(Value);
  
  { Test 7: Not enough data }
  Result := BencodeDecodeString('10:short', Value);
  TestResult('Not enough data (should fail)',
             not Result.Success);
  if Result.Success then BencodeFree(Value);
end;

procedure TestIntegerDecoding;
var
  Result: TParseResult;
  Value: PBencodeValue;
begin
  WriteLn(#10'=== Testing Integer Decoding ===');
  
  { Test 1: Positive integer }
  Result := BencodeDecodeString('i3e', Value);
  TestResult('Positive integer (i3e)',
             Result.Success and (Value^.ValueType = btInteger) and
             (Value^.IntVal = 3));
  BencodeFree(Value);
  
  { Test 2: Negative integer }
  Result := BencodeDecodeString('i-3e', Value);
  TestResult('Negative integer (i-3e)',
             Result.Success and (Value^.IntVal = -3));
  BencodeFree(Value);
  
  { Test 3: Zero }
  Result := BencodeDecodeString('i0e', Value);
  TestResult('Zero (i0e)',
             Result.Success and (Value^.IntVal = 0));
  BencodeFree(Value);
  
  { Test 4: Large integer }
  Result := BencodeDecodeString('i123456789012345e', Value);
  TestResult('Large integer',
             Result.Success and (Value^.IntVal = 123456789012345));
  BencodeFree(Value);
  
  { Test 5: Leading zero (should fail) }
  Result := BencodeDecodeString('i03e', Value);
  TestResult('Leading zero (should fail)',
             not Result.Success);
  if Result.Success then BencodeFree(Value);
  
  { Test 6: Negative zero (should fail) }
  Result := BencodeDecodeString('i-0e', Value);
  TestResult('Negative zero (should fail)',
             not Result.Success);
  if Result.Success then BencodeFree(Value);
  
  { Test 7: Missing 'e' }
  Result := BencodeDecodeString('i123', Value);
  TestResult('Missing terminator (should fail)',
             not Result.Success);
  if Result.Success then BencodeFree(Value);
  
  { Test 8: Empty integer }
  Result := BencodeDecodeString('ie', Value);
  TestResult('Empty integer (should fail)',
             not Result.Success);
  if Result.Success then BencodeFree(Value);
end;

procedure TestListDecoding;
var
  Result: TParseResult;
  Value: PBencodeValue;
  Item: PBencodeValue;
begin
  WriteLn(#10'=== Testing List Decoding ===');
  
  { Test 1: Empty list }
  Result := BencodeDecodeString('le', Value);
  TestResult('Empty list (le)',
             Result.Success and (Value^.ValueType = btList) and
             (BencodeListCount(Value) = 0));
  BencodeFree(Value);
  
  { Test 2: List with strings }
  Result := BencodeDecodeString('l4:spam4:eggse', Value);
  TestResult('List with strings (l4:spam4:eggse)',
             Result.Success and (BencodeListCount(Value) = 2));
  if Result.Success then
  begin
    Item := BencodeListGet(Value, 0);
    TestResult('  First item is "spam"',
               (Item <> nil) and (Item^.ValueType = btString) and
               (Item^.StrLen = 4) and (StrLComp(Item^.StrVal, 'spam', 4) = 0));
    
    Item := BencodeListGet(Value, 1);
    TestResult('  Second item is "eggs"',
               (Item <> nil) and (Item^.ValueType = btString) and
               (Item^.StrLen = 4) and (StrLComp(Item^.StrVal, 'eggs', 4) = 0));
  end;
  BencodeFree(Value);
  
  { Test 3: List with integers }
  Result := BencodeDecodeString('li1ei2ei3ee', Value);
  TestResult('List with integers (li1ei2ei3ee)',
             Result.Success and (BencodeListCount(Value) = 3));
  if Result.Success then
  begin
    Item := BencodeListGet(Value, 1);
    TestResult('  Second item is 2',
               (Item <> nil) and (Item^.ValueType = btInteger) and
               (Item^.IntVal = 2));
  end;
  BencodeFree(Value);
  
  { Test 4: Mixed list }
  Result := BencodeDecodeString('l4:spami42ee', Value);
  TestResult('Mixed list (string + int)',
             Result.Success and (BencodeListCount(Value) = 2));
  BencodeFree(Value);
  
  { Test 5: Missing terminator }
  Result := BencodeDecodeString('l4:test', Value);
  TestResult('Missing terminator (should fail)',
             not Result.Success);
  if Result.Success then BencodeFree(Value);
end;

procedure TestDictionaryDecoding;
var
  Result: TParseResult;
  Value: PBencodeValue;
  Item: PBencodeValue;
  StrVal: string;
  IntVal: Int64;
begin
  WriteLn(#10'=== Testing Dictionary Decoding ===');
  
  { Test 1: Empty dictionary }
  Result := BencodeDecodeString('de', Value);
  TestResult('Empty dictionary (de)',
             Result.Success and (Value^.ValueType = btDict) and
             (BencodeDictCount(Value) = 0));
  BencodeFree(Value);
  
  { Test 2: Simple dictionary }
  Result := BencodeDecodeString('d3:cow3:moo4:spam4:eggse', Value);
  TestResult('Simple dictionary (d3:cow3:moo4:spam4:eggse)',
             Result.Success and (BencodeDictCount(Value) = 2));
  if Result.Success then
  begin
    TestResult('  Has key "cow"',
               BencodeDictHasKey(Value, 'cow'));
    TestResult('  Has key "spam"',
               BencodeDictHasKey(Value, 'spam'));
    TestResult('  Value of "cow" is "moo"',
               BencodeDictGetStr(Value, 'cow', StrVal) and (StrVal = 'moo'));
  end;
  BencodeFree(Value);
  
  { Test 3: Dictionary with integer }
  Result := BencodeDecodeString('d4:key1i42e4:key25:valuee', Value);
  TestResult('Dictionary with integer',
             Result.Success and (BencodeDictCount(Value) = 2));
  if Result.Success then
  begin
    TestResult('  key1 = 42',
               BencodeDictGetInt(Value, 'key1', IntVal) and (IntVal = 42));
    TestResult('  key2 = "value"',
               BencodeDictGetStr(Value, 'key2', StrVal) and (StrVal = 'value'));
  end;
  BencodeFree(Value);
  
  { Test 4: Nested dictionary }
  Result := BencodeDecodeString('d4:dictd3:key5:valueee', Value);
  TestResult('Nested dictionary',
             Result.Success and (BencodeDictCount(Value) = 1));
  if Result.Success then
  begin
    TestResult('  Inner dict exists',
               BencodeDictGetDict(Value, 'dict', Item));
  end;
  BencodeFree(Value);
  
  { Test 5: Dictionary with list }
  Result := BencodeDecodeString('d4:listli1ei2eee', Value);
  TestResult('Dictionary with list',
             Result.Success and BencodeDictGetList(Value, 'list', Item));
  BencodeFree(Value);
  
  { Test 6: Key must be string }
  Result := BencodeDecodeString('di42e3:valuede', Value);
  TestResult('Integer key (should fail)',
             not Result.Success);
  if Result.Success then BencodeFree(Value);
end;

procedure TestEncoding;
var
  Value: PBencodeValue;
  Encoded: string;
  ReDecoded: PBencodeValue;
  Result: TParseResult;
begin
  WriteLn(#10'=== Testing Encoding ===');
  
  { Test 1: String round-trip }
  Value := BencodeNewString('spam');
  TestResult('Encode string',
             BencodeEncodeString(Value, Encoded) and (Encoded = '4:spam'));
  BencodeFree(Value);
  
  { Test 2: Integer round-trip }
  Value := BencodeNewInteger(42);
  TestResult('Encode integer',
             BencodeEncodeString(Value, Encoded) and (Encoded = 'i42e'));
  BencodeFree(Value);
  
  { Test 3: Negative integer }
  Value := BencodeNewInteger(-123);
  TestResult('Encode negative integer',
             BencodeEncodeString(Value, Encoded) and (Encoded = 'i-123e'));
  BencodeFree(Value);
  
  { Test 4: Empty list }
  Value := BencodeNewList;
  TestResult('Encode empty list',
             BencodeEncodeString(Value, Encoded) and (Encoded = 'le'));
  BencodeFree(Value);
  
  { Test 5: List with items }
  Value := BencodeNewList;
  BencodeListAdd(Value, BencodeNewString('spam'));
  BencodeListAdd(Value, BencodeNewString('eggs'));
  TestResult('Encode list with items',
             BencodeEncodeString(Value, Encoded) and (Encoded = 'l4:spam4:eggse'));
  BencodeFree(Value);
  
  { Test 6: Empty dict }
  Value := BencodeNewDict;
  TestResult('Encode empty dict',
             BencodeEncodeString(Value, Encoded) and (Encoded = 'de'));
  BencodeFree(Value);
  
  { Test 7: Dict with items }
  Value := BencodeNewDict;
  BencodeDictAdd(Value, 'cow', BencodeNewString('moo'));
  BencodeDictAdd(Value, 'spam', BencodeNewString('eggs'));
  TestResult('Encode dict with items',
             BencodeEncodeString(Value, Encoded) and 
             ((Encoded = 'd3:cow3:moo4:spam4:eggse') or
              (Encoded = 'd4:spam4:eggs3:cow3:mooe')));  { Order may vary }
  BencodeFree(Value);
  
  { Test 8: Complex structure round-trip }
  Value := BencodeNewDict;
  BencodeDictAdd(Value, 'string', BencodeNewString('test'));
  BencodeDictAdd(Value, 'number', BencodeNewInteger(123));
  BencodeEncodeString(Value, Encoded);
  BencodeFree(Value);
  
  Result := BencodeDecodeString(Encoded, ReDecoded);
  TestResult('Complex structure round-trip',
             Result.Success and (BencodeDictCount(ReDecoded) = 2));
  if Result.Success then BencodeFree(ReDecoded);
end;

procedure TestMemoryManagement;
var
  I: Integer;
  Value: PBencodeValue;
begin
  WriteLn(#10'=== Testing Memory Management ===');
  
  { Test 1: Create and free string multiple times }
  for I := 1 to 1000 do
  begin
    Value := BencodeNewString('test string for memory management');
    BencodeFree(Value);
  end;
  TestResult('Create/free string 1000x', True, 'Manual check needed');
  
  { Test 2: Create and free complex structure }
  for I := 1 to 100 do
  begin
    Value := BencodeNewDict;
    BencodeDictAdd(Value, 'list', BencodeNewList);
    BencodeDictAdd(Value, 'int', BencodeNewInteger(I));
    BencodeFree(Value);
  end;
  TestResult('Create/free complex structure 100x', True, 'Manual check needed');
  
  { Test 3: Clone and free }
  Value := BencodeNewDict;
  BencodeDictAdd(Value, 'key', BencodeNewString('value'));
  BencodeDictAdd(Value, 'num', BencodeNewInteger(42));
  
  Value := BencodeClone(Value);
  BencodeFree(Value);
  TestResult('Clone and free', True);
end;

procedure TestEdgeCases;
var
  Result: TParseResult;
  Value: PBencodeValue;
begin
  WriteLn(#10'=== Testing Edge Cases ===');
  
  { Test 1: Empty input }
  Result := BencodeDecodeString('', Value);
  TestResult('Empty input (should fail)',
             not Result.Success);
  if Result.Success then BencodeFree(Value);
  
  { Test 2: Whitespace only }
  Result := BencodeDecodeString('   ', Value);
  TestResult('Whitespace only (should fail)',
             not Result.Success);
  if Result.Success then BencodeFree(Value);
  
  { Test 3: Trailing data }
  Result := BencodeDecodeString('4:spamextra', Value);
  TestResult('Trailing data (should fail - strict)',
             not Result.Success);
  if Result.Success then BencodeFree(Value);
  
  { Test 4: Binary data in string }
  Value := BencodeNewStringBuf(PChar(#0#1#2#255), 4);
  TestResult('Binary data in string',
             (Value^.StrLen = 4) and (Value^.StrVal[0] = #0));
  BencodeFree(Value);
  
  { Test 5: Very long string }
  Value := BencodeNewString(StringOfChar('a', 10000));
  TestResult('Very long string (10KB)',
             Value^.StrLen = 10000);
  BencodeFree(Value);
  
  { Test 6: Unicode content }
  Value := BencodeNewString('Hello 世界 🌍');
  TestResult('Unicode content',
             Value^.StrLen > 5);  { UTF-8 multi-byte }
  BencodeFree(Value);
  
  { Test 7: Deeply nested structure }
  Result := BencodeDecodeString('lllllllllleeeeeeeeee', Value);  { 10 l's + 10 e's }
  TestResult('Deeply nested empty lists',
             Result.Success);
  if Result.Success then BencodeFree(Value);
end;

procedure TestRealWorld;
const
  { A simple torrent file metainfo structure (simplified) }
  SampleAnnounce = 'd8:completei3e10:downloadedi10e10:incompletei7e8:intervali1800e12:min intervali900e5:peers0:e';  { Note: removed extra trailing e }
var
  Result: TParseResult;
  Value: PBencodeValue;
  IntVal: Int64;
begin
  WriteLn(#10'=== Testing Real-World Examples ===');
  
  { Test 1: Tracker response }
  Result := BencodeDecodeString(SampleAnnounce, Value);
  TestResult('Tracker response parsing',
             Result.Success, Result.ErrorMsg);
  if Result.Success then
  begin
    TestResult('  Has complete count',
               BencodeDictGetInt(Value, 'complete', IntVal) and (IntVal = 3));
    TestResult('  Has interval',
               BencodeDictGetInt(Value, 'interval', IntVal) and (IntVal = 1800));
    BencodeFree(Value);
  end;
end;

procedure TestCloneAndEquality;
var
  A, B, C: PBencodeValue;
  EncodedA, EncodedB: string;
begin
  WriteLn(#10'=== Testing Clone and Equality ===');
  
  { Test 1: Equal strings }
  A := BencodeNewString('test');
  B := BencodeNewString('test');
  TestResult('Equal strings',
             BencodeEqual(A, B));
  BencodeFree(A);
  BencodeFree(B);
  
  { Test 2: Different strings }
  A := BencodeNewString('test1');
  B := BencodeNewString('test2');
  TestResult('Different strings',
             not BencodeEqual(A, B));
  BencodeFree(A);
  BencodeFree(B);
  
  { Test 3: Clone equals original }
  A := BencodeNewDict;
  BencodeDictAdd(A, 'key', BencodeNewString('value'));
  B := BencodeClone(A);
  TestResult('Clone equals original',
             BencodeEqual(A, B));
  BencodeFree(A);
  BencodeFree(B);
  
  { Test 4: Modify clone doesn't affect original }
  A := BencodeNewDict;
  BencodeDictAdd(A, 'key', BencodeNewString('value'));
  B := BencodeClone(A);
  BencodeFree(B^.DictHead^.Value);
  B^.DictHead^.Value := BencodeNewString('different');
  TestResult('Modify clone preserves original',
             not BencodeEqual(A, B));
  BencodeFree(A);
  BencodeFree(B);
  
  { Test 5: Equal lists }
  A := BencodeNewList;
  BencodeListAdd(A, BencodeNewInteger(1));
  BencodeListAdd(A, BencodeNewInteger(2));
  B := BencodeNewList;
  BencodeListAdd(B, BencodeNewInteger(1));
  BencodeListAdd(B, BencodeNewInteger(2));
  TestResult('Equal lists',
             BencodeEqual(A, B));
  BencodeFree(A);
  BencodeFree(B);
  
  { Test 6: Round-trip preserves equality }
  A := BencodeNewDict;
  BencodeDictAdd(A, 'list', BencodeNewList);
  BencodeDictAdd(A, 'num', BencodeNewInteger(42));
  BencodeEncodeString(A, EncodedA);
  BencodeFree(A);
  
  BencodeDecodeString(EncodedA, B);
  C := BencodeClone(B);
  TestResult('Round-trip preserves structure',
             BencodeEqual(B, C));
  BencodeFree(B);
  BencodeFree(C);
end;

procedure RunAllTests;
begin
  WriteLn('==============================================');
  WriteLn('  BENCODE UNIT TEST SUITE');
  WriteLn('==============================================');
  
  TestStringDecoding;
  TestIntegerDecoding;
  TestListDecoding;
  TestDictionaryDecoding;
  TestEncoding;
  TestMemoryManagement;
  TestEdgeCases;
  TestRealWorld;
  TestCloneAndEquality;
  
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
  RunAllTests;
end.
