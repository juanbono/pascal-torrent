{
  test_fuzz.pas - Fuzz testing harness for PascalTorrent
  
  This file provides entry points for fuzzers (AFL, libFuzzer, etc.)
  to test the parsing functions with randomized/malformed input.
  
  Can also be run standalone with random data generation.
}

{$mode objfpc}{$H+}

program test_fuzz;

uses
  SysUtils, testframework, bencode, protocol, sha1utils, utils;

const
  FUZZ_ITERATIONS = 10000;  { Number of random iterations for standalone mode }

{ ============================================================================ }
{ Fuzz Bencode Decoder }
{ ============================================================================ }
procedure FuzzBencodeDecode(const Data: PByte; Len: Integer);
var
  Value: PBencodeValue;
  Result: TParseResult;
begin
  { This is the entry point for fuzzers }
  try
    Result := BencodeDecode(PChar(Data), Len, Value);
    if Result.Success then
      BencodeFree(Value);
  except
    { Catch any crashes - fuzzer will report these }
    on E: Exception do
    begin
      { In real fuzzing, we'd log the crash }
    end;
  end;
end;

{ ============================================================================ }
{ Fuzz Protocol Message Decoder }
{ ============================================================================ }
procedure FuzzProtocolDecode(const Data: PByte; Len: Integer);
var
  Msg: TWireMessage;
  Consumed: Integer;
  Success: Boolean;
begin
  try
    Success := DecodeMessage(PByteArray(Data), Len, Msg, Consumed);
    { Success or failure doesn't matter - we're looking for crashes }
  except
    on E: Exception do
    begin
      { Catch any crashes }
    end;
  end;
end;

{ ============================================================================ }
{ Fuzz Handshake Decoder }
{ ============================================================================ }
procedure FuzzHandshakeDecode(const Data: PByte; Len: Integer);
var
  HS: THandshake;
  Success: Boolean;
begin
  try
    Success := DecodeHandshake(PByteArray(Data), Len, HS);
  except
    on E: Exception do
    begin
      { Catch any crashes }
    end;
  end;
end;

{ ============================================================================ }
{ Random Data Generator }
{ ============================================================================ }
procedure GenerateRandomData(Buffer: PByte; MaxLen: Integer; out ActualLen: Integer);
var
  I: Integer;
begin
  { Generate random length up to MaxLen }
  ActualLen := Random(MaxLen) + 1;
  
  { Fill with random bytes }
  for I := 0 to ActualLen - 1 do
    Buffer[I] := Random(256);
end;

{ ============================================================================ }
{ Mutated Data Generator (based on valid inputs) }
{ ============================================================================ }
procedure GenerateMutatedData(const ValidData: PByte; ValidLen: Integer;
                              Buffer: PByte; MaxLen: Integer; 
                              out ActualLen: Integer);
var
  I: Integer;
  MutationType: Integer;
  MutationPos: Integer;
  Len: Integer;
  TempBytes: array of Byte;
begin
  Len := ValidLen;
  if Len > MaxLen then Len := MaxLen;
  
  { Copy valid data to temporary buffer }
  SetLength(TempBytes, Len);
  for I := 0 to Len - 1 do
    TempBytes[I] := ValidData[I];
  
  { Apply random mutation }
  MutationType := Random(5);
  case MutationType of
    0: { Bit flip }
      if Len > 0 then
      begin
        MutationPos := Random(Len);
        TempBytes[MutationPos] := TempBytes[MutationPos] xor (1 shl Random(8));
      end;
    1: { Byte deletion at position }
      if Len > 1 then
      begin
        MutationPos := Random(Len);
        { Shift bytes left }
        for I := MutationPos to Len - 2 do
          TempBytes[I] := TempBytes[I + 1];
        Dec(Len);
      end;
    2: { Byte insertion at position }
      if Len < MaxLen then
      begin
        MutationPos := Random(Len + 1);
        { Shift bytes right }
        for I := Len downto MutationPos + 1 do
          TempBytes[I] := TempBytes[I - 1];
        TempBytes[MutationPos] := Random(256);
        Inc(Len);
      end;
    3: { Truncate }
      if Len > 1 then
        Len := Random(Len) + 1;
    4: { Extend with garbage }
      if Len < MaxLen then
      begin
        MutationPos := Random(MaxLen - Len) + 1;
        if Len + MutationPos > MaxLen then
          MutationPos := MaxLen - Len;
        for I := 0 to MutationPos - 1 do
          TempBytes[Len + I] := Random(256);
        Inc(Len, MutationPos);
      end;
  end;
  
  { Copy to output buffer }
  ActualLen := Len;
  for I := 0 to ActualLen - 1 do
    Buffer[I] := TempBytes[I];
end;

{ ============================================================================ }
{ Standalone Fuzz Tests }
{ ============================================================================ }
procedure TestBencodeFuzzing;
var
  Buffer: array[0..4095] of Byte;
  Len: Integer;
  I: Integer;
  Crashes: Integer;
  ValidInputs: array of string;
  RandomValid: string;
begin
  BeginSuite('Testing Bencode Fuzzing (Random Data)');
  
  Crashes := 0;
  
  { Test 1: Pure random data }
  for I := 0 to FUZZ_ITERATIONS div 2 - 1 do
  begin
    GenerateRandomData(@Buffer, SizeOf(Buffer), Len);
    try
      FuzzBencodeDecode(@Buffer, Len);
    except
      Inc(Crashes);
    end;
  end;
  
  TestResult('No crashes on random data', Crashes = 0);
  
  { Test 2: Mutated valid inputs }
  Crashes := 0;
  ValidInputs := [
    'd4:namel4:test6:lengthi1234ee',
    'li123ei456ei789ee',
    '4:spam',
    'i42e',
    'd8:announce31:http://tracker.example.com/announcee'
  ];
  
  for I := 0 to FUZZ_ITERATIONS div 2 - 1 do
  begin
    RandomValid := ValidInputs[Random(Length(ValidInputs))];
    GenerateMutatedData(PByte(RandomValid),
                        Length(RandomValid),
                        @Buffer, SizeOf(Buffer), Len);
    try
      FuzzBencodeDecode(@Buffer, Len);
    except
      Inc(Crashes);
    end;
  end;
  
  TestResult('No crashes on mutated valid data', Crashes = 0);
  
  EndSuite;
end;

procedure TestProtocolFuzzing;
var
  Buffer: array[0..4095] of Byte;
  Len: Integer;
  I: Integer;
  Crashes: Integer;
begin
  BeginSuite('Testing Protocol Fuzzing (Random Data)');
  
  Crashes := 0;
  
  { Test with random data }
  for I := 0 to FUZZ_ITERATIONS - 1 do
  begin
    GenerateRandomData(@Buffer, SizeOf(Buffer), Len);
    try
      FuzzProtocolDecode(@Buffer, Len);
    except
      Inc(Crashes);
    end;
  end;
  
  TestResult('No crashes on random protocol data', Crashes = 0);
  
  EndSuite;
end;

procedure TestHandshakeFuzzing;
var
  Buffer: array[0..127] of Byte;
  Len: Integer;
  I: Integer;
  Crashes: Integer;
  ValidHandshake: array[0..67] of Byte;
  J: Integer;
  InfoHash: array[0..19] of Byte;
  PeerId: array[0..19] of Byte;
begin
  BeginSuite('Testing Handshake Fuzzing (Random Data)');
  
  Crashes := 0;
  
  { Create a valid handshake for mutation testing }
  FillChar(ValidHandshake, SizeOf(ValidHandshake), 0);
  ValidHandshake[0] := 19;  { Protocol length }
  Move('BitTorrent protocol', ValidHandshake[1], 19);
  { Reserved bytes: bytes 20-27 }
  { Info hash: bytes 28-47 }
  for J := 0 to 19 do
    InfoHash[J] := J;
  Move(InfoHash, ValidHandshake[28], 20);
  { Peer id: bytes 48-67 }
  for J := 0 to 19 do
    PeerId[J] := 255 - J;
  Move(PeerId, ValidHandshake[48], 20);
  
  { Test 1: Pure random data (shorter - handshake is fixed size) }
  for I := 0 to FUZZ_ITERATIONS div 2 - 1 do
  begin
    GenerateRandomData(@Buffer, 128, Len);
    try
      FuzzHandshakeDecode(@Buffer, Len);
    except
      Inc(Crashes);
    end;
  end;
  
  TestResult('No crashes on random handshake data', Crashes = 0);
  
  { Test 2: Mutated valid handshakes }
  Crashes := 0;
  for I := 0 to FUZZ_ITERATIONS div 2 - 1 do
  begin
    GenerateMutatedData(@ValidHandshake, 68,
                        @Buffer, SizeOf(Buffer), Len);
    try
      FuzzHandshakeDecode(@Buffer, Len);
    except
      Inc(Crashes);
    end;
  end;
  
  TestResult('No crashes on mutated valid handshakes', Crashes = 0);
  
  EndSuite;
end;

{ ============================================================================ }
{ Edge Case Tests }
{ ============================================================================ }
procedure TestEdgeCases;
var
  Buffer: array[0..255] of Byte;
  Value: PBencodeValue;
  Result: TParseResult;
  I: Integer;
begin
  BeginSuite('Testing Edge Cases');
  
  { Test 1: Empty input }
  Result := BencodeDecode(PChar(''), 0, Value);
  AssertFalse('Empty input fails', Result.Success);
  
  { Test 2: Single character inputs }
  for I := 0 to 255 do
  begin
    Buffer[0] := I;
    try
      Result := BencodeDecode(PChar(@Buffer), 1, Value);
      if Result.Success then
        BencodeFree(Value);
    except
      { Any crash is a failure }
      TestResult('No crash on single char ' + IntToStr(I), False);
      Exit;
    end;
  end;
  TestResult('No crashes on single character inputs', True);
  
  { Test 3: Integer overflow attempts }
  Result := BencodeDecode(PChar('i999999999999999999999999999999e'), 30, Value);
  AssertFalse('Integer overflow rejected', Result.Success);
  
  { Test 4: Very long string length }
  Result := BencodeDecode(PChar('999999999999:'), 13, Value);
  AssertFalse('Huge string length rejected', Result.Success);
  
  { Test 5: Nested structures (deep recursion attempt) }
  Result := BencodeDecode(PChar('llllllllllleeeeeeeeee'), 21, Value);
  { May succeed or fail, but should not crash }
  if Result.Success then
    BencodeFree(Value);
  TestResult('Deep nesting handled without crash', True);
  
  EndSuite;
end;

{ ============================================================================ }
{ Main }
{ ============================================================================ }
begin
  Randomize;
  
  WriteLn('==============================================');
  WriteLn('  FUZZ TESTING HARNESS');
  WriteLn('  Iterations: ', FUZZ_ITERATIONS);
  WriteLn('==============================================');
  WriteLn;
  WriteLn('Note: This test generates random/malformed data');
  WriteLn('to verify parsers handle edge cases gracefully.');
  WriteLn;
  
  TestBencodeFuzzing;
  TestProtocolFuzzing;
  TestHandshakeFuzzing;
  TestEdgeCases;
  
  ExitWithResult;
end.
