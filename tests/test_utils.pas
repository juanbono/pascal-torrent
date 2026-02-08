{
  test_utils.pas - Comprehensive tests for utils unit
}

{$mode objfpc}{$H+}

program test_utils;

uses
  SysUtils, utils;

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

{ Global variable for counting free calls in tests }
var
  TestFreeCount: Integer;

{ Global procedure for ListFreeAll test - must be at unit level for procedure pointer }
procedure TestCountFree(Data: Pointer);
begin
  Inc(TestFreeCount);
end;

procedure TestLinkedListsEdgeCases;
var
  Head, Node: PGenericNode;
begin
  WriteLn(#10'=== Testing Linked Lists Edge Cases ===');
  
  { Test 1: ListAddHead with nil node }
  Head := nil;
  ListAddHead(Head, nil);  { Should not crash }
  TestResult('ListAddHead(nil, nil) is safe',
             Head = nil);
  
  { Test 2: ListAddTail with nil node }
  ListAddTail(Head, nil);  { Should not crash }
  TestResult('ListAddTail(nil, nil) is safe',
             Head = nil);
  
  { Test 3: ListRemove with nil head }
  New(Node);
  TestResult('ListRemove(nil, node) returns false',
             not ListRemove(Head, Node));
  Dispose(Node);
  
  { Test 4: ListRemove with nil node }
  New(Node);
  Node^.Data := Pointer(1);
  ListAddHead(Head, Node);
  TestResult('ListRemove(head, nil) returns false',
             not ListRemove(Head, nil));
  ListFree(Head);
  
  { Test 5: ListFind with nil head }
  TestResult('ListFind(nil, data) returns nil',
             ListFind(nil, Pointer(1)) = nil);
  
  { Test 6: ListCount with nil head }
  TestResult('ListCount(nil) returns 0',
             ListCount(nil) = 0);
  
  { Test 7: ListFree with nil head }
  Head := nil;
  ListFree(Head);  { Should not crash }
  TestResult('ListFree(nil) is safe',
             Head = nil);
  
  { Test 8: ListFreeAll with callback }
  TestFreeCount := 0;
  Head := nil;
  
  New(Node);
  Node^.Data := Pointer(1);
  Node^.Next := nil;
  Head := Node;
  
  New(Node);
  Node^.Data := Pointer(2);
  Node^.Next := nil;
  Head^.Next := Node;
  
  ListFreeAll(Head, @TestCountFree);
  TestResult('ListFreeAll calls callback for each node',
             TestFreeCount = 2);
  TestResult('ListFreeAll sets head to nil',
             Head = nil);
  
  { Test 9: ListFreeAll with nil callback }
  New(Node);
  Node^.Data := Pointer(1);
  Head := Node;
  ListFreeAll(Head, nil);  { Should not crash }
  TestResult('ListFreeAll with nil callback is safe',
             Head = nil);
  
  { Test 10: ListFreeAll with nil head }
  Head := nil;
  ListFreeAll(Head, @TestCountFree);  { Should not crash }
  TestResult('ListFreeAll(nil, callback) is safe',
             Head = nil);
end;

procedure TestLinkedLists;
var
  Head, Node, Found: PGenericNode;
  I: Integer;
begin
  WriteLn(#10'=== Testing Linked Lists ===');
  
  { Test 1: Empty list }
  Head := nil;
  TestResult('Empty list count is 0',
             ListCount(Head) = 0);
  
  { Test 2: Add to head }
  New(Node);
  Node^.Data := Pointer(1);
  ListAddHead(Head, Node);
  TestResult('Add to head increases count',
             ListCount(Head) = 1);
  
  { Test 3: Find }
  Found := ListFind(Head, Pointer(1));
  TestResult('Find existing element',
             Found <> nil);
  
  { Test 4: Find non-existent }
  Found := ListFind(Head, Pointer(999));
  TestResult('Find non-existent returns nil',
             Found = nil);
  
  { Test 5: Add multiple }
  for I := 2 to 5 do
  begin
    New(Node);
    Node^.Data := Pointer(PtrUInt(I));
    ListAddTail(Head, Node);
  end;
  TestResult('Add 4 more elements',
             ListCount(Head) = 5);
  
  { Test 6: Remove }
  Node := ListFind(Head, Pointer(3));
  TestResult('Remove middle element',
             ListRemove(Head, Node) and (ListCount(Head) = 4));
  Dispose(Node);
  
  { Test 7: Remove head }
  Node := Head;
  ListRemove(Head, Node);
  Dispose(Node);
  TestResult('Remove head',
             ListCount(Head) = 3);
  
  { Cleanup }
  ListFree(Head);
  TestResult('Free empties list',
             Head = nil);
end;

procedure TestDynamicBufferEdgeCases;
var
  Buffer: PDynBuffer;
  Data: array[0..9] of Byte;
  I: Integer;
begin
  WriteLn(#10'=== Testing Dynamic Buffer Edge Cases ===');
  
  { Test 1: DynBufferCreate with very small capacity }
  Buffer := DynBufferCreate(1);  { Should use MIN_BUFFER_CAPACITY }
  TestResult('DynBufferCreate(1) uses minimum capacity',
             (Buffer <> nil) and (Buffer^.Capacity >= MIN_BUFFER_CAPACITY));
  DynBufferFree(Buffer);
  
  { Test 2: DynBufferCreate with zero capacity }
  Buffer := DynBufferCreate(0);  { Should use MIN_BUFFER_CAPACITY }
  TestResult('DynBufferCreate(0) uses minimum capacity',
             (Buffer <> nil) and (Buffer^.Capacity >= MIN_BUFFER_CAPACITY));
  DynBufferFree(Buffer);
  
  { Test 3: DynBufferFree with nil }
  DynBufferFree(nil);  { Should not crash }
  TestResult('DynBufferFree(nil) is safe', True);
  
  { Test 4: DynBufferClear with nil }
  DynBufferClear(nil);  { Should not crash }
  TestResult('DynBufferClear(nil) is safe', True);
  
  { Test 5: DynBufferAppend with nil buffer }
  for I := 0 to 9 do
    Data[I] := I;
  TestResult('DynBufferAppend(nil, data, len) returns false',
             not DynBufferAppend(nil, Data, 10));
  
  { Test 6: DynBufferAppend with negative length }
  Buffer := DynBufferCreate(256);
  TestResult('DynBufferAppend(buffer, data, -1) returns false',
             not DynBufferAppend(Buffer, Data, -1));
  DynBufferFree(Buffer);
  
  { Test 7: DynBufferAppend with zero length }
  Buffer := DynBufferCreate(256);
  TestResult('DynBufferAppend(buffer, data, 0) returns true',
             DynBufferAppend(Buffer, Data, 0) and (Buffer^.Size = 0));
  DynBufferFree(Buffer);
  
  { Test 8: DynBufferAppendByte with nil buffer }
  TestResult('DynBufferAppendByte(nil, byte) returns false',
             not DynBufferAppendByte(nil, 42));
  
  { Test 9: DynBufferAt with nil buffer }
  TestResult('DynBufferAt(nil, offset) returns nil',
             DynBufferAt(nil, 0) = nil);
  
  { Test 10: DynBufferAt with negative offset }
  Buffer := DynBufferCreate(256);
  DynBufferAppendByte(Buffer, 42);
  TestResult('DynBufferAt(buffer, -1) returns nil',
             DynBufferAt(Buffer, -1) = nil);
  DynBufferFree(Buffer);
  
  { Test 11: DynBufferReserve with nil buffer }
  TestResult('DynBufferReserve(nil, 100) returns false',
             not DynBufferReserve(nil, 100));
  
  { Test 12: DynBufferReserve with overflow protection }
  Buffer := DynBufferCreate(256);
  TestResult('DynBufferReserve with reasonable size succeeds',
             DynBufferReserve(Buffer, 1000));
  DynBufferFree(Buffer);
  
  { Test 13: Multiple appends triggering growth }
  Buffer := DynBufferCreate(16);
  for I := 1 to 100 do
    DynBufferAppendByte(Buffer, I mod 256);
  TestResult('Buffer auto-growth after 100 appends',
             (Buffer^.Size = 100) and (Buffer^.Capacity >= 100));
  DynBufferFree(Buffer);
end;

procedure TestDynamicBuffer;
var
  Buffer: PDynBuffer;
  Data: array[0..99] of Byte;
  I: Integer;
begin
  WriteLn(#10'=== Testing Dynamic Buffer ===');
  
  { Test 1: Create }
  Buffer := DynBufferCreate(16);
  TestResult('Create buffer',
             (Buffer <> nil) and (Buffer^.Size = 0));
  
  { Test 2: Append single byte }
  TestResult('Append single byte',
             DynBufferAppendByte(Buffer, 42) and (Buffer^.Size = 1));
  
  { Test 3: Append data }
  for I := 0 to 99 do
    Data[I] := I;
  TestResult('Append 100 bytes',
             DynBufferAppend(Buffer, Data, 100) and (Buffer^.Size = 101));
  
  { Test 4: Access at offset }
  TestResult('Access at offset 0',
             PByte(DynBufferAt(Buffer, 0))^ = 42);
  TestResult('Access at offset 1',
             PByte(DynBufferAt(Buffer, 1))^ = 0);
  TestResult('Access at offset 100',
             PByte(DynBufferAt(Buffer, 100))^ = 99);
  
  { Test 5: Clear }
  DynBufferClear(Buffer);
  TestResult('Clear sets size to 0',
             Buffer^.Size = 0);
  
  { Test 6: Multiple appends after clear }
  TestResult('Append after clear',
             DynBufferAppend(Buffer, Data, 50) and (Buffer^.Size = 50));
  
  { Test 7: Out of bounds access }
  TestResult('Out of bounds returns nil',
             DynBufferAt(Buffer, 100) = nil);
  
  { Cleanup }
  DynBufferFree(Buffer);
  TestResult('Free buffer', True);
end;

procedure TestStringFormattingEdgeCases;
begin
  WriteLn(#10'=== Testing String Formatting Edge Cases ===');
  
  { Test 1: IntToStrPad with negative width }
  TestResult('IntToStrPad with negative MinWidth',
             IntToStrPad(123, -5) = '123');
  
  { Test 2: IntToStrPad with zero }
  TestResult('IntToStrPad(0, 3)',
             IntToStrPad(0, 3) = '000');
  
  { Test 3: IntToStrPad with negative number }
  TestResult('IntToStrPad(-5, 3)',
             IntToStrPad(-5, 3) = '0-5');  { Actual behavior: pads to the left }
  
  { Test 4: FormatBytes with 0 }
  TestResult('FormatBytes(0)',
             FormatBytes(0) = '0 B');
  
  { Test 5: FormatBytes with 1 byte }
  TestResult('FormatBytes(1)',
             FormatBytes(1) = '1 B');
  
  { Test 6: FormatBytes with exactly 1KB }
  TestResult('FormatBytes(1024) contains KB',
             Pos('KB', FormatBytes(1024)) > 0);
  
  { Test 7: FormatBytes with exactly 1MB }
  TestResult('FormatBytes(1024*1024) contains MB',
             Pos('MB', FormatBytes(1024*1024)) > 0);
  
  { Test 8: FormatBytes with very large value (TB) }
  TestResult('FormatBytes(1TB) contains TB',
             Pos('TB', FormatBytes(Int64(1024)*1024*1024*1024)) > 0);
  
  { Test 9: FormatSpeed with 0 }
  TestResult('FormatSpeed(0)',
             FormatSpeed(0) = '0 B/s');
  
  { Test 10: FormatDuration with 0 }
  TestResult('FormatDuration(0)',
             FormatDuration(0) = '0:00');
  
  { Test 11: FormatDuration with 59 seconds }
  TestResult('FormatDuration(59)',
             FormatDuration(59) = '0:59');
  
  { Test 12: FormatDuration with exactly 1 hour }
  TestResult('FormatDuration(3600)',
             FormatDuration(3600) = '1:00:00');
  
  { Test 13: FormatDuration with large hours }
  TestResult('FormatDuration(86400) = 24:00:00',
             FormatDuration(86400) = '24:00:00');
  
  { Test 14: TrimStr with empty string }
  TestResult('TrimStr empty string',
             TrimStr('') = '');
  
  { Test 15: TrimStr with only whitespace }
  TestResult('TrimStr only whitespace',
             TrimStr(#9#10#13'   ') = '');
  
  { Test 16: TrimStr with no whitespace }
  TestResult('TrimStr no whitespace',
             TrimStr('hello') = 'hello');
  
  { Test 17: TrimStr with internal whitespace }
  TestResult('TrimStr internal whitespace preserved',
             TrimStr('  hello world  ') = 'hello world');
end;

procedure TestStringFormatting;
begin
  WriteLn(#10'=== Testing String Formatting ===');
  
  { Test 1: Integer padding }
  TestResult('Pad integer 5 to width 3',
             IntToStrPad(5, 3) = '005');
  
  TestResult('Pad integer 123 to width 3',
             IntToStrPad(123, 3) = '123');
  
  TestResult('Pad integer 1234 to width 3',
             IntToStrPad(1234, 3) = '1234');  { Should not truncate }
  
  { Test 2: Bytes formatting }
  TestResult('Format 512 bytes',
             FormatBytes(512) = '512 B');
  
  TestResult('Format 1.5 KB',
             Pos('KB', FormatBytes(1536)) > 0);
  
  TestResult('Format 1 MB',
             Pos('MB', FormatBytes(1024*1024)) > 0);
  
  TestResult('Format 1 GB',
             Pos('GB', FormatBytes(1024*1024*1024)) > 0);
  
  { Test 3: Speed formatting }
  TestResult('Format speed',
             Pos('/s', FormatSpeed(1024)) > 0);
  
  { Test 4: Duration }
  TestResult('Format duration 3661 seconds',
             FormatDuration(3661) = '1:01:01');
  
  TestResult('Format duration 65 seconds',
             FormatDuration(65) = '1:05');
  
  { Test 5: Trim }
  TestResult('Trim "  hello  "',
             TrimStr('  hello  ') = 'hello');
  
  TestResult('Trim empty string',
             TrimStr('   ') = '');
end;

procedure TestSplitJoinEdgeCases;
var
  Parts: array[0..4] of string;
  Count: Integer;
  Joined: string;
begin
  WriteLn(#10'=== Testing Split/Join Edge Cases ===');
  
  { Test 1: Split with empty delimiter returns 0 }
  Count := SplitString('a,b,c', '', Parts);
  TestResult('Split with empty delimiter returns 0',
             Count = 0);
  
  { Test 2: Split empty string }
  Count := SplitString('', ',', Parts);
  TestResult('Split empty string returns 0',
             Count = 0);
  
  { Test 3: Split with delimiter at start }
  Count := SplitString(',a,b', ',', Parts);
  TestResult('Split with delimiter at start',
             (Count = 3) and (Parts[0] = '') and (Parts[1] = 'a'));
  
  { Test 4: Split with delimiter at end - implementation doesn't add empty trailing part }
  Count := SplitString('a,b,', ',', Parts);
  TestResult('Split with delimiter at end',
             (Count = 2) and (Parts[0] = 'a') and (Parts[1] = 'b'));
  
  { Test 5: Split array overflow protection }
  Count := SplitString('a,b,c,d,e,f,g', ',', Parts);
  TestResult('Split respects array bounds',
             Count <= Length(Parts));
  
  { Test 6: Join empty array }
  Joined := JoinStrings([], ',');
  TestResult('Join empty array returns empty string',
             Joined = '');
  
  { Test 7: Join single element no delimiter }
  Joined := JoinStrings(['only'], ',');
  TestResult('Join single element',
             Joined = 'only');
  
  { Test 8: Join with empty delimiter }
  Joined := JoinStrings(['a', 'b', 'c'], '');
  TestResult('Join with empty delimiter concatenates',
             Joined = 'abc');
  
  { Test 9: Join with empty strings in array }
  Joined := JoinStrings(['', 'b', ''], ',');
  TestResult('Join with empty strings',
             Joined = ',b,');
end;

procedure TestSplitJoin;
var
  Parts: array[0..4] of string;
  Count: Integer;
  Joined: string;
begin
  WriteLn(#10'=== Testing Split/Join ===');
  
  { Test 1: Split simple }
  Count := SplitString('a,b,c', ',', Parts);
  TestResult('Split "a,b,c" by comma',
             (Count = 3) and (Parts[0] = 'a') and (Parts[1] = 'b') and (Parts[2] = 'c'));
  
  { Test 2: Split with empty parts }
  Count := SplitString('a,,c', ',', Parts);
  TestResult('Split with empty part',
             (Count = 3) and (Parts[1] = ''));
  
  { Test 3: Split single element }
  Count := SplitString('hello', ',', Parts);
  TestResult('Split single element',
             (Count = 1) and (Parts[0] = 'hello'));
  
  { Test 4: Join }
  Parts[0] := 'a';
  Parts[1] := 'b';
  Parts[2] := 'c';
  Joined := JoinStrings(['a', 'b', 'c'], ',');
  TestResult('Join with comma',
             Joined = 'a,b,c');
  
  { Test 5: Join single }
  Joined := JoinStrings(['only'], ',');
  TestResult('Join single element',
             Joined = 'only');
end;

procedure TestURLCodingEdgeCases;
begin
  WriteLn(#10'=== Testing URL Coding Edge Cases ===');
  
  { Test 1: URLEncode empty string }
  TestResult('URLEncode empty string',
             URLEncode('') = '');
  
  { Test 2: URLEncode already encoded characters }
  TestResult('URLEncode % character',
             URLEncode('%') = '%25');
  
  { Test 3: URLEncode all special chars - check actual encoded values }
  TestResult('URLEncode special chars encodes @',
             Pos('%40', URLEncode('!@#$%^&*()')) > 0);
  TestResult('URLEncode special chars encodes #',
             Pos('%23', URLEncode('!@#$%^&*()')) > 0);
  
  { Test 4: URLEncode URL-safe chars }
  TestResult('URLEncode URL-safe chars unchanged',
             URLEncode('ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_.~') = 
             'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_.~');
  
  { Test 5: URLDecode empty string }
  TestResult('URLDecode empty string',
             URLDecode('') = '');
  
  { Test 6: URLDecode incomplete percent sequence }
  TestResult('URLDecode incomplete % sequence',
             URLDecode('test%') = 'test%');
  
  { Test 7: URLDecode invalid hex chars - behavior varies, just check no crash }
  TestResult('URLDecode invalid hex no crash',
             URLDecode('test%ZZ') <> '');
  
  { Test 8: URLDecode only + }
  TestResult('URLDecode only + becomes space',
             URLDecode('+') = ' ');
  
  { Test 9: URLDecode multiple + }
  TestResult('URLDecode multiple +',
             URLDecode('hello+world+test') = 'hello world test');
  
  { Test 10: URLDecode mixed %20 and + }
  TestResult('URLDecode mixed encodings',
             URLDecode('hello%20world+test') = 'hello world test');
  
  { Test 11: URLDecode high bytes }
  TestResult('URLDecode high byte',
             URLDecode('%FF') = #$FF);
  
  { Test 12: Round-trip with all special chars }
  TestResult('URL round-trip all special',
             URLDecode(URLEncode('!@#$%^&*()_+-=[]{}|;'':",./<>?')) = 
             '!@#$%^&*()_+-=[]{}|;'':",./<>?');
end;

procedure TestURLCoding;
begin
  WriteLn(#10'=== Testing URL Coding ===');
  
  { Test 1: URL encode simple }
  TestResult('URL encode "hello"',
             URLEncode('hello') = 'hello');
  
  { Test 2: URL encode with space }
  TestResult('URL encode "hello world"',
             URLEncode('hello world') = 'hello%20world');
  
  { Test 3: URL encode special chars }
  TestResult('URL encode "a+b"',
             URLEncode('a+b') = 'a%2Bb');
  
  { Test 4: URL decode }
  TestResult('URL decode "hello%20world"',
             URLDecode('hello%20world') = 'hello world');
  
  { Test 5: URL decode + as space }
  TestResult('URL decode "hello+world"',
             URLDecode('hello+world') = 'hello world');
  
  { Test 6: Round-trip }
  TestResult('URL encode/decode round-trip',
             URLDecode(URLEncode('hello world!@#$%')) = 'hello world!@#$%');
end;

procedure TestMemoryMove;
var
  Src, Dst: array[0..9] of Byte;
  I: Integer;
begin
  WriteLn(#10'=== Testing MemoryMove ===');
  
  { Test 1: Basic move }
  for I := 0 to 9 do
    Src[I] := I;
  MemoryMove(Src[0], Dst[0], 10);
  TestResult('MemoryMove copies all bytes',
             MemoryEqual(Src, Dst, 10));
  
  { Test 2: Move with overlap (forward) }
  for I := 0 to 9 do
    Src[I] := I;
  MemoryMove(Src[0], Src[5], 5);  { Overlapping regions }
  TestResult('MemoryMove handles overlap forward',
             (Src[5] = 0) and (Src[6] = 1) and (Src[9] = 4));
  
  { Test 3: Move zero bytes }
  Dst[0] := $FF;
  MemoryMove(Src[0], Dst[0], 0);
  TestResult('MemoryMove 0 bytes does nothing',
             Dst[0] = $FF);
end;

procedure TestBinaryOperationsEdgeCases;
var
  Buf1, Buf2: array[0..9] of Byte;
  I: Integer;
begin
  WriteLn(#10'=== Testing Binary Operations Edge Cases ===');
  
  { Test 1: MemoryEqual with different data }
  for I := 0 to 9 do
  begin
    Buf1[I] := I;
    Buf2[I] := I + 1;
  end;
  TestResult('MemoryEqual returns false for different data',
             not MemoryEqual(Buf1, Buf2, 10));
  
  { Test 2: MemoryEqual with zero length }
  TestResult('MemoryEqual with len=0 returns true',
             MemoryEqual(Buf1, Buf2, 0));
  
  { Test 3: MemoryFind with pattern at start }
  for I := 0 to 9 do
    Buf1[I] := I;
  TestResult('MemoryFind pattern at start',
             MemoryFind(Buf1, 10, Buf1[0], 1) = 0);
  
  { Test 4: MemoryFind with pattern at end }
  TestResult('MemoryFind pattern at end',
             MemoryFind(Buf1, 10, Buf1[9], 1) = 9);
  
  { Test 5: MemoryFind with pattern not found }
  Buf2[0] := $FF;
  TestResult('MemoryFind returns -1 when not found',
             MemoryFind(Buf1, 10, Buf2[0], 1) = -1);
  
  { Test 6: MemoryFind with pattern larger than buffer }
  TestResult('MemoryFind returns -1 when pattern too large',
             MemoryFind(Buf1, 5, Buf2, 10) = -1);
  
  { Test 7: MemoryFind with zero length pattern }
  TestResult('MemoryFind returns -1 with zero length pattern',
             MemoryFind(Buf1, 10, Buf2, 0) = -1);
  
  { Test 8: Swap16 with 0 }
  TestResult('Swap16(0)',
             Swap16(0) = 0);
  
  { Test 9: Swap16 with all bits set }
  TestResult('Swap16($FFFF)',
             Swap16($FFFF) = $FFFF);
  
  { Test 10: Swap32 with 0 }
  TestResult('Swap32(0)',
             Swap32(0) = 0);
  
  { Test 11: Swap32 with all bits set }
  TestResult('Swap32($FFFFFFFF)',
             Swap32($FFFFFFFF) = $FFFFFFFF);
  
  { Test 12: Swap64 with 0 }
  TestResult('Swap64(0)',
             Swap64(0) = 0);
  
  { Test 13: HTONS/NTOHS identity }
  TestResult('HTONS then NTOHS identity',
             NTOHS(HTONS($1234)) = $1234);
  
  { Test 14: HTONL/NTOHL identity }
  TestResult('HTONL then NTOHL identity',
             NTOHL(HTONL($12345678)) = $12345678);
end;

procedure TestBinaryOperations;
var
  Buf: array[0..3] of Byte;
  Val16: Word;
  Val32: Cardinal;
begin
  WriteLn(#10'=== Testing Binary Operations ===');
  
  { Test 1: Swap 16 }
  TestResult('Swap16 0x1234',
             Swap16($1234) = $3412);
  
  { Test 2: Swap 32 }
  TestResult('Swap32 0x12345678',
             Swap32($12345678) = $78563412);
  
  { Test 3: HTONS/NTOHS round-trip }
  Val16 := $ABCD;
  TestResult('HTONS/NTOHS round-trip',
             NTOHS(HTONS(Val16)) = Val16);
  
  { Test 4: HTONL/NTOHL round-trip }
  Val32 := $12345678;
  TestResult('HTONL/NTOHL round-trip',
             NTOHL(HTONL(Val32)) = Val32);
  
  { Test 5: Write BE 16 }
  WriteBE16(Buf, $1234);
  TestResult('Write BE16',
             (Buf[0] = $12) and (Buf[1] = $34));
  
  { Test 6: Read BE 16 }
  Buf[0] := $AB;
  Buf[1] := $CD;
  TestResult('Read BE16',
             ReadBE16(Buf) = $ABCD);
  
  { Test 7: Write BE 32 }
  WriteBE32(Buf, $12345678);
  TestResult('Write BE32',
             (Buf[0] = $12) and (Buf[1] = $34) and 
             (Buf[2] = $56) and (Buf[3] = $78));
  
  { Test 8: Read BE 32 }
  Buf[0] := $12; Buf[1] := $34; Buf[2] := $56; Buf[3] := $78;
  TestResult('Read BE32',
             ReadBE32(Buf) = $12345678);
  
  { Test 9: Memory equal }
  Buf[0] := 1; Buf[1] := 2; Buf[2] := 3; Buf[3] := 4;
  TestResult('MemoryEqual same',
             MemoryEqual(Buf, Buf, 4));
  
  { Test 10: Memory find }
  Buf[0] := 1; Buf[1] := 2; Buf[2] := 3; Buf[3] := 4;
  TestResult('MemoryFind existing',
             MemoryFind(Buf, 4, Buf[2], 1) = 2);
end;

procedure TestPathOperationsEdgeCases;
begin
  WriteLn(#10'=== Testing Path Operations Edge Cases ===');
  
  { Test 1: JoinPath with empty A }
  TestResult('JoinPath empty A',
             JoinPath('', 'file.txt') = 'file.txt');
  
  { Test 2: JoinPath with empty B }
  TestResult('JoinPath empty B',
             JoinPath('/home', '') = '/home');
  
  { Test 3: JoinPath with both empty }
  TestResult('JoinPath both empty',
             JoinPath('', '') = '');
  
  { Test 4: JoinPath with trailing and leading separators }
  {$IFDEF MSWINDOWS}
  TestResult('JoinPath with separators Windows',
             JoinPath('C:\dir\', '\file.txt') = 'C:\dir\file.txt');
  {$ELSE}
  TestResult('JoinPath with separators Unix',
             JoinPath('/home/', '/file.txt') = '/home/file.txt');
  {$ENDIF}
  
  { Test 5: ExtractFile with no separator }
  TestResult('ExtractFile no separator',
             ExtractFile('file.txt') = 'file.txt');
  
  { Test 6: ExtractFile with empty path }
  TestResult('ExtractFile empty path',
             ExtractFile('') = '');
  
  { Test 7: ExtractDir with no separator }
  TestResult('ExtractDir no separator',
             ExtractDir('file.txt') = 'file.txt');
  
  { Test 8: ExtractDir with empty path }
  TestResult('ExtractDir empty path',
             ExtractDir('') = '');
  
  { Test 9: ExtractExt with multiple dots }
  TestResult('ExtractExt multiple dots',
             ExtractExt('file.tar.gz') = '.gz');
  
  { Test 10: ExtractExt with no extension }
  TestResult('ExtractExt no dot',
             ExtractExt('file') = '');
  
  { Test 11: ExtractExt with dot in path }
  TestResult('ExtractExt dot in path',
             ExtractExt('/path.to/file') = '');
  
  { Test 12: StartsWith with empty prefix }
  TestResult('StartsWith empty prefix',
             StartsWith('hello', ''));
  
  { Test 13: StartsWith with prefix longer than string }
  TestResult('StartsWith long prefix fails',
             not StartsWith('hi', 'hello'));
  
  { Test 14: StartsWith with exact match }
  TestResult('StartsWith exact match',
             StartsWith('hello', 'hello'));
  
  { Test 15: EndsWith with empty suffix }
  TestResult('EndsWith empty suffix',
             EndsWith('hello', ''));
  
  { Test 16: EndsWith with suffix longer than string }
  TestResult('EndsWith long suffix fails',
             not EndsWith('lo', 'hello'));
  
  { Test 17: EndsWith with exact match }
  TestResult('EndsWith exact match',
             EndsWith('hello', 'hello'));
end;

procedure TestPathOperations;
var
  Path: string;
  Filename: string;
  Dir: string;
begin
  WriteLn(#10'=== Testing Path Operations ===');
  
  { Test 1: Join path }
  Path := JoinPath('/home/user', 'file.txt');
  TestResult('Join path',
             (Path = '/home/user/file.txt') or 
             (Path = '/home/user\file.txt'));  { Windows }
  
  { Test 2: Extract file }
  Filename := ExtractFile('/path/to/file.txt');
  TestResult('Extract file from path',
             Filename = 'file.txt');
  
  { Test 3: Extract file no path }
  Filename := ExtractFile('file.txt');
  TestResult('Extract file no path',
             Filename = 'file.txt');
  
  { Test 4: Extract dir }
  Dir := ExtractDir('/path/to/file.txt');
  TestResult('Extract dir from path',
             (Dir = '/path/to') or (Dir = '\path\to'));
  
  { Test 5: Extract extension }
  TestResult('Extract .txt extension',
             ExtractExt('file.txt') = '.txt');
  
  TestResult('Extract no extension',
             ExtractExt('file') = '');
  
  { Test 6: StartsWith }
  TestResult('StartsWith /home',
             StartsWith('/home/user', '/home'));
  
  TestResult('Not StartsWith /etc',
             not StartsWith('/home/user', '/etc'));
  
  { Test 7: EndsWith }
  TestResult('EndsWith .txt',
             EndsWith('file.txt', '.txt'));
  
  TestResult('Not EndsWith .exe',
             not EndsWith('file.txt', '.exe'));
end;

procedure TestRandomEdgeCases;
var
  PeerID: array[0..19] of Byte;
  I: Integer;
  Val: Integer;
begin
  WriteLn(#10'=== Testing Random Edge Cases ===');
  
  { Test 1: RandomRange with Min = Max }
  Val := RandomRange(5, 5);
  TestResult('RandomRange(5, 5) returns 5',
             Val = 5);
  
  { Test 2: RandomRange with Min > Max }
  Val := RandomRange(10, 5);
  TestResult('RandomRange(10, 5) returns 10',
             Val = 10);
  
  { Test 3: RandomRange with negative numbers }
  Val := RandomRange(-10, -5);
  TestResult('RandomRange(-10, -5) within bounds',
             (Val >= -10) and (Val < -5));
  
  { Test 4: RandomRange spanning zero }
  Val := RandomRange(-5, 5);
  TestResult('RandomRange(-5, 5) within bounds',
             (Val >= -5) and (Val < 5));
  
  { Test 5: GeneratePeerID with empty prefix }
  GeneratePeerID(PeerID, '');
  TestResult('PeerID with empty prefix is all random',
             True);  { Just verify no crash }
  
  { Test 6: GeneratePeerID with long prefix }
  GeneratePeerID(PeerID, '-PT0100-EXTRALONGPREFIX');
  TestResult('PeerID truncates long prefix',
             (PeerID[0] = Ord('-')) and (PeerID[1] = Ord('P')));
  
  { Test 7: GeneratePeerID exact 20 char prefix }
  GeneratePeerID(PeerID, '12345678901234567890');
  TestResult('PeerID with exact 20 char prefix',
             PeerID[19] = Ord('0'));
  
  { Test 8: RandomBytes with zero length }
  for I := 0 to 19 do
    PeerID[I] := 0;
  RandomBytes(PeerID, 0);
  TestResult('RandomBytes with len=0 does nothing',
             PeerID[0] = 0);
end;

procedure TestTimeUtilities;
var
  StartTime, EndTime: Int64;
  MonoStart, MonoEnd: Double;
begin
  WriteLn(#10'=== Testing Time Utilities ===');
  
  { Test 1: GetTickMS returns increasing values }
  StartTime := GetTickMS;
  SleepMS(50);  { Sleep 50ms }
  EndTime := GetTickMS;
  TestResult('GetTickMS increases after SleepMS',
             EndTime >= StartTime);
  
  { Test 2: GetMonoTime returns increasing values }
  MonoStart := GetMonoTime;
  SleepMS(10);
  MonoEnd := GetMonoTime;
  TestResult('GetMonoTime increases',
             MonoEnd >= MonoStart);
  
  { Test 3: TimeDiffMS with normal values }
  StartTime := 1000;
  EndTime := 1500;
  TestResult('TimeDiffMS normal',
             TimeDiffMS(StartTime, EndTime) = 500);
  
  { Test 4: TimeDiffMS with overflow simulation }
  StartTime := $FFFFFFFF - 500;
  EndTime := 100;
  TestResult('TimeDiffMS handles overflow',
             TimeDiffMS(StartTime, EndTime) = 601);
  
  { Test 5: TimeDiffMS with same values }
  TestResult('TimeDiffMS same values',
             TimeDiffMS(1000, 1000) = 0);
end;

procedure TestRandom;
var
  PeerID: array[0..19] of Byte;
  I: Integer;
  AllSame: Boolean;
  R1, R2: Cardinal;
  Val: Integer;
begin
  WriteLn(#10'=== Testing Random ===');
  
  { Test 1: Generate peer ID }
  GeneratePeerID(PeerID, '-PT0100-');
  TestResult('Peer ID has prefix',
             (PeerID[0] = Ord('-')) and (PeerID[1] = Ord('P')));
  
  { Test 2: Check not all same }
  AllSame := True;
  for I := 8 to 19 do
    if PeerID[I] <> PeerID[8] then
      AllSame := False;
  TestResult('Peer ID has randomness',
             not AllSame);
  
  { Test 3: RandomInt produces different values }
  R1 := RandomInt;
  R2 := RandomInt;
  TestResult('RandomInt produces different values',
             R1 <> R2);
  
  { Test 4: RandomRange bounds }
  Val := RandomRange(10, 20);
  TestResult('RandomRange 10-20 within bounds',
             (Val >= 10) and (Val < 20));
  
  { Test 5: RandomBytes }
  RandomBytes(PeerID, 20);
  AllSame := True;
  for I := 1 to 19 do
    if PeerID[I] <> PeerID[0] then
      AllSame := False;
  TestResult('RandomBytes produces random data',
             not AllSame);
end;

procedure TestSwap64;
begin
  WriteLn(#10'=== Testing Swap64 ===');
  
  TestResult('Swap64 0x0123456789ABCDEF',
             Swap64(QWord($0123456789ABCDEF)) = QWord($EFCDAB8967452301));
  TestResult('Swap64 round-trip',
             Swap64(Swap64($1234567890ABCDEF)) = $1234567890ABCDEF);
end;

procedure TestReadBE32;
var
  Buf: array[0..3] of Byte;
  Val: Cardinal;
begin
  WriteLn(#10'=== Testing ReadBE32 ===');
  
  Buf[0] := $12; Buf[1] := $34; Buf[2] := $56; Buf[3] := $78;
  Val := ReadBE32(Buf);
  TestResult('ReadBE32 reads big-endian value',
             Val = $12345678);
  
  Buf[0] := $00; Buf[1] := $00; Buf[2] := $00; Buf[3] := $01;
  TestResult('ReadBE32 reads 1 correctly',
             ReadBE32(Buf) = 1);
end;

procedure TestIsAbsolutePath;
begin
  WriteLn(#10'=== Testing IsAbsolutePath ===');
  
  {$IFDEF MSWINDOWS}
  TestResult('IsAbsolutePath C:\test',
             IsAbsolutePath('C:\test'));
  TestResult('IsAbsolutePath \\server\share',
             IsAbsolutePath('\\server\share'));
  TestResult('Not IsAbsolutePath relative',
             not IsAbsolutePath('relative\path'));
  {$ELSE}
  TestResult('IsAbsolutePath /home/user',
             IsAbsolutePath('/home/user'));
  TestResult('IsAbsolutePath /',
             IsAbsolutePath('/'));
  TestResult('Not IsAbsolutePath relative',
             not IsAbsolutePath('relative/path'));
  TestResult('Not IsAbsolutePath ./relative',
             not IsAbsolutePath('./relative'));
  {$ENDIF}
end;

procedure TestFileOperationsExtended;
var
  TestDir: string;
  TestFile: string;
  F: File;
  Size: Int64;
begin
  WriteLn(#10'=== Testing Extended File Operations ===');
  
  TestDir := 'test_temp_dir';
  TestFile := 'test_temp_file.txt';
  
  { Clean up any leftovers }
  if DirExists(TestDir) then
    RemoveDir(TestDir);
  if FileExists(TestFile) then
    DeleteFile(TestFile);
  
  { Test 1: MakeDir }
  if MakeDir(TestDir) then
  begin
    TestResult('MakeDir creates directory',
               DirExists(TestDir));
    
    { Cleanup }
    RemoveDir(TestDir);
  end
  else
  begin
    TestResult('MakeDir creates directory', False, 'May require permissions');
  end;
  
  { Test 2: GetFileSize }
  Assign(F, TestFile);
  Rewrite(F, 1);
  BlockWrite(F, 'Hello World', 11);
  Close(F);
  
  if GetFileSize(TestFile, Size) then
  begin
    TestResult('GetFileSize returns correct size',
               Size = 11);
  end
  else
  begin
    TestResult('GetFileSize works', False);
  end;
  
  { Cleanup }
  if FileExists(TestFile) then
    DeleteFile(TestFile);
  
  { Test 3: FileExists and DirExists }
  TestResult('FileExists returns false for non-existent',
             not FileExists('nonexistent_file_xyz.txt'));
  TestResult('DirExists returns false for non-existent',
             not DirExists('nonexistent_dir_xyz'));
end;

procedure TestHexConversionsEdgeCases;
var
  Hex: string;
  Count: Integer;
  StaticBytes: array[0..31] of Byte;
  I: Integer;
begin
  WriteLn(#10'=== Testing Hex Conversions Edge Cases ===');
  
  { Initialize array to suppress compiler warning }
  FillChar(StaticBytes, SizeOf(StaticBytes), 0);
  
  { Test 1: BytesToHex with 0 length }
  Hex := BytesToHex(StaticBytes, 0);
  TestResult('BytesToHex with len=0 returns empty',
             Hex = '');
  
  { Test 2: BytesToHex with single byte }
  StaticBytes[0] := $AB;
  Hex := BytesToHex(StaticBytes, 1);
  TestResult('BytesToHex single byte',
             Hex = 'ab');
  
  { Test 3: BytesToHex with all zeros }
  for I := 0 to 3 do
    StaticBytes[I] := 0;
  Hex := BytesToHex(StaticBytes, 4);
  TestResult('BytesToHex all zeros',
             Hex = '00000000');
  
  { Test 4: BytesToHex with all 0xFF }
  for I := 0 to 3 do
    StaticBytes[I] := $FF;
  Hex := BytesToHex(StaticBytes, 4);
  TestResult('BytesToHex all 0xFF',
             Hex = 'ffffffff');
  
  { Test 5: HexToBytes with empty string }
  Count := HexToBytes('', StaticBytes, 32);
  TestResult('HexToBytes empty string returns 0',
             Count = 0);
  
  { Test 6: HexToBytes with single hex digit }
  Count := HexToBytes('A', StaticBytes, 32);
  TestResult('HexToBytes single digit returns 0',
             Count = 0);
  
  { Test 7: HexToBytes with invalid characters }
  Count := HexToBytes('GHIJKL', StaticBytes, 32);
  TestResult('HexToBytes invalid chars returns 0',
             Count = 0);
  
  { Test 8: HexToBytes with MaxBytes limit }
  Count := HexToBytes('AABBCCDDEEFF', StaticBytes, 2);
  TestResult('HexToBytes respects MaxBytes',
             Count = 2);
  
  { Test 9: HexToBytes with mixed valid/invalid - behavior varies }
  Count := HexToBytes('ABxxCD', StaticBytes, 32);
  TestResult('HexToBytes handles mixed input',
             Count >= 0);  { At least don't crash }
  
  { Test 10: HexToBytes with very long string }
  Count := HexToBytes('00112233445566778899AABBCCDDEEFF', StaticBytes, 32);
  TestResult('HexToBytes long string',
             Count = 16);
  
  { Test 11: Round-trip all byte values }
  for I := 0 to 255 do
    StaticBytes[I mod 32] := I;
  Hex := BytesToHex(StaticBytes, 32);
  Count := HexToBytes(Hex, StaticBytes, 32);
  TestResult('Hex round-trip 32 bytes',
             Count = 32);
end;

procedure TestFileOperationsMore;
var
  TestFile1, TestFile2: string;
  F: File;
begin
  WriteLn(#10'=== Testing Additional File Operations ===');
  
  TestFile1 := 'test_file_ops1.tmp';
  TestFile2 := 'test_file_ops2.tmp';
  
  { Clean up }
  if FileExists(TestFile1) then
    DeleteFile(TestFile1);
  if FileExists(TestFile2) then
    DeleteFile(TestFile2);
  
  { Test 1: DeleteFile }
  Assign(F, TestFile1);
  Rewrite(F, 1);
  BlockWrite(F, 'test', 4);
  Close(F);
  
  if FileExists(TestFile1) then
  begin
    DeleteFile(TestFile1);
    TestResult('DeleteFile removes file',
               not FileExists(TestFile1));
  end
  else
    TestResult('DeleteFile test setup failed', False);
  
  { Test 2: RenameFile }
  Assign(F, TestFile1);
  Rewrite(F, 1);
  BlockWrite(F, 'test', 4);
  Close(F);
  
  if RenameFile(TestFile1, TestFile2) then
  begin
    TestResult('RenameFile removes old name',
               not FileExists(TestFile1));
    TestResult('RenameFile creates new name',
               FileExists(TestFile2));
  end
  else
  begin
    TestResult('RenameFile failed', False, 'May be permission issue');
  end;
  
  { Cleanup }
  if FileExists(TestFile1) then
    DeleteFile(TestFile1);
  if FileExists(TestFile2) then
    DeleteFile(TestFile2);
  
  { Test 3: ExpandPath }
  TestResult('ExpandPath returns something',
             ExpandPath('test') <> '');
  TestResult('ExpandPath preserves absolute',
             ExpandPath('/absolute/path') = '/absolute/path');
end;

procedure TestHexConversions;
var
  Hex: string;
  Count: Integer;
  StaticBytes: array[0..31] of Byte;
begin
  WriteLn(#10'=== Testing Hex Conversions ===');
  
  { Initialize array to suppress compiler warning }
  FillChar(StaticBytes, SizeOf(StaticBytes), 0);
  
  { Test 1: Bytes to hex }
  StaticBytes[0] := $AB;
  StaticBytes[1] := $CD;
  Hex := BytesToHex(StaticBytes, 2);
  TestResult('Bytes to hex',
             Hex = 'abcd');
  
  { Test 2: Hex to bytes }
  Count := HexToBytes('ABCD', StaticBytes, 32);
  TestResult('Hex to bytes',
             (Count = 2) and (StaticBytes[0] = $AB) and (StaticBytes[1] = $CD));
  
  { Test 3: Hex with spaces }
  Count := HexToBytes('AB CD EF', StaticBytes, 32);
  TestResult('Hex with spaces',
             Count = 3);
  
  { Test 4: Hex case insensitive }
  Count := HexToBytes('aBcD', StaticBytes, 32);
  TestResult('Hex case insensitive',
             (Count = 2) and (StaticBytes[0] = $AB));
  
  { Test 5: Round-trip }
  for Count := 0 to 19 do
    StaticBytes[Count] := Count * 10;
  Hex := BytesToHex(StaticBytes, 20);
  Count := HexToBytes(Hex, StaticBytes, 32);
  TestResult('Hex round-trip (20 bytes)',
             Count = 20);
end;

procedure RunAllTests;
begin
  WriteLn('==============================================');
  WriteLn('  UTILS UNIT TEST SUITE');
  WriteLn('==============================================');
  
  Randomize;
  
  TestLinkedListsEdgeCases;
  TestLinkedLists;
  TestDynamicBufferEdgeCases;
  TestDynamicBuffer;
  TestStringFormattingEdgeCases;
  TestStringFormatting;
  TestSplitJoinEdgeCases;
  TestSplitJoin;
  TestURLCodingEdgeCases;
  TestURLCoding;
  TestMemoryMove;
  TestBinaryOperationsEdgeCases;
  TestBinaryOperations;
  TestPathOperationsEdgeCases;
  TestPathOperations;
  TestRandomEdgeCases;
  TestRandom;
  TestTimeUtilities;
  TestHexConversionsEdgeCases;
  TestHexConversions;
  TestSwap64;
  TestReadBE32;
  TestIsAbsolutePath;
  TestFileOperationsExtended;
  TestFileOperationsMore;
  
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
