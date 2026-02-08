{
  test_peer_integration.pas - Peer Wire Protocol Integration Tests
  
  Tests complete peer workflows including:
  - Handshake exchange
  - Bitfield exchange
  - Choke/unchoke state machine
  - Interest management
  - Request/piece message flow
}

program test_peer_integration;

{$mode objfpc}{$H+}

uses
  SysUtils, Classes, sockwrap, protocol, sha1utils, utils, metainfo;

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
{ Test Helper: Create a mock torrent info hash                        }
{ ============================================================================ }

procedure CreateMockInfoHash(out InfoHash: array of Byte);
var
  I: Integer;
begin
  for I := 0 to 19 do
    InfoHash[I] := I * 7 + 13;  { Deterministic but unique pattern }
end;

procedure CreateMockPeerId(out PeerId: array of Byte);
var
  I: Integer;
begin
  for I := 0 to 19 do
    PeerId[I] := 255 - I * 3;  { Different pattern from info hash }
end;

{ ============================================================================ }
{ Test 1: Complete Handshake Exchange                                          }
{ ============================================================================ }

procedure TestCompleteHandshakeExchange;
var
  Server, Client, Accepted: PSocketContext;
  Res: Integer;
  TestPort: Word;
  LocalInfoHash, RemoteInfoHash: array[0..19] of Byte;
  LocalPeerId, RemotePeerId: array[0..19] of Byte;
  SendBuffer, RecvBuffer: array[0..127] of Byte;
  LocalHS, RemoteHS: THandshake;
  Timeout: Integer;
begin
  WriteLn(#10'=== Testing Complete Handshake Exchange ===');
  
  TestPort := 50000 + Random(1000);
  
  CreateMockInfoHash(LocalInfoHash);
  CreateMockInfoHash(RemoteInfoHash);
  CreateMockPeerId(LocalPeerId);
  CreateMockPeerId(RemotePeerId);
  
  SocketInit;
  
  { Setup server }
  Server := SocketCreate;
  SocketBind(Server, '127.0.0.1', TestPort);
  SocketListen(Server, 1);
  
  { Connect client }
  Client := SocketCreate;
  SocketConnect(Client, '127.0.0.1', TestPort, 5000);
  
  { Accept connection }
  Timeout := 0;
  Accepted := nil;
  while Timeout < 50 do
  begin
    if SocketHasPending(Server) then
    begin
      Res := SocketAccept(Server, Accepted);
      if Res = SOCK_OK then
        Break;
    end;
    Sleep(100);
    Inc(Timeout);
  end;
  
  if Accepted = nil then
  begin
    TestResult('Server accepts connection', False);
    SocketDestroy(Client);
    SocketDestroy(Server);
    SocketCleanup;
    Exit;
  end;
  
  try
    { Client sends handshake }
    Res := EncodeHandshake(LocalInfoHash, LocalPeerId, @SendBuffer, SizeOf(SendBuffer));
    TestResult('Client encodes handshake', Res = HANDSHAKE_LEN);
    
    if Res = HANDSHAKE_LEN then
    begin
      Res := SocketSendAll(Client, @SendBuffer, HANDSHAKE_LEN);
      TestResult('Client sends handshake', Res = SOCK_OK);
    end;
    
    { Server receives handshake }
    Res := SocketReceiveAll(Accepted, @RecvBuffer, HANDSHAKE_LEN);
    TestResult('Server receives handshake', Res = SOCK_OK);
    
    if Res = SOCK_OK then
    begin
      TestResult('Server decodes handshake', 
                 DecodeHandshake(@RecvBuffer, HANDSHAKE_LEN, RemoteHS));
      TestResult('Server validates handshake', ValidateHandshake(RemoteHS));
      TestResult('Server sees correct info hash', 
                 CompareMem(@LocalInfoHash, @RemoteHS.InfoHash, 20));
      TestResult('Server sees correct peer ID', 
                 CompareMem(@LocalPeerId, @RemoteHS.PeerId, 20));
    end;
    
    { Server sends handshake response }
    Res := EncodeHandshake(LocalInfoHash, RemotePeerId, @SendBuffer, SizeOf(SendBuffer));
    if Res = HANDSHAKE_LEN then
    begin
      Res := SocketSendAll(Accepted, @SendBuffer, HANDSHAKE_LEN);
      TestResult('Server sends handshake response', Res = SOCK_OK);
    end;
    
    { Client receives handshake response }
    Res := SocketReceiveAll(Client, @RecvBuffer, HANDSHAKE_LEN);
    TestResult('Client receives handshake response', Res = SOCK_OK);
    
    if Res = SOCK_OK then
    begin
      TestResult('Client decodes handshake response', 
                 DecodeHandshake(@RecvBuffer, HANDSHAKE_LEN, LocalHS));
      TestResult('Client validates handshake', ValidateHandshake(LocalHS));
      TestResult('Client sees matching info hash', 
                 CompareMem(@LocalInfoHash, @LocalHS.InfoHash, 20));
    end;
  finally
    SocketDestroy(Accepted);
    SocketDestroy(Client);
    SocketDestroy(Server);
    SocketCleanup;
  end;
end;

{ ============================================================================ }
{ Test 2: Bitfield Exchange                                                    }
{ ============================================================================ }

procedure TestBitfieldExchange;
var
  Server, Client, Accepted: PSocketContext;
  Res: Integer;
  TestPort: Word;
  InfoHash, PeerId: array[0..19] of Byte;
  Buffer: array[0..4095] of Byte;
  Bitfield: array of Byte;
  BitfieldLen: Integer;
  I: Integer;
  DecodedMsg: TWireMessage;
  BytesConsumed: Integer;
  Timeout: Integer;
begin
  WriteLn(#10'=== Testing Bitfield Exchange ===');
  
  TestPort := 51000 + Random(1000);
  
  CreateMockInfoHash(InfoHash);
  CreateMockPeerId(PeerId);
  
  { Create 100-piece bitfield (13 bytes) }
  BitfieldLen := 13;
  SetLength(Bitfield, BitfieldLen);
  for I := 0 to BitfieldLen - 1 do
    Bitfield[I] := I * 17 mod 256;
  
  SocketInit;
  
  { Setup server }
  Server := SocketCreate;
  SocketBind(Server, '127.0.0.1', TestPort);
  SocketListen(Server, 1);
  
  { Connect client }
  Client := SocketCreate;
  SocketConnect(Client, '127.0.0.1', TestPort, 5000);
  
  { Accept connection }
  Timeout := 0;
  Accepted := nil;
  while Timeout < 50 do
  begin
    if SocketHasPending(Server) then
    begin
      Res := SocketAccept(Server, Accepted);
      if Res = SOCK_OK then
        Break;
    end;
    Sleep(100);
    Inc(Timeout);
  end;
  
  if Accepted = nil then
  begin
    TestResult('Server accepts connection', False);
    SocketDestroy(Client);
    SocketDestroy(Server);
    SocketCleanup;
    Exit;
  end;
  
  try
    { Exchange handshakes first }
    Res := EncodeHandshake(InfoHash, PeerId, @Buffer, SizeOf(Buffer));
    SocketSendAll(Client, @Buffer, HANDSHAKE_LEN);
    SocketReceiveAll(Accepted, @Buffer, HANDSHAKE_LEN);
    SocketSendAll(Accepted, @Buffer, HANDSHAKE_LEN);
    SocketReceiveAll(Client, @Buffer, HANDSHAKE_LEN);
    
    { Server sends bitfield }
    TestResult('Server builds bitfield message', 
               BuildBitfield(@Bitfield[0], BitfieldLen, @Buffer, SizeOf(Buffer), Res));
    
    if Res > 0 then
    begin
      Res := SocketSendAll(Accepted, @Buffer, 5 + BitfieldLen);
      TestResult('Server sends bitfield', Res = SOCK_OK);
    end;
    
    { Client receives and decodes bitfield }
    Res := SocketReceiveAll(Client, @Buffer, 5 + BitfieldLen);
    TestResult('Client receives bitfield', Res = SOCK_OK);
    
    if Res = SOCK_OK then
    begin
      TestResult('Client decodes bitfield', 
                 DecodeMessage(@Buffer, 5 + BitfieldLen, DecodedMsg, BytesConsumed));
      if DecodeMessage(@Buffer, 5 + BitfieldLen, DecodedMsg, BytesConsumed) then
      begin
        TestResult('Message type is BITFIELD', DecodedMsg.MsgId = MSG_BITFIELD);
        TestResult('Bitfield length correct', DecodedMsg.BitfieldLen = BitfieldLen);
        TestResult('Bitfield data matches', 
                   CompareMem(@Bitfield[0], DecodedMsg.BitfieldData, BitfieldLen));
      end;
    end;
    
    { Client sends empty bitfield (have nothing) }
    if BuildBitfield(nil, 0, @Buffer, SizeOf(Buffer), Res) then
    begin
      Res := SocketSendAll(Client, @Buffer, 5);
      TestResult('Client sends empty bitfield', Res = SOCK_OK);
    end;
    
    { Server receives empty bitfield }
    Res := SocketReceiveAll(Accepted, @Buffer, 5);
    if Res = SOCK_OK then
    begin
      DecodeMessage(@Buffer, 5, DecodedMsg, BytesConsumed);
      TestResult('Server receives empty bitfield', DecodedMsg.BitfieldLen = 0);
    end;
  finally
    SocketDestroy(Accepted);
    SocketDestroy(Client);
    SocketDestroy(Server);
    SocketCleanup;
  end;
end;

{ ============================================================================ }
{ Test 3: Choke/Unchoke State Machine                                          }
{ ============================================================================ }

procedure TestChokeUnchokeStateMachine;
var
  Server, Client, Accepted: PSocketContext;
  Res: Integer;
  TestPort: Word;
  InfoHash, PeerId: array[0..19] of Byte;
  Buffer: array[0..1023] of Byte;
  DecodedMsg: TWireMessage;
  BytesConsumed: Integer;
  Timeout: Integer;
  ClientChoked: Boolean;
  ServerChoked: Boolean;
begin
  WriteLn(#10'=== Testing Choke/Unchoke State Machine ===');
  
  TestPort := 52000 + Random(1000);
  
  CreateMockInfoHash(InfoHash);
  CreateMockPeerId(PeerId);
  
  SocketInit;
  
  { Setup server }
  Server := SocketCreate;
  SocketBind(Server, '127.0.0.1', TestPort);
  SocketListen(Server, 1);
  
  { Connect client }
  Client := SocketCreate;
  SocketConnect(Client, '127.0.0.1', TestPort, 5000);
  
  { Accept connection }
  Timeout := 0;
  Accepted := nil;
  while Timeout < 50 do
  begin
    if SocketHasPending(Server) then
    begin
      Res := SocketAccept(Server, Accepted);
      if Res = SOCK_OK then
        Break;
    end;
    Sleep(100);
    Inc(Timeout);
  end;
  
  if Accepted = nil then
  begin
    TestResult('Server accepts connection', False);
    SocketDestroy(Client);
    SocketDestroy(Server);
    SocketCleanup;
    Exit;
  end;
  
  try
    { Exchange handshakes }
    Res := EncodeHandshake(InfoHash, PeerId, @Buffer, SizeOf(Buffer));
    SocketSendAll(Client, @Buffer, HANDSHAKE_LEN);
    SocketReceiveAll(Accepted, @Buffer, HANDSHAKE_LEN);
    SocketSendAll(Accepted, @Buffer, HANDSHAKE_LEN);
    SocketReceiveAll(Client, @Buffer, HANDSHAKE_LEN);
    
    { Initial state: both choked }
    ClientChoked := True;
    ServerChoked := True;
    
    { Server unchokes client }
    BuildUnchoke(@Buffer, SizeOf(Buffer), Res);
    SocketSendAll(Accepted, @Buffer, Res);
    
    { Client receives unchoke }
    Res := SocketReceiveAll(Client, @Buffer, 5);
    if Res = SOCK_OK then
    begin
      DecodeMessage(@Buffer, 5, DecodedMsg, BytesConsumed);
      TestResult('Client receives UNCHOKE', DecodedMsg.MsgId = MSG_UNCHOKE);
      if DecodedMsg.MsgId = MSG_UNCHOKE then
        ClientChoked := False;
    end;
    
    TestResult('Client state: unchoked', not ClientChoked);
    
    { Client unchokes server }
    BuildUnchoke(@Buffer, SizeOf(Buffer), Res);
    SocketSendAll(Client, @Buffer, Res);
    
    { Server receives unchoke }
    Res := SocketReceiveAll(Accepted, @Buffer, 5);
    if Res = SOCK_OK then
    begin
      DecodeMessage(@Buffer, 5, DecodedMsg, BytesConsumed);
      TestResult('Server receives UNCHOKE', DecodedMsg.MsgId = MSG_UNCHOKE);
      if DecodedMsg.MsgId = MSG_UNCHOKE then
        ServerChoked := False;
    end;
    
    TestResult('Server state: unchoked', not ServerChoked);
    
    { Server chokes client again }
    BuildChoke(@Buffer, SizeOf(Buffer), Res);
    SocketSendAll(Accepted, @Buffer, Res);
    
    { Client receives choke }
    Res := SocketReceiveAll(Client, @Buffer, 5);
    if Res = SOCK_OK then
    begin
      DecodeMessage(@Buffer, 5, DecodedMsg, BytesConsumed);
      TestResult('Client receives CHOKE', DecodedMsg.MsgId = MSG_CHOKE);
      if DecodedMsg.MsgId = MSG_CHOKE then
        ClientChoked := True;
    end;
    
    TestResult('Client state: choked again', ClientChoked);
    
    { Test interested/not interested }
    BuildInterested(@Buffer, SizeOf(Buffer), Res);
    SocketSendAll(Client, @Buffer, Res);
    
    Res := SocketReceiveAll(Accepted, @Buffer, 5);
    if Res = SOCK_OK then
    begin
      DecodeMessage(@Buffer, 5, DecodedMsg, BytesConsumed);
      TestResult('Server receives INTERESTED', DecodedMsg.MsgId = MSG_INTERESTED);
    end;
    
    BuildNotInterested(@Buffer, SizeOf(Buffer), Res);
    SocketSendAll(Client, @Buffer, Res);
    
    Res := SocketReceiveAll(Accepted, @Buffer, 5);
    if Res = SOCK_OK then
    begin
      DecodeMessage(@Buffer, 5, DecodedMsg, BytesConsumed);
      TestResult('Server receives NOT_INTERESTED', DecodedMsg.MsgId = MSG_NOT_INTERESTED);
    end;
  finally
    SocketDestroy(Accepted);
    SocketDestroy(Client);
    SocketDestroy(Server);
    SocketCleanup;
  end;
end;

{ ============================================================================ }
{ Test 4: Request/Piece Message Flow                                           }
{ ============================================================================ }

procedure TestRequestPieceFlow;
var
  Server, Client, Accepted: PSocketContext;
  Res: Integer;
  TestPort: Word;
  InfoHash, PeerId: array[0..19] of Byte;
  Buffer: array[0..65535] of Byte;
  PieceData: array[0..16383] of Byte;
  DecodedMsg: TWireMessage;
  BytesConsumed: Integer;
  Timeout: Integer;
  I: Integer;
  DataMatches: Boolean;
begin
  WriteLn(#10'=== Testing Request/Piece Message Flow ===');
  
  TestPort := 53000 + Random(1000);
  
  CreateMockInfoHash(InfoHash);
  CreateMockPeerId(PeerId);
  
  { Create test piece data }
  for I := 0 to High(PieceData) do
    PieceData[I] := I mod 256;
  
  SocketInit;
  
  { Setup server (seeder) }
  Server := SocketCreate;
  SocketBind(Server, '127.0.0.1', TestPort);
  SocketListen(Server, 1);
  
  { Connect client (leecher) }
  Client := SocketCreate;
  SocketConnect(Client, '127.0.0.1', TestPort, 5000);
  
  { Accept connection }
  Timeout := 0;
  Accepted := nil;
  while Timeout < 50 do
  begin
    if SocketHasPending(Server) then
    begin
      Res := SocketAccept(Server, Accepted);
      if Res = SOCK_OK then
        Break;
    end;
    Sleep(100);
    Inc(Timeout);
  end;
  
  if Accepted = nil then
  begin
    TestResult('Server accepts connection', False);
    SocketDestroy(Client);
    SocketDestroy(Server);
    SocketCleanup;
    Exit;
  end;
  
  try
    { Exchange handshakes }
    Res := EncodeHandshake(InfoHash, PeerId, @Buffer, SizeOf(Buffer));
    SocketSendAll(Client, @Buffer, HANDSHAKE_LEN);
    SocketReceiveAll(Accepted, @Buffer, HANDSHAKE_LEN);
    SocketSendAll(Accepted, @Buffer, HANDSHAKE_LEN);
    SocketReceiveAll(Client, @Buffer, HANDSHAKE_LEN);
    
    { Server sends bitfield (has piece 0) }
    Buffer[0] := $80;  { First bit set = has piece 0 }
    for I := 1 to 12 do
      Buffer[I] := 0;
    if BuildBitfield(@Buffer, 13, @Buffer, SizeOf(Buffer), Res) then
      SocketSendAll(Accepted, @Buffer, 5 + 13);
    
    { Client receives bitfield }
    Res := SocketReceiveAll(Client, @Buffer, 5 + 13);
    
    { Client sends interested }
    BuildInterested(@Buffer, SizeOf(Buffer), Res);
    SocketSendAll(Client, @Buffer, Res);
    SocketReceiveAll(Accepted, @Buffer, 5);  { Server receives interested }
    
    { Server unchokes client }
    BuildUnchoke(@Buffer, SizeOf(Buffer), Res);
    SocketSendAll(Accepted, @Buffer, Res);
    SocketReceiveAll(Client, @Buffer, 5);  { Client receives unchoke }
    
    { Client sends request for piece 0, block 0 }
    BuildRequest(0, 0, 16384, @Buffer, SizeOf(Buffer), Res);
    TestResult('Client builds REQUEST message', Res = 17);
    
    Res := SocketSendAll(Client, @Buffer, 17);
    TestResult('Client sends REQUEST', Res = SOCK_OK);
    
    { Server receives request }
    Res := SocketReceiveAll(Accepted, @Buffer, 17);
    TestResult('Server receives REQUEST', Res = SOCK_OK);
    
    if Res = SOCK_OK then
    begin
      TestResult('Server decodes REQUEST', 
                 DecodeMessage(@Buffer, 17, DecodedMsg, BytesConsumed));
      if DecodeMessage(@Buffer, 17, DecodedMsg, BytesConsumed) then
      begin
        TestResult('Request is for piece 0', DecodedMsg.ReqIndex = 0);
        TestResult('Request begin is 0', DecodedMsg.ReqBegin = 0);
        TestResult('Request length is 16384', DecodedMsg.ReqLength = 16384);
      end;
    end;
    
    { Server sends piece }
    TestResult('Server builds PIECE message', 
               BuildPiece(0, 0, @PieceData, 16384, @Buffer, SizeOf(Buffer), Res));
    
    if Res > 0 then
    begin
      Res := SocketSendAll(Accepted, @Buffer, 13 + 16384);
      TestResult('Server sends PIECE', Res = SOCK_OK);
    end;
    
    { Client receives piece }
    Res := SocketReceiveAll(Client, @Buffer, 13 + 16384);
    TestResult('Client receives PIECE', Res = SOCK_OK);
    
    if Res = SOCK_OK then
    begin
      TestResult('Client decodes PIECE', 
                 DecodeMessage(@Buffer, 13 + 16384, DecodedMsg, BytesConsumed));
      if DecodeMessage(@Buffer, 13 + 16384, DecodedMsg, BytesConsumed) then
      begin
        TestResult('Piece index is 0', DecodedMsg.PieceIndex = 0);
        TestResult('Piece begin is 0', DecodedMsg.PieceBegin = 0);
        TestResult('Piece data length is 16384', DecodedMsg.PieceDataLen = 16384);
        
        { Verify data }
        DataMatches := CompareMem(@PieceData, DecodedMsg.PieceData, 16384);
        TestResult('Piece data matches request', DataMatches);
      end;
    end;
  finally
    SocketDestroy(Accepted);
    SocketDestroy(Client);
    SocketDestroy(Server);
    SocketCleanup;
  end;
end;

{ ============================================================================ }
{ Test 5: Have Message Announcements                                           }
{ ============================================================================ }

procedure TestHaveAnnouncements;
var
  Server, Client, Accepted: PSocketContext;
  Res: Integer;
  TestPort: Word;
  InfoHash, PeerId: array[0..19] of Byte;
  Buffer: array[0..1023] of Byte;
  DecodedMsg: TWireMessage;
  BytesConsumed: Integer;
  Timeout: Integer;
  I: Integer;
begin
  WriteLn(#10'=== Testing Have Message Announcements ===');
  
  TestPort := 54000 + Random(1000);
  
  CreateMockInfoHash(InfoHash);
  CreateMockPeerId(PeerId);
  
  SocketInit;
  
  { Setup server }
  Server := SocketCreate;
  SocketBind(Server, '127.0.0.1', TestPort);
  SocketListen(Server, 1);
  
  { Connect client }
  Client := SocketCreate;
  SocketConnect(Client, '127.0.0.1', TestPort, 5000);
  
  { Accept connection }
  Timeout := 0;
  Accepted := nil;
  while Timeout < 50 do
  begin
    if SocketHasPending(Server) then
    begin
      Res := SocketAccept(Server, Accepted);
      if Res = SOCK_OK then
        Break;
    end;
    Sleep(100);
    Inc(Timeout);
  end;
  
  if Accepted = nil then
  begin
    TestResult('Server accepts connection', False);
    SocketDestroy(Client);
    SocketDestroy(Server);
    SocketCleanup;
    Exit;
  end;
  
  try
    { Exchange handshakes }
    Res := EncodeHandshake(InfoHash, PeerId, @Buffer, SizeOf(Buffer));
    SocketSendAll(Client, @Buffer, HANDSHAKE_LEN);
    SocketReceiveAll(Accepted, @Buffer, HANDSHAKE_LEN);
    SocketSendAll(Accepted, @Buffer, HANDSHAKE_LEN);
    SocketReceiveAll(Client, @Buffer, HANDSHAKE_LEN);
    
    { Server announces multiple pieces via HAVE messages }
    for I := 0 to 4 do
    begin
      BuildHave(I * 100, @Buffer, SizeOf(Buffer), Res);  { Pieces 0, 100, 200, 300, 400 }
      SocketSendAll(Accepted, @Buffer, Res);
    end;
    
    { Client receives HAVE messages }
    for I := 0 to 4 do
    begin
      Res := SocketReceiveAll(Client, @Buffer, 9);
      if Res = SOCK_OK then
      begin
        if DecodeMessage(@Buffer, 9, DecodedMsg, BytesConsumed) then
        begin
          TestResult(Format('Client receives HAVE(%d)', [I * 100]), 
                     (DecodedMsg.MsgId = MSG_HAVE) and 
                     (DecodedMsg.HaveIndex = Cardinal(I * 100)));
        end
        else
          TestResult(Format('Client decodes HAVE(%d)', [I * 100]), False);
      end
      else
        TestResult(Format('Client receives HAVE(%d)', [I * 100]), False);
    end;
  finally
    SocketDestroy(Accepted);
    SocketDestroy(Client);
    SocketDestroy(Server);
    SocketCleanup;
  end;
end;

{ ============================================================================ }
{ Test 6: Cancel Message Flow                                                  }
{ ============================================================================ }

procedure TestCancelFlow;
var
  Server, Client, Accepted: PSocketContext;
  Res: Integer;
  TestPort: Word;
  InfoHash, PeerId: array[0..19] of Byte;
  Buffer: array[0..1023] of Byte;
  DecodedMsg: TWireMessage;
  BytesConsumed: Integer;
  Timeout: Integer;
begin
  WriteLn(#10'=== Testing Cancel Message Flow ===');
  
  TestPort := 55000 + Random(1000);
  
  CreateMockInfoHash(InfoHash);
  CreateMockPeerId(PeerId);
  
  SocketInit;
  
  { Setup server }
  Server := SocketCreate;
  SocketBind(Server, '127.0.0.1', TestPort);
  SocketListen(Server, 1);
  
  { Connect client }
  Client := SocketCreate;
  SocketConnect(Client, '127.0.0.1', TestPort, 5000);
  
  { Accept connection }
  Timeout := 0;
  Accepted := nil;
  while Timeout < 50 do
  begin
    if SocketHasPending(Server) then
    begin
      Res := SocketAccept(Server, Accepted);
      if Res = SOCK_OK then
        Break;
    end;
    Sleep(100);
    Inc(Timeout);
  end;
  
  if Accepted = nil then
  begin
    TestResult('Server accepts connection', False);
    SocketDestroy(Client);
    SocketDestroy(Server);
    SocketCleanup;
    Exit;
  end;
  
  try
    { Exchange handshakes }
    Res := EncodeHandshake(InfoHash, PeerId, @Buffer, SizeOf(Buffer));
    SocketSendAll(Client, @Buffer, HANDSHAKE_LEN);
    SocketReceiveAll(Accepted, @Buffer, HANDSHAKE_LEN);
    SocketSendAll(Accepted, @Buffer, HANDSHAKE_LEN);
    SocketReceiveAll(Client, @Buffer, HANDSHAKE_LEN);
    
    { Client sends request }
    BuildRequest(5, 32768, 16384, @Buffer, SizeOf(Buffer), Res);
    SocketSendAll(Client, @Buffer, Res);
    SocketReceiveAll(Accepted, @Buffer, 17);
    
    { Client immediately cancels the request }
    BuildCancel(5, 32768, 16384, @Buffer, SizeOf(Buffer), Res);
    TestResult('Client builds CANCEL message', Res = 17);
    
    Res := SocketSendAll(Client, @Buffer, 17);
    TestResult('Client sends CANCEL', Res = SOCK_OK);
    
    { Server receives cancel }
    Res := SocketReceiveAll(Accepted, @Buffer, 17);
    TestResult('Server receives CANCEL', Res = SOCK_OK);
    
    if Res = SOCK_OK then
    begin
      TestResult('Server decodes CANCEL', 
                 DecodeMessage(@Buffer, 17, DecodedMsg, BytesConsumed));
      if DecodeMessage(@Buffer, 17, DecodedMsg, BytesConsumed) then
      begin
        TestResult('Cancel piece index matches', DecodedMsg.CancelIndex = 5);
        TestResult('Cancel begin matches', DecodedMsg.CancelBegin = 32768);
        TestResult('Cancel length matches', DecodedMsg.CancelLength = 16384);
      end;
    end;
  finally
    SocketDestroy(Accepted);
    SocketDestroy(Client);
    SocketDestroy(Server);
    SocketCleanup;
  end;
end;

{ ============================================================================ }
{ Main Program                                                                 }
{ ============================================================================ }

begin
  WriteLn('==============================================');
  WriteLn('  PEER WIRE PROTOCOL INTEGRATION TESTS');
  WriteLn('==============================================');
  
  Randomize;
  
  TestCompleteHandshakeExchange;
  TestBitfieldExchange;
  TestChokeUnchokeStateMachine;
  TestRequestPieceFlow;
  TestHaveAnnouncements;
  TestCancelFlow;
  
  { Summary }
  WriteLn(#10'==============================================');
  WriteLn('  RESULTS: ', PassedTests, '/', TotalTests, ' tests passed');
  WriteLn('==============================================');
  
  if FailedTests > 0 then
  begin
    WriteLn('FAILED: ', FailedTests, ' tests failed');
    Halt(1);
  end
  else
    WriteLn('SUCCESS: All integration tests passed!');
end.
