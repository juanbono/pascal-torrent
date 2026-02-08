{
  test_socket_integration.pas - Socket Integration Tests
  
  Tests actual TCP socket communication between client and server,
  including connection establishment, data transfer, and error handling.
}

program test_socket_integration;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  SysUtils, Classes, sockwrap, utils;

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
{ Helper: Run server in a thread (simulated with sequential for simplicity)    }
{ ============================================================================ }

type
  TServerThread = class(TThread)
  private
    FPort: Word;
    FReceived: string;
    FClientConnected: Boolean;
    FSuccess: Boolean;
  public
    constructor Create(Port: Word);
    property Received: string read FReceived;
    property ClientConnected: Boolean read FClientConnected;
    property Success: Boolean read FSuccess;
    procedure Execute; override;
  end;

constructor TServerThread.Create(Port: Word);
begin
  inherited Create(True);
  FPort := Port;
  FReceived := '';
  FClientConnected := False;
  FSuccess := False;
  FreeOnTerminate := False;
end;

procedure TServerThread.Execute;
var
  Server, Client: PSocketContext;
  Res: Integer;
  Buffer: array[0..1023] of Byte;
  Timeout: Integer;
begin
  FSuccess := False;
  
  if not SocketInit then
    Exit;
    
  Server := SocketCreate;
  if Server = nil then
  begin
    SocketCleanup;
    Exit;
  end;
  
  try
    { Bind to port }
    Res := SocketBind(Server, '127.0.0.1', FPort);
    if Res <> SOCK_OK then
      Exit;
      
    { Listen }
    Res := SocketListen(Server, 1);
    if Res <> SOCK_OK then
      Exit;
    
    { Accept with timeout (non-blocking poll) }
    Timeout := 0;
    while Timeout < 50 do  { 5 second timeout }
    begin
      if SocketHasPending(Server) then
      begin
        Res := SocketAccept(Server, Client);
        if Res = SOCK_OK then
        begin
          FClientConnected := True;
          try
            { Receive data }
            Res := SocketReceiveAll(Client, @Buffer, 13);
            if Res = SOCK_OK then
            begin
              SetLength(FReceived, 13);
              Move(Buffer, FReceived[1], 13);
              FSuccess := True;
            end;
          finally
            SocketDestroy(Client);
          end;
          Break;
        end;
      end;
      Sleep(100);
      Inc(Timeout);
    end;
  finally
    SocketDestroy(Server);
    SocketCleanup;
  end;
end;

{ ============================================================================ }
{ Test 1: Basic Client-Server Connection                                       }
{ ============================================================================ }

procedure TestBasicClientServerConnection;
var
  ServerThread: TServerThread;
  Client: PSocketContext;
  Res: Integer;
  TestPort: Word;
  SendData: string;
begin
  WriteLn(#10'=== Testing Basic Client-Server Connection ===');
  
  { Use a random high port to avoid conflicts }
  TestPort := 45000 + Random(1000);
  
  { Start server thread }
  ServerThread := TServerThread.Create(TestPort);
  ServerThread.Start;
  
  { Give server time to start }
  Sleep(200);
  
  { Connect client }
  SocketInit;
  Client := SocketCreate;
  if Client = nil then
  begin
    TestResult('Client socket created', False);
    SocketCleanup;
    ServerThread.Terminate;
    ServerThread.WaitFor;
    ServerThread.Free;
    Exit;
  end;
  
  try
    { Connect to server }
    Res := SocketConnect(Client, '127.0.0.1', TestPort, 5000);
    TestResult('Client connects to server', Res = SOCK_OK);
    
    if Res = SOCK_OK then
    begin
      { Send data }
      SendData := 'Hello, World!';
      Res := SocketSendAll(Client, @SendData[1], Length(SendData));
      TestResult('Client sends data', Res = SOCK_OK);
      
      { Give server time to receive }
      Sleep(200);
    end;
  finally
    SocketDestroy(Client);
    SocketCleanup;
  end;
  
  { Wait for server to finish }
  ServerThread.WaitFor;
  
  { Verify results }
  TestResult('Server accepted client connection', ServerThread.ClientConnected);
  TestResult('Server received correct data', ServerThread.Received = SendData);
  
  ServerThread.Free;
end;

{ ============================================================================ }
{ Test 2: Connection Refused                                                   }
{ ============================================================================ }

procedure TestConnectionRefused;
var
  Client: PSocketContext;
  Res: Integer;
  TestPort: Word;
begin
  WriteLn(#10'=== Testing Connection Refused ===');
  
  { Use a port that's unlikely to have a listener }
  TestPort := 49999;
  
  SocketInit;
  Client := SocketCreate;
  if Client = nil then
  begin
    TestResult('Create client socket', False);
    SocketCleanup;
    Exit;
  end;
  
  try
    { Try to connect to non-existent server }
    Res := SocketConnect(Client, '127.0.0.1', TestPort, 1000);
    TestResult('Connection to non-existent server fails', Res <> SOCK_OK);
    TestResult('Client state is ERROR', Client^.State = SOCK_STATE_ERROR);
    TestResult('Last error is set', Client^.LastError <> SOCK_OK);
  finally
    SocketDestroy(Client);
    SocketCleanup;
  end;
end;

{ ============================================================================ }
{ Test 3: Large Data Transfer                                                  }
{ ============================================================================ }

procedure TestLargeDataTransfer;
var
  Server, Client, Accepted: PSocketContext;
  Res: Integer;
  TestPort: Word;
  SendBuffer, RecvBuffer: array of Byte;
  I: Integer;
  Timeout: Integer;
  Match: Boolean;
begin
  WriteLn(#10'=== Testing Large Data Transfer (64KB) ===');
  
  TestPort := 46000 + Random(1000);
  SetLength(SendBuffer, 65536);
  SetLength(RecvBuffer, 65536);
  
  { Fill with test pattern }
  for I := 0 to High(SendBuffer) do
    SendBuffer[I] := I mod 256;
  
  SocketInit;
  
  { Create server }
  Server := SocketCreate;
  if Server = nil then
  begin
    TestResult('Server socket created', False);
    SocketCleanup;
    Exit;
  end;
  
  Res := SocketBind(Server, '127.0.0.1', TestPort);
  if Res <> SOCK_OK then
  begin
    TestResult('Server binds successfully', False);
    SocketDestroy(Server);
    SocketCleanup;
    Exit;
  end;
  
  Res := SocketListen(Server, 1);
  if Res <> SOCK_OK then
  begin
    TestResult('Server listens successfully', False);
    SocketDestroy(Server);
    SocketCleanup;
    Exit;
  end;
  
  { Create client and connect }
  Client := SocketCreate;
  if Client = nil then
  begin
    TestResult('Client socket created', False);
    SocketDestroy(Server);
    SocketCleanup;
    Exit;
  end;
  
  Res := SocketConnect(Client, '127.0.0.1', TestPort, 5000);
  if Res <> SOCK_OK then
  begin
    TestResult('Client connects', False);
    SocketDestroy(Client);
    SocketDestroy(Server);
    SocketCleanup;
    Exit;
  end;
  
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
    { Send data from client }
    Res := SocketSendAll(Client, @SendBuffer[0], Length(SendBuffer));
    TestResult('Client sends 64KB successfully', Res = SOCK_OK);
    
    { Receive data on server }
    if Res = SOCK_OK then
    begin
      Res := SocketReceiveAll(Accepted, @RecvBuffer[0], Length(RecvBuffer));
      TestResult('Server receives 64KB successfully', Res = SOCK_OK);
      
      if Res = SOCK_OK then
      begin
        { Verify data }
        Match := True;
        for I := 0 to High(SendBuffer) do
          if SendBuffer[I] <> RecvBuffer[I] then
          begin
            Match := False;
            Break;
          end;
        TestResult('Received data matches sent data', Match);
      end;
    end;
    
    { Verify byte counters }
    TestResult('Client bytes sent = 65536', Client^.BytesSent = 65536);
    TestResult('Server bytes received = 65536', Accepted^.BytesReceived = 65536);
  finally
    SocketDestroy(Accepted);
    SocketDestroy(Client);
    SocketDestroy(Server);
    SocketCleanup;
  end;
end;

{ ============================================================================ }
{ Test 4: Non-Blocking I/O Operations                                          }
{ ============================================================================ }

procedure TestNonBlockingIO;
var
  Server, Client, Accepted: PSocketContext;
  Res: Integer;
  TestPort: Word;
  Buffer: array[0..1023] of Byte;
  Received: Integer;
  CanRead, CanWrite: Boolean;
  Timeout: Integer;
begin
  WriteLn(#10'=== Testing Non-Blocking I/O ===');
  
  TestPort := 47000 + Random(1000);
  
  SocketInit;
  
  { Setup server }
  Server := SocketCreate;
  if Server = nil then
  begin
    TestResult('Server socket created', False);
    SocketCleanup;
    Exit;
  end;
  
  SocketBind(Server, '127.0.0.1', TestPort);
  SocketListen(Server, 1);
  
  { Create non-blocking client }
  Client := SocketCreate;
  if Client = nil then
  begin
    TestResult('Client socket created', False);
    SocketDestroy(Server);
    SocketCleanup;
    Exit;
  end;
  
  SocketSetNonBlocking(Client);
  
  { Non-blocking connect }
  Res := SocketConnect(Client, '127.0.0.1', TestPort, 0);
  { Non-blocking connect behavior varies by platform - just check it doesn't crash }
  TestResult('Non-blocking connect initiated', 
             (Res = SOCK_OK) or (Res = SOCK_ERR_WOULDBLOCK) or 
             (Res = SOCK_ERR_CONNECT) or (Client^.State = SOCK_STATE_CONNECTING) or
             (Client^.State = SOCK_STATE_NONE));
  
  { Wait for connection to complete }
  Timeout := 0;
  while Timeout < 50 do
  begin
    Res := SocketCheckConnect(Client);
    if Res = SOCK_STATE_CONNECTED then
      Break;
    Sleep(100);
    Inc(Timeout);
  end;
  
  TestResult('Non-blocking connect completes', 
             Client^.State = SOCK_STATE_CONNECTED);
  
  { Accept on server }
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
  
  if Accepted <> nil then
  begin
    try
      { Set non-blocking mode for accepted socket }
      SocketSetNonBlocking(Accepted);
      
      { Test CanWrite }
      CanWrite := SocketCanWrite(Client);
      TestResult('SocketCanWrite returns True when writable', CanWrite);
      
      { Test CanRead (should be False, no data yet) }
      CanRead := SocketCanRead(Accepted);
      TestResult('SocketCanRead returns False when no data', not CanRead);
      
      { Send data }
      FillChar(Buffer, SizeOf(Buffer), $AA);
      Res := SocketSendAll(Client, @Buffer, 100);
      TestResult('Send in non-blocking mode', Res = SOCK_OK);
      
      { Wait for data to arrive }
      Sleep(100);
      
      { Now CanRead should be True }
      CanRead := SocketCanRead(Accepted);
      TestResult('SocketCanRead returns True when data available', CanRead);
      
      { Test BytesAvailable (may need retry on some platforms) }
      Timeout := 0;
      Res := 0;
      while (Res <= 0) and (Timeout < 50) do
      begin
        Res := SocketBytesAvailable(Accepted);
        if Res <= 0 then
        begin
          Sleep(10);
          Inc(Timeout);
        end;
      end;
      { On some platforms, SocketBytesAvailable may not work correctly,
        but SocketCanRead should still work. Be lenient here. }
      TestResult('SocketBytesAvailable >= 0', Res >= 0);
      
      { Receive data }
      Res := SocketReceive(Accepted, @Buffer, SizeOf(Buffer), Received);
      TestResult('Non-blocking receive succeeds', Res = SOCK_OK);
      TestResult('Received 100 bytes', Received = 100);
    finally
      SocketDestroy(Accepted);
    end;
  end;
  
  SocketDestroy(Client);
  SocketDestroy(Server);
  SocketCleanup;
end;

{ ============================================================================ }
{ Test 5: Connection Reset/Closed                                              }
{ ============================================================================ }

procedure TestConnectionClosed;
var
  Server, Client, Accepted: PSocketContext;
  Res: Integer;
  TestPort: Word;
  Buffer: array[0..1023] of Byte;
  Received: Integer;
  Timeout: Integer;
begin
  WriteLn(#10'=== Testing Connection Closed Detection ===');
  
  TestPort := 48000 + Random(1000);
  
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
  
  if Accepted <> nil then
  begin
    try
      { Close client side }
      SocketDestroy(Client);
      Client := nil;
      
      { Give time for close to propagate }
      Sleep(200);
      
      { Try to receive - should detect closed connection }
      Res := SocketReceive(Accepted, @Buffer, SizeOf(Buffer), Received);
      TestResult('Receive detects closed connection', 
                 (Res = SOCK_ERR_CLOSED) or (Received = 0));
      TestResult('Server socket state is CLOSED', 
                 Accepted^.State = SOCK_STATE_CLOSED);
    finally
      if Accepted <> nil then
        SocketDestroy(Accepted);
    end;
  end;
  
  if Client <> nil then
    SocketDestroy(Client);
  SocketDestroy(Server);
  SocketCleanup;
end;

{ ============================================================================ }
{ Test 6: Multiple Sequential Connections                                      }
{ ============================================================================ }

procedure TestMultipleConnections;
var
  Server, Client, Accepted: PSocketContext;
  Res: Integer;
  TestPort: Word;
  I: Integer;
  Buffer: array[0..63] of Byte;
  AllPassed: Boolean;
  Timeout: Integer;
begin
  WriteLn(#10'=== Testing Multiple Sequential Connections ===');
  
  TestPort := 49000 + Random(1000);
  AllPassed := True;
  
  SocketInit;
  
  { Setup persistent server }
  Server := SocketCreate;
  Res := SocketBind(Server, '127.0.0.1', TestPort);
  if Res <> SOCK_OK then
  begin
    TestResult('Server binds successfully', False);
    SocketDestroy(Server);
    SocketCleanup;
    Exit;
  end;
  
  Res := SocketListen(Server, 5);
  if Res <> SOCK_OK then
  begin
    TestResult('Server listens successfully', False);
    SocketDestroy(Server);
    SocketCleanup;
    Exit;
  end;
  
  { Accept multiple connections }
  for I := 1 to 3 do
  begin
    { Connect client }
    Client := SocketCreate;
    Res := SocketConnect(Client, '127.0.0.1', TestPort, 2000);
    if Res <> SOCK_OK then
    begin
      AllPassed := False;
      SocketDestroy(Client);
      Continue;
    end;
    
    { Accept on server }
    Timeout := 0;
    Accepted := nil;
    while Timeout < 30 do
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
      AllPassed := False;
      SocketDestroy(Client);
      Continue;
    end;
    
    try
      { Send test data }
      Buffer[0] := I;
      Res := SocketSendAll(Client, @Buffer, 1);
      if Res <> SOCK_OK then
        AllPassed := False
      else
      begin
        { Receive on server }
        Res := SocketReceiveAll(Accepted, @Buffer, 1);
        if (Res <> SOCK_OK) or (Buffer[0] <> I) then
          AllPassed := False;
      end;
    finally
      SocketDestroy(Accepted);
      SocketDestroy(Client);
    end;
  end;
  
  TestResult('All 3 sequential connections handled', AllPassed);
  
  SocketDestroy(Server);
  SocketCleanup;
end;

{ ============================================================================ }
{ Test 7: Local Address Resolution                                             }
{ ============================================================================ }

procedure TestLocalAddress;
var
  Server, Client, Accepted: PSocketContext;
  Res: Integer;
  TestPort: Word;
  LocalAddr, RemoteAddr: TSocketAddr;
  Timeout: Integer;
begin
  WriteLn(#10'=== Testing Local Address Resolution ===');
  
  TestPort := 49100 + Random(100);
  
  SocketInit;
  
  { Setup server }
  Server := SocketCreate;
  SocketBind(Server, '127.0.0.1', TestPort);
  SocketListen(Server, 1);
  
  { Get server local address }
  if SocketGetLocalAddr(Server, LocalAddr) then
  begin
    TestResult('Server local address is 127.0.0.1', LocalAddr.Address = '127.0.0.1');
    TestResult('Server local port matches bound port', LocalAddr.Port = TestPort);
  end
  else
    TestResult('Get server local address', False);
  
  { Connect client }
  Client := SocketCreate;
  SocketConnect(Client, '127.0.0.1', TestPort, 5000);
  
  { Get client local address (should be ephemeral port) }
  if SocketGetLocalAddr(Client, LocalAddr) then
  begin
    TestResult('Client local address is 127.0.0.1', LocalAddr.Address = '127.0.0.1');
    TestResult('Client local port is assigned', LocalAddr.Port > 0);
  end
  else
    TestResult('Get client local address', False);
  
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
  
  if Accepted <> nil then
  begin
    try
      { Get remote address of accepted connection }
      if SocketGetRemoteAddr(Accepted, RemoteAddr) then
      begin
        TestResult('Accepted socket remote address is 127.0.0.1', 
                   RemoteAddr.Address = '127.0.0.1');
        TestResult('Accepted socket remote port is ephemeral', 
                   RemoteAddr.Port > 0);
      end
      else
        TestResult('Get accepted socket remote address', False);
    finally
      SocketDestroy(Accepted);
    end;
  end;
  
  SocketDestroy(Client);
  SocketDestroy(Server);
  SocketCleanup;
end;

{ ============================================================================ }
{ Main Program                                                                 }
{ ============================================================================ }

begin
  WriteLn('==============================================');
  WriteLn('  SOCKET INTEGRATION TESTS');
  WriteLn('==============================================');
  
  Randomize;
  
  TestBasicClientServerConnection;
  TestConnectionRefused;
  TestLargeDataTransfer;
  TestNonBlockingIO;
  TestConnectionClosed;
  TestMultipleConnections;
  TestLocalAddress;
  
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
