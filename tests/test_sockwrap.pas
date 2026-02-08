{
  test_sockwrap.pas - Unit tests for Socket Wrapper
  
  Tests socket operations including lifecycle, connections, and error handling.
  Note: Full connection tests require network access.
}

program test_sockwrap;

{$mode objfpc}{$H+}

uses
  SysUtils, sockwrap, testframework;

{ ============================================================================ }
{ Socket Lifecycle Tests                                                       }
{ ============================================================================ }

procedure TestSocketLifecycle;
var
  Context: PSocketContext;
  Success: Boolean;
begin
  BeginSuite('Testing Socket Lifecycle');
  
  { Test 1: Initialize socket subsystem }
  Success := SocketInit;
  TestResult('SocketInit succeeds', Success);
  
  { Test 2: Create socket }
  Context := SocketCreate;
  TestResult('SocketCreate returns valid context', 
             Context <> nil);
  
  if Context <> nil then
  begin
    { Test 3: Initial state }
    TestResult('Initial state is NONE', 
               Context^.State = SOCK_STATE_NONE);
    TestResult('Initial error code is OK', 
               Context^.LastError = SOCK_OK);
    TestResult('Handle is valid', 
               Context^.Handle >= 0);
    TestResult('Bytes sent is 0', 
               Context^.BytesSent = 0);
    TestResult('Bytes received is 0', 
               Context^.BytesReceived = 0);
    
    { Test 4: Close socket }
    SocketClose(Context);
    TestResult('State after close is CLOSED', 
               Context^.State = SOCK_STATE_CLOSED);
    
    { Test 5: Destroy context }
    SocketDestroy(Context);
    TestResult('SocketDestroy completes', True);
  end;
  
  SocketCleanup;
  
  EndSuite;
end;

procedure TestSocketErrorHandling;
var
  ErrorStr: string;
begin
  BeginSuite('Testing Socket Error Handling');
  
  { Test error strings }
  ErrorStr := SocketErrorString(SOCK_OK);
  TestResult('SOCK_OK string is "No error"', 
             ErrorStr = 'No error');
  
  ErrorStr := SocketErrorString(SOCK_ERR_CREATE);
  TestResult('SOCK_ERR_CREATE string is correct', 
             Pos('create', LowerCase(ErrorStr)) > 0);
  
  ErrorStr := SocketErrorString(SOCK_ERR_CONNECT);
  TestResult('SOCK_ERR_CONNECT string is correct', 
             Pos('connection', LowerCase(ErrorStr)) > 0);
  
  ErrorStr := SocketErrorString(SOCK_ERR_WOULDBLOCK);
  TestResult('SOCK_ERR_WOULDBLOCK string is correct', 
             Pos('block', LowerCase(ErrorStr)) > 0);
  
  ErrorStr := SocketErrorString(999);
  TestResult('Unknown error string mentions "Unknown"', 
             Pos('unknown', LowerCase(ErrorStr)) > 0);
  
  { Test nil context handling }
  SocketClose(nil);
  TestResult('SocketClose(nil) does not crash', True);
  
  SocketDestroy(nil);
  TestResult('SocketDestroy(nil) does not crash', True);
  
  EndSuite;
end;

{ ============================================================================ }
{ Address Resolution Tests                                                     }
{ ============================================================================ }

procedure TestAddressResolution;
var
  IP: string;
begin
  BeginSuite('Testing Address Resolution');
  
  SocketInit;
  
  { Test 1: Empty hostname }
  IP := SocketResolveHost('');
  TestResult('Empty hostname returns empty', 
             IP = '');
  
  { Test 2: Localhost }
  IP := SocketResolveHost('localhost');
  TestResult('localhost resolves to IP', 
             (IP = '127.0.0.1') or (IP <> ''));
  
  { Test 3: IP address passthrough }
  IP := SocketResolveHost('192.168.1.1');
  TestResult('IP address returns unchanged', 
             IP = '192.168.1.1');
  
  { Test 4: Public DNS (if network available) }
  IP := SocketResolveHost('127.0.0.1');
  TestResult('127.0.0.1 returns unchanged', 
             IP = '127.0.0.1');
  
  SocketCleanup;
  
  EndSuite;
end;

{ ============================================================================ }
{ Server Socket Tests                                                          }
{ ============================================================================ }

procedure TestServerSocket;
var
  Server: PSocketContext;
  Res: Integer;
  Addr: TSocketAddr;
begin
  BeginSuite('Testing Server Socket Operations');
  
  SocketInit;
  
  { Test 1: Create server socket }
  Server := SocketCreate;
  TestResult('Server socket created', 
             Server <> nil);
  
  if Server = nil then
  begin
    SocketCleanup;
    EndSuite;
    Exit;
  end;
  
  { Test 2: Bind to port }
  Res := SocketBind(Server, '127.0.0.1', 0);
  TestResult('Bind to 127.0.0.1:0 succeeds', 
             Res = SOCK_OK);
  
  if Res = SOCK_OK then
  begin
    { Test 3: Get local address }
    if SocketGetLocalAddr(Server, Addr) then
    begin
      TestResult('Local address is 127.0.0.1', 
                 Addr.Address = '127.0.0.1');
      TestResult('Local port is assigned', 
                 Addr.Port > 0);
    end
    else
      TestResult('GetLocalAddr returns True', False);
    
    { Test 4: Start listening }
    Res := SocketListen(Server, 5);
    TestResult('Listen succeeds', 
               Res = SOCK_OK);
    TestResult('State is LISTENING', 
               Server^.State = SOCK_STATE_LISTENING);
  end;
  
  SocketDestroy(Server);
  SocketCleanup;
  
  EndSuite;
end;

procedure TestServerBindErrors;
var
  Server1, Server2: PSocketContext;
  Res: Integer;
  Port: Word;
  Addr: TSocketAddr;
begin
  BeginSuite('Testing Server Bind Errors');
  
  SocketInit;
  
  { Test: Bind two sockets to same port }
  Server1 := SocketCreate;
  Server2 := SocketCreate;
  
  if (Server1 <> nil) and (Server2 <> nil) then
  begin
    { Bind first socket to specific port }
    Res := SocketBind(Server1, '127.0.0.1', 0);
    if Res = SOCK_OK then
    begin
      { Get the assigned port }
      if SocketGetLocalAddr(Server1, Addr) then
      begin
        Port := Addr.Port;
        
        { Try to bind second socket to same port }
        Res := SocketBind(Server2, '127.0.0.1', Port);
        TestResult('Second bind to same port fails', 
                   Res <> SOCK_OK);
        TestResult('Second socket has error state', 
                   Server2^.LastError <> SOCK_OK);
      end;
    end;
  end;
  
  SocketDestroy(Server1);
  SocketDestroy(Server2);
  SocketCleanup;
  
  EndSuite;
end;

{ ============================================================================ }
{ Blocking Mode Tests                                                          }
{ ============================================================================ }

procedure TestBlockingMode;
var
  Context: PSocketContext;
  Success: Boolean;
begin
  BeginSuite('Testing Blocking Mode Operations');
  
  SocketInit;
  
  Context := SocketCreate;
  if Context <> nil then
  begin
    { Test 1: Set non-blocking }
    Success := SocketSetNonBlocking(Context);
    TestResult('SetNonBlocking succeeds', Success);
    
    { Test 2: Set blocking }
    Success := SocketSetBlocking(Context);
    TestResult('SetBlocking succeeds', Success);
    
    SocketDestroy(Context);
  end
  else
  begin
    TestResult('Create socket for blocking tests', False);
  end;
  
  SocketCleanup;
  
  EndSuite;
end;

{ ============================================================================ }
{ Socket State Tests                                                           }
{ ============================================================================ }

procedure TestSocketStates;
var
  Context: PSocketContext;
begin
  BeginSuite('Testing Socket States');
  
  SocketInit;
  
  Context := SocketCreate;
  if Context = nil then
  begin
    TestResult('Create socket for state tests', False);
    SocketCleanup;
    EndSuite;
    Exit;
  end;
  
  { Initial state }
  TestResult('Initial state is NONE (0)', 
             Context^.State = SOCK_STATE_NONE);
  
  { State constants }
  TestResult('SOCK_STATE_NONE = 0', SOCK_STATE_NONE = 0);
  TestResult('SOCK_STATE_CONNECTING = 1', SOCK_STATE_CONNECTING = 1);
  TestResult('SOCK_STATE_CONNECTED = 2', SOCK_STATE_CONNECTED = 2);
  TestResult('SOCK_STATE_LISTENING = 3', SOCK_STATE_LISTENING = 3);
  TestResult('SOCK_STATE_ERROR = 4', SOCK_STATE_ERROR = 4);
  TestResult('SOCK_STATE_CLOSED = 5', SOCK_STATE_CLOSED = 5);
  
  SocketDestroy(Context);
  SocketCleanup;
  
  EndSuite;
end;

{ ============================================================================ }
{ Error Code Tests                                                             }
{ ============================================================================ }

procedure TestErrorCodes;
begin
  BeginSuite('Testing Error Code Constants');
  
  TestResult('SOCK_OK = 0', SOCK_OK = 0);
  TestResult('SOCK_ERR_CREATE < 0', SOCK_ERR_CREATE < 0);
  TestResult('SOCK_ERR_CONNECT < 0', SOCK_ERR_CONNECT < 0);
  TestResult('SOCK_ERR_BIND < 0', SOCK_ERR_BIND < 0);
  TestResult('SOCK_ERR_LISTEN < 0', SOCK_ERR_LISTEN < 0);
  TestResult('SOCK_ERR_ACCEPT < 0', SOCK_ERR_ACCEPT < 0);
  TestResult('SOCK_ERR_SEND < 0', SOCK_ERR_SEND < 0);
  TestResult('SOCK_ERR_RECV < 0', SOCK_ERR_RECV < 0);
  TestResult('SOCK_ERR_CLOSED < 0', SOCK_ERR_CLOSED < 0);
  TestResult('SOCK_ERR_WOULDBLOCK < 0', SOCK_ERR_WOULDBLOCK < 0);
  TestResult('SOCK_ERR_INVALID < 0', SOCK_ERR_INVALID < 0);
  TestResult('SOCK_ERR_RESOLVE < 0', SOCK_ERR_RESOLVE < 0);
  
  EndSuite;
end;

{ ============================================================================ }
{ Main Program                                                                 }
{ ============================================================================ }

begin
  { Lifecycle tests }
  TestSocketLifecycle;
  TestSocketErrorHandling;
  
  { Address tests }
  TestAddressResolution;
  
  { Server tests }
  TestServerSocket;
  TestServerBindErrors;
  
  { Mode tests }
  TestBlockingMode;
  
  { State and error tests }
  TestSocketStates;
  TestErrorCodes;
  
  ExitWithResult;
end.
