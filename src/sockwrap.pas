{
  sockets.pas - TCP Socket Wrapper
  
  Provides a clean, procedural interface to TCP sockets with:
  - Connection management (client/server)
  - Non-blocking I/O operations
  - Explicit buffer management
  - Proper error handling
  
  This unit has minimal dependencies and is designed for testability.
}

unit sockwrap;

{$mode objfpc}{$H+}

{$IF DEFINED(UNIX) AND NOT DEFINED(WINDOWS)}
  {$DEFINE USE_SOCKETS}
{$ENDIF}
{$IFDEF WINDOWS}
  {$DEFINE USE_WINSOCK}
{$ENDIF}

interface

uses
  SysUtils, Sockets, BaseUnix, Unix, netdb;

{ ============================================================================ }
{ Constants                                                                    }
{ ============================================================================ }

const
  { Socket states }
  SOCK_STATE_NONE       = 0;
  SOCK_STATE_CONNECTING = 1;
  SOCK_STATE_CONNECTED  = 2;
  SOCK_STATE_LISTENING  = 3;
  SOCK_STATE_ERROR      = 4;
  SOCK_STATE_CLOSED     = 5;
  
  { Error codes }
  SOCK_OK               = 0;
  SOCK_ERR_CREATE       = -1;
  SOCK_ERR_CONNECT      = -2;
  SOCK_ERR_BIND         = -3;
  SOCK_ERR_LISTEN       = -4;
  SOCK_ERR_ACCEPT       = -5;
  SOCK_ERR_SEND         = -6;
  SOCK_ERR_RECV         = -7;
  SOCK_ERR_CLOSED       = -8;
  SOCK_ERR_WOULDBLOCK   = -9;
  SOCK_ERR_INVALID      = -10;
  SOCK_ERR_RESOLVE      = -11;
  
  { Default buffer sizes }
  DEFAULT_RECV_BUF_SIZE = 16384;  { 16 KB receive buffer }
  DEFAULT_SEND_BUF_SIZE = 16384;  { 16 KB send buffer }
  
  { Timeout defaults (milliseconds) }
  DEFAULT_CONNECT_TIMEOUT = 10000;  { 10 seconds }
  DEFAULT_RECV_TIMEOUT    = 30000;  { 30 seconds }
  
  { ioctl constants }
  FIONREAD = $541B;

{ ============================================================================ }
{ Data Types                                                                   }
{ ============================================================================ }

type
  { Socket handle type }
  {$IFDEF WINDOWS}
  TSocketHandle = THandle;
  {$ELSE}
  TSocketHandle = LongInt;
  {$ENDIF}
  
  { Socket context - minimal state for a socket }
  PSocketContext = ^TSocketContext;
  TSocketContext = record
    Handle: TSocketHandle;       { OS socket handle }
    State: Integer;              { SOCK_STATE_* }
    LastError: Integer;          { Last error code }
    
    { Address info }
    RemoteAddr: string;          { Connected peer address }
    RemotePort: Word;            { Connected peer port }
    LocalPort: Word;             { Local port (for listeners) }
    
    { Statistics }
    BytesSent: QWord;
    BytesReceived: QWord;
    
    { User data pointer (for callbacks/state) }
    UserData: Pointer;
  end;
  
  { Socket address info }
  PSocketAddr = ^TSocketAddr;
  TSocketAddr = record
    Address: string;
    Port: Word;
  end;

{ ============================================================================ }
{ Initialization and Cleanup                                                   }
{ ============================================================================ }

{ Initialize socket subsystem (call once at startup)
  Returns: True on success, False on failure }
function SocketInit: Boolean;

{ Cleanup socket subsystem (call at shutdown) }
procedure SocketCleanup;

{ ============================================================================ }
{ Socket Creation and Management                                               }
{ ============================================================================ }

{ Create a new socket context
  Returns: Pointer to new context, or nil on failure }
function SocketCreate: PSocketContext;

{ Destroy a socket context and close the underlying socket
  Always call this to cleanup, even if socket was never connected }
procedure SocketDestroy(Context: PSocketContext);

{ Close socket but keep context (for reuse) }
procedure SocketClose(Context: PSocketContext);

{ Get last error as human-readable string }
function SocketErrorString(ErrorCode: Integer): string;

{ ============================================================================ }
{ Client Operations (Outgoing Connections)                                     }
{ ============================================================================ }

{ Connect to a remote host (blocking with timeout)
  
  Context: Socket context from SocketCreate
  Host:    IP address (e.g., "192.168.1.1") or hostname
  Port:    TCP port number
  Timeout: Milliseconds to wait for connection (0 = default)
  
  Returns: SOCK_OK on success, error code on failure }
function SocketConnect(Context: PSocketContext; const Host: string;
                       Port: Word; Timeout: Integer): Integer;

{ Check if a non-blocking connect has completed
  Returns: SOCK_STATE_CONNECTED if connected, SOCK_STATE_CONNECTING if still
           in progress, or SOCK_STATE_ERROR on failure }
function SocketCheckConnect(Context: PSocketContext): Integer;

{ Set socket to non-blocking mode
  Returns: True on success, False on failure }
function SocketSetNonBlocking(Context: PSocketContext): Boolean;

{ Set socket to blocking mode (default)
  Returns: True on success, False on failure }
function SocketSetBlocking(Context: PSocketContext): Boolean;

{ ============================================================================ }
{ Server Operations (Incoming Connections)                                     }
{ ============================================================================ }

{ Bind socket to local address and port
  
  Context: Socket context from SocketCreate
  Address: Local address to bind (empty string = all interfaces)
  Port:    TCP port to listen on
  
  Returns: SOCK_OK on success, error code on failure }
function SocketBind(Context: PSocketContext; const Address: string;
                    Port: Word): Integer;

{ Start listening for incoming connections
  
  Context:   Socket from successful SocketBind
  Backlog:   Maximum pending connections (0 = default)
  
  Returns: SOCK_OK on success, error code on failure }
function SocketListen(Context: PSocketContext; Backlog: Integer): Integer;

{ Accept an incoming connection (blocking)
  
  Server:    Listening socket context
  Client:    Output parameter for new client socket
  
  Returns: SOCK_OK on success, error code on failure
  Note: Client context must be freed with SocketDestroy when done }
function SocketAccept(Server: PSocketContext;
                      out Client: PSocketContext): Integer;

{ Check if a listening socket has pending connections (non-blocking)
  Returns: True if connection available, False otherwise }
function SocketHasPending(Server: PSocketContext): Boolean;

{ ============================================================================ }
{ Data Transfer                                                                }
{ ============================================================================ }

{ Send data over socket
  
  Context: Socket context
  Data:    Pointer to data buffer
  Len:     Number of bytes to send
  Sent:    Output - actual bytes sent (may be less than Len for non-blocking)
  
  Returns: SOCK_OK on success, SOCK_ERR_WOULDBLOCK if would block,
           or other error code on failure }
function SocketSend(Context: PSocketContext; Data: Pointer;
                    Len: Integer; out Sent: Integer): Integer;

{ Receive data from socket
  
  Context: Socket context
  Buffer:  Buffer to receive into
  BufLen:  Size of buffer
  Received: Output - actual bytes received (0 = no data available)
  
  Returns: SOCK_OK on success (even if Received = 0),
           SOCK_ERR_WOULDBLOCK if would block,
           SOCK_ERR_CLOSED if peer closed connection,
           or other error code on failure }
function SocketReceive(Context: PSocketContext; Buffer: Pointer;
                       BufLen: Integer; out Received: Integer): Integer;

{ Send all data (blocking until complete or error)
  Returns: SOCK_OK on success, error code on failure }
function SocketSendAll(Context: PSocketContext; Data: Pointer;
                       Len: Integer): Integer;

{ Receive exact amount of data (blocking until complete or error)
  Returns: SOCK_OK on success, error code on failure }
function SocketReceiveAll(Context: PSocketContext; Buffer: Pointer;
                          Len: Integer): Integer;

{ ============================================================================ }
{ Utility Functions                                                            }
{ ============================================================================ }

{ Check if socket is ready for reading (non-blocking)
  Returns: True if data available, False otherwise }
function SocketCanRead(Context: PSocketContext): Boolean;

{ Check if socket is ready for writing (non-blocking)
  Returns: True if can write, False otherwise }
function SocketCanWrite(Context: PSocketContext): Boolean;

{ Get number of bytes available to read without blocking
  Returns: Number of bytes available, 0 if none or error }
function SocketBytesAvailable(Context: PSocketContext): Integer;

{ Resolve hostname to IP address
  Returns: IP address string, or empty string on failure }
function SocketResolveHost(const Hostname: string): string;

{ Get local socket address and port
  Returns: True on success, False on failure }
function SocketGetLocalAddr(Context: PSocketContext;
                            out Addr: TSocketAddr): Boolean;

{ Get remote peer address and port
  Returns: True on success, False on failure }
function SocketGetRemoteAddr(Context: PSocketContext;
                             out Addr: TSocketAddr): Boolean;

{ ============================================================================ }
{ Poll/Select Wrapper                                                          }
{ ============================================================================ }

{ Wait for activity on multiple sockets (single socket version)
  
  Context:    Socket to wait on
  WantRead:   Wait until socket has data to read
  WantWrite:  Wait until socket can accept writes
  Timeout:    Milliseconds to wait (0 = return immediately, -1 = infinite)
  
  Returns: True if condition met, False on timeout or error }
function SocketWait(Context: PSocketContext; WantRead, WantWrite: Boolean;
                    Timeout: Integer): Boolean;

implementation

{ ============================================================================ }
{ Platform-Specific Helpers                                                    }
{ ============================================================================ }

{$IFDEF WINDOWS}
var
  WinsockInitialized: Boolean = False;
  WSAData: TWSAData;
{$ENDIF}

function GetLastSocketError: Integer;
begin
  {$IFDEF WINDOWS}
  Result := WSAGetLastError;
  {$ELSE}
  Result := fpgeterrno;
  {$ENDIF}
end;

procedure SetSocketError(Ctx: PSocketContext; ErrCode: Integer);
begin
  if Ctx <> nil then
  begin
    Ctx^.LastError := ErrCode;
    if ErrCode <> SOCK_OK then
      Ctx^.State := SOCK_STATE_ERROR;
  end;
end;

{ ============================================================================ }
{ Thread-Safe IP Address Helpers (replacing deprecated inet_addr/inet_ntoa)    }
{ ============================================================================ }

{ Convert IP address string to network byte order (thread-safe)
  Returns: True on success, False on failure }
function IPStringToAddr(const IPStr: string; out Addr: Cardinal): Boolean;
var
  Parts: array[0..3] of Integer;
  I, J, PartIdx: Integer;
  PartStr: string;
  Code: Integer;
begin
  Result := False;
  Addr := 0;
  
  if IPStr = '' then Exit;
  
  { Parse IP string manually (e.g., "192.168.1.1") }
  PartIdx := 0;
  PartStr := '';
  
  for I := 1 to Length(IPStr) do
  begin
    if IPStr[I] = '.' then
    begin
      if PartIdx > 3 then Exit;  { Too many parts }
      Val(PartStr, Parts[PartIdx], Code);
      if Code <> 0 then Exit;  { Invalid number }
      if (Parts[PartIdx] < 0) or (Parts[PartIdx] > 255) then Exit;
      PartStr := '';
      Inc(PartIdx);
    end
    else if IPStr[I] in ['0'..'9'] then
      PartStr := PartStr + IPStr[I]
    else
      Exit;  { Invalid character }
  end;
  
  { Process last part }
  if PartIdx <> 3 then Exit;  { Wrong number of parts }
  Val(PartStr, Parts[3], Code);
  if Code <> 0 then Exit;
  if (Parts[3] < 0) or (Parts[3] > 255) then Exit;
  
  { Combine into network byte order (big-endian) }
  Addr := (Cardinal(Parts[0]) shl 24) or
          (Cardinal(Parts[1]) shl 16) or
          (Cardinal(Parts[2]) shl 8) or
          Cardinal(Parts[3]);
  Result := True;
end;

{ Convert network byte order to IP address string (thread-safe)
  Returns: IP string like "192.168.1.1" }
function AddrToIPString(Addr: Cardinal): string;
begin
  Result := IntToStr((Addr shr 24) and $FF) + '.' +
            IntToStr((Addr shr 16) and $FF) + '.' +
            IntToStr((Addr shr 8) and $FF) + '.' +
            IntToStr(Addr and $FF);
end;

{ Check if string is a valid IPv4 address }
function IsValidIP(const IPStr: string): Boolean;
var
  Dummy: Cardinal;
begin
  Result := IPStringToAddr(IPStr, Dummy);
end;

{ ============================================================================ }
{ Initialization and Cleanup                                                   }
{ ============================================================================ }

function SocketInit: Boolean;
begin
  Result := True;
  
  {$IFDEF WINDOWS}
  if not WinsockInitialized then
  begin
    Result := WSAStartup($0202, WSAData) = 0;
    if Result then
      WinsockInitialized := True;
  end;
  {$ENDIF}
end;

procedure SocketCleanup;
begin
  {$IFDEF WINDOWS}
  if WinsockInitialized then
  begin
    WSACleanup;
    WinsockInitialized := False;
  end;
  {$ENDIF}
end;

{ ============================================================================ }
{ Socket Creation and Management                                               }
{ ============================================================================ }

function SocketCreate: PSocketContext;
var
  Handle: TSocketHandle;
begin
  Result := nil;
  
  { Create TCP socket }
  {$IFDEF WINDOWS}
  Handle := Winsock2.socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
  {$ELSE}
  Handle := fpSocket(AF_INET, SOCK_STREAM, 0);
  {$ENDIF}
  
  {$IFDEF WINDOWS}
  if Handle = INVALID_SOCKET then Exit;
  {$ELSE}
  if Handle < 0 then Exit;
  {$ENDIF}
  
  { Allocate context }
  New(Result);
  if Result = nil then
  begin
    {$IFDEF WINDOWS}
    CloseSocket(Handle);
    {$ELSE}
    fpClose(Handle);
    {$ENDIF}
    Exit;
  end;
  
  { Initialize context }
  FillChar(Result^, SizeOf(Result^), 0);
  Result^.Handle := Handle;
  Result^.State := SOCK_STATE_NONE;
  Result^.LastError := SOCK_OK;
end;

procedure SocketDestroy(Context: PSocketContext);
begin
  if Context = nil then Exit;
  
  SocketClose(Context);
  Dispose(Context);
end;

procedure SocketClose(Context: PSocketContext);
begin
  if Context = nil then Exit;
  
  if Context^.Handle <> {$IFDEF WINDOWS}INVALID_SOCKET{$ELSE}-1{$ENDIF} then
  begin
    {$IFDEF WINDOWS}
    CloseSocket(Context^.Handle);
    {$ELSE}
    fpClose(Context^.Handle);
    {$ENDIF}
    Context^.Handle := {$IFDEF WINDOWS}INVALID_SOCKET{$ELSE}-1{$ENDIF};
  end;
  
  Context^.State := SOCK_STATE_CLOSED;
end;

function SocketErrorString(ErrorCode: Integer): string;
begin
  case ErrorCode of
    SOCK_OK:             Result := 'No error';
    SOCK_ERR_CREATE:     Result := 'Failed to create socket';
    SOCK_ERR_CONNECT:    Result := 'Connection failed';
    SOCK_ERR_BIND:       Result := 'Bind failed';
    SOCK_ERR_LISTEN:     Result := 'Listen failed';
    SOCK_ERR_ACCEPT:     Result := 'Accept failed';
    SOCK_ERR_SEND:       Result := 'Send failed';
    SOCK_ERR_RECV:       Result := 'Receive failed';
    SOCK_ERR_CLOSED:     Result := 'Connection closed';
    SOCK_ERR_WOULDBLOCK: Result := 'Would block';
    SOCK_ERR_INVALID:    Result := 'Invalid socket';
    SOCK_ERR_RESOLVE:    Result := 'Host resolution failed';
  else
    Result := 'Unknown error (' + IntToStr(ErrorCode) + ')';
  end;
end;

{ ============================================================================ }
{ Client Operations                                                            }
{ ============================================================================ }

function SocketSetNonBlocking(Context: PSocketContext): Boolean;
{$IFDEF WINDOWS}
var
  NonBlock: LongWord;
{$ELSE}
var
  Flags: Integer;
{$ENDIF}
begin
  Result := False;
  if Context = nil then Exit;
  {$IFDEF WINDOWS}
  if Context^.Handle = INVALID_SOCKET then Exit;
  NonBlock := 1;
  Result := ioctlsocket(Context^.Handle, FIONBIO, NonBlock) = 0;
  {$ELSE}
  if Context^.Handle < 0 then Exit;
  Flags := fpFcntl(Context^.Handle, F_GETFL, 0);
  if Flags < 0 then Exit;
  Result := fpFcntl(Context^.Handle, F_SETFL, Flags or O_NONBLOCK) >= 0;
  {$ENDIF}
end;

function SocketSetBlocking(Context: PSocketContext): Boolean;
{$IFDEF WINDOWS}
var
  NonBlock: LongWord;
{$ELSE}
var
  Flags: Integer;
{$ENDIF}
begin
  Result := False;
  if Context = nil then Exit;
  {$IFDEF WINDOWS}
  if Context^.Handle = INVALID_SOCKET then Exit;
  NonBlock := 0;
  Result := ioctlsocket(Context^.Handle, FIONBIO, NonBlock) = 0;
  {$ELSE}
  if Context^.Handle < 0 then Exit;
  Flags := fpFcntl(Context^.Handle, F_GETFL, 0);
  if Flags < 0 then Exit;
  Result := fpFcntl(Context^.Handle, F_SETFL, Flags and not O_NONBLOCK) >= 0;
  {$ENDIF}
end;

function SocketConnect(Context: PSocketContext; const Host: string;
                       Port: Word; Timeout: Integer): Integer;
var
  Addr: {$IFDEF WINDOWS}TSockAddrIn{$ELSE}TInetSockAddr{$ENDIF};
  IPAddr: string;
  Res: Integer;
begin
  Result := SOCK_ERR_INVALID;
  if Context = nil then Exit;
  
  { Resolve hostname }
  IPAddr := SocketResolveHost(Host);
  if IPAddr = '' then
  begin
    SetSocketError(Context, SOCK_ERR_RESOLVE);
    Exit;
  end;
  
  { Setup address structure }
  FillChar(Addr, SizeOf(Addr), 0);
  Addr.sin_family := AF_INET;
  Addr.sin_port := htons(Port);
  {$IFDEF WINDOWS}
  { Use thread-safe IP parsing instead of deprecated inet_addr }
  if not IPStringToAddr(IPAddr, Addr.sin_addr.S_addr) then
  begin
    Result := SOCK_ERR_RESOLVE;
    SetSocketError(Context, Result);
    Exit;
  end;
  {$ELSE}
  Addr.sin_addr := StrToNetAddr(IPAddr);
  {$ENDIF}
  
  Context^.State := SOCK_STATE_CONNECTING;
  Context^.RemoteAddr := Host;
  Context^.RemotePort := Port;
  
  { Attempt connection }
  {$IFDEF WINDOWS}
  Res := connect(Context^.Handle, TSockAddr(Addr), SizeOf(Addr));
  {$ELSE}
  Res := fpConnect(Context^.Handle, @Addr, SizeOf(Addr));
  {$ENDIF}
  
  if Res = 0 then
  begin
    Result := SOCK_OK;
    Context^.State := SOCK_STATE_CONNECTED;
  end
  else
  begin
    Result := SOCK_ERR_CONNECT;
    SetSocketError(Context, Result);
  end;
end;

function SocketCheckConnect(Context: PSocketContext): Integer;
var
  OptVal: Integer;
  OptLen: Integer;
  Res: Integer;
{$IFDEF UNIX}
  SoError: tsocklen;
{$ELSE}
  SoError: LongInt;
{$ENDIF}
begin
  if Context = nil then
  begin
    Result := SOCK_STATE_ERROR;
    Exit;
  end;
  
  { Check if we can write (connect completed) }
  if SocketCanWrite(Context) then
  begin
    { Verify no error occurred }
    OptLen := SizeOf(OptVal);
    {$IFDEF WINDOWS}
    Res := getsockopt(Context^.Handle, SOL_SOCKET, SO_ERROR, @OptVal, OptLen);
    {$ELSE}
    SoError := SizeOf(OptVal);
    Res := fpGetSockOpt(Context^.Handle, SOL_SOCKET, SO_ERROR, @OptVal, @SoError);
    {$ENDIF}
    
    if (Res = 0) and (OptVal = 0) then
    begin
      Result := SOCK_STATE_CONNECTED;
      Context^.State := SOCK_STATE_CONNECTED;
    end
    else
    begin
      Result := SOCK_STATE_ERROR;
      SetSocketError(Context, SOCK_ERR_CONNECT);
    end;
  end
  else if Context^.State = SOCK_STATE_CONNECTING then
    Result := SOCK_STATE_CONNECTING
  else
    Result := Context^.State;
end;

{ ============================================================================ }
{ Server Operations                                                            }
{ ============================================================================ }

function SocketBind(Context: PSocketContext; const Address: string;
                    Port: Word): Integer;
var
  Addr: {$IFDEF WINDOWS}TSockAddrIn{$ELSE}TInetSockAddr{$ENDIF};
  BindAddr: string;
  Res: Integer;
begin
  Result := SOCK_ERR_INVALID;
  if Context = nil then Exit;
  
  { Use provided address or default to all interfaces }
  if Address = '' then
    BindAddr := '0.0.0.0'
  else
    BindAddr := Address;
  
  { Setup address structure }
  FillChar(Addr, SizeOf(Addr), 0);
  Addr.sin_family := AF_INET;
  Addr.sin_port := htons(Port);
  {$IFDEF WINDOWS}
  { Use thread-safe IP parsing instead of deprecated inet_addr }
  if BindAddr = '' then
    Addr.sin_addr.S_addr := INADDR_ANY
  else if not IPStringToAddr(BindAddr, Addr.sin_addr.S_addr) then
  begin
    Result := SOCK_ERR_BIND;
    SetSocketError(Context, Result);
    Exit;
  end;
  {$ELSE}
  if BindAddr = '' then
    Addr.sin_addr.s_addr := INADDR_ANY
  else
    Addr.sin_addr := StrToNetAddr(BindAddr);
  {$ENDIF}
  
  {$IFDEF WINDOWS}
  Res := bind(Context^.Handle, TSockAddr(Addr), SizeOf(Addr));
  {$ELSE}
  Res := fpBind(Context^.Handle, @Addr, SizeOf(Addr));
  {$ENDIF}
  
  if Res = 0 then
  begin
    Result := SOCK_OK;
    Context^.LocalPort := Port;
  end
  else
  begin
    Result := SOCK_ERR_BIND;
    SetSocketError(Context, Result);
  end;
end;

function SocketListen(Context: PSocketContext; Backlog: Integer): Integer;
var
  Res: Integer;
begin
  Result := SOCK_ERR_INVALID;
  if Context = nil then Exit;
  
  if Backlog <= 0 then
    Backlog := 5;  { Default backlog }
  
  {$IFDEF WINDOWS}
  Res := listen(Context^.Handle, Backlog);
  {$ELSE}
  Res := fpListen(Context^.Handle, Backlog);
  {$ENDIF}
  
  if Res = 0 then
  begin
    Result := SOCK_OK;
    Context^.State := SOCK_STATE_LISTENING;
  end
  else
  begin
    Result := SOCK_ERR_LISTEN;
    SetSocketError(Context, Result);
  end;
end;

function SocketAccept(Server: PSocketContext;
                      out Client: PSocketContext): Integer;
var
  {$IFDEF WINDOWS}
  ClientAddr: TSockAddrIn;
  ClientAddrLen: Integer;
  {$ELSE}
  ClientAddr: TInetSockAddr;
  ClientAddrLen: tsocklen;
  {$ENDIF}
  ClientHandle: TSocketHandle;
begin
  Result := SOCK_ERR_INVALID;
  Client := nil;
  if Server = nil then Exit;
  
  {$IFDEF WINDOWS}
  ClientAddrLen := SizeOf(ClientAddr);
  ClientHandle := accept(Server^.Handle, @ClientAddr, @ClientAddrLen);
  if ClientHandle = INVALID_SOCKET then
  {$ELSE}
  ClientAddrLen := SizeOf(ClientAddr);
  ClientHandle := fpAccept(Server^.Handle, @ClientAddr, @ClientAddrLen);
  if ClientHandle < 0 then
  {$ENDIF}
  begin
    Result := SOCK_ERR_ACCEPT;
    SetSocketError(Server, Result);
    Exit;
  end;
  
  { Create client context }
  New(Client);
  if Client = nil then
  begin
    {$IFDEF WINDOWS}
    CloseSocket(ClientHandle);
    {$ELSE}
    fpClose(ClientHandle);
    {$ENDIF}
    Result := SOCK_ERR_CREATE;
    Exit;
  end;
  
  FillChar(Client^, SizeOf(Client^), 0);
  Client^.Handle := ClientHandle;
  Client^.State := SOCK_STATE_CONNECTED;
  
  { Get client address - use thread-safe conversion }
  {$IFDEF WINDOWS}
  Client^.RemoteAddr := AddrToIPString(ntohl(ClientAddr.sin_addr.S_addr));
  {$ELSE}
  Client^.RemoteAddr := NetAddrToStr(ClientAddr.sin_addr);
  {$ENDIF}
  Client^.RemotePort := ntohs(ClientAddr.sin_port);
  
  Result := SOCK_OK;
end;

function SocketHasPending(Server: PSocketContext): Boolean;
begin
  Result := False;
  if Server = nil then Exit;
  Result := SocketCanRead(Server);
end;

{ ============================================================================ }
{ Data Transfer                                                                }
{ ============================================================================ }

function SocketSend(Context: PSocketContext; Data: Pointer;
                    Len: Integer; out Sent: Integer): Integer;
var
  Res: Integer;
{$IFDEF WINDOWS}
  Flags: Integer;
{$ENDIF}
begin
  Result := SOCK_ERR_INVALID;
  Sent := 0;
  if (Context = nil) or (Data = nil) or (Len <= 0) then Exit;
  if Context^.State <> SOCK_STATE_CONNECTED then
  begin
    Result := SOCK_ERR_CLOSED;
    Exit;
  end;
  
  {$IFDEF WINDOWS}
  Flags := 0;
  Res := send(Context^.Handle, Data^, Len, Flags);
  {$ELSE}
  Res := fpSend(Context^.Handle, Data, Len, 0);
  {$ENDIF}
  
  if Res > 0 then
  begin
    Sent := Res;
    Inc(Context^.BytesSent, Sent);
    Result := SOCK_OK;
  end
  else if Res = 0 then
  begin
    Result := SOCK_ERR_CLOSED;
    Context^.State := SOCK_STATE_CLOSED;
  end
  else
  begin
    {$IFDEF WINDOWS}
    if WSAGetLastError = WSAEWOULDBLOCK then
    {$ELSE}
    if fpgeterrno = ESysEAGAIN then
    {$ENDIF}
      Result := SOCK_ERR_WOULDBLOCK
    else
    begin
      Result := SOCK_ERR_SEND;
      SetSocketError(Context, Result);
    end;
  end;
end;

function SocketReceive(Context: PSocketContext; Buffer: Pointer;
                       BufLen: Integer; out Received: Integer): Integer;
var
  Res: Integer;
{$IFDEF WINDOWS}
  Flags: Integer;
{$ENDIF}
begin
  Result := SOCK_ERR_INVALID;
  Received := 0;
  if (Context = nil) or (Buffer = nil) or (BufLen <= 0) then Exit;
  if Context^.State <> SOCK_STATE_CONNECTED then
  begin
    Result := SOCK_ERR_CLOSED;
    Exit;
  end;
  
  {$IFDEF WINDOWS}
  Flags := 0;
  Res := recv(Context^.Handle, Buffer^, BufLen, Flags);
  {$ELSE}
  Res := fpRecv(Context^.Handle, Buffer, BufLen, 0);
  {$ENDIF}
  
  if Res > 0 then
  begin
    Received := Res;
    Inc(Context^.BytesReceived, Received);
    Result := SOCK_OK;
  end
  else if Res = 0 then
  begin
    { Peer closed connection }
    Result := SOCK_ERR_CLOSED;
    Context^.State := SOCK_STATE_CLOSED;
  end
  else
  begin
    {$IFDEF WINDOWS}
    if WSAGetLastError = WSAEWOULDBLOCK then
    {$ELSE}
    if fpgeterrno = ESysEAGAIN then
    {$ENDIF}
      Result := SOCK_ERR_WOULDBLOCK
    else
    begin
      Result := SOCK_ERR_RECV;
      SetSocketError(Context, Result);
    end;
  end;
end;

function SocketSendAll(Context: PSocketContext; Data: Pointer;
                       Len: Integer): Integer;
const
  MAX_RETRIES = 10000;  { Prevent infinite loop }
var
  Sent: Integer;
  TotalSent: Integer;
  Res: Integer;
  DataPtr: PByte;
  Retries: Integer;
begin
  Result := SOCK_ERR_INVALID;
  if (Context = nil) or (Data = nil) or (Len <= 0) then Exit;
  
  Result := SOCK_OK;
  TotalSent := 0;
  DataPtr := Data;
  Retries := 0;
  
  while TotalSent < Len do
  begin
    Res := SocketSend(Context, DataPtr, Len - TotalSent, Sent);
    if Res <> SOCK_OK then
    begin
      Result := Res;
      Exit;
    end;
    
    if Sent = 0 then
    begin
      { Would block - try again with retry limit }
      Inc(Retries);
      if Retries > MAX_RETRIES then
      begin
        Result := SOCK_ERR_SEND;
        SetSocketError(Context, Result);
        Exit;
      end;
      Sleep(1);
      Continue;
    end;
    
    Retries := 0;  { Reset counter on successful send }
    Inc(DataPtr, Sent);
    Inc(TotalSent, Sent);
  end;
end;

function SocketReceiveAll(Context: PSocketContext; Buffer: Pointer;
                          Len: Integer): Integer;
const
  MAX_RETRIES = 10000;  { Prevent infinite loop }
var
  Received: Integer;
  TotalReceived: Integer;
  Res: Integer;
  BufPtr: PByte;
  Retries: Integer;
begin
  Result := SOCK_ERR_INVALID;
  if (Context = nil) or (Buffer = nil) or (Len <= 0) then Exit;
  
  Result := SOCK_OK;
  TotalReceived := 0;
  BufPtr := Buffer;
  Retries := 0;
  
  while TotalReceived < Len do
  begin
    Res := SocketReceive(Context, BufPtr, Len - TotalReceived, Received);
    if Res <> SOCK_OK then
    begin
      Result := Res;
      Exit;
    end;
    
    if Received = 0 then
    begin
      { Would block - try again with retry limit }
      Inc(Retries);
      if Retries > MAX_RETRIES then
      begin
        Result := SOCK_ERR_RECV;
        SetSocketError(Context, Result);
        Exit;
      end;
      Sleep(1);
      Continue;
    end;
    
    Retries := 0;  { Reset counter on successful receive }
    Inc(BufPtr, Received);
    Inc(TotalReceived, Received);
  end;
end;

{ ============================================================================ }
{ Utility Functions                                                            }
{ ============================================================================ }

function SocketCanRead(Context: PSocketContext): Boolean;
var
  ReadFds: {$IFDEF WINDOWS}TFDSet{$ELSE}TFDSet{$ENDIF};
  Timeout: {$IFDEF WINDOWS}TTimeVal{$ELSE}TTimeVal{$ENDIF};
  Res: Integer;
begin
  Result := False;
  if Context = nil then Exit;
  {$IFDEF WINDOWS}
  if Context^.Handle = INVALID_SOCKET then Exit;
  {$ELSE}
  if Context^.Handle < 0 then Exit;
  {$ENDIF}
  {$IFDEF WINDOWS}
  FD_ZERO(ReadFds);
  FD_SET(Context^.Handle, ReadFds);
  {$ELSE}
  fpFD_ZERO(ReadFds);
  fpFD_SET(Context^.Handle, ReadFds);
  {$ENDIF}
  
  Timeout.tv_sec := 0;
  Timeout.tv_usec := 0;
  
  {$IFDEF WINDOWS}
  Res := select(0, @ReadFds, nil, nil, @Timeout);
  {$ELSE}
  Res := fpSelect(Context^.Handle + 1, @ReadFds, nil, nil, @Timeout);
  {$ENDIF}
  
  Result := (Res > 0);
  if Result then
  begin
    {$IFDEF WINDOWS}
    Result := FD_ISSET(Context^.Handle, ReadFds) <> 0;
    {$ELSE}
    Result := fpFD_ISSET(Context^.Handle, ReadFds) <> 0;
    {$ENDIF}
  end;
end;

function SocketCanWrite(Context: PSocketContext): Boolean;
var
  WriteFds: {$IFDEF WINDOWS}TFDSet{$ELSE}TFDSet{$ENDIF};
  Timeout: {$IFDEF WINDOWS}TTimeVal{$ELSE}TTimeVal{$ENDIF};
  Res: Integer;
begin
  Result := False;
  if Context = nil then Exit;
  {$IFDEF WINDOWS}
  if Context^.Handle = INVALID_SOCKET then Exit;
  {$ELSE}
  if Context^.Handle < 0 then Exit;
  {$ENDIF}
  {$IFDEF WINDOWS}
  FD_ZERO(WriteFds);
  FD_SET(Context^.Handle, WriteFds);
  {$ELSE}
  fpFD_ZERO(WriteFds);
  fpFD_SET(Context^.Handle, WriteFds);
  {$ENDIF}
  
  Timeout.tv_sec := 0;
  Timeout.tv_usec := 0;
  
  {$IFDEF WINDOWS}
  Res := select(0, nil, @WriteFds, nil, @Timeout);
  {$ELSE}
  Res := fpSelect(Context^.Handle + 1, nil, @WriteFds, nil, @Timeout);
  {$ENDIF}
  
  Result := (Res > 0);
  if Result then
  begin
    {$IFDEF WINDOWS}
    Result := FD_ISSET(Context^.Handle, WriteFds) <> 0;
    {$ELSE}
    Result := fpFD_ISSET(Context^.Handle, WriteFds) <> 0;
    {$ENDIF}
  end;
end;

function SocketBytesAvailable(Context: PSocketContext): Integer;
{$IFDEF WINDOWS}
var
  Available: LongWord;
{$ELSE}
var
  Available: Integer;
{$ENDIF}
begin
  Result := 0;
  if Context = nil then Exit;
  {$IFDEF WINDOWS}
  if Context^.Handle = INVALID_SOCKET then Exit;
  if ioctlsocket(Context^.Handle, FIONREAD, Available) = 0 then
    Result := Integer(Available);
  {$ELSE}
  if Context^.Handle < 0 then Exit;
  if fpIOCtl(Context^.Handle, FIONREAD, @Available) = 0 then
    Result := Available;
  {$ENDIF}
end;

function SocketResolveHost(const Hostname: string): string;
{$IFDEF WINDOWS}
var
  HostEnt: PHostEnt;
  Addr: PChar;
{$ELSE}
var
  HostEntry: THostEntry;
{$ENDIF}
begin
  Result := '';
  if Hostname = '' then Exit;
  
  { Check if already an IP address - use thread-safe check }
  if IsValidIP(Hostname) then
  begin
    Result := Hostname;
    Exit;
  end;
  
  { Resolve hostname }
  {$IFDEF WINDOWS}
  { Note: gethostbyname is deprecated but Windows alternatives require }
  { getaddrinfo which needs different headers. For now, this is wrapped }
  { to minimize thread-safety issues - consider getaddrinfo for new code. }
  HostEnt := gethostbyname(PChar(Hostname));
  if HostEnt <> nil then
  begin
    Addr := HostEnt^.h_addr_list[0];
    if Addr <> nil then
      { Use thread-safe conversion instead of inet_ntoa }
      Result := AddrToIPString(ntohl(PInAddr(Addr)^.S_addr));
  end;
  {$ELSE}
  if GetHostByName(Hostname, HostEntry) then
    Result := NetAddrToStr(HostEntry.Addr);
  {$ENDIF}
end;

function SocketGetLocalAddr(Context: PSocketContext;
                            out Addr: TSocketAddr): Boolean;
var
  {$IFDEF WINDOWS}
  SockAddr: TSockAddrIn;
  {$ELSE}
  SockAddr: TInetSockAddr;
  {$ENDIF}
  Len: {$IFDEF WINDOWS}Integer{$ELSE}tsocklen{$ENDIF};
begin
  Result := False;
  if Context = nil then Exit;
  
  Len := SizeOf(SockAddr);
  {$IFDEF WINDOWS}
  if getsockname(Context^.Handle, TSockAddr(SockAddr), Len) = 0 then
  begin
    { Use thread-safe conversion instead of inet_ntoa }
    Addr.Address := AddrToIPString(ntohl(SockAddr.sin_addr.S_addr));
    Addr.Port := ntohs(SockAddr.sin_port);
    Result := True;
  end;
  {$ELSE}
  if fpGetSockName(Context^.Handle, @SockAddr, @Len) = 0 then
  begin
    Addr.Address := NetAddrToStr(SockAddr.sin_addr);
    Addr.Port := ntohs(SockAddr.sin_port);
    Result := True;
  end;
  {$ENDIF}
end;

function SocketGetRemoteAddr(Context: PSocketContext;
                             out Addr: TSocketAddr): Boolean;
begin
  Result := False;
  if Context = nil then Exit;
  
  if Context^.State = SOCK_STATE_CONNECTED then
  begin
    Addr.Address := Context^.RemoteAddr;
    Addr.Port := Context^.RemotePort;
    Result := True;
  end;
end;

{ ============================================================================ }
{ Poll/Select Wrapper                                                          }
{ ============================================================================ }

function SocketWait(Context: PSocketContext; WantRead, WantWrite: Boolean;
                    Timeout: Integer): Boolean;
var
  ReadFds, WriteFds: {$IFDEF WINDOWS}TFDSet{$ELSE}TFDSet{$ENDIF};
  TimeVal: {$IFDEF WINDOWS}TTimeVal{$ELSE}TTimeVal{$ENDIF};
  Res: Integer;
  MaxFd: Integer;
begin
  Result := False;
  if Context = nil then Exit;
  {$IFDEF WINDOWS}
  if Context^.Handle = INVALID_SOCKET then Exit;
  {$ELSE}
  if Context^.Handle < 0 then Exit;
  {$ENDIF}
  {$IFDEF WINDOWS}
  if WantRead then
  begin
    FD_ZERO(ReadFds);
    FD_SET(Context^.Handle, ReadFds);
  end;
  if WantWrite then
  begin
    FD_ZERO(WriteFds);
    FD_SET(Context^.Handle, WriteFds);
  end;
  {$ELSE}
  if WantRead then
  begin
    fpFD_ZERO(ReadFds);
    fpFD_SET(Context^.Handle, ReadFds);
  end;
  if WantWrite then
  begin
    fpFD_ZERO(WriteFds);
    fpFD_SET(Context^.Handle, WriteFds);
  end;
  {$ENDIF}
  
  if Timeout >= 0 then
  begin
    TimeVal.tv_sec := Timeout div 1000;
    TimeVal.tv_usec := (Timeout mod 1000) * 1000;
  end;
  
  {$IFDEF WINDOWS}
  MaxFd := 0;
  {$ELSE}
  MaxFd := Context^.Handle + 1;
  {$ENDIF}
  
  {$IFDEF WINDOWS}
  Res := select(MaxFd,
                @ReadFds,
                @WriteFds,
                nil,
                @TimeVal);
  {$ELSE}
  Res := fpSelect(MaxFd,
                  @ReadFds,
                  @WriteFds,
                  nil,
                  @TimeVal);
  {$ENDIF}
  
  Result := Res > 0;
end;

end.
