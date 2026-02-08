{
  logging.pas - Logging infrastructure for PascalTorrent
  
  Provides configurable logging with multiple levels and output destinations.
}

unit logging;

{$mode objfpc}{$H+}

interface

uses
  SysUtils
  {$IFDEF LOGGING_THREADSAFE}
  {$IFDEF MSWINDOWS}
  , Windows
  {$ELSE}
  , cthreads, cmem, pthreads
  {$ENDIF}
  {$ENDIF}
  ;

type
  TLogLevel = (llDebug, llInfo, llWarning, llError, llFatal);
  
  TLogDestination = (ldConsole, ldFile);
  TLogDestinations = set of TLogDestination;
  
  PLogEntry = ^TLogEntry;
  TLogEntry = record
    Timestamp: TDateTime;
    Level: TLogLevel;
    Message: string;
    Category: string;
    ThreadID: Cardinal;
    Next: PLogEntry;
  end;
  
  PLogger = ^TLogger;
  TLogger = record
    Destinations: TLogDestinations;
    MinLevel: TLogLevel;
    LogFile: Text;
    LogFileOpen: Boolean;
    LogFilename: string;
    UseColors: Boolean;
    IncludeTimestamp: Boolean;
    IncludeLevel: Boolean;
    IncludeCategory: Boolean;
    Queue: PLogEntry;
    QueueTail: PLogEntry;
    MaxQueueSize: Integer;
    QueueSize: Integer;
    {$IFDEF LOGGING_THREADSAFE}
    {$IFDEF MSWINDOWS}
    Sync: TRTLCriticalSection;
    {$ELSE}
    Sync: pthread_mutex_t; { POSIX thread mutex }
    {$ENDIF}
    {$ELSE}
    Sync: Pointer;  { Reserved for future thread safety }
    {$ENDIF}
  end;

{ Global logger instance }
var
  GlobalLogger: PLogger = nil;

{ ============================================================================ }
{ Logger Management                                                           }
{ ============================================================================ }

{ Create a new logger }
function LoggerCreate: PLogger;

{ Destroy a logger }
procedure LoggerDestroy(Logger: PLogger);

{ Initialize global logger }
function InitLogging: Boolean;

{ Shutdown global logger }
procedure ShutdownLogging;

{ ============================================================================ }
{ Configuration                                                               }
{ ============================================================================ }

{ Set minimum log level }
procedure LoggerSetLevel(Logger: PLogger; Level: TLogLevel);

{ Set log destinations }
procedure LoggerSetDestinations(Logger: PLogger; Destinations: TLogDestinations);

{ Set log file (opens the file) }
function LoggerSetFile(Logger: PLogger; const Filename: string; 
                       DoAppend: Boolean = True): Boolean;

{ Close log file }
procedure LoggerCloseFile(Logger: PLogger);

{ Enable/disable console colors }
procedure LoggerSetColors(Logger: PLogger; Enable: Boolean);

{ ============================================================================ }
{ Logging Functions                                                           }
{ ============================================================================ }

{ Log a message with specific level }
procedure Log(Logger: PLogger; Level: TLogLevel; const Category, Msg: string); overload;
procedure Log(Logger: PLogger; Level: TLogLevel; const Category, Fmt: string; 
              const Args: array of const); overload;

{ Convenience functions }
procedure LogDebug(const Category, Msg: string); overload;
procedure LogDebug(const Category, Fmt: string; const Args: array of const); overload;
procedure LogInfo(const Category, Msg: string); overload;
procedure LogInfo(const Category, Fmt: string; const Args: array of const); overload;
procedure LogWarning(const Category, Msg: string); overload;
procedure LogWarning(const Category, Fmt: string; const Args: array of const); overload;
procedure LogError(const Category, Msg: string); overload;
procedure LogError(const Category, Fmt: string; const Args: array of const); overload;
procedure LogFatal(const Category, Msg: string); overload;
procedure LogFatal(const Category, Fmt: string; const Args: array of const); overload;

{ Log to global logger }
procedure Log(Level: TLogLevel; const Category, Msg: string); overload;

{ ============================================================================ }
{ Utility Functions                                                           }
{ ============================================================================ }

{ Convert log level to string }
function LogLevelToStr(Level: TLogLevel): string;

{ Convert string to log level }
function StrToLogLevel(const S: string; out Level: TLogLevel): Boolean;

{ Get ANSI color code for log level }
function LogLevelColor(Level: TLogLevel): string;

{ Reset ANSI color }
function ColorReset: string;

{ Flush log queue }
procedure LoggerFlush(Logger: PLogger);

implementation

{$IFDEF MSWINDOWS}
uses
  Windows;
{$ENDIF}

{ ============================================================================ }
{ Helper Functions                                                            }
{ ============================================================================ }

function LogLevelToStr(Level: TLogLevel): string;
const
  Names: array[TLogLevel] of string = ('DEBUG', 'INFO', 'WARN', 'ERROR', 'FATAL');
begin
  Result := Names[Level];
end;

function StrToLogLevel(const S: string; out Level: TLogLevel): Boolean;
var
  UpperS: string;
  L: TLogLevel;
begin
  UpperS := UpperCase(S);
  for L := Low(TLogLevel) to High(TLogLevel) do
  begin
    if UpperS = LogLevelToStr(L) then
    begin
      Level := L;
      Result := True;
      Exit;
    end;
  end;
  Result := False;
end;

function LogLevelColor(Level: TLogLevel): string;
begin
  if (GlobalLogger = nil) or not GlobalLogger^.UseColors then
  begin
    Result := '';
    Exit;
  end;
  
  case Level of
    llDebug:   Result := #27'[36m';  { Cyan }
    llInfo:    Result := #27'[32m';  { Green }
    llWarning: Result := #27'[33m';  { Yellow }
    llError:   Result := #27'[31m';  { Red }
    llFatal:   Result := #27'[35m';  { Magenta }
  end;
end;

function ColorReset: string;
begin
  if (GlobalLogger <> nil) and GlobalLogger^.UseColors then
    Result := #27'[0m'
  else
    Result := '';
end;

{ ============================================================================ }
{ Logger Management                                                           }
{ ============================================================================ }

procedure LoggerLock(Logger: PLogger); forward;
procedure LoggerUnlock(Logger: PLogger); forward;

function LoggerCreate: PLogger;
begin
  New(Result);
  Result^.Destinations := [ldConsole];
  Result^.MinLevel := llInfo;
  Result^.LogFileOpen := False;
  Result^.LogFilename := '';
  Result^.UseColors := True;
  Result^.IncludeTimestamp := True;
  Result^.IncludeLevel := True;
  Result^.IncludeCategory := True;
  Result^.Queue := nil;
  Result^.QueueTail := nil;
  Result^.MaxQueueSize := 1000;
  Result^.QueueSize := 0;
  
  {$IFDEF LOGGING_THREADSAFE}
  {$IFDEF MSWINDOWS}
  InitializeCriticalSection(Result^.Sync);
  {$ELSE}
  pthread_mutex_init(@Result^.Sync, nil);
  {$ENDIF}
  {$ENDIF}
  
  {$IFDEF MSWINDOWS}
  { Disable colors on Windows unless ANSICON is set }
  if GetEnvironmentVariable('ANSICON') = '' then
    Result^.UseColors := False;
  {$ENDIF}
end;

procedure LoggerDestroy(Logger: PLogger);
begin
  if Logger = nil then Exit;
  
  LoggerFlush(Logger);
  LoggerCloseFile(Logger);
  
  {$IFDEF LOGGING_THREADSAFE}
  {$IFDEF MSWINDOWS}
  DeleteCriticalSection(Logger^.Sync);
  {$ELSE}
  pthread_mutex_destroy(@Logger^.Sync);
  {$ENDIF}
  {$ENDIF}
  
  Dispose(Logger);
end;

{ Lock/unlock helper procedures }
procedure LoggerLock(Logger: PLogger);
begin
  {$IFDEF LOGGING_THREADSAFE}
  {$IFDEF MSWINDOWS}
  EnterCriticalSection(Logger^.Sync);
  {$ELSE}
  pthread_mutex_lock(@Logger^.Sync);
  {$ENDIF}
  {$ENDIF}
end;

procedure LoggerUnlock(Logger: PLogger);
begin
  {$IFDEF LOGGING_THREADSAFE}
  {$IFDEF MSWINDOWS}
  LeaveCriticalSection(Logger^.Sync);
  {$ELSE}
  pthread_mutex_unlock(@Logger^.Sync);
  {$ENDIF}
  {$ENDIF}
end;

function InitLogging: Boolean;
begin
  if GlobalLogger <> nil then
  begin
    Result := True;
    Exit;
  end;
  
  GlobalLogger := LoggerCreate;
  Result := GlobalLogger <> nil;
end;

procedure ShutdownLogging;
begin
  if GlobalLogger = nil then Exit;
  LoggerDestroy(GlobalLogger);
  GlobalLogger := nil;
end;

{ ============================================================================ }
{ Configuration                                                               }
{ ============================================================================ }

procedure LoggerSetLevel(Logger: PLogger; Level: TLogLevel);
begin
  if Logger = nil then Exit;
  Logger^.MinLevel := Level;
end;

procedure LoggerSetDestinations(Logger: PLogger; Destinations: TLogDestinations);
begin
  if Logger = nil then Exit;
  Logger^.Destinations := Destinations;
end;

function LoggerSetFile(Logger: PLogger; const Filename: string; 
                       DoAppend: Boolean): Boolean;
{$IFDEF FPC}
begin
  Result := False;
  if Logger = nil then Exit;
  
  LoggerCloseFile(Logger);
  
  Assign(Logger^.LogFile, Filename);
  {$I-}
  if DoAppend and SysUtils.FileExists(Filename) then
    System.Append(Logger^.LogFile)
  else
    Rewrite(Logger^.LogFile);
  {$I+}
  
  if IOResult <> 0 then Exit;
  
  Logger^.LogFileOpen := True;
  Logger^.LogFilename := Filename;
  Include(Logger^.Destinations, ldFile);
  Result := True;
end;
{$ELSE}
begin
  { Non-FPC version }
  Result := False;
  if Logger = nil then Exit;
  
  LoggerCloseFile(Logger);
  
  Assign(Logger^.LogFile, Filename);
  {$I-}
  if DoAppend and FileExists(Filename) then
    System.Append(Logger^.LogFile)
  else
    Rewrite(Logger^.LogFile);
  {$I+}
  
  if IOResult <> 0 then Exit;
  
  Logger^.LogFileOpen := True;
  Logger^.LogFilename := Filename;
  Include(Logger^.Destinations, ldFile);
  Result := True;
end;
{$ENDIF}

procedure LoggerCloseFile(Logger: PLogger);
begin
  if (Logger = nil) or not Logger^.LogFileOpen then Exit;
  
  {$I-}
  Close(Logger^.LogFile);
  {$I+}
  
  Logger^.LogFileOpen := False;
  Logger^.LogFilename := '';
  Exclude(Logger^.Destinations, ldFile);
end;

procedure LoggerSetColors(Logger: PLogger; Enable: Boolean);
begin
  if Logger = nil then Exit;
  Logger^.UseColors := Enable;
end;

{ ============================================================================ }
{ Output Functions                                                            }
{ ============================================================================ }

procedure WriteToConsole(const Msg: string; Level: TLogLevel);
var
  Colored: string;
begin
  if (GlobalLogger <> nil) and GlobalLogger^.UseColors then
  begin
    Colored := LogLevelColor(Level) + Msg + ColorReset;
    WriteLn(Colored);
  end
  else
    WriteLn(Msg);
end;

procedure WriteToFile(Logger: PLogger; const Msg: string);
begin
  if (Logger = nil) or not Logger^.LogFileOpen then Exit;
  
  {$I-}
  WriteLn(Logger^.LogFile, Msg);
  Flush(Logger^.LogFile);
  {$I+}
end;

procedure FormatAndOutput(Logger: PLogger; Entry: PLogEntry);
var
  Output: string;
  Parts: Integer;

  procedure AddPart(const S: string);
  begin
    if Parts > 0 then
      Output := Output + ' ';
    Output := Output + S;
    Inc(Parts);
  end;

begin
  Output := '';
  Parts := 0;
  
  if Logger^.IncludeTimestamp then
    AddPart(FormatDateTime('YYYY-MM-DD HH:NN:SS.zzz', Entry^.Timestamp));
  
  if Logger^.IncludeLevel then
    AddPart('[' + LogLevelToStr(Entry^.Level) + ']');
  
  if Logger^.IncludeCategory and (Entry^.Category <> '') then
    AddPart('[' + Entry^.Category + ']');
  
  Output := Output + ' ' + Entry^.Message;
  
  if ldConsole in Logger^.Destinations then
    WriteToConsole(Output, Entry^.Level);
  
  if ldFile in Logger^.Destinations then
    WriteToFile(Logger, Output);
end;

{ ============================================================================ }
{ Logging Functions                                                           }
{ ============================================================================ }

procedure Log(Logger: PLogger; Level: TLogLevel; const Category, Msg: string);
var
  Entry: PLogEntry;
  PrevTail: PLogEntry;
  ShouldOutput: Boolean;
begin
  if Logger = nil then Exit;
  if Level < Logger^.MinLevel then Exit;
  
  { Check queue size before allocating }
  if Logger^.QueueSize >= Logger^.MaxQueueSize then
    Exit;  { Queue full - drop message }
  
  { Create entry }
  New(Entry);
  Entry^.Timestamp := Now;
  Entry^.Level := Level;
  Entry^.Message := Msg;
  Entry^.Category := Category;
  Entry^.ThreadID := 0;  { Could use ThreadID }
  Entry^.Next := nil;
  
  { Lock for queue operations }
  LoggerLock(Logger);
  try
    { Double-check queue size after locking }
    if Logger^.QueueSize >= Logger^.MaxQueueSize then
    begin
      Dispose(Entry);
      Exit;
    end;
    
    { Add to queue }
    if Logger^.Queue = nil then
    begin
      Logger^.Queue := Entry;
      Logger^.QueueTail := Entry;
    end
    else
    begin
      PrevTail := Logger^.QueueTail;
      PrevTail^.Next := Entry;
      Logger^.QueueTail := Entry;
    end;
    Inc(Logger^.QueueSize);
    
    { Output while holding lock to prevent race with LoggerFlush }
    FormatAndOutput(Logger, Entry);
    
    { Remove from queue }
    Logger^.Queue := Entry^.Next;
    if Logger^.Queue = nil then
      Logger^.QueueTail := nil;
    Dec(Logger^.QueueSize);
  except
    { Ignore errors - entry will be cleaned up by LoggerDestroy }
  end;
  LoggerUnlock(Logger);
  
  Dispose(Entry);
end;

procedure Log(Logger: PLogger; Level: TLogLevel; const Category, Fmt: string; 
              const Args: array of const);
begin
  Log(Logger, Level, Category, Format(Fmt, Args));
end;

procedure Log(Level: TLogLevel; const Category, Msg: string);
begin
  Log(GlobalLogger, Level, Category, Msg);
end;

procedure LogDebug(const Category, Msg: string);
begin
  Log(GlobalLogger, llDebug, Category, Msg);
end;

procedure LogDebug(const Category, Fmt: string; const Args: array of const);
begin
  Log(GlobalLogger, llDebug, Category, Format(Fmt, Args));
end;

procedure LogInfo(const Category, Msg: string);
begin
  Log(GlobalLogger, llInfo, Category, Msg);
end;

procedure LogInfo(const Category, Fmt: string; const Args: array of const);
begin
  Log(GlobalLogger, llInfo, Category, Format(Fmt, Args));
end;

procedure LogWarning(const Category, Msg: string);
begin
  Log(GlobalLogger, llWarning, Category, Msg);
end;

procedure LogWarning(const Category, Fmt: string; const Args: array of const);
begin
  Log(GlobalLogger, llWarning, Category, Format(Fmt, Args));
end;

procedure LogError(const Category, Msg: string);
begin
  Log(GlobalLogger, llError, Category, Msg);
end;

procedure LogError(const Category, Fmt: string; const Args: array of const);
begin
  Log(GlobalLogger, llError, Category, Format(Fmt, Args));
end;

procedure LogFatal(const Category, Msg: string);
begin
  Log(GlobalLogger, llFatal, Category, Msg);
end;

procedure LogFatal(const Category, Fmt: string; const Args: array of const);
begin
  Log(GlobalLogger, llFatal, Category, Format(Fmt, Args));
end;

{ ============================================================================ }
{ Utility Functions                                                           }
{ ============================================================================ }

procedure LoggerFlush(Logger: PLogger);
var
  Entry, Next: PLogEntry;
begin
  if Logger = nil then Exit;
  
  LoggerLock(Logger);
  try
    Entry := Logger^.Queue;
    Logger^.Queue := nil;
    Logger^.QueueTail := nil;
    Logger^.QueueSize := 0;
  except
    Entry := nil;
  end;
  LoggerUnlock(Logger);
  
  { Process entries outside of lock }
  while Entry <> nil do
  begin
    Next := Entry^.Next;
    FormatAndOutput(Logger, Entry);
    Dispose(Entry);
    Entry := Next;
  end;
end;

initialization
  { Nothing - lazy initialization }

finalization
  ShutdownLogging;

end.
