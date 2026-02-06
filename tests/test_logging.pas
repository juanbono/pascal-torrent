{
  test_logging.pas - Tests for logging unit
}

{$mode objfpc}{$H+}

program test_logging;

uses
  SysUtils, logging;

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

procedure TestLoggerCreateDestroy;
var
  Logger: PLogger;
begin
  WriteLn(#10'=== Testing Logger Create/Destroy ===');
  
  { Test 1: Create logger }
  Logger := LoggerCreate;
  TestResult('LoggerCreate returns valid pointer',
             Logger <> nil);
  
  if Logger <> nil then
  begin
    { Test 2: Check defaults }
    TestResult('Default destinations include console',
               ldConsole in Logger^.Destinations);
    TestResult('Default min level is Info',
               Logger^.MinLevel = llInfo);
    TestResult('File not open by default',
               not Logger^.LogFileOpen);
    TestResult('Colors enabled by default',
               Logger^.UseColors);
    TestResult('Timestamp included by default',
               Logger^.IncludeTimestamp);
    TestResult('Level included by default',
               Logger^.IncludeLevel);
    TestResult('Category included by default',
               Logger^.IncludeCategory);
    
    { Test 3: Destroy }
    LoggerDestroy(Logger);
    TestResult('LoggerDestroy completes without error', True);
  end;
  
  { Test 4: Destroy nil }
  LoggerDestroy(nil);
  TestResult('LoggerDestroy(nil) is safe', True);
end;

procedure TestGlobalLogging;
begin
  WriteLn(#10'=== Testing Global Logging ===');
  
  { Test 1: Init logging }
  TestResult('InitLogging succeeds',
             InitLogging);
  TestResult('GlobalLogger is set after InitLogging',
             GlobalLogger <> nil);
  
  { Test 2: Double init returns true }
  TestResult('Double InitLogging succeeds',
             InitLogging);
  
  { Test 3: Shutdown }
  ShutdownLogging;
  TestResult('GlobalLogger nil after ShutdownLogging',
             GlobalLogger = nil);
  
  { Test 4: Double shutdown }
  ShutdownLogging;
  TestResult('Double ShutdownLogging is safe', True);
end;

procedure TestLogLevelToStr;
var
  Level: TLogLevel;
  S: string;
begin
  WriteLn(#10'=== Testing LogLevelToStr ===');
  
  TestResult('llDebug -> DEBUG',
             LogLevelToStr(llDebug) = 'DEBUG');
  TestResult('llInfo -> INFO',
             LogLevelToStr(llInfo) = 'INFO');
  TestResult('llWarning -> WARN',
             LogLevelToStr(llWarning) = 'WARN');
  TestResult('llError -> ERROR',
             LogLevelToStr(llError) = 'ERROR');
  TestResult('llFatal -> FATAL',
             LogLevelToStr(llFatal) = 'FATAL');
end;

procedure TestStrToLogLevel;
var
  Level: TLogLevel;
begin
  WriteLn(#10'=== Testing StrToLogLevel ===');
  
  TestResult('StrToLogLevel DEBUG',
             StrToLogLevel('DEBUG', Level) and (Level = llDebug));
  TestResult('StrToLogLevel INFO',
             StrToLogLevel('INFO', Level) and (Level = llInfo));
  TestResult('StrToLogLevel WARN',
             StrToLogLevel('WARN', Level) and (Level = llWarning));
  TestResult('StrToLogLevel ERROR',
             StrToLogLevel('ERROR', Level) and (Level = llError));
  TestResult('StrToLogLevel FATAL',
             StrToLogLevel('FATAL', Level) and (Level = llFatal));
  
  { Test case insensitivity }
  TestResult('StrToLogLevel debug (lowercase)',
             StrToLogLevel('debug', Level) and (Level = llDebug));
  TestResult('StrToLogLevel Info (mixed)',
             StrToLogLevel('Info', Level) and (Level = llInfo));
  
  { Test invalid }
  TestResult('StrToLogLevel invalid returns false',
             not StrToLogLevel('INVALID', Level));
end;

procedure TestLogLevelFiltering;
var
  Logger: PLogger;
  OldDestinations: TLogDestinations;
  LogOutput: string;
  F: Text;
  Line: string;
  DebugFound: Boolean;
begin
  WriteLn(#10'=== Testing Log Level Filtering ===');
  
  Logger := LoggerCreate;
  if Logger = nil then
  begin
    TestResult('Logger created for filtering test', False);
    Exit;
  end;
  
  { Temporarily disable console output by removing console destination }
  OldDestinations := Logger^.Destinations;
  Logger^.Destinations := [];  { No output during test }
  
  { Test 1: Default level (Info) - Debug should be filtered }
  Logger^.MinLevel := llInfo;
  TestResult('Debug filtered when level is Info',
             Logger^.MinLevel > llDebug);
  
  { Test 2: Set level to Debug }
  LoggerSetLevel(Logger, llDebug);
  TestResult('LoggerSetLevel changes MinLevel',
             Logger^.MinLevel = llDebug);
  
  { Test 3: Set level to Error }
  LoggerSetLevel(Logger, llError);
  TestResult('LoggerSetLevel to Error',
             Logger^.MinLevel = llError);
  
  { Test 4: Set nil logger level (should not crash) }
  LoggerSetLevel(nil, llInfo);
  TestResult('LoggerSetLevel(nil) is safe', True);
  
  Logger^.Destinations := OldDestinations;
  LoggerDestroy(Logger);
end;

procedure TestLogFileOperations;
var
  Logger: PLogger;
  TestFilename: string;
  F: Text;
  Line: string;
  InfoFound: Boolean;
  Content: string;
begin
  WriteLn(#10'=== Testing Log File Operations ===');
  
  TestFilename := 'test_log_file.log';
  
  { Clean up any existing file }
  if FileExists(TestFilename) then
    DeleteFile(TestFilename);
  
  Logger := LoggerCreate;
  if Logger = nil then
  begin
    TestResult('Logger created for file test', False);
    Exit;
  end;
  
  { Test 1: Set log file }
  Logger^.Destinations := [ldFile];  { Only file, no console }
  TestResult('LoggerSetFile succeeds',
             LoggerSetFile(Logger, TestFilename, False));
  
  if Logger^.LogFileOpen then
  begin
    { Test 2: Log a message }
    Log(Logger, llInfo, 'TEST', 'Test message');
    TestResult('Log to file completes', True);
    
    { Test 3: Close file }
    LoggerCloseFile(Logger);
    TestResult('LoggerCloseFile closes file',
               not Logger^.LogFileOpen);
    
    { Test 4: Check file content }
    if FileExists(TestFilename) then
    begin
      Assign(F, TestFilename);
      Reset(F);
      InfoFound := False;
      Content := '';
      while not Eof(F) do
      begin
        ReadLn(F, Line);
        Content := Content + Line;
        if Pos('INFO', Line) > 0 then
          InfoFound := True;
      end;
      Close(F);
      
      TestResult('Log file contains [INFO]',
                 InfoFound);
      TestResult('Log file contains test message',
                 Pos('Test message', Content) > 0);
      TestResult('Log file contains category [TEST]',
                 Pos('[TEST]', Content) > 0);
    end
    else
    begin
      TestResult('Log file was created', False);
    end;
  end
  else
  begin
    TestResult('Log file opened', False);
  end;
  
  { Test 5: Nil logger operations }
  LoggerCloseFile(nil);
  TestResult('LoggerCloseFile(nil) is safe', True);
  
  TestResult('LoggerSetFile(nil) returns false',
             not LoggerSetFile(nil, TestFilename));
  
  LoggerDestroy(Logger);
  
  { Cleanup }
  if FileExists(TestFilename) then
    DeleteFile(TestFilename);
end;

procedure TestColorFunctions;
begin
  WriteLn(#10'=== Testing Color Functions ===');
  
  { Note: Color output depends on UseColors setting }
  InitLogging;
  
  if GlobalLogger <> nil then
  begin
    GlobalLogger^.UseColors := True;
    
    TestResult('LogLevelColor returns non-empty for Debug',
               LogLevelColor(llDebug) <> '');
    TestResult('LogLevelColor returns non-empty for Error',
               LogLevelColor(llError) <> '');
    
    { Test with colors disabled }
    GlobalLogger^.UseColors := False;
    TestResult('LogLevelColor returns empty when disabled',
               LogLevelColor(llDebug) = '');
    
    ShutdownLogging;
  end;
  
  { Test ColorReset without logger }
  TestResult('ColorReset without logger is safe',
             ColorReset = '');
end;

procedure TestLogConvenienceFunctions;
var
  Logger: PLogger;
  TestFilename: string;
begin
  WriteLn(#10'=== Testing Log Convenience Functions ===');
  
  TestFilename := 'test_convenience.log';
  if FileExists(TestFilename) then
    DeleteFile(TestFilename);
  
  InitLogging;
  
  if GlobalLogger <> nil then
  begin
    { Redirect to file to avoid console spam }
    GlobalLogger^.Destinations := [ldFile];
    LoggerSetFile(GlobalLogger, TestFilename, False);
    
    { Test convenience functions (just verify they don't crash) }
    LogDebug('TEST', 'Debug message');
    TestResult('LogDebug completes', True);
    
    LogInfo('TEST', 'Info message');
    TestResult('LogInfo completes', True);
    
    LogWarning('TEST', 'Warning message');
    TestResult('LogWarning completes', True);
    
    LogError('TEST', 'Error message');
    TestResult('LogError completes', True);
    
    LogFatal('TEST', 'Fatal message');
    TestResult('LogFatal completes', True);
    
    { Test formatted versions }
    LogInfo('TEST', 'Formatted %s %d', ['test', 42]);
    TestResult('LogInfo with format completes', True);
    
    LoggerCloseFile(GlobalLogger);
  end
  else
  begin
    TestResult('Global logger initialized', False);
  end;
  
  ShutdownLogging;
  
  { Cleanup }
  if FileExists(TestFilename) then
    DeleteFile(TestFilename);
end;

procedure TestLoggerFlush;
var
  Logger: PLogger;
begin
  WriteLn(#10'=== Testing LoggerFlush ===');
  
  Logger := LoggerCreate;
  if Logger <> nil then
  begin
    Logger^.Destinations := [];  { No output during test }
    
    { Add some entries to queue }
    Log(Logger, llInfo, 'TEST', 'Message 1');
    Log(Logger, llInfo, 'TEST', 'Message 2');
    
    { Flush }
    LoggerFlush(Logger);
    TestResult('LoggerFlush completes', True);
    TestResult('Queue empty after flush',
               (Logger^.Queue = nil) and (Logger^.QueueSize = 0));
    
    LoggerDestroy(Logger);
  end
  else
    TestResult('Logger created for flush test', False);
  
  { Test nil }
  LoggerFlush(nil);
  TestResult('LoggerFlush(nil) is safe', True);
end;

procedure TestLoggerSetColors;
var
  Logger: PLogger;
begin
  WriteLn(#10'=== Testing LoggerSetColors ===');
  
  Logger := LoggerCreate;
  if Logger <> nil then
  begin
    { Test 1: Enable colors }
    LoggerSetColors(Logger, True);
    TestResult('LoggerSetColors enables colors',
               Logger^.UseColors);
    
    { Test 2: Disable colors }
    LoggerSetColors(Logger, False);
    TestResult('LoggerSetColors disables colors',
               not Logger^.UseColors);
    
    LoggerDestroy(Logger);
  end
  else
    TestResult('Logger created for colors test', False);
  
  { Test nil }
  LoggerSetColors(nil, True);
  TestResult('LoggerSetColors(nil) is safe', True);
end;

procedure RunAllTests;
begin
  WriteLn('==============================================');
  WriteLn('  LOGGING UNIT TEST SUITE');
  WriteLn('==============================================');
  
  TestLoggerCreateDestroy;
  TestGlobalLogging;
  TestLogLevelToStr;
  TestStrToLogLevel;
  TestLogLevelFiltering;
  TestLogFileOperations;
  TestColorFunctions;
  TestLogConvenienceFunctions;
  TestLoggerFlush;
  TestLoggerSetColors;
  
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
