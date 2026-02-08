{
  testframework.pas - Shared test framework for PascalTorrent
  
  Provides common test utilities to eliminate duplication across test files.
  This unit is designed to be used by all test programs.
}

unit testframework;

{$mode objfpc}{$H+}

interface

uses
  SysUtils;

{ ============================================================================ }
{ Test Statistics                                                              }
{ ============================================================================ }

var
  TotalTests: Integer = 0;
  PassedTests: Integer = 0;
  FailedTests: Integer = 0;
  CurrentSuite: string = '';

{ ============================================================================ }
{ Basic Test Assertions                                                        }
{ ============================================================================ }

{ Record a test result }
procedure TestResult(const TestName: string; Passed: Boolean; 
                     const Msg: string = '');

{ Assert that condition is true }
procedure AssertTrue(const TestName: string; Condition: Boolean;
                     const Msg: string = '');

{ Assert that condition is false }
procedure AssertFalse(const TestName: string; Condition: Boolean;
                      const Msg: string = '');

{ Assert equality for integers }
procedure AssertEquals(const TestName: string; Expected, Actual: Integer;
                       const Msg: string = '');

{ Assert equality for strings }
procedure AssertEquals(const TestName: string; const Expected, Actual: string;
                       const Msg: string = '');

{ Assert that pointer is not nil }
procedure AssertNotNil(const TestName: string; Ptr: Pointer;
                       const Msg: string = '');

{ Assert that pointer is nil }
procedure AssertNil(const TestName: string; Ptr: Pointer;
                    const Msg: string = '');

{ ============================================================================ }
{ Test Suite Management                                                        }
{ ============================================================================ }

{ Begin a new test suite }
procedure BeginSuite(const SuiteName: string);

{ End current test suite }
procedure EndSuite;

{ ============================================================================ }
{ Test Summary                                                                 }
{ ============================================================================ }

{ Print test summary }
procedure TestSummary;

{ Check if all tests passed }
function AllTestsPassed: Boolean;

{ Exit with appropriate error code }
procedure ExitWithResult;

{ ============================================================================ }
{ Utility Functions                                                            }
{ ============================================================================ }

{ Format time for display }
function FormatTime(Milliseconds: Double): string;

{ ============================================================================ }
{ Extended Assertions (added during migration)                                   }
{ ============================================================================ }

{ Assert equality for QWord (for large file sizes) }
procedure AssertEquals(const TestName: string; Expected, Actual: QWord;
                       const Msg: string = '');

{ Assert not equal for integers }
procedure AssertNotEquals(const TestName: string; Expected, Actual: Integer;
                          const Msg: string = '');

{ Assert that pointer is not nil - alias for AssertNotNil for convenience }
procedure AssertAssigned(const TestName: string; Ptr: Pointer;
                         const Msg: string = '');

implementation

{ ============================================================================ }
{ Basic Test Assertions                                                        }
{ ============================================================================ }

procedure TestResult(const TestName: string; Passed: Boolean; 
                     const Msg: string = '');
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

procedure AssertTrue(const TestName: string; Condition: Boolean;
                     const Msg: string = '');
begin
  if Condition then
    TestResult(TestName, True)
  else
  begin
    if Msg <> '' then
      TestResult(TestName, False, Msg)
    else
      TestResult(TestName, False, 'Expected true, got false');
  end;
end;

procedure AssertFalse(const TestName: string; Condition: Boolean;
                      const Msg: string = '');
begin
  if not Condition then
    TestResult(TestName, True)
  else
  begin
    if Msg <> '' then
      TestResult(TestName, False, Msg)
    else
      TestResult(TestName, False, 'Expected false, got true');
  end;
end;

procedure AssertEquals(const TestName: string; Expected, Actual: Integer;
                       const Msg: string = '');
var
  FullMsg: string;
begin
  if Expected = Actual then
    TestResult(TestName, True)
  else
  begin
    FullMsg := Format('Expected %d, got %d', [Expected, Actual]);
    if Msg <> '' then
      FullMsg := FullMsg + ' - ' + Msg;
    TestResult(TestName, False, FullMsg);
  end;
end;

procedure AssertEquals(const TestName: string; const Expected, Actual: string;
                       const Msg: string = '');
var
  FullMsg: string;
begin
  if Expected = Actual then
    TestResult(TestName, True)
  else
  begin
    FullMsg := Format('Expected "%s", got "%s"', [Expected, Actual]);
    if Msg <> '' then
      FullMsg := FullMsg + ' - ' + Msg;
    TestResult(TestName, False, FullMsg);
  end;
end;

procedure AssertNotNil(const TestName: string; Ptr: Pointer;
                       const Msg: string = '');
begin
  if Ptr <> nil then
    TestResult(TestName, True)
  else
  begin
    if Msg <> '' then
      TestResult(TestName, False, Msg)
    else
      TestResult(TestName, False, 'Expected non-nil pointer');
  end;
end;

procedure AssertNil(const TestName: string; Ptr: Pointer;
                    const Msg: string = '');
begin
  if Ptr = nil then
    TestResult(TestName, True)
  else
  begin
    if Msg <> '' then
      TestResult(TestName, False, Msg)
    else
      TestResult(TestName, False, 'Expected nil pointer');
  end;
end;

{ ============================================================================ }
{ Test Suite Management                                                        }
{ ============================================================================ }

procedure BeginSuite(const SuiteName: string);
begin
  CurrentSuite := SuiteName;
  WriteLn('');
  WriteLn('==============================================');
  WriteLn('  ', SuiteName);
  WriteLn('==============================================');
end;

procedure EndSuite;
begin
  WriteLn('');
  CurrentSuite := '';
end;

{ ============================================================================ }
{ Test Summary                                                                 }
{ ============================================================================ }

procedure TestSummary;
var
  Percent: Double;
begin
  WriteLn('');
  WriteLn('==============================================');
  if TotalTests > 0 then
    Percent := (PassedTests * 100.0) / TotalTests
  else
    Percent := 0;
  WriteLn(Format('  RESULTS: %d/%d tests passed (%.1f%%)', 
                 [PassedTests, TotalTests, Percent]));
  WriteLn('==============================================');
  
  if FailedTests = 0 then
    WriteLn('SUCCESS: All tests passed!')
  else
    WriteLn(Format('FAILURE: %d test(s) failed', [FailedTests]));
end;

function AllTestsPassed: Boolean;
begin
  Result := (TotalTests > 0) and (FailedTests = 0);
end;

procedure ExitWithResult;
begin
  TestSummary;
  if FailedTests > 0 then
    Halt(1)
  else
    Halt(0);
end;

{ ============================================================================ }
{ Utility Functions                                                            }
{ ============================================================================ }

function FormatTime(Milliseconds: Double): string;
begin
  if Milliseconds < 1 then
    Result := Format('%.3f μs', [Milliseconds * 1000])
  else if Milliseconds < 1000 then
    Result := Format('%.3f ms', [Milliseconds])
  else
    Result := Format('%.3f s', [Milliseconds / 1000]);
end;

{ ============================================================================ }
{ Extended Assertions Implementation                                             }
{ ============================================================================ }

procedure AssertEquals(const TestName: string; Expected, Actual: QWord;
                       const Msg: string = '');
var
  FullMsg: string;
begin
  if Expected = Actual then
    TestResult(TestName, True)
  else
  begin
    FullMsg := Format('Expected %u, got %u', [Expected, Actual]);
    if Msg <> '' then
      FullMsg := FullMsg + ' - ' + Msg;
    TestResult(TestName, False, FullMsg);
  end;
end;

procedure AssertNotEquals(const TestName: string; Expected, Actual: Integer;
                          const Msg: string = '');
var
  FullMsg: string;
begin
  if Expected <> Actual then
    TestResult(TestName, True)
  else
  begin
    FullMsg := Format('Expected different from %d', [Expected]);
    if Msg <> '' then
      FullMsg := FullMsg + ' - ' + Msg;
    TestResult(TestName, False, FullMsg);
  end;
end;

procedure AssertAssigned(const TestName: string; Ptr: Pointer;
                         const Msg: string = '');
begin
  { Just an alias for AssertNotNil for more natural language }
  AssertNotNil(TestName, Ptr, Msg);
end;

end.
