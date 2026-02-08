# Test Framework Migration Plan

**Objective:** Migrate all test files to use the shared `testframework.pas` unit, eliminating code duplication and standardizing test output.

**Current State:** 3 of 17 test files use the framework (test_sha1.pas, test_large_files.pas, test_fuzz.pas)

**Target State:** All 17 test files use the framework

---

## Executive Summary

### Migration Complexity Analysis

| Test File | Lines | Tests | Complexity | Estimated Effort |
|-----------|-------|-------|------------|------------------|
| test_bencode.pas | 650 | 63 | Low | 1 hour |
| test_bencode_extended.pas | 480 | 75 | Low | 1 hour |
| test_protocol.pas | 890 | 99 | Medium | 2 hours |
| test_filemgr.pas | 940 | 95 | Medium | 2 hours |
| test_metainfo.pas | 780 | 93 | Medium | 2 hours |
| test_utils.pas | 850 | 218 | Low | 1.5 hours |
| test_logging.pas | 420 | 64 | Low | 1 hour |
| test_sockwrap.pas | 520 | 53 | Medium | 1.5 hours |
| test_integration.pas | 680 | 67 | Low | 1 hour |
| test_runner.pas | 280 | 31 | Low | 0.5 hours |
| test_bencode_extended.pas | 480 | 75 | Low | 1 hour |
| test_error_scenarios.pas | 620 | 67 | Medium | 1.5 hours |
| test_file_protocol_integration.pas | 540 | 62 | Low | 1 hour |
| test_peer_integration.pas | 580 | 61 | Medium | 1.5 hours |
| test_socket_integration.pas | 420 | 46 | Medium | 1.5 hours |
| test_stress.pas | 680 | 38 | Low | 1 hour |
| **Total** | **9,230** | **1,082** | - | **~20 hours** |

### Risk Assessment
- **Low Risk:** Pure refactoring, no logic changes
- **Test Count Risk:** Must maintain exact test count (1,082)
- **Output Format Risk:** CI/CD may depend on specific output format

---

## Phase 1: Framework Enhancement (Day 1)

### 1.1 Add Missing Assertion Functions

**File:** `tests/testframework.pas`

Add these additional assertions that are commonly needed:

```pascal
{ Assert equality for QWord (for large file sizes) }
procedure AssertEquals(const TestName: string; Expected, Actual: QWord;
                       const Msg: string = '');

{ Assert equality for Double (with epsilon for floating point) }
procedure AssertEquals(const TestName: string; Expected, Actual: Double;
                       Epsilon: Double = 0.0001; const Msg: string = '');

{ Assert that string contains substring }
procedure AssertContains(const TestName: string; const SubStr, Str: string;
                         const Msg: string = '');

{ Assert that string starts with prefix }
procedure AssertStartsWith(const TestName: string; const Prefix, Str: string;
                           const Msg: string = '');

{ Assert that condition becomes true within timeout (ms) }
procedure AssertTrueWithin(const TestName: string; Condition: BooleanFunc;
                           TimeoutMs: Integer; const Msg: string = '');

{ Assert memory buffers are equal }
procedure AssertMemoryEqual(const TestName: string; const Buf1, Buf2: Pointer;
                            Len: Integer; const Msg: string = '');
```

### 1.2 Add Setup/Teardown Hooks

```pascal
type
  TTestProc = procedure;

{ Run test with automatic exception handling }
procedure RunTest(const TestName: string; TestProc: TTestProc);

{ Run test with setup and teardown }
procedure RunTestWithSetup(const TestName: string; TestProc: TTestProc;
                           SetupProc: TTestProc = nil; 
                           TeardownProc: TTestProc = nil);
```

### 1.3 Add Test Categories/Suites Helper

```pascal
{ Run all tests in a category }
procedure BeginCategory(const CategoryName: string);
procedure EndCategory;

{ Skip test with reason }
procedure SkipTest(const TestName, Reason: string);
```

---

## Phase 2: Pilot Migration (Days 2-3)

### 2.1 Migrate test_utils.pas (Simple Case)

**Rationale:** test_utils.pas is straightforward with many simple assertions - perfect for validating the migration pattern.

**Steps:**
1. Add `testframework` to uses clause
2. Remove duplicate `TestResult` procedure and statistics variables
3. Replace `WriteLn('=== Testing X ===')` with `BeginSuite('Testing X')`
4. Add `ExitWithResult` at end
5. Verify all 218 tests still pass

**Validation Checklist:**
- [ ] Compiles without errors
- [ ] Compiles without warnings
- [ ] All 218 tests pass
- [ ] Output format matches expected
- [ ] Exit code correct (0 on pass, 1 on fail)

### 2.2 Migrate test_bencode.pas (Medium Case)

**Rationale:** Tests complex data structures and error cases.

**Steps:**
1. Add `testframework` to uses clause
2. Remove duplicate `TestResult` procedure and statistics variables
3. Convert complex assertions to use framework helpers
4. Use `AssertNotNil` for pointer checks
5. Use `AssertEquals` for value comparisons

**Pattern Examples:**

**Before:**
```pascal
TestResult('Simple string (4:spam)', 
           Result.Success and (Value^.ValueType = btString) and 
           (Value^.StrLen = 4) and (StrLComp(Value^.StrVal, 'spam', 4) = 0));
```

**After:**
```pascal
AssertTrue('Simple string (4:spam) parses', Result.Success);
if Result.Success then
begin
  AssertEquals('String type', Ord(btString), Ord(Value^.ValueType));
  AssertEquals('String length', 4, Value^.StrLen);
  AssertMemoryEqual('String content', Value^.StrVal, PChar('spam'), 4);
end;
```

---

## Phase 3: Core Module Migrations (Days 4-7)

### 3.1 test_protocol.pas (Day 4)

**Challenges:**
- Many binary buffer comparisons
- Complex message structure validation
- Round-trip encoding/decoding tests

**Migration Strategy:**
1. Create helper function `AssertMessageEqual`
2. Use `AssertMemoryEqual` for buffer comparisons
3. Group related tests into suites

### 3.2 test_filemgr.pas (Day 5)

**Challenges:**
- File system dependencies
- Complex setup/teardown for each test
- Error condition testing

**Migration Strategy:**
1. Create `TestTempDir` helper
2. Use `RunTestWithSetup` for file-based tests
3. Ensure proper cleanup with `try..finally`

### 3.3 test_metainfo.pas (Day 6)

**Challenges:**
- Torrent file parsing tests
- Hash comparisons
- Multi-file torrent tests

**Migration Strategy:**
1. Create helper for creating test torrents
2. Use `AssertMemoryEqual` for hash comparisons

### 3.4 test_sockwrap.pas (Day 7)

**Challenges:**
- Network-dependent tests
- Platform-specific code paths
- Timing-sensitive tests

**Migration Strategy:**
1. Use `AssertTrueWithin` for timing tests
2. Group platform tests with `#IFDEF` blocks
3. Add `SkipTest` for network-unavailable scenarios

---

## Phase 4: Integration Test Migrations (Days 8-10)

### 4.1 test_integration.pas (Day 8)

**Focus:** Cross-module integration tests

### 4.2 test_file_protocol_integration.pas (Day 8)

**Focus:** File manager + protocol integration

### 4.3 test_peer_integration.pas (Day 9)

**Focus:** Peer communication tests

### 4.4 test_socket_integration.pas (Day 9)

**Focus:** Socket + protocol integration

### 4.5 test_error_scenarios.pas (Day 10)

**Focus:** Error handling across modules

### 4.6 test_stress.pas (Day 10)

**Focus:** Performance and load tests

---

## Phase 5: Final Migrations (Day 11)

### 5.1 test_bencode_extended.pas

### 5.2 test_logging.pas

### 5.3 test_runner.pas

**Note:** test_runner.pas may need special handling as it's the original Phase 1 test runner.

---

## Phase 6: Verification & Cleanup (Days 12-13)

### 6.1 Full Test Suite Verification

```bash
make clean
make all
make test
```

**Verification Checklist:**
- [ ] All 17 test programs compile
- [ ] Zero compilation warnings
- [ ] All 1,082 tests pass
- [ ] Exit codes correct for all tests
- [ ] CI/CD pipeline passes

### 6.2 Output Format Validation

Ensure consistent output format:

```
==============================================
  TEST SUITE NAME
==============================================

[PASS] Test name 1
[PASS] Test name 2
[FAIL] Test name 3
       Error message

==============================================
  RESULTS: 65/66 tests passed (98.5%)
==============================================
```

### 6.3 Documentation Updates

Update these files with new test patterns:
- `AGENTS.md` - Update testing guidelines
- `README.md` - Verify test count still accurate
- `docs/INTEGRATION_TESTS_GUIDE.md` - Update examples

---

## Migration Patterns Reference

### Pattern 1: Simple Test Replacement

**Before:**
```pascal
procedure TestSomething;
begin
  WriteLn(#10'=== Testing Something ===');
  TestResult('Test name', SomeCondition);
end;
```

**After:**
```pascal
procedure TestSomething;
begin
  BeginSuite('Testing Something');
  AssertTrue('Test name', SomeCondition);
  EndSuite;
end;
```

### Pattern 2: Complex Assertion Replacement

**Before:**
```pascal
TestResult('Complex check', 
           (A = B) and (C <> D) and Assigned(P));
```

**After:**
```pascal
AssertEquals('A equals B', A, B);
AssertNotEquals('C differs from D', C, D);
AssertNotNil('P is assigned', P);
```

### Pattern 3: Test with Setup/Teardown

**Before:**
```pascal
procedure TestFileOperation;
var
  F: File;
begin
  WriteLn(#10'=== Testing File Operation ===');
  Assign(F, 'test.tmp');
  Rewrite(F);
  try
    { test code }
    TestResult('File test', Condition);
  finally
    Close(F);
    DeleteFile('test.tmp');
  end;
end;
```

**After:**
```pascal
procedure SetupFileTest;
begin
  Assign(TestFile, 'test.tmp');
  Rewrite(TestFile);
end;

procedure TeardownFileTest;
begin
  Close(TestFile);
  DeleteFile('test.tmp');
end;

procedure TestFileOperation;
begin
  BeginSuite('Testing File Operation');
  RunTestWithSetup('File test', 
                   @ActualTestProc, 
                   @SetupFileTest, 
                   @TeardownFileTest);
  EndSuite;
end;
```

### Pattern 4: Statistics Variable Removal

**Before:**
```pascal
var
  TotalTests: Integer = 0;
  PassedTests: Integer = 0;
  FailedTests: Integer = 0;

procedure TestResult(...);  { Duplicate implementation }
```

**After:**
```pascal
{ Remove all statistics variables - they come from testframework }

uses
  SysUtils, ..., testframework;
```

### Pattern 5: Main Program Block

**Before:**
```pascal
begin
  WriteLn('==============================================');
  WriteLn('  MODULE UNIT TEST SUITE');
  WriteLn('==============================================');
  WriteLn;
  
  TestFeature1;
  TestFeature2;
  
  WriteLn;
  WriteLn('==============================================');
  WriteLn('  RESULTS: ', PassedTests, '/', TotalTests, ' tests passed');
  WriteLn('==============================================');
  
  if FailedTests > 0 then
    Halt(1);
end.
```

**After:**
```pascal
begin
  BeginSuite('MODULE UNIT TEST SUITE');
  
  TestFeature1;
  TestFeature2;
  
  EndSuite;
  ExitWithResult;
end.
```

---

## Testing the Migration

### Automated Verification Script

Create `tests/verify_migration.sh`:

```bash
#!/bin/bash
# Verify migration completed successfully

set -e

echo "=== Verification Script ==="

# 1. Check all test files use testframework
echo "Checking testframework usage..."
for f in tests/test_*.pas; do
    if ! grep -q "testframework" "$f"; then
        echo "ERROR: $f does not use testframework"
        exit 1
    fi
done
echo "✓ All test files use testframework"

# 2. Check no duplicate TestResult implementations
echo "Checking for duplicate TestResult..."
for f in tests/test_*.pas; do
    count=$(grep -c "procedure TestResult" "$f" || true)
    if [ "$count" -gt 0 ]; then
        echo "ERROR: $f has duplicate TestResult"
        exit 1
    fi
done
echo "✓ No duplicate TestResult implementations"

# 3. Count total tests
echo "Counting tests..."
total=$(grep -r "TestResult\|Assert" tests/test_*.pas | grep -c "('")
echo "Found $total test assertions"

# 4. Build and run
echo "Building tests..."
make clean > /dev/null
make all > /dev/null
echo "✓ Build successful"

echo "Running tests..."
make test
echo "✓ All tests passed"

echo ""
echo "=== Migration Verification Complete ==="
```

---

## Rollback Plan

If issues are discovered during migration:

1. **Immediate Rollback:** Restore from git backup
   ```bash
   git checkout HEAD -- tests/test_*.pas
   ```

2. **Selective Rollback:** Revert specific test file
   ```bash
   git checkout HEAD -- tests/test_bencode.pas
   ```

3. **Partial Migration:** Keep some files using old pattern temporarily

---

## Timeline Summary

| Phase | Days | Deliverable |
|-------|------|-------------|
| 1. Framework Enhancement | 1 | Enhanced testframework.pas |
| 2. Pilot Migration | 2 | test_utils.pas, test_bencode.pas migrated |
| 3. Core Modules | 4 | test_protocol, test_filemgr, test_metainfo, test_sockwrap |
| 4. Integration Tests | 3 | All integration test files migrated |
| 5. Final Migrations | 1 | Remaining files migrated |
| 6. Verification | 2 | Full test suite validation |
| **Total** | **13 days** | All 17 files migrated |

---

## Success Criteria

- ✅ All 17 test files use `testframework.pas`
- ✅ Zero duplicate `TestResult` implementations
- ✅ All 1,082 tests pass
- ✅ Zero compilation warnings
- ✅ Consistent output format across all tests
- ✅ CI/CD pipeline passes
- ✅ Documentation updated

---

## Post-Migration Benefits

1. **Code Reduction:** ~500 lines of duplicated test infrastructure removed
2. **Maintainability:** Changes to test reporting in one place
3. **Consistency:** All tests have same output format
4. **Extensibility:** New assertion types available everywhere
5. **Onboarding:** New developers learn one test pattern

---

*End of Migration Plan*
