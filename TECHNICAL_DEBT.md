# Technical Debt Register

**Project:** PascalTorrent  
**Last Updated:** 2026-02-07

---

## Debt Items

### TD-001: Incomplete Unix Thread Safety

**Location:** `src/logging.pas`  
**Severity:** Medium  
**Effort:** 2 hours

**Description:**
The logging module has thread safety infrastructure for Windows but only a placeholder for Unix:

```pascal
{$IFDEF MSWINDOWS}
  Sync: TRTLCriticalSection;
{$ELSE}
  Sync: Pointer; { pthread_mutex_t wrapper - TODO }
{$ENDIF}
```

**Impact:**
- Logging may not be thread-safe on Unix/macOS
- Potential for garbled log output in multi-threaded scenarios

**Resolution:**
Implement pthread_mutex_t wrapper for Unix systems.

---

### TD-002: Memory Allocation Without Failure Checks

**Location:** `src/bencode.pas` (lines 1142, 1158, 1165, 1172)  
**Severity:** Low  
**Effort:** 4 hours

**Description:**
Several `New()` calls don't check for allocation failure:

```pascal
New(Result);  { No nil check }
Result^.ValueType := btString;
```

**Impact:**
- On memory exhaustion, will crash instead of gracefully failing
- Modern systems rarely exhaust memory for small allocations

**Resolution:**
Add nil checks after New() or use exception handling.

---

### TD-003: Buffer Size Validation Missing

**Location:** `src/protocol.pas` (Build* functions)  
**Severity:** Medium  
**Effort:** 4 hours

**Description:**
Protocol message builders don't validate buffer sizes:

```pascal
procedure BuildChoke(Buffer: PByteArray; out Len: Integer);
{ No BufLen parameter - can't validate Buffer is large enough }
```

**Impact:**
- Potential buffer overflows if caller provides insufficient buffer
- Security risk if accepting untrusted input

**Resolution:**
Add buffer size parameter to all Build* functions.

---

### TD-004: Large File Testing Gap

**Location:** Test suite  
**Severity:** Medium  
**Effort:** 2 hours

**Description:**
No tests for files >4GB (32-bit limit boundary).

**Impact:**
- Potential undetected integer overflow issues
- 64-bit offset calculations not verified

**Resolution:**
Add stress tests with simulated large file operations.

---

### TD-005: Unit Size

**Location:** `src/utils.pas`, `src/metainfo.pas`  
**Severity:** Low  
**Effort:** 8 hours

**Description:**
Two units exceed 1000 lines:
- utils.pas: 1150 lines, 90 functions
- metainfo.pas: 1250 lines, 80 functions

**Impact:**
- Reduced maintainability
- Slower compilation
- Harder to navigate

**Resolution:**
Split into smaller focused units:
- utils.pas → stringutils.pas, fileutils.pas, timeutils.pas
- metainfo.pas → metaparser.pas, metautils.pas

---

### TD-006: Duplicate IOResult Handling

**Location:** Multiple files  
**Severity:** Low  
**Effort:** 2 hours

**Description:**
Pattern repeated 20+ times:

```pascal
{$I-}
SomeFileOp;
{$I+}
if IOResult <> 0 then
begin
  Result.ErrorMsg := '...';
  Exit;
end;
```

**Impact:**
- Code bloat
- Inconsistent error messages
- Harder to maintain

**Resolution:**
Create helper functions:

```pascal
function CheckIOResult(const Operation: string): Boolean;
function CheckIOResultMsg(const Operation, Msg: string): Boolean;
```

---

### TD-007: Missing Fuzz Testing

**Location:** Test suite  
**Severity:** Medium  
**Effort:** 4 hours

**Description:**
No randomized input testing for parsers (bencode, protocol).

**Impact:**
- Undiscovered parsing edge cases
- Potential security vulnerabilities

**Resolution:**
Add fuzz test entry points:

```pascal
procedure FuzzBencodeDecode(const Data: PByte; Len: Integer);
procedure FuzzProtocolDecode(const Data: PByte; Len: Integer);
```

---

### TD-008: Hardcoded Limits

**Location:** Various  
**Severity:** Low  
**Effort:** 2 hours

**Description:**
Some magic numbers remain:
- Retry counts (3, 5, 10)
- Buffer sizes (1024, 4096)
- Timeout values (1000, 5000)

**Impact:**
- Less configurable
- Harder to tune for different environments

**Resolution:**
Move to constants or configuration.

---

### TD-009: Test Code Duplication

**Location:** Test files  
**Severity:** Low  
**Effort:** 6 hours

**Description:**
16 test files duplicate test framework code. Partially addressed with testframework.pas.

**Impact:**
- ~180 lines of duplicated code
- Inconsistent test output format

**Resolution:**
Complete migration to testframework.pas for all test files.

---

### TD-010: No Async I/O

**Location:** `src/filemgr.pas`, `src/sockwrap.pas`  
**Severity:** Low  
**Effort:** 16 hours

**Description:**
All I/O is synchronous (blocking).

**Impact:**
- Lower performance for concurrent operations
- Thread blocking during I/O

**Resolution:**
Implement async I/O with callbacks or use threading.

---

## Debt Metrics

| Severity | Count | Total Effort |
|----------|-------|--------------|
| High | 0 | 0h |
| Medium | 4 | 12h |
| Low | 6 | 32h |
| **Total** | **10** | **44h** |

---

## Payback Strategy

### Phase 1: Critical (Sprint 1)
- [ ] TD-003: Buffer size validation
- [ ] TD-004: Large file testing

### Phase 2: Important (Sprint 2)
- [ ] TD-001: Unix thread safety
- [ ] TD-002: Memory allocation checks
- [ ] TD-007: Fuzz testing

### Phase 3: Cleanup (Sprint 3)
- [ ] TD-005: Unit splitting
- [ ] TD-006: IOResult helpers
- [ ] TD-009: Complete testframework migration
- [ ] TD-008: Hardcoded limits

### Phase 4: Enhancement (Future)
- [ ] TD-010: Async I/O

---

*Technical debt should be addressed incrementally to prevent accumulation*
