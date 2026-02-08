# PascalTorrent Troubleshooting Guide

Common issues and solutions.

---

## Table of Contents

1. [Compilation Issues](#compilation-issues)
2. [Runtime Errors](#runtime-errors)
3. [Test Failures](#test-failures)
4. [Platform-Specific Issues](#platform-specific-issues)
5. [Performance Issues](#performance-issues)
6. [Debugging Tips](#debugging-tips)

---

## Compilation Issues

### Error: `Illegal parameter: -O0`

**Problem:** macOS FPC doesn't support `-O0` optimization level.

**Solution:** Use `-O1` or higher.
```bash
# Wrong
make CFLAGS="-O0"

# Correct
make CFLAGS="-O1"
```

---

### Error: `Identifier not found` / `Unknown identifier`

**Problem:** Missing unit in uses clause.

**Solution:** Add the required unit.
```pascal
uses
  SysUtils, utils, bencode;  // Add missing unit
```

**Common unit dependencies:**
| Function | Unit |
|----------|------|
| `SHA1String` | `sha1utils` |
| `BencodeDecodeString` | `bencode` |
| `SocketCreate` | `sockwrap` |
| `FormatBytes` | `utils` |

---

### Error: `Type mismatch`

**Problem:** Variable types don't match in assignment or parameter.

**Solution:** Check type declarations. Common mismatches:
- `Integer` vs `Int64` - use explicit casts
- `PChar` vs `string` - use `PChar(S)` or string conversion
- `Byte` vs `Char` - use `Ord()` or `Chr()`

---

### Warning: `Variable does not seem to be initialized`

**Problem:** Variable used before assignment.

**Solution:** Initialize before use.
```pascal
// Wrong
var
  Context: TSHA1Context;
begin
  SHA1Update(Context, ...);  // Context not initialized!

// Correct
var
  Context: TSHA1Context;
begin
  SHA1Init(Context);  // Initialize first
  SHA1Update(Context, ...);
```

---

### Error: `Forward declaration not solved`

**Problem:** Function declared but not implemented.

**Solution:** Add implementation or remove declaration.

---

## Runtime Errors

### Access Violation (SIGSEGV)

**Symptom:** Program crashes with "Access violation" or "Segmentation fault"

**Common Causes:**

1. **Nil pointer dereference**
   ```pascal
   // Wrong
   Value := BencodeNewDict;
   BencodeDictAdd(Value, 'key', BencodeNewString('value'));
   // If Value is nil, crash!
   
   // Correct
   Value := BencodeNewDict;
   if Value = nil then Exit;  // Check allocation
   BencodeDictAdd(Value, 'key', BencodeNewString('value'));
   ```

2. **Use after free**
   ```pascal
   // Wrong
   BencodeFree(Value);
   BencodeDictAdd(Value, ...);  // Value is freed!
   
   // Correct
   BencodeFree(Value);
   Value := nil;  // Set to nil after free
   ```

3. **Buffer overflow**
   ```pascal
   // Wrong
   var
     Buffer: array[0..9] of Byte;
   begin
     Move(LargeData, Buffer, 100);  // Overflow!
   
   // Correct
   Move(LargeData, Buffer, Min(100, SizeOf(Buffer)));
   ```

**Debugging:**
```bash
# Build with debug symbols
make debug

# Run with debugger
lldb ./bin/test_name
(lldb) run
(lldb) bt  # Get backtrace on crash
```

---

### Division by Zero

**Symptom:** Runtime error 200 (Division by zero)

**Common Causes:**

1. **Timing calculations with zero elapsed**
   ```pascal
   // Wrong
   Speed := BytesRead / Elapsed;  // Elapsed could be 0
   
   // Correct
   if Elapsed < 0.001 then
     Elapsed := 0.001;  // Minimum time
   Speed := BytesRead / Elapsed;
   ```

2. **Progress calculation with zero total**
   ```pascal
   // Wrong
   Percent := (Current * 100) div Total;  // Total could be 0
   
   // Correct
   if Total > 0 then
     Percent := (Current * 100) div Total
   else
     Percent := 0;
   ```

---

### Out of Memory

**Symptom:** `EOutOfMemory` exception or nil return from allocation

**Common Causes:**

1. **Trying to allocate too large a buffer**
   ```pascal
   // Wrong - might overflow
   GetMem(Buffer, PieceLength);  // Could be huge!
   
   // Correct - check size first
   if PieceLength > MAX_PIECE_SIZE then
   begin
     ErrorMsg := 'Piece too large';
     Exit;
   end;
   GetMem(Buffer, PieceLength);
   ```

2. **Memory leak - not freeing allocations**
   ```pascal
   // Wrong - memory leak
   while HaveWork do
   begin
     New(Node);
     // Use Node
     // Forgot to free!
   end;
   
   // Correct
   while HaveWork do
   begin
     New(Node);
     try
       // Use Node
     finally
       Dispose(Node);
     end;
   end;
   ```

---

### Stack Overflow

**Symptom:** Runtime error 202 (Stack overflow)

**Common Causes:**

1. **Too deep recursion**
   ```pascal
   // Wrong - deep recursion
   procedure ProcessNode(Node: PNode);
   begin
     ProcessNode(Node^.Next);  // Could recurse deeply
   end;
   
   // Correct - use iteration
   procedure ProcessList(Head: PNode);
   var
     Current: PNode;
   begin
     Current := Head;
     while Current <> nil do
     begin
       // Process Current
       Current := Current^.Next;
     end;
   end;
   ```

2. **Large stack allocations**
   ```pascal
   // Wrong - large stack allocation
   var
     Buffer: array[0..999999] of Byte;  // 1MB on stack!
   
   // Correct - allocate on heap
   var
     Buffer: PByteArray;
   begin
     GetMem(Buffer, 1000000);
     try
       // Use Buffer
     finally
       FreeMem(Buffer);
     end;
   end;
   ```

---

## Test Failures

### Test Fails: Hash verification fails

**Problem:** SHA1 hash doesn't match expected value.

**Common Causes:**

1. **Wrong hash indexing**
   ```pascal
   // Wrong - wrong offset calculation
   ExpectedHash := @Meta^.Pieces^[PieceIdx];
   
   // Correct
   ExpectedHash := @Meta^.Pieces^[PieceIdx * SHA1_HASH_SIZE];
   ```

2. **Data corruption**
   - Check buffer boundaries
   - Verify data written matches data read
   - Check for buffer overruns

**Debugging:**
```pascal
// Print actual vs expected
WriteLn('Expected: ', SHA1DigestToHex(ExpectedHash));
WriteLn('Actual:   ', SHA1DigestToHex(ActualHash));
```

---

### Test Fails: File I/O errors

**Problem:** File operations fail in tests.

**Common Causes:**

1. **Permission issues**
   ```bash
   # Check temp directory permissions
   ls -la /tmp
   
   # Use user-writable directory
   export TMPDIR=$HOME/tmp
   mkdir -p $TMPDIR
   ```

2. **File still open from previous test**
   ```pascal
   // Always close files in finally block
   try
     Assign(F, Filename);
     Rewrite(F);
     // Use F
   finally
     Close(F);
   end;
   ```

3. **Files left from previous failed run**
   ```bash
   # Clean up temp files
   rm -rf /tmp/pascaltorrent_test
   ```

---

### Test Fails: Socket tests on macOS

**Problem:** Socket tests fail with connection errors.

**Solution:** macOS may require elevated privileges for low ports.
```pascal
// Use high port numbers (>1024) in tests
Port := 15000 + Random(1000);
```

---

### Test Fails: Timing-related failures

**Problem:** Tests with timing constraints fail intermittently.

**Solution:** Timing tests are inherently flaky. Increase tolerance:
```pascal
// Increase tolerance
TestResult('Operation completes in time', 
           Elapsed < Expected * 2.0);  // 2x tolerance
```

---

## Platform-Specific Issues

### macOS

#### Issue: Non-blocking connect returns different error

**Problem:** On macOS, `connect()` returns `EINPROGRESS` not `EWOULDBLOCK`.

**Solution:** Already handled in sockwrap.pas - check for both.

---

#### Issue: Stack size different from Linux

**Problem:** Default stack size may be smaller.

**Solution:** Reduce stack allocations or increase stack size at compile time:
```bash
# Increase stack size
fpc -Cs65536  # 64KB stack
```

---

#### Issue: Timer precision is ~16ms

**Problem:** `Now` has limited precision on macOS.

**Solution:** Use `GetTickCount` or accept higher timing variance.
```pascal
// Use high precision timer if available
uses
  BaseUnix;
  
function GetHighResTime: Int64;
var
  tv: TTimeVal;
begin
  fpGetTimeOfDay(@tv, nil);
  Result := Int64(tv.tv_sec) * 1000000 + tv.tv_usec;
end;
```

---

### Linux

#### Issue: Different socket error codes

**Problem:** Socket error codes differ between platforms.

**Solution:** Use `SocketErrorString()` to get human-readable messages.

---

#### Issue: File locking behavior

**Problem:** File locking semantics differ.

**Solution:** Use exclusive access patterns, avoid relying on OS-specific locking.

---

### Windows

#### Issue: Path separator

**Problem:** Windows uses `\` not `/`.

**Solution:** Use `JoinPath()` function which handles both.
```pascal
// Correct
Path := JoinPath('dir', 'file.txt');  // Works on both platforms
```

---

#### Issue: Line endings (CRLF vs LF)

**Problem:** Windows uses CRLF line endings.

**Solution:** Open files in binary mode for consistent behavior.
```pascal
Reset(F, 1);  // Binary mode, 1 byte records
```

---

## Performance Issues

### Slow File I/O

**Problem:** File operations are slow.

**Solutions:**

1. **Use larger buffer sizes**
   ```pascal
   // Small buffer - slow
   BlockRead(F, Buffer, 1024);
   
   // Larger buffer - faster
   BlockRead(F, Buffer, 65536);
   ```

2. **Keep files open**
   ```pascal
   // Don't open/close for each operation
   // File manager keeps files open with LRU cache
   ```

3. **Pre-allocate files**
   ```pascal
   // Pre-allocate to reduce fragmentation
   FileManagerPreallocateFiles(FM);
   ```

---

### High Memory Usage

**Problem:** Program uses too much memory.

**Solutions:**

1. **Free unused data structures**
   ```pascal
   // Parse torrent, extract info, then free
   Result := ParseTorrentFile('file.torrent', Meta);
   if Result.Success then
   begin
     try
       // Use Meta
     finally
       FreeTorrentMeta(Meta);  // Free when done
     end;
   end;
   ```

2. **Process pieces incrementally**
   ```pascal
   // Don't load entire file into memory
   // Process piece by piece
   for PieceIdx := 0 to Meta^.PieceCount - 1 do
   begin
     FileManagerReadPiece(FM, PieceIdx, Buffer, ...);
     // Process Buffer
     // Buffer reused for next piece
   end;
   ```

3. **Limit queue sizes**
   ```pascal
   // Logging queue can grow unbounded
   Logger^.MaxQueueSize := 1000;  // Limit queue size
   ```

---

### Slow Hashing

**Problem:** SHA1 hashing is slow.

**Solutions:**

1. **Use incremental hashing for large files**
   ```pascal
   // Instead of loading entire file
   SHA1Init(Context);
   while not EOF(F) do
   begin
     BlockRead(F, Buffer, SizeOf(Buffer), BytesRead);
     SHA1Update(Context, Buffer, BytesRead);
   end;
   SHA1Final(Context, Digest);
   ```

2. **Use progress callback for UI updates**
   ```pascal
   procedure HashProgress(Percent: Integer);
   begin
     UpdateProgressBar(Percent);
   end;
   
   Digest := SHA1File('large.bin', @HashProgress);
   ```

---

## Debugging Tips

### Enable Debug Output

```pascal
{$DEFINE DEBUG}

{$IFDEF DEBUG}
  WriteLn('Debug: Reached point X, Value = ', Value);
{$ENDIF}
```

---

### Use Assertions

```pascal
{$ASSERTIONS ON}

procedure DoSomething(P: Pointer);
begin
  Assert(P <> nil, 'DoSomething: P cannot be nil');
  // Use P
end;
```

---

### Memory Debugging

```bash
# Enable heap trace
make memcheck

# Or compile with -gh
fpc -gh test.pas

# Run and check for leaks
./test 2>&1 | grep "heap trace"
```

---

### Stack Traces on Crash

```bash
# Build with debug info
make debug

# Use debugger
lldb ./bin/test_name
(lldb) run
(lldb) thread backtrace  # Get full stack trace
(lldb) frame variable     # Show local variables
```

---

### Logging for Debugging

```pascal
uses
  logging;

procedure DebugNetwork;
begin
  InitLogging;
  LoggerSetLevel(GlobalLogger, llDebug);
  
  LogDebug('Network', 'Connecting to %s:%d', ['192.168.1.1', 6881]);
  
  // ... network operations ...
  
  LogDebug('Network', 'Connection result: %d', [Result]);
  
  ShutdownLogging;
end;
```

---

### Unit Testing Individual Functions

```pascal
program test_single;

uses
  SysUtils, bencode;

begin
  // Test single function in isolation
  TestDecodeString;
  TestDecodeInteger;
  
  WriteLn('Tests completed');
end.
```

---

### Profiling Performance

```bash
# Build with profiling
make coverage

# Run to generate gmon.out
./bin/test_stress

# Analyze
gprof ./bin/test_stress gmon.out > profile.txt
```

---

## Common Error Messages

| Error | Meaning | Solution |
|-------|---------|----------|
| `IOError 2` | File not found | Check filename and path |
| `IOError 3` | Path not found | Create directories first |
| `IOError 5` | Access denied | Check file permissions |
| `IOError 28` | No space left | Free disk space |
| `Socket error 111` | Connection refused | Check if server is running |
| `Socket error 110` | Connection timeout | Check network/firewall |

---

## Getting Help

1. **Check documentation**
   - `README.md` - Build and basic usage
   - `AGENTS.md` - Development guide
   - `docs/API_REFERENCE.md` - API documentation

2. **Run tests**
   ```bash
   make test
   ```

3. **Enable verbose output**
   ```pascal
   Verbose := True;  // In test files
   ```

4. **Check recent changes**
   ```bash
   git log --oneline -10
   ```

---

*End of Troubleshooting Guide*
