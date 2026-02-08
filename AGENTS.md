# AGENTS.md - PascalTorrent Project Guide

This file contains essential information for AI agents working on the PascalTorrent project.

## Project Overview

**PascalTorrent** is a BitTorrent client implementation in Free Pascal using purely procedural programming (no OOP). It consists of 8 core modules and comprehensive test suites.

## Quick Reference

### Build Commands
```bash
make all           # Build all tests
make test          # Run all tests (1,082 tests)
make clean         # Clean build artifacts
make coverage      # Generate coverage report
```

### Key Architecture Points

1. **No OOP Allowed**: Use records and pointers only, never classes/objects
2. **Memory Management**: Every allocation must have a corresponding free
3. **Error Handling**: Use Boolean returns and TParseResult records
4. **Cross-Platform**: Windows/Unix socket abstractions in sockwrap.pas

### Module Descriptions

| Module | Purpose | Lines | Functions |
|--------|---------|-------|-----------|
| `bencode.pas` | Bencode encoding/decoding (BitTorrent format) | ~750 | 50+ |
| `metainfo.pas` | Torrent file parsing & metadata management | ~1250 | 80+ |
| `filemgr.pas` | Piece-based file I/O with verification | ~1150 | 70+ |
| `protocol.pas` | Peer Wire Protocol message encoding/decoding | ~800 | 60+ |
| `sockwrap.pas` | Cross-platform TCP socket wrapper | ~950 | 50+ |
| `sha1utils.pas` | SHA1 hashing with progress callbacks | ~450 | 25+ |
| `utils.pas` | General utilities (buffers, strings, timers) | ~1150 | 90+ |
| `logging.pas` | Thread-safe logging infrastructure | ~540 | 30+ |

### Module Dependencies

```
bencode ─────┐
             ├──> utils
sha1utils ───┤
             │
metainfo ────┼──> bencode
             ├──> utils
             └──> sha1utils
             └──> logging

filemgr ─────┬──> metainfo
             ├──> sha1utils
             ├──> utils
             └──> logging

protocol ────┬──> metainfo
             ├──> bencode
             ├──> sha1utils
             ├──> filemgr
             └──> utils

sockwrap ────┬──> utils
             └──> logging
```

## Common Patterns

### Buffer Types

```pascal
type
  PByteArray = ^TByteArray;
  TByteArray = array[0..MaxInt - 1] of Byte;
  
  PBuffer = ^TBuffer;
  TBuffer = record
    Data: PByteArray;
    Size: Cardinal;
    Capacity: Cardinal;
  end;
```

### Progress Callback Pattern
```pascal
type
  TProgressCallback = procedure(Percent: Integer);
  
// Usage in functions
function LongRunningOperation(Callback: TProgressCallback): Boolean;
begin
  for I := 0 to Total do
  begin
    { do work }
    if Assigned(Callback) then
      Callback((I * 100) div Total);
  end;
end;
```

### Result Record Pattern
```pascal
type
  TMyResult = record
    Success: Boolean;
    ErrorMsg: string;
    { other fields }
  end;
```

### Handle Pattern
```pascal
type
  PMyHandle = ^TMyHandleRecord;
  TMyHandleRecord = record
    { fields }
  end;

function MyCreate: PMyHandle;
procedure MyDestroy(var Handle: PMyHandle);
```

### Buffer Safety Pattern
```pascal
// Always check buffer bounds
if Offset + Count > BufferLen then
begin
  Result.Success := False;
  Result.ErrorMsg := 'Buffer overflow';
  Exit;
end;
```

## Known Issues to Watch

### Critical Bugs to Avoid

1. **Hash Indexing**: `Meta^.Pieces` is PByteArray (byte-indexed), not array of TSHA1Digest
   - Wrong: `Move(PieceHashes[P], Meta^.Pieces^[P], 20)`
   - Right: `Move(PieceHashes[P], Meta^.Pieces^[P * SHA1_HASH_SIZE], 20)`
   - Note: This was a real bug that caused hash verification failures!

2. **String Indexing in Pascal**: Strings are 1-indexed, not 0-indexed
   - First character is at position 1: `S[1]`
   - Use `S[Low(S)]` for first char to be explicit

3. **Move() Parameters**: Order is source, destination, count
   - Wrong: `Move(Dest, Src, Count)`
   - Right: `Move(Src, Dest, Count)`

### Platform-Specific Issues

4. **Socket Non-Blocking Connect**: On macOS, returns SOCK_OK, SOCK_ERR_WOULDBLOCK, or CONNECTING
   - Don't assume only SOCK_OK means success
   - Check for `SOCK_ERR_WOULDBLOCK` (macOS/Linux) or `CONNECTING` state

5. **Timer Precision**: `Now` has ~16ms precision on macOS
   - Use `Elapsed > 0.001` for minimum time calculations to avoid division by zero

6. **Stack Depth**: Free Pascal default stack is ~8MB
   - Don't recurse more than ~50 levels deep
   - For deep structures, use heap allocation instead

### Memory Management Pitfalls

7. **Managed Types**: Strings, dynamic arrays are reference-counted
   - No need to manually free them (handled by compiler)
   - But be careful with circular references

8. **Untyped Pointers**: `Pointer` has no type info
   - Always cast to proper type before dereferencing
   - Use `PByteArray` for raw byte buffers

9. **GetMem/FreeMem Pairs**: Every GetMem must have exactly one FreeMem
   - Use `try..finally` to ensure cleanup
   - Set pointer to nil after FreeMem to prevent double-free

## Testing Guidelines

### Test File Naming
- Unit tests: `test_<module>.pas`
- Integration tests: `test_<feature>_integration.pas`
- Error tests: `test_error_scenarios.pas`

### Test Organization
Tests are self-contained Pascal programs (not using a framework):
```pascal
program test_module;

uses
  SysUtils, module_name;

var
  Passed: Integer = 0;
  Failed: Integer = 0;

// Individual test procedures
procedure TestFeature;
begin
  WriteLn('  Test: Feature description...');
  // ... test code ...
  Passed := Passed + 1;
  WriteLn('    PASS');
end;

// Main
begin
  WriteLn('==============================================');
  WriteLn('  MODULE UNIT TEST SUITE');
  WriteLn('==============================================');
  WriteLn;
  
  TestFeature;
  // ... more tests ...
  
  WriteLn;
  WriteLn('==============================================');
  WriteLn('  RESULTS: ', Passed, '/', Passed + Failed, ' tests passed');
  WriteLn('==============================================');
  
  if Failed > 0 then
    Halt(1);  // Exit with error code for CI
end.
```

### Test Categories
Each module test file should cover:
- **Basic functionality**: Core features work correctly
- **Edge cases**: Empty inputs, boundary values, maximum sizes
- **Error handling**: Invalid inputs, nil pointers, invalid state
- **Resource cleanup**: Memory freed, files closed, handles released
- **Integration points**: Interactions with other modules

### Test Structure
```pascal
procedure TestSomething;
begin
  WriteLn('  Test: Description...');
  { setup }
  try
    { test code }
    Passed := Passed + 1;
    WriteLn('    PASS');
  except
    Failed := Failed + 1;
    WriteLn('    FAIL');
  end;
  { cleanup }
end;
```

### Required Test Cases
- Normal operation (happy path)
- Empty/null inputs
- Boundary conditions (zero, max values)
- Error conditions (invalid input, OOM scenarios)

## Code Style

### Indentation
- 2 spaces (not tabs)

### Naming Conventions
- Types: `TName` or `PName` (pointer)
- Functions: `VerbNoun` (e.g., `BufferCreate`)
- Constants: `UPPER_CASE`
- Variables: `CamelCase`

### Comments
```pascal
{ Block comment for function explanation }
procedure DoSomething;
begin
  // Inline comment for logic explanation
end;
```

### Error Messages
- Be specific about what went wrong
- Include relevant context (parameter values if safe)
- Use consistent phrasing: "Failed to X: reason"

## Cross-Platform Notes

### {$IFDEF} Usage
```pascal
{$IFDEF MSWINDOWS}
  // Windows-specific code
{$ELSE}
  // Unix/Linux/macOS code
{$ENDIF}
```

### macOS Specific
- Non-blocking connect may return CONNECTING (-2)
- Timer precision is limited
- Stack size may differ from Linux

### Windows Specific
- Socket errors use WSAGetLastError
- Path separators are backslash
- Line endings may be CRLF

## Critical Constants

These magic numbers should NOT be hardcoded - use named constants:

```pascal
{ From sha1utils.pas }
const
  SHA1_HASH_SIZE       = 20;           // 20 bytes = 160 bits
  SHA1_HEX_LENGTH      = 40;           // 40 hex characters
  SHA1_BASE32_LENGTH   = 32;           // 32 base32 chars for magnet links

{ From protocol.pas - Message length prefixes }
const
  LEN_KEEPALIVE        = 0;            // 0 bytes - no message ID
  LEN_CHOKE            = 1;            // 1 byte - message ID only
  LEN_UNCHOKE          = 1;            // 1 byte - message ID only
  LEN_INTERESTED       = 1;            // 1 byte - message ID only
  LEN_NOT_INTERESTED   = 1;            // 1 byte - message ID only
  LEN_HAVE             = 5;            // 1 byte ID + 4 bytes piece index
  LEN_REQUEST          = 13;           // 1 + 4 + 4 + 4 (index, begin, length)
  LEN_CANCEL           = 13;           // Same structure as request
  LEN_PIECE_HEADER     = 9;            // 1 + 4 + 4 (index, begin) without data

{ From metainfo.pas }
const
  MD5_HEX_LENGTH       = 32;           // 32 hex chars (128 bits)
  MAX_PIECE_LENGTH     = 16777216;     // 16 MB max piece size
  DEFAULT_PIECE_LENGTH = 262144;       // 256 KB default

{ From filemgr.pas }
const
  DEFAULT_BLOCK_SIZE   = 16384;        // 16 KB standard block size
  MAX_BLOCK_SIZE       = 131072;       // 128 KB max block size
  
{ Protocol message IDs (from protocol.pas) }
const
  MSG_CHOKE            = 0;
  MSG_UNCHOKE          = 1;
  MSG_INTERESTED       = 2;
  MSG_NOT_INTERESTED   = 3;
  MSG_HAVE             = 4;
  MSG_BITFIELD         = 5;
  MSG_REQUEST          = 6;
  MSG_PIECE            = 7;
  MSG_CANCEL           = 8;
  MSG_PORT             = 9;            // DHT extension
```

## Common Tasks

### Adding a New Module
1. Create `src/newmodule.pas`
2. Create `tests/test_newmodule.pas`
3. Add to Makefile
4. Add to `make all` dependency
5. Run `make test` to verify

### Adding a New Constant
1. Define in appropriate module's const section
2. Use UPPER_CASE naming
3. Add comment explaining usage
4. Replace existing magic numbers with new constant

### Fixing a Bug
1. Create minimal reproduction test
2. Verify test fails before fix
3. Apply minimal fix
4. Verify test passes after fix
5. Run full test suite

### Refactoring Code
1. Ensure all tests pass before starting
2. Make incremental changes
3. Run tests after each change
4. Update AGENTS.md if new patterns emerge
5. Document any breaking changes

### Adding Thread Safety
1. Define `LOGGING_THREADSAFE` for conditional compilation
2. Use platform-specific synchronization primitives:
   - Windows: `TRTLCriticalSection` with `InitializeCriticalSection`
   - Unix: `pthread_mutex_t` (or TMutex wrapper)
3. Always pair lock/unlock with try..finally
4. Keep critical sections as short as possible
5. Avoid calling external code while holding locks

## Compiler Flags Reference

### Build Configurations

| Target | Flags | Purpose |
|--------|-------|---------|
| `make all` | `-Mobjfpc -Sh -O2 -gl -vewh` | Standard optimized build |
| `make debug` | `-Mobjfpc -Sh -O1 -g -gl -vewh` | Debug symbols, no optimization |
| `make release` | `-Mobjfpc -Sh -O3 -Xs -vewh` | Maximum optimization, strip symbols |
| `make quick` | `-Mobjfpc -Sh -O1 -vewh` | Fast compile for development |
| `make lint` | `-Mobjfpc -Sh -O2 -g -vewh -vw -Sew -Criot -Co -Ct` | Strict warnings |
| `make coverage` | `-Mobjfpc -Sh -O1 -g -gl -pg -vewh` | Profiling support |
| `make memcheck` | `-Mobjfpc -Sh -O1 -g -gl -gh -vewh` | Heap trace |

### Flag Meanings
- `-Mobjfpc`: Use Object Pascal mode
- `-Sh`: Use ansistrings by default
- `-O2`: Optimization level 2 (balanced)
- `-O3`: Maximum optimization
- `-O1`: Basic optimization (faster compile)
- `-g`: Generate debug info
- `-gl`: Generate line number info
- `-vewh`: Verbose errors/warnings/hints
- `-vw`: Show all warnings
- `-Sew`: Treat warnings as errors
- `-Criot`: Range/overflow/IO/stack checks
- `-Xs`: Strip symbols from executable
- `-pg`: Profile guided optimization
- `-gh`: Heap trace (memory leak detection)

## Common Operations

### Working with Byte Arrays
```pascal
{ Allocate dynamic byte array }
var
  Buffer: PByteArray;
begin
  GetMem(Buffer, Size);
  try
    FillChar(Buffer^, Size, 0);  { Zero fill }
    { use buffer }
  finally
    FreeMem(Buffer);
  end;
end;

{ Copy data between buffers }
Move(Source^, Dest^, Count);

{ Compare buffers }
if CompareByte(Buf1^, Buf2^, Count) = 0 then
  { buffers are equal };
```

### File I/O with Error Handling
```pascal
{ Safe file read }
var
  F: File;
  Buffer: array[0..4095] of Byte;
  BytesRead: Integer;
begin
  Assign(F, Filename);
  {$I-}
  Reset(F, 1);
  {$I+}
  if IOResult <> 0 then
  begin
    Result.ErrorMsg := 'Failed to open file';
    Exit;
  end;
  
  try
    {$I-}
    BlockRead(F, Buffer, SizeOf(Buffer), BytesRead);
    {$I+}
    if IOResult <> 0 then
    begin
      Result.ErrorMsg := 'Failed to read file';
      Exit;
    end;
  finally
    {$I-}
    Close(F);
    {$I+}
  end;
end;
```

### String Operations
```pascal
{ Pascal strings are 1-indexed }
S := 'Hello';
FirstChar := S[1];  { 'H', not S[0] }

{ String length }
Len := Length(S);

{ Substring }
Sub := Copy(S, StartPos, Count);

{ Convert to/from bytes }
SetLength(Str, ByteCount);
Move(Bytes^, Str[1], ByteCount);
```

## Troubleshooting Guide

### Compilation Errors

| Error | Cause | Solution |
|-------|-------|----------|
| `Illegal parameter: -O0` | macOS FPC doesn't support -O0 | Use `-O1` instead |
| `Identifier not found` | Missing unit in uses clause | Add the unit to uses |
| `Type mismatch` | Variable types don't match | Check type declarations |
| `Can't determine which overloaded function` | Ambiguous call | Cast parameters explicitly |
| `Forward declaration not solved` | Missing implementation | Add procedure/function body |

### Runtime Errors

| Error | Cause | Solution |
|-------|-------|----------|
| `Division by zero` | Timing calculations with zero elapsed | Add check: `if Elapsed < 0.001 then Elapsed := 0.001` |
| `Access violation` | Nil pointer dereference | Check `if Ptr = nil then Exit` |
| `Stack overflow` | Too deep recursion | Use heap allocation or limit depth |
| `Out of memory` | Large allocations failing | Check available memory, use streaming |
| `Heap corruption` | Double-free or use-after-free | Set pointer to nil after FreeMem |

### Test Failures

| Symptom | Likely Cause | Solution |
|---------|--------------|----------|
| Hash verification fails | Wrong hash indexing | Use `P * SHA1_HASH_SIZE` not just `P` |
| Socket tests fail on macOS | Non-blocking connect behavior | Accept CONNECTING state as valid |
| File tests fail | Permission issues | Check `/tmp` access, use `mktemp` |
| Timing tests fail | Low precision timers | Increase tolerance thresholds |
| Stack overflow in tests | Deep recursion | Reduce nesting depth (max ~20) |

## Debugging Tips

### Memory Leaks
Use `make memcheck` (if available) or add debug output to constructors/destructors.

### Buffer Overflows
Always validate length parameters. Use assertions in debug builds:
```pascal
{$IFDEF DEBUG}
  Assert(Offset + Count <= BufferLen);
{$ENDIF}
```

### Socket Issues
Enable logging for socket operations. Check `GetLastSocketError` after failures.

### Enabling Debug Output
```pascal
{$DEFINE DEBUG}

{$IFDEF DEBUG}
  WriteLn('Debug: Reached point X, Value = ', Value);
{$ENDIF}
```

### Using the Debugger (FPC/Lazarus)
1. Build with `make debug` (includes `-g` flag)
2. Use `gdb` or Lazarus debugger:
   ```bash
   gdb ./bin/test_module
   (gdb) run
   (gdb) bt  # backtrace on crash
   ```

## File Organization Patterns

### Source File Structure
```pascal
unit module_name;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, { other units };

{ ============================================================================ }
{ Constants                                                                    }
{ ============================================================================ }

const
  MODULE_CONSTANT = value;

{ ============================================================================ }
{ Types                                                                        }
{ ============================================================================ }

type
  PHandle = ^THandle;
  THandle = record
    { fields }
  end;

{ ============================================================================ }
{ Public Functions                                                             }
{ ============================================================================ }

function ModuleCreate: PHandle;
procedure ModuleDestroy(Handle: PHandle);

implementation

{ ============================================================================ }
{ Private Helper Functions                                                     }
{ ============================================================================ }

procedure InternalHelper;
begin
  { implementation }
end;

{ ============================================================================ }
{ Public Function Implementations                                              }
{ ============================================================================ }

function ModuleCreate: PHandle;
begin
  New(Result);
  { initialize }
end;

procedure ModuleDestroy(Handle: PHandle);
begin
  if Handle = nil then Exit;
  { cleanup }
  Dispose(Handle);
end;

initialization
  { module initialization (optional) }

finalization
  { module cleanup (optional) }

end.
```

## Data Flow Examples

### Loading a Torrent File
```
File → BencodeDecodeFile() → PBencodeValue
                               ↓
ParseTorrentBencode() → PTorrentMeta
                               ↓
FileManagerCreate() → PFileManager
                               ↓
FileManagerInitialize() → Ready for download
```

### Receiving a Piece
```
Socket → SocketReceive() → Raw bytes
                              ↓
DecodeWireMessage() → TWireMessage (PIECE)
                              ↓
VerifyPieceHash() → Hash check
                              ↓
FileManagerWritePiece() → Disk
                              ↓
UpdateBitfield() → Mark as have
```

### Saving Progress (Resume)
```
FileManager → SerializeBitfield() → Bytes
                                     ↓
Write to .bitfield file
                                     ↓
On restart: ReadBitfieldFile() → Restore state
```

## Resources

### BitTorrent Specifications
- [BEP 3 - Peer Protocol](http://bittorrent.org/beps/bep_0003.html)
- [BEP 5 - DHT Protocol](http://bittorrent.org/beps/bep_0005.html)
- [BEP 10 - Extension Protocol](http://bittorrent.org/beps/bep_0010.html)
- [BEP 12 - Multitracker](http://bittorrent.org/beps/bep_0012.html)
- [BEP 15 - UDP Tracker](http://bittorrent.org/beps/bep_0015.html)

### Free Pascal Documentation
- [Language Reference](https://www.freepascal.org/docs-html/ref/ref.html)
- [RTL Units](https://www.freepascal.org/docs-html/rtl/)
- [FPC Programmer's Guide](https://www.freepascal.org/docs-html/prog/prog.html)

### Project Documentation
- [Project Docs](docs/) - Architecture decisions, BEP summaries
- [README.md](README.md) - Build instructions, quick start
- [Makefile](Makefile) - Build targets and dependencies

## Questions?

When in doubt:
1. Check existing similar code in the codebase
2. Follow the principle of minimal change
3. Ensure all tests pass
4. Document any new patterns introduced
