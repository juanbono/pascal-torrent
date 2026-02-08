# Integration Tests Guide

**Quick reference for running and understanding PascalTorrent integration tests.**

---

## Running Tests

### Run All Tests
```bash
make test
```

### Run Specific Integration Tests
```bash
# Socket communication tests
make test-socket-integration

# Peer protocol tests
make test-peer-integration

# File + protocol integration tests
make test-file-protocol-integration

# Error scenario tests
make test-error-scenarios

# Performance/stress tests
make test-stress
```

---

## Test Descriptions

### test_socket_integration
Tests actual TCP socket communication.

**Key Scenarios:**
- Client-server connection establishment
- 64KB data transfer
- Non-blocking I/O
- Connection closed detection

**Expected Runtime:** ~5 seconds

---

### test_peer_integration
Tests Peer Wire Protocol over sockets.

**Key Scenarios:**
- Handshake exchange
- Bitfield exchange
- Choke/unchoke state machine
- Request/Piece message flow
- Have announcements

**Expected Runtime:** ~3 seconds

---

### test_file_protocol_integration
Tests file operations with protocol messages.

**Key Scenarios:**
- Piece write via protocol
- Multi-piece download simulation
- Block-level operations
- Resume capability

**Expected Runtime:** ~2 seconds

---

### test_error_scenarios
Tests error handling across all modules.

**Key Scenarios:**
- Malformed messages
- Invalid piece indices
- Buffer overflows
- Socket errors

**Expected Runtime:** ~1 second

---

### test_stress
Performance and stress tests.

**Key Scenarios:**
- 1000+ piece torrents
- Rapid create/free cycles
- Protocol encoding performance
- SHA1 performance
- File I/O throughput

**Expected Runtime:** ~30 seconds

---

## Test Architecture

### Unit Tests vs Integration Tests

| Unit Tests | Integration Tests |
|------------|-------------------|
| Test single module | Test multiple modules |
| Mock dependencies | Use real dependencies |
| Fast execution | Slower execution |
| Example: test_protocol.pas | Example: test_peer_integration.pas |

### Test Dependencies

```
test_socket_integration
  └── sockwrap.pas, utils.pas

test_peer_integration
  └── protocol.pas, sockwrap.pas, sha1utils.pas, utils.pas

test_file_protocol_integration
  └── bencode.pas, metainfo.pas, filemgr.pas, protocol.pas

test_error_scenarios
  └── All modules

test_stress
  └── All modules
```

---

## Writing New Integration Tests

### Template Structure

```pascal
program test_new_integration;

{$mode objfpc}{$H+}

uses
  SysUtils, {required units};

var
  TotalTests: Integer = 0;
  PassedTests: Integer = 0;

procedure TestResult(const TestName: string; Passed: Boolean);
begin
  Inc(TotalTests);
  if Passed then
  begin
    Inc(PassedTests);
    WriteLn('[PASS] ', TestName);
  end
  else
    WriteLn('[FAIL] ', TestName);
end;

{ Test procedures here }

begin
  WriteLn('==============================================');
  WriteLn('  TEST NAME');
  WriteLn('==============================================');
  
  { Run tests }
  
  WriteLn(#10'==============================================');
  WriteLn('  RESULTS: ', PassedTests, '/', TotalTests, ' tests passed');
  WriteLn('==============================================');
  
  if PassedTests < TotalTests then
    Halt(1);
end.
```

### Best Practices

1. **Use temporary directories** for file operations
2. **Clean up** after tests (files, memory)
3. **Set timeouts** for socket operations
4. **Handle errors** gracefully
5. **Make tests independent** - no ordering dependencies

---

## Troubleshooting

### Socket Tests Fail
- Check firewall settings
- Ensure port range 45000-55000 is available
- Run with sufficient permissions

### Stress Tests Fail
- May require more memory
- Timing-sensitive - may vary by system load
- Not critical for functionality

### File Tests Fail
- Check disk space
- Ensure temp directory is writable
- Verify no file locks from previous runs

---

## Test Coverage Matrix

| Feature | Unit Test | Integration Test |
|---------|-----------|------------------|
| Bencode | ✅ test_bencode | ✅ test_error_scenarios |
| SHA1 | ✅ test_sha1 | ✅ test_peer_integration |
| Utils | ✅ test_utils | ✅ Indirect |
| Logging | ✅ test_logging | ❌ N/A |
| Metainfo | ✅ test_metainfo | ✅ test_integration |
| File Manager | ✅ test_filemgr | ✅ test_file_protocol_integration |
| Protocol | ✅ test_protocol | ✅ test_peer_integration |
| Sockets | ✅ test_sockwrap | ✅ test_socket_integration |

---

## Continuous Integration

For CI systems, run tests in order:

```bash
# Fast tests first
make test-bencode
gmake test-sha1
gmake test-utils
gmake test-logging
make test-metainfo
gmake test-filemgr
gmake test-protocol
gmake test-sockwrap

# Integration tests
make test-socket-integration
gmake test-peer-integration
gmake test-file-protocol-integration
gmake test-error-scenarios

# Slow tests last
make test-stress
```

---

*Last updated: 2026-02-07*
