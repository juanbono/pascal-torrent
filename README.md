# PascalTorrent

A BitTorrent client implementation in Free Pascal, written using purely imperative/procedural programming techniques.

## Overview

PascalTorrent is a complete BitTorrent client implementation that avoids object-oriented programming constructs in favor of records, pointers, and procedural programming. This design choice makes it suitable for embedded systems, educational purposes, or environments where OOP overhead is undesirable.

### Key Features

- **Purely Procedural**: No classes, objects, or methods - only records and procedures
- **Cross-Platform**: Supports Linux, macOS, and Windows
- **Modular Design**: Clean separation of concerns across 8 core modules
- **Comprehensive Testing**: 1,082 tests with high code coverage
- **BitTorrent Protocol Compliance**: Implements BEP 3 (Peer Protocol) and related specs

## Architecture

```
┌─────────────────────────────────────────────────────────────┐
│                        PascalTorrent                         │
├─────────────────────────────────────────────────────────────┤
│  bencode.pas    - Bencode encoding/decoding (BitTorrent fmt)│
│  metainfo.pas   - Torrent file parsing & metadata          │
│  filemgr.pas    - File I/O with piece management           │
│  protocol.pas   - Peer Wire Protocol messages              │
│  sockwrap.pas   - TCP socket wrapper (cross-platform)      │
│  sha1utils.pas  - SHA1 hashing for piece verification      │
│  utils.pas      - General utilities (buffers, strings)     │
│  logging.pas    - Logging infrastructure                   │
└─────────────────────────────────────────────────────────────┘
```

## Building

### Prerequisites

- Free Pascal Compiler (FPC) 3.2.2 or later
- Make utility
- 100MB free disk space

### Build All Tests

```bash
make all
```

### Run All Tests

```bash
make test
```

### Build Individual Components

```bash
make test-bencode    # Bencode unit tests
make test-protocol   # Protocol unit tests
make test-filemgr    # File manager tests
# ... etc
```

### Debug Build

```bash
make debug
```

### Release Build

```bash
make release
```

## Testing

The project includes comprehensive test coverage:

| Module | Tests | Status |
|--------|-------|--------|
| bencode | 138 | ✅ Passing |
| protocol | 99 | ✅ Passing |
| filemgr | 95 | ✅ Passing |
| metainfo | 93 | ✅ Passing |
| utils | 209 | ✅ Passing |
| sha1utils | 42 | ✅ Passing |
| logging | 64 | ✅ Passing |
| sockwrap | 53 | ✅ Passing |
| error-scenarios | 67 | ✅ Passing |
| large-files | 20 | ✅ Passing |
| **Integration** | **193** | ✅ **Passing** |
| **Total** | **1,082** | ✅ **Passing** |

### Running Integration Tests

```bash
# Socket communication tests
make test-socket-integration

# Peer protocol tests
make test-peer-integration

# File + protocol integration
make test-file-protocol-integration

# Error scenarios
make test-error-scenarios

# Performance/stress tests
make test-stress
```

## Project Structure

```
pascaltorrent/
├── src/                    # Source code
│   ├── bencode.pas        # Bencode codec
│   ├── metainfo.pas       # Torrent metadata
│   ├── filemgr.pas        # File management
│   ├── protocol.pas       # Peer protocol
│   ├── sockwrap.pas       # Socket wrapper
│   ├── sha1utils.pas      # SHA1 utilities
│   ├── utils.pas          # General utilities
│   └── logging.pas        # Logging
│
├── tests/                  # Test suites
│   ├── test_*.pas         # Unit tests
│   └── test_*_integration.pas  # Integration tests
│
├── docs/                   # Documentation
│   ├── BEP_003.md         # BitTorrent Protocol
│   ├── BEP_005.md         # DHT Protocol
│   └── ...
│
├── bin/                    # Build output (generated)
├── Makefile               # Build system
└── README.md              # This file
```

## Usage Example

```pascal
program download_torrent;

uses
  bencode, metainfo, filemgr, sha1utils, utils;

var
  Root: PBencodeValue;
  Meta: PTorrentMeta;
  FM: PFileManager;
  Result: TParseResult;
begin
  { Load and parse torrent file }
  Result := BencodeDecodeFile('example.torrent', Root);
  if not Result.Success then
  begin
    WriteLn('Failed to load torrent');
    Exit;
  end;
  
  { Parse metadata }
  Result := ParseTorrentBencode(Root, Meta);
  if not Result.Success then
  begin
    WriteLn('Failed to parse: ', Result.ErrorMsg);
    BencodeFree(Root);
    Exit;
  end;
  
  { Create file manager }
  if not FileManagerCreate(Meta, './downloads', FM) then
  begin
    WriteLn('Failed to create file manager');
    FreeTorrentMeta(Meta);
    BencodeFree(Root);
    Exit;
  end;
  
  FileManagerInitialize(FM);
  
  { ... proceed with download ... }
  
  { Cleanup }
  FileManagerDestroy(FM);
  FreeTorrentMeta(Meta);
  BencodeFree(Root);
end.
```

## Documentation

- [BitTorrent Protocol Specification](docs/BEP_003.md) - BEP 3
- [DHT Protocol](docs/BEP_005.md) - BEP 5
- [Integration Tests Guide](docs/INTEGRATION_TESTS_GUIDE.md)

## Development Guidelines

### Coding Standards

1. **No OOP**: Use records and pointers, not classes
2. **Explicit Memory Management**: Always pair allocations with deallocations
3. **Error Handling**: Use Boolean returns and result records
4. **Documentation**: Document all public functions
5. **Testing**: Add tests for new functionality

### Adding a New Feature

1. Write tests first (`tests/test_feature.pas`)
2. Implement in appropriate module under `src/`
3. Add integration tests if cross-module
4. Update documentation
5. Run `make test` to verify

## Roadmap

### Completed ✅
- Phase 1: Foundation (bencode, SHA1, utils)
- Phase 2: File Management & Metadata
- Phase 3: Networking & Protocol

### In Progress 🚧
- Phase 4: Peer Connection Management

### Planned 📋
- Phase 5: Tracker Communication (HTTP/UDP)
- Phase 6: DHT Support
- Phase 7: Upload/Ratio Management

## Contributing

Contributions are welcome! Please:

1. Ensure tests pass (`make test`)
2. Follow existing code style
3. Add tests for new functionality
4. Update relevant documentation

## License

This project is provided as-is for educational purposes.

## Acknowledgments

- BitTorrent Protocol specifications from [BEPs](http://bittorrent.org/beps/bep_0000.html)
- Free Pascal Compiler team
