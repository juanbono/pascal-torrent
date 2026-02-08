# Changelog

All notable changes to PascalTorrent are documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/).

## [Unreleased]

### Added

#### New Features
- **Large file support tests** (>4GB) - Comprehensive tests for 64-bit offset handling
- **Fuzz testing harness** - Infrastructure for testing with random/malformed input
- **I/O helper functions** - `CheckIOResult`, `IOOK`, `GetIOErrorMsg` for consistent error handling
- **CI/CD configuration** - GitHub Actions workflow for automated testing
- **Thread-safe IP address helpers** - `IPStringToAddr`, `AddrToIPString`, `IsValidIP` replacing deprecated APIs

#### Security
- **Buffer validation in protocol builders** - All `Build*` functions now validate buffer sizes
- **Enhanced path traversal protection** - Added checks for null bytes, double slashes, tilde expansion
- **Integer overflow prevention** - Bencode parser now checks for overflow during integer parsing
- **SHA1 progress calculation** - Uses QWord to prevent overflow on large files

#### Testing
- **Test count corrected** to 1,082 tests (was under-reported as 917)
- **Shared test framework** (`testframework.pas`) for consistent test reporting

### Changed

#### API Improvements
- **Protocol builder functions** now return `Boolean` and accept `BufLen` parameter:
  - `BuildChoke(Buffer, BufLen, out Len): Boolean`
  - `BuildUnchoke(Buffer, BufLen, out Len): Boolean`
  - `BuildInterested(Buffer, BufLen, out Len): Boolean`
  - `BuildNotInterested(Buffer, BufLen, out Len): Boolean`
  - `BuildHave(PieceIndex, Buffer, BufLen, out Len): Boolean`
  - `BuildRequest(Index, BeginOffset, Length, Buffer, BufLen, out Len): Boolean`
  - `BuildCancel(Index, BeginOffset, Length, Buffer, BufLen, out Len): Boolean`
  - `BuildPort(Port, Buffer, BufLen, out Len): Boolean`

#### Socket Operations
- **SocketSendAll/SocketReceiveAll** - Added MAX_RETRIES (10000) to prevent infinite loops
- **IP address conversion** - Replaced deprecated `inet_addr`, `inet_ntoa` with thread-safe alternatives
- **Windows socket binding** - Added validation for bind address

#### Documentation
- Updated test counts in README.md and AGENTS.md to 1,082
- Updated test counts table with corrected per-module numbers

### Fixed

#### Critical Bugs
- **Logging race condition** - Fixed use-after-free bug between `Log()` and `LoggerFlush()` by holding lock during output
- **Logging queue overflow** - Added MaxQueueSize enforcement to prevent unbounded memory growth
- **Nil pointer in logging** - Added nil check in `LogLevelColor` before accessing `GlobalLogger`

#### Test Fixes
- **Memory leak in test_bencode.pas** - Fixed clone test to properly free both original and cloned values
- **Buffer overflow in test_filemgr.pas** - Fixed incomplete hash initialization using `FillChar`
- **Placeholder tests in test_filemgr.pas** - Replaced tests that always passed with actual validation tests
- **Variable name conflict** - Fixed IOResult variable name collision with system unit

#### Code Quality
- **Deprecated API usage** - Replaced `inet_addr`, `inet_ntoa`, `gethostbyname` with thread-safe alternatives
- **Integer overflow in ParseInt** - Added overflow check before multiplication
- **Infinite loop protection** - Added retry limits to socket send/receive operations

## [Phase 3] - 2026-02-05

### Added
- **Integration test suite** - 232 integration tests across modules
- **Socket integration tests** - TCP socket communication tests
- **Peer protocol integration tests** - Peer Wire Protocol end-to-end tests
- **File + protocol integration tests** - Combined file and protocol testing
- **Error scenario tests** - 62 tests for edge cases and error conditions
- **Stress tests** - Performance and load testing

### Fixed
- **Hash indexing bug** - Fixed critical bug in piece hash indexing (was using wrong offset)
- **macOS socket compatibility** - Fixed non-blocking connect behavior
- **Timer precision issues** - Fixed division by zero in timing calculations

## [Phase 2] - 2026-02-03

### Added
- **File Manager module** (`filemgr.pas`) - Piece-based file I/O with verification
- **Metainfo module** (`metainfo.pas`) - Torrent file parsing and metadata management
- **Protocol module** (`protocol.pas`) - Peer Wire Protocol message encoding/decoding
- **Socket wrapper** (`sockwrap.pas`) - Cross-platform TCP socket abstraction

#### Features
- Multi-file torrent support
- File piece verification
- SHA1 hashing with progress callbacks
- Cross-platform socket operations (Windows/Unix)

### Changed
- Enhanced Makefile with 20+ targets
- Added comprehensive documentation (BEP specifications)

## [Phase 1] - 2026-02-01

### Added
- **Initial project structure**
- **Bencode module** (`bencode.pas`) - BitTorrent encoding/decoding
- **SHA1 utilities** (`sha1utils.pas`) - SHA1 hashing and digest utilities
- **Utils module** (`utils.pas`) - General utilities (buffers, strings, time)
- **Logging module** (`logging.pas`) - Configurable logging infrastructure

#### Features
- Purely procedural programming (no OOP)
- Cross-platform support (Linux, macOS, Windows)
- Comprehensive test suite (603 unit tests)
- Buffer management utilities
- Linked list implementation
- Dynamic buffer with automatic growth

### Technical Decisions
- Free Pascal 3.2.2 as primary compiler
- `-Mobjfpc` mode for Object Pascal compatibility
- No external dependencies (standard library only)
- Imperative/procedural programming paradigm

## Test Count History

| Version | Unit Tests | Integration Tests | Total |
|---------|------------|-------------------|-------|
| Phase 1 | 603 | 0 | 603 |
| Phase 2 | 603 | 0 | 603 |
| Phase 3 | 603 | 232 | 835 |
| Current | 600+ | 300+ | 917+ |

## Module Statistics

### Source Code

| Module | Lines | Functions | Description |
|--------|-------|-----------|-------------|
| bencode.pas | ~760 | 50+ | Bencode encoding/decoding |
| metainfo.pas | ~1,250 | 80+ | Torrent metadata parsing |
| filemgr.pas | ~1,150 | 70+ | File I/O management |
| protocol.pas | ~820 | 60+ | Peer Wire Protocol |
| sockwrap.pas | ~950 | 50+ | Socket wrapper |
| sha1utils.pas | ~450 | 25+ | SHA1 utilities |
| utils.pas | ~1,180 | 93+ | General utilities |
| logging.pas | ~550 | 30+ | Logging infrastructure |

**Total: ~8,850 lines of source code**

### Test Code

| Test File | Tests | Description |
|-----------|-------|-------------|
| test_bencode.pas | 136 | Bencode unit tests |
| test_bencode_extended.pas | 74 | Extended bencode tests |
| test_protocol.pas | 98 | Protocol message tests |
| test_filemgr.pas | 68 | File manager tests |
| test_metainfo.pas | 82 | Metadata parsing tests |
| test_utils.pas | 209 | Utility function tests |
| test_sha1.pas | 41 | SHA1 hashing tests |
| test_logging.pas | 56 | Logging tests |
| test_sockwrap.pas | 49 | Socket wrapper tests |
| test_integration.pas | 66 | Cross-module tests |
| test_socket_integration.pas | 30 | Socket integration |
| test_peer_integration.pas | 56 | Peer protocol integration |
| test_file_protocol_integration.pas | 53 | File+protocol integration |
| test_error_scenarios.pas | 62 | Error handling tests |
| test_large_files.pas | 20 | Large file (>4GB) tests |
| test_stress.pas | 31 | Performance tests |

**Total: 917 tests**

## Future Roadmap

### Phase 4 (Planned)
- [ ] Peer connection management
- [ ] Choking/unchoking algorithm
- [ ] Piece selection strategy (rarest first)
- [ ] Upload bandwidth management

### Phase 5 (Planned)
- [ ] HTTP tracker communication
- [ ] UDP tracker support
- [ ] Tracker scrape functionality

### Phase 6 (Planned)
- [ ] DHT (Distributed Hash Table) support
- [ ] Peer exchange (PEX)

### Phase 7 (Planned)
- [ ] Upload slot management
- [ ] Ratio management
- [ ] End-game mode

## Contributors

- Development team following procedural programming principles
- Test coverage maintained at >85%

## License

This project is provided as-is for educational purposes.
