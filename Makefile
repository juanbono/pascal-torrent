# Makefile for PascalTorrent - Phase 1

# Compiler settings
FPC = fpc
FPCFLAGS = -Mobjfpc -Sh -O2 -gl -vewh

# Directories
SRCDIR = src
TESTDIR = tests
BINDIR = bin

# Source files
BENCODE_SRC = $(SRCDIR)/bencode.pas
SHA1UTILS_SRC = $(SRCDIR)/sha1utils.pas
UTILS_SRC = $(SRCDIR)/utils.pas
LOGGING_SRC = $(SRCDIR)/logging.pas
METAINFO_SRC = $(SRCDIR)/metainfo.pas
FILEMGR_SRC = $(SRCDIR)/filemgr.pas
PROTOCOL_SRC = $(SRCDIR)/protocol.pas

# Test executables
TEST_BENCODE = $(BINDIR)/test_bencode
TEST_BENCODE_EXTENDED = $(BINDIR)/test_bencode_extended
TEST_SHA1 = $(BINDIR)/test_sha1
TEST_UTILS = $(BINDIR)/test_utils
TEST_LOGGING = $(BINDIR)/test_logging
TEST_RUNNER = $(BINDIR)/test_runner
TEST_METAINFO = $(BINDIR)/test_metainfo
TEST_FILEMGR = $(BINDIR)/test_filemgr
TEST_PROTOCOL = $(BINDIR)/test_protocol
TEST_INTEGRATION = $(BINDIR)/test_integration

# Default target
.PHONY: all clean test dirs debug release

all: dirs $(TEST_RUNNER) $(TEST_BENCODE) $(TEST_BENCODE_EXTENDED) $(TEST_SHA1) $(TEST_UTILS) $(TEST_LOGGING) $(TEST_METAINFO) $(TEST_FILEMGR) $(TEST_PROTOCOL) $(TEST_INTEGRATION)

dirs:
	@mkdir -p $(BINDIR)

# Test executables
$(TEST_BENCODE): $(TESTDIR)/test_bencode.pas $(BENCODE_SRC)
	$(FPC) $(FPCFLAGS) -Fu$(SRCDIR) -o$@ $<

$(TEST_BENCODE_EXTENDED): $(TESTDIR)/test_bencode_extended.pas $(BENCODE_SRC)
	$(FPC) $(FPCFLAGS) -Fu$(SRCDIR) -o$@ $<

$(TEST_SHA1): $(TESTDIR)/test_sha1.pas $(SHA1UTILS_SRC)
	$(FPC) $(FPCFLAGS) -Fu$(SRCDIR) -o$@ $<

$(TEST_UTILS): $(TESTDIR)/test_utils.pas $(UTILS_SRC)
	$(FPC) $(FPCFLAGS) -Fu$(SRCDIR) -o$@ $<

$(TEST_LOGGING): $(TESTDIR)/test_logging.pas $(LOGGING_SRC)
	$(FPC) $(FPCFLAGS) -Fu$(SRCDIR) -o$@ $<

$(TEST_RUNNER): $(TESTDIR)/test_runner.pas $(BENCODE_SRC) $(SHA1UTILS_SRC) $(UTILS_SRC)
	$(FPC) $(FPCFLAGS) -Fu$(SRCDIR) -o$@ $<

$(TEST_METAINFO): $(TESTDIR)/test_metainfo.pas $(METAINFO_SRC) $(BENCODE_SRC) $(SHA1UTILS_SRC) $(UTILS_SRC) $(LOGGING_SRC)
	$(FPC) $(FPCFLAGS) -Fu$(SRCDIR) -o$@ $<

$(TEST_FILEMGR): $(TESTDIR)/test_filemgr.pas $(FILEMGR_SRC) $(METAINFO_SRC) $(BENCODE_SRC) $(SHA1UTILS_SRC) $(UTILS_SRC) $(LOGGING_SRC)
	$(FPC) $(FPCFLAGS) -Fu$(SRCDIR) -o$@ $<

$(TEST_PROTOCOL): $(TESTDIR)/test_protocol.pas $(PROTOCOL_SRC) $(SHA1UTILS_SRC) $(UTILS_SRC)
	$(FPC) $(FPCFLAGS) -Fu$(SRCDIR) -o$@ $<

$(TEST_INTEGRATION): $(TESTDIR)/test_integration.pas $(BENCODE_SRC) $(METAINFO_SRC) $(FILEMGR_SRC) $(PROTOCOL_SRC) $(SHA1UTILS_SRC) $(UTILS_SRC)
	$(FPC) $(FPCFLAGS) -Fu$(SRCDIR) -o$@ $<

# Run all tests
test: all
	@echo "=============================================="
	@echo "  RUNNING ALL TESTS"
	@echo "=============================================="
	@echo ""
	@echo "--- Running Master Test Runner ---"
	@$(TEST_RUNNER) || exit 1
	@echo ""
	@echo "--- Running Detailed Bencode Tests ---"
	@$(TEST_BENCODE) || exit 1
	@echo ""
	@echo "--- Running Extended Bencode Tests ---"
	@$(TEST_BENCODE_EXTENDED) || exit 1
	@echo ""
	@echo "--- Running Detailed SHA1 Tests ---"
	@$(TEST_SHA1) || exit 1
	@echo ""
	@echo "--- Running Detailed Utils Tests ---"
	@$(TEST_UTILS) || exit 1
	@echo ""
	@echo "--- Running Logging Tests ---"
	@$(TEST_LOGGING) || exit 1
	@echo ""
	@echo "--- Running Metainfo Tests ---"
	@$(TEST_METAINFO) || exit 1
	@echo ""
	@echo "--- Running File Manager Tests ---"
	@$(TEST_FILEMGR) || exit 1
	@echo ""
	@echo "--- Running Protocol Tests ---"
	@$(TEST_PROTOCOL) || exit 1
	@echo ""
	@echo "--- Running Integration Tests ---"
	@$(TEST_INTEGRATION) || exit 1
	@echo ""
	@echo "=============================================="
	@echo "  ALL TESTS PASSED!"
	@echo "=============================================="

# Run individual test suites
test-bencode: dirs $(TEST_BENCODE) $(TEST_BENCODE_EXTENDED)
	$(TEST_BENCODE)
	$(TEST_BENCODE_EXTENDED)

test-sha1: dirs $(TEST_SHA1)
	$(TEST_SHA1)

test-utils: dirs $(TEST_UTILS)
	$(TEST_UTILS)

test-logging: dirs $(TEST_LOGGING)
	$(TEST_LOGGING)

test-runner: dirs $(TEST_RUNNER)
	$(TEST_RUNNER)

test-metainfo: dirs $(TEST_METAINFO)
	$(TEST_METAINFO)

test-filemgr: dirs $(TEST_FILEMGR)
	$(TEST_FILEMGR)

test-protocol: dirs $(TEST_PROTOCOL)
	$(TEST_PROTOCOL)

test-integration: dirs $(TEST_INTEGRATION)
	$(TEST_INTEGRATION)

# Debug build (with debug info, no optimization)
debug: FPCFLAGS = -Mobjfpc -Sh -O0 -g -gl -vewh
debug: all

# Release build (optimized, no debug info)
release: FPCFLAGS = -Mobjfpc -Sh -O3 -Xs -vewh
release: all

# Clean build artifacts
clean:
	rm -rf $(BINDIR)
	rm -f $(SRCDIR)/*.o $(SRCDIR)/*.ppu
	rm -f $(TESTDIR)/*.o $(TESTDIR)/*.ppu
	rm -f *.o *.ppu
	rm -f test_sha1_tmp.txt

# Install (copy to system - optional)
install: release
	@echo "Installing PascalTorrent..."
	@echo "(Installation target not yet implemented)"

# Help
help:
	@echo "PascalTorrent - Phase 2 Build System"
	@echo ""
	@echo "Targets:"
	@echo "  all          - Build all test executables (default)"
	@echo "  test         - Build and run all tests"
	@echo "  test-bencode - Run bencode unit tests only"
	@echo "  test-sha1    - Run SHA1 unit tests only"
	@echo "  test-utils   - Run utils unit tests only"
	@echo "  test-logging - Run logging unit tests only"
	@echo "  test-runner  - Run master test runner only"
	@echo "  test-metainfo- Run metainfo unit tests only"
	@echo "  test-filemgr - Run file manager unit tests only"
	@echo "  test-protocol- Run protocol unit tests only"
	@echo "  test-integration - Run integration tests only"
	@echo "  debug        - Build with debug symbols"
	@echo "  release      - Build optimized release"
	@echo "  clean        - Remove all build artifacts"
	@echo "  help         - Show this help"
	@echo ""
	@echo "Compiler: $(FPC)"
	@echo "Flags: $(FPCFLAGS)"
