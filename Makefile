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
SOCKWRAP_SRC = $(SRCDIR)/sockwrap.pas

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
TEST_SOCKET_INTEGRATION = $(BINDIR)/test_socket_integration
TEST_PEER_INTEGRATION = $(BINDIR)/test_peer_integration
TEST_FILE_PROTOCOL_INTEGRATION = $(BINDIR)/test_file_protocol_integration
TEST_ERROR_SCENARIOS = $(BINDIR)/test_error_scenarios
TEST_STRESS = $(BINDIR)/test_stress
TEST_LARGE_FILES = $(BINDIR)/test_large_files
TEST_FUZZ = $(BINDIR)/test_fuzz

# Default target
.PHONY: all clean test dirs debug release

all: dirs $(TEST_RUNNER) $(TEST_BENCODE) $(TEST_BENCODE_EXTENDED) $(TEST_SHA1) $(TEST_UTILS) $(TEST_LOGGING) $(TEST_METAINFO) $(TEST_FILEMGR) $(TEST_PROTOCOL) $(TEST_INTEGRATION) $(TEST_SOCKET_INTEGRATION) $(TEST_PEER_INTEGRATION) $(TEST_FILE_PROTOCOL_INTEGRATION) $(TEST_ERROR_SCENARIOS) $(TEST_STRESS) $(TEST_LARGE_FILES) $(TEST_FUZZ)

dirs:
	@mkdir -p $(BINDIR)

# Test executables
$(TEST_BENCODE): $(TESTDIR)/test_bencode.pas $(BENCODE_SRC) $(TESTDIR)/testframework.pas
	$(FPC) $(FPCFLAGS) -Fu$(SRCDIR) -Fu$(TESTDIR) -o$@ $<

$(TEST_BENCODE_EXTENDED): $(TESTDIR)/test_bencode_extended.pas $(BENCODE_SRC) $(TESTDIR)/testframework.pas
	$(FPC) $(FPCFLAGS) -Fu$(SRCDIR) -Fu$(TESTDIR) -o$@ $<

$(TEST_SHA1): $(TESTDIR)/test_sha1.pas $(SHA1UTILS_SRC) $(TESTDIR)/testframework.pas
	$(FPC) $(FPCFLAGS) -Fu$(SRCDIR) -Fu$(TESTDIR) -o$@ $<

$(TEST_UTILS): $(TESTDIR)/test_utils.pas $(UTILS_SRC) $(TESTDIR)/testframework.pas
	$(FPC) $(FPCFLAGS) -Fu$(SRCDIR) -Fu$(TESTDIR) -o$@ $<

$(TEST_LOGGING): $(TESTDIR)/test_logging.pas $(LOGGING_SRC) $(TESTDIR)/testframework.pas
	$(FPC) $(FPCFLAGS) -Fu$(SRCDIR) -Fu$(TESTDIR) -o$@ $<

$(TEST_RUNNER): $(TESTDIR)/test_runner.pas $(BENCODE_SRC) $(SHA1UTILS_SRC) $(UTILS_SRC) $(TESTDIR)/testframework.pas
	$(FPC) $(FPCFLAGS) -Fu$(SRCDIR) -Fu$(TESTDIR) -o$@ $<

$(TEST_METAINFO): $(TESTDIR)/test_metainfo.pas $(METAINFO_SRC) $(BENCODE_SRC) $(SHA1UTILS_SRC) $(UTILS_SRC) $(LOGGING_SRC) $(TESTDIR)/testframework.pas
	$(FPC) $(FPCFLAGS) -Fu$(SRCDIR) -Fu$(TESTDIR) -o$@ $<

$(TEST_FILEMGR): $(TESTDIR)/test_filemgr.pas $(FILEMGR_SRC) $(METAINFO_SRC) $(BENCODE_SRC) $(SHA1UTILS_SRC) $(UTILS_SRC) $(LOGGING_SRC) $(TESTDIR)/testframework.pas
	$(FPC) $(FPCFLAGS) -Fu$(SRCDIR) -Fu$(TESTDIR) -o$@ $<

$(TEST_PROTOCOL): $(TESTDIR)/test_protocol.pas $(PROTOCOL_SRC) $(SHA1UTILS_SRC) $(UTILS_SRC) $(TESTDIR)/testframework.pas
	$(FPC) $(FPCFLAGS) -Fu$(SRCDIR) -Fu$(TESTDIR) -o$@ $<

$(TEST_INTEGRATION): $(TESTDIR)/test_integration.pas $(BENCODE_SRC) $(METAINFO_SRC) $(FILEMGR_SRC) $(PROTOCOL_SRC) $(SHA1UTILS_SRC) $(UTILS_SRC) $(TESTDIR)/testframework.pas
	$(FPC) $(FPCFLAGS) -Fu$(SRCDIR) -Fu$(TESTDIR) -o$@ $<

$(TEST_SOCKET_INTEGRATION): $(TESTDIR)/test_socket_integration.pas $(SOCKWRAP_SRC) $(UTILS_SRC) $(TESTDIR)/testframework.pas
	$(FPC) $(FPCFLAGS) -Fu$(SRCDIR) -Fu$(TESTDIR) -o$@ $<

$(TEST_PEER_INTEGRATION): $(TESTDIR)/test_peer_integration.pas $(PROTOCOL_SRC) $(SOCKWRAP_SRC) $(SHA1UTILS_SRC) $(UTILS_SRC) $(TESTDIR)/testframework.pas
	$(FPC) $(FPCFLAGS) -Fu$(SRCDIR) -Fu$(TESTDIR) -o$@ $<

$(TEST_FILE_PROTOCOL_INTEGRATION): $(TESTDIR)/test_file_protocol_integration.pas $(BENCODE_SRC) $(METAINFO_SRC) $(FILEMGR_SRC) $(PROTOCOL_SRC) $(SHA1UTILS_SRC) $(UTILS_SRC) $(TESTDIR)/testframework.pas
	$(FPC) $(FPCFLAGS) -Fu$(SRCDIR) -Fu$(TESTDIR) -o$@ $<

$(TEST_ERROR_SCENARIOS): $(TESTDIR)/test_error_scenarios.pas $(BENCODE_SRC) $(METAINFO_SRC) $(FILEMGR_SRC) $(PROTOCOL_SRC) $(SHA1UTILS_SRC) $(UTILS_SRC) $(SOCKWRAP_SRC) $(TESTDIR)/testframework.pas
	$(FPC) $(FPCFLAGS) -Fu$(SRCDIR) -Fu$(TESTDIR) -o$@ $<

$(TEST_STRESS): $(TESTDIR)/test_stress.pas $(BENCODE_SRC) $(METAINFO_SRC) $(FILEMGR_SRC) $(PROTOCOL_SRC) $(SHA1UTILS_SRC) $(UTILS_SRC) $(TESTDIR)/testframework.pas
	$(FPC) $(FPCFLAGS) -Fu$(SRCDIR) -Fu$(TESTDIR) -o$@ $<

$(TEST_LARGE_FILES): $(TESTDIR)/test_large_files.pas $(METAINFO_SRC) $(FILEMGR_SRC) $(BENCODE_SRC) $(SHA1UTILS_SRC) $(UTILS_SRC) $(TESTDIR)/testframework.pas
	$(FPC) $(FPCFLAGS) -Fu$(SRCDIR) -Fu$(TESTDIR) -o$@ $<

$(TEST_FUZZ): $(TESTDIR)/test_fuzz.pas $(BENCODE_SRC) $(PROTOCOL_SRC) $(SHA1UTILS_SRC) $(UTILS_SRC) $(TESTDIR)/testframework.pas
	$(FPC) $(FPCFLAGS) -Fu$(SRCDIR) -Fu$(TESTDIR) -o$@ $<

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
	@echo "--- Running Socket Integration Tests ---"
	@$(TEST_SOCKET_INTEGRATION) || exit 1
	@echo ""
	@echo "--- Running Peer Integration Tests ---"
	@$(TEST_PEER_INTEGRATION) || exit 1
	@echo ""
	@echo "--- Running File+Protocol Integration Tests ---"
	@$(TEST_FILE_PROTOCOL_INTEGRATION) || exit 1
	@echo ""
	@echo "--- Running Error Scenario Tests ---"
	@$(TEST_ERROR_SCENARIOS) || exit 1
	@echo ""
	@echo "--- Running Large File Tests ---"
	@$(TEST_LARGE_FILES) || exit 1
	@echo ""
	@echo "--- Running Stress Tests ---"
	@$(TEST_STRESS) || exit 1
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

test-socket-integration: dirs $(TEST_SOCKET_INTEGRATION)
	$(TEST_SOCKET_INTEGRATION)

test-peer-integration: dirs $(TEST_PEER_INTEGRATION)
	$(TEST_PEER_INTEGRATION)

test-file-protocol-integration: dirs $(TEST_FILE_PROTOCOL_INTEGRATION)
	$(TEST_FILE_PROTOCOL_INTEGRATION)

test-error-scenarios: dirs $(TEST_ERROR_SCENARIOS)
	$(TEST_ERROR_SCENARIOS)

test-stress: dirs $(TEST_STRESS)
	$(TEST_STRESS)

test-large-files: dirs $(TEST_LARGE_FILES)
	$(TEST_LARGE_FILES)

test-fuzz: dirs $(TEST_FUZZ)
	$(TEST_FUZZ)

# Debug build (with debug info, no optimization)
debug: FPCFLAGS = -Mobjfpc -Sh -O1 -g -gl -vewh
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
	rm -f gmon.out

# Install (copy to system - optional)
install: release
	@echo "Installing PascalTorrent..."
	@echo "(Installation target not yet implemented)"

# Coverage build (with line profiling support)
coverage: FPCFLAGS = -Mobjfpc -Sh -O1 -g -gl -pg -vewh
coverage: all
	@echo ""
	@echo "=============================================="
	@echo "  COVERAGE BUILD COMPLETE"
	@echo "=============================================="
	@echo "Profiling data will be generated in gmon.out"
	@echo "Use 'gprof bin/test_runner gmon.out' to analyze"

# Lint - strict compilation with all warnings
lint: FPCFLAGS = -Mobjfpc -Sh -O2 -g -vewh -vw -Sew -Criot -Co -Ct
lint: dirs $(TEST_BENCODE) $(TEST_SHA1) $(TEST_UTILS) $(TEST_LOGGING) $(TESTDIR)/testframework.pas
	@echo ""
	@echo "=============================================="
	@echo "  LINT CHECK COMPLETE"
	@echo "=============================================="

# Format check - verify indentation consistency
format-check:
	@echo "=============================================="
	@echo "  CHECKING CODE FORMAT"
	@echo "=============================================="
	@echo ""
	@echo "Checking for tabs instead of spaces..."
	@! grep -r $$'\t' $(SRCDIR)/*.pas 2>/dev/null | grep -v "^Binary" | head -5 || echo "  [OK] No tabs found"
	@echo ""
	@echo "Checking line length (max 120 chars)..."
	@awk 'length > 120 {print FILENAME ":" NR ": " $$0}' $(SRCDIR)/*.pas $(TESTDIR)/*.pas | head -5 || echo "  [OK] All lines under 120 chars"
	@echo ""
	@echo "Format check complete."

# Continuous Integration target - runs all checks
ci: clean format-check lint all test
	@echo ""
	@echo "=============================================="
	@echo "  CI CHECKS PASSED"
	@echo "=============================================="

# Run benchmarks (stress tests with performance metrics)
benchmark: dirs $(TEST_STRESS)
	@echo "=============================================="
	@echo "  RUNNING BENCHMARKS"
	@echo "=============================================="
	@$(TEST_STRESS)
	@echo ""
	@echo "Benchmark complete."

# Statistics - show code metrics
stats:
	@echo "=============================================="
	@echo "  PROJECT STATISTICS"
	@echo "=============================================="
	@echo ""
	@echo "Source Files:"
	@echo "  $$(ls -1 $(SRCDIR)/*.pas 2>/dev/null | wc -l) units in src/"
	@echo "  $$(ls -1 $(TESTDIR)/*.pas 2>/dev/null | wc -l) test files in tests/"
	@echo ""
	@echo "Lines of Code:"
	@echo "  Source: $$(cat $(SRCDIR)/*.pas 2>/dev/null | wc -l) lines"
	@echo "  Tests:  $$(cat $(TESTDIR)/*.pas 2>/dev/null | wc -l) lines"
	@echo "  Total:  $$(cat $(SRCDIR)/*.pas $(TESTDIR)/*.pas 2>/dev/null | wc -l) lines"
	@echo ""
	@echo "Functions/Procedures:"
	@echo "  Source: $$(grep -h '^function\|^procedure' $(SRCDIR)/*.pas 2>/dev/null | wc -l)"
	@echo "  Tests:  $$(grep -h '^function\|^procedure' $(TESTDIR)/*.pas 2>/dev/null | wc -l)"
	@echo ""
	@echo "Constants:"
	@echo "  Source: $$(grep -h '  [A-Z_]* =' $(SRCDIR)/*.pas 2>/dev/null | grep -v '^  //' | wc -l)"
	@echo ""
	@echo "Types:"
	@echo "  Source: $$(grep -h '^  P[A-Z]\|^  T[A-Z]' $(SRCDIR)/*.pas 2>/dev/null | grep '=' | wc -l)"
	@echo ""
	@echo "=============================================="

# Memory check (run tests with heap trace)
memcheck: FPCFLAGS = -Mobjfpc -Sh -O1 -g -gl -gh -vewh
memcheck: dirs $(TEST_RUNNER)
	@echo "=============================================="
	@echo "  MEMORY LEAK CHECK"
	@echo "=============================================="
	@HEAPTRC="keep” $(TEST_RUNNER) 2>&1 | grep -E "(Memory|Heap|Leak|unfreed)" || echo "Memory check complete."

# Quick build - faster compilation for development
quick: FPCFLAGS = -Mobjfpc -Sh -O1 -vewh
quick: all

# Help
help:
	@echo "PascalTorrent - Build System"
	@echo ""
	@echo "Build Targets:"
	@echo "  all          - Build all test executables (default)"
	@echo "  quick        - Fast build for development (no optimization)"
	@echo "  debug        - Build with debug symbols"
	@echo "  release      - Build optimized release"
	@echo "  coverage     - Build with profiling support"
	@echo "  clean        - Remove all build artifacts"
	@echo ""
	@echo "Test Targets:"
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
	@echo "  test-socket-integration - Run socket integration tests"
	@echo "  test-peer-integration - Run peer protocol integration tests"
	@echo "  test-file-protocol-integration - Run file+protocol integration tests"
	@echo "  test-error-scenarios - Run error handling tests"
	@echo "  test-large-files - Run large file (>4GB) handling tests"
	@echo "  test-fuzz    - Run fuzz testing harness"
	@echo "  test-stress  - Run performance and stress tests"
	@echo "  benchmark    - Run performance benchmarks"
	@echo ""
	@echo "Quality Targets:"
	@echo "  lint         - Strict compilation with all warnings"
	@echo "  format-check - Verify code formatting consistency"
	@echo "  memcheck     - Check for memory leaks"
	@echo "  stats        - Show project statistics"
	@echo ""
	@echo "CI Targets:"
	@echo "  ci           - Run all CI checks (format + lint + test)"
	@echo "  coverage     - Build with profiling support"
	@echo ""
	@echo "Help:"
	@echo "  help         - Show this help"
	@echo ""
	@echo "Compiler: $(FPC)"
	@echo "Flags: $(FPCFLAGS)"
