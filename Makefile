# Ezeka development tasks.
#
# EMACS defaults to whatever is on PATH. On macOS you may need to point it at
# the app bundle, e.g.:
#   make test EMACS=/Applications/MacPorts/Emacs.app/Contents/MacOS/Emacs
EMACS ?= emacs

# Core source files in load order. Excludes the Octavo integration
# (ezeka-octavo*.el), which requires the external `octavo' package.
SRC = ezeka-base.el ezeka-file.el ezeka-meta.el ezeka-syslog.el \
      ezeka-compose.el ezeka.el ezeka-breadcrumbs.el ezeka-virtual.el

# Test files. All run headless: anything touching the filesystem goes through
# `ezeka-test-with-zettelkasten' (tests/ezeka-test.el), which copies the
# fixture Kasten in tests/resources to a temporary directory, so no test reads
# or writes the live Zettelkasten.
TESTS = tests/tests-ezeka-base.el tests/tests-ezeka-file.el \
        tests/tests-ezeka-meta.el tests/tests-ezeka-syslog.el \
        tests/tests-ezeka.el

LOAD_TESTS = $(foreach t,$(TESTS),-l $(t))

# ERT selector as Lisp data, optionally quoted:
#   make test SELECTOR='"^ezeka-link"'
#   make test SELECTOR="'(member ezeka-link-p)"
SELECTOR ?= t
# Preserve literal dollar signs too, rather than expanding them as Make code.
unexport SELECTOR
export EZEKA_TEST_SELECTOR = $(value SELECTOR)

# Keep ERT's standard reporter; VERBOSE=1 removes backtrace truncation.
export EZEKA_TEST_VERBOSE = $(VERBOSE)

.PHONY: test compile clean

# The runner selects fresh source/bytecode and loads Ezeka before the tests.
test:
	$(EMACS) -Q --batch -L . -L tests -l tests/ezeka-test-runner.el \
	  $(LOAD_TESTS) -f ezeka-test-run-batch

# Byte-compile the core files; acts as a linter (unused vars, bad arglists,
# obsolete functions, etc.).
compile:
	$(EMACS) -Q --batch -L . -f batch-byte-compile $(SRC)

clean:
	rm -f *.elc
