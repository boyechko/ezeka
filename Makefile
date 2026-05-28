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

# Test files. tests-ezeka.el is omitted on purpose: it is interactive
# (prompts with y-or-n-p, creates real files) and cannot run headless yet.
TESTS = tests/tests-ezeka-base.el tests/tests-ezeka-file.el \
        tests/tests-ezeka-meta.el tests/tests-ezeka-syslog.el

LOAD_TESTS = $(foreach t,$(TESTS),-l $(t))

.PHONY: test compile clean

# Run the headless ERT suite. The package is loaded first because the test
# files assume the functions they exercise are already defined.
test:
	$(EMACS) -Q --batch -L . -L tests -l ert -l ezeka \
	  $(LOAD_TESTS) \
	  -f ert-run-tests-batch-and-exit

# Byte-compile the core files; acts as a linter (unused vars, bad arglists,
# obsolete functions, etc.).
compile:
	$(EMACS) -Q --batch -L . -f batch-byte-compile $(SRC)

clean:
	rm -f *.elc
