;;; ezeka-test-runner.el --- Headless ERT runner -*- lexical-binding: t -*-

;;; Commentary:

;; Loaded by `make test' before the test files.  Selector and verbosity
;; settings arrive through the environment so Lisp never enters shell code.

;;; Code:

;; Set this before any package dependencies can load stale bytecode.
(setq load-prefer-newer t)

(require 'ert)
(require 'ezeka)

(defun ezeka-test-run-batch ()
  "Run ERT using the selector and verbosity supplied by Make.
EZEKA_TEST_SELECTOR is Lisp data, optionally quoted, and defaults to t.
EZEKA_TEST_VERBOSE=1 shows untruncated backtraces."
  (let* ((selector (read (or (getenv "EZEKA_TEST_SELECTOR") "t")))
         (ert-batch-backtrace-right-margin
          (if (equal (getenv "EZEKA_TEST_VERBOSE") "1")
              nil
            ert-batch-backtrace-right-margin)))
    ;; Accept the quoted selectors used with the previous --eval recipe
    ;; without evaluating arbitrary forms from the environment.
    (ert-run-tests-batch-and-exit
     (if (eq (car-safe selector) 'quote)
         (cadr selector)
       selector))))

(provide 'ezeka-test-runner)
;;; ezeka-test-runner.el ends here
