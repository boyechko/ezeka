;;; tests-ezeka-syslog.el --- Unit tests for ezeka-syslog.el -*- lexical-binding: t -*-

;; Copyright (C) 2025 Richard Boyechko

;; Author: Richard Boyechko <code@diachronic.net>
;; Created: 2025-07-17
;; Version: 0.1
;; Package-Requires: ((emacs 28.2))
;; Keywords: none
;; URL: https://github.com/boyechko/

;; This file is not part of Emacs

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(require 'ert)
(require 'ezeka-syslog)

;; Make `M-x eval-buffer' in this file enough to find `ezeka-test'.
(add-to-list 'load-path (file-name-directory (or load-file-name buffer-file-name)))
(require 'ezeka-test)

(ert-deftest ezeka--system-log-record ()
  (should (ezeka--system-log-record nil 'update-modified 'note "a-1234"))
  (should (ezeka--system-log-record
           (parse-time-string "2024-01-01T00:00:00")
           'update-modified 'note "a-1234")))

(ert-deftest ezeka--system-log-repeat-record-p ()
  (should (ezeka--system-log-repeat-record-p
           (ezeka--system-log-record nil 'update-modified 'note "a-1234")
           (ezeka--system-log-record
            (parse-time-string "2024-01-01T00:00:00")
            'update-modified 'note "a-1234"))))

(ert-deftest ezeka--add-to-system-log ()
  (ezeka-test-with-zettelkasten
    ;; The function returns whatever `save-buffer' does, i.e. nil, so assert
    ;; the effect on the log rather than the return value.
    (ezeka--add-to-system-log 'move nil
      'from "k-7952"
      'to "20150603T2323")
    (should (= 1 (length (ezeka--system-log-trail "k-7952"))))))

(ert-deftest ezeka--add-to-system-log-cancels-out ()
  "A move and its reverse should leave no trail for the note.
`ezeka--add-to-system-log' only suppresses an immediately repeated
record (see `ezeka--system-log-repeat-record-p'); it never removes an
earlier entry that a later one undoes, so both moves stay in the log."
  :expected-result :failed
  (ezeka-test-with-zettelkasten
    (ezeka--add-to-system-log 'move nil
      'from "k-7952"
      'to "20150603T2323")
    (ezeka--add-to-system-log 'move nil
      'from "20150603T2323"
      'to "k-7952")
    (should-not (ezeka--system-log-trail "k-7952"))))

(provide 'tests-ezeka-syslog)
;;; tests-ezeka-syslog.el ends here
