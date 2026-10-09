;;; ezeka-test.el --- Test fixture for the Ezeka suite -*- lexical-binding: t -*-

;; Copyright (C) 2026 Richard Boyechko

;; Author: Richard Boyechko <code@diachronic.net>
;; Created: 2026-08-30
;; Version: 0.1
;; Package-Requires: ((emacs 29.1))
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

;; Every test that touches the filesystem must run against the fixture
;; Zettelkasten in `tests/resources', never against the user's live one.
;; `ezeka-test-with-zettelkasten' copies the fixture to a temporary directory
;; and binds `ezeka-directory' to it, so tests may create, rename, and delete
;; files freely: the copy is discarded afterwards and the fixture in the repo
;; stays pristine.
;;
;; The fixture holds one note per Kasten plus a symlink:
;;
;;   numerus/a/a-0000  {χ}      created 2022-08-08 Mon 18:25
;;   numerus/q/q-8148  {μ}      has a `parent' field
;;   numerus/x/x-1613  {χ}
;;   tempus/2016/20160313T2228  {Class}
;;   scriptum/a-1234/a-1234~01  {Class}
;;   scriptum/a-1234/a-1234~02  {Class}
;;   scriptum/a-1234/a-1234~03  symlink to ~01
;;   auto/system.log            two pre-existing entries
;;
;; Tests should refer to those IDs and derive expected paths from
;; `ezeka-directory' rather than hardcoding absolute ones.

;;; Code:

(require 'ert)
(require 'ert-x)
(require 'ezeka-file)
(require 'ezeka-meta)                   ; `ezeka-placeholder-genus'

(defmacro ezeka-test-with-zettelkasten (&rest body)
  "Run BODY with `ezeka-directory' bound to a fresh copy of the fixture.
The copy is deleted when BODY exits, however it exits."
  (declare (indent 0) (debug t))
  `(ert-with-temp-directory zk
     (copy-directory (ert-resource-directory) zk nil t t)
     ;; A numerus Kasten has one subdirectory per letter and
     ;; `ezeka-new-numerus-currens' picks among them at random, so all 26 must
     ;; exist.  They are created here rather than checked in because git does
     ;; not track empty directories.
     (dolist (letter (number-sequence ?a ?z))
       (make-directory (expand-file-name (format "numerus/%c" letter) zk) t))
     ;; `ezeka-placeholder-genus' is nil by default and set in the user's
     ;; init, so pin it here too: no test should depend on init settings.
     (let ((ezeka-directory (file-name-as-directory zk))
           (ezeka-placeholder-genus ?ψ))
       ,@body)))

(defun ezeka-test-file (relative-path)
  "Return the absolute name of RELATIVE-PATH within the fixture Kasten.
Only meaningful inside `ezeka-test-with-zettelkasten'."
  (expand-file-name relative-path ezeka-directory))

(provide 'ezeka-test)
;;; ezeka-test.el ends here
