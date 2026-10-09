;;; ezeka-tests.el --- Unit tests for ezeka.el -*- lexical-binding: t -*-

;; Copyright (C) 2015-2022 Richard Boyechko

;; Author: Richard Boyechko <code@diachronic.net>
;; Version: 0.1
;; Package-Requires: ((emacs "25.1") (org "9.5"))
;; Keywords: zettelkasten org
;; URL: https://github.com/boyechko/ezeka

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

;; These are unit tests for ezeka.el

;;; Code:

(require 'ezeka)
(require 'ert)
(require 'ert-x)

;; Make `M-x eval-buffer' in this file enough to find `ezeka-test'.
(add-to-list 'load-path (file-name-directory (or load-file-name buffer-file-name)))
(require 'ezeka-test)

(define-key emacs-lisp-mode-map (kbd "C-c C-e") 'ert)

;;;=============================================================================
;;; Tests
;;;=============================================================================

(ert-deftest ezeka--create-placeholder ()
  "Create a placeholder symlink for a-1234 pointing at a-0000.
This currently fails inside `ezeka--create-placeholder': `ezeka--select-file'
hands it a file name, which it passes to `ezeka-link-file' (link -> file)
where it wants `ezeka-file-link' (file -> link).  The result is nil, and
`file-relative-name' then signals.  Both call sites -- the `link-target'
binding and the `ezeka--add-to-move-log' call below it -- treat `link-to' as
a link rather than the file it is."
  :expected-result :failed
  (ezeka-test-with-zettelkasten
    (let* ((mdata (ezeka-metadata "a-1234"
                    'label "ψ"
                    'caption "ezeka--create-placeholder test"))
           (path (ezeka-link-path "a-1234" mdata)))
      ;; The function asks for the Kasten holding the symlink target, then
      ;; for the note itself.
      (should (ert-simulate-keys "numerus\ra-0000 {χ} sample numerus note\r"
                (ezeka--create-placeholder "a-1234" mdata 'quietly)))
      (should (file-symlink-p path)))))

(provide 'tests-ezeka)
;;; tests-ezeka.el ends here

