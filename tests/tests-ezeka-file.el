;;; tests-ezeka-file.el --- Unit tests for ezeka-file.el -*- lexical-binding: t -*-

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
(require 'ezeka-file)

(ert-deftest ezeka--make-symbolic-link ()
  (let ((target (make-temp-file "ezeka-target"))
        (linkname (expand-file-name "ezeka-symlink" (temporary-file-directory))))
    (ezeka--make-symbolic-link target linkname)
    (should (and (file-exists-p linkname) (file-symlink-p linkname)))
    (should-not (when (file-symlink-p linkname)
                  (delete-file linkname)
                  (file-exists-p linkname)))))

(ert-deftest ezeka--directory-files ()
  (let ((all-files (ezeka--directory-files "scriptum"))
        (symlinks (ezeka--directory-files "scriptum"
                                          (lambda (file)
                                            (file-symlink-p file)))))
   (should all-files)
   (should (< (length symlinks) (length all-files)))))

(ert-deftest ezeka--pasteurize-file-name ()
  (should (string= (ezeka--pasteurize-file-name "/Mickey 17/ (dir. Bong Joon-ho, 2025)")
                   "_Mickey 17_")))

(ert-deftest ezeka-id-valid-p ()
  (should-not (ezeka-id-valid-p "goggly-gook"))
  (should (ezeka-id-valid-p "a-1234"))
  (should (ezeka-id-valid-p "327-C-02-A")))

(ert-deftest ezeka-link-kasten ()
  (should (string= (ezeka-link-kasten "a-1234") "numerus"))
  (should (string= (ezeka-link-kasten "20240729T1511") "tempus"))
  (should (string= (ezeka-link-kasten "a-1234~56") "scriptum")))

(ert-deftest ezeka-link-path ()
  (should
   (string-match-p "numerus/a/a-1234 {ψ} ezeka--create-placeholder test.txt$"
                   (ezeka-link-path "a-1234"
                                    '((link . "a-1234")
                                      (label . "ψ")
                                      (caption . "ezeka--create-placeholder test"))))))

(provide 'tests-ezeka-file)
;;; tests-ezeka-file.el ends here
