;;; tests-ezeka-base.el --- Tests for ezeka-base.el -*- lexical-binding: t -*-

;; Copyright (C) 2025 Richard Boyechko

;; Author: Richard Boyechko <code@diachronic.net>
;; Created: 2025-07-18
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

(ert-deftest ezeka--space-or-punct-p ()
  (should (ezeka--space-or-punct-p ?:))
  (should-not (ezeka--space-or-punct-p ?a)))

(ert-deftest ezeka--with-space ()
  (should (string= " foo" (ezeka--with-space "foo" 'before)))
  (should (string= "foo " (ezeka--with-space "foo" 'after)))
  (should (string= " foo " (ezeka--with-space "foo" 'both)))
  (should (string= "  " (ezeka--with-space " " 'before)))
  (should (string-empty-p (ezeka--with-space nil 'before))))

(ert-deftest ezeka--replace-in-string ()
  (let ((replacements '(("foo" "bar")
                        ("b\\(u\\)zz" "zz\\1b" regexp))))
    (should (string= "bar bar zzub"
                     (apply #'ezeka--replace-in-string
                            "foo bar buzz" replacements)))))

(ert-deftest ezeka--concat-strings ()
  (should (string= "one, two, three"
                   (ezeka--concat-strings ", " "one" "two" "three"))))

(ert-deftest ezeka--regexp-strip-named-groups ()
  (let ((regexp "\\(?1:foo\\) \\(?2:bar\\) \\(?3:buzz\\) \\(foobar\\)"))
    (should (string= "\\(foo\\) \\(bar\\) \\(buzz\\) \\(foobar\\)"
                     (ezeka--regexp-strip-named-groups regexp)))))

(provide 'tests-ezeka-base)
;;; tests-ezeka-base.el ends here
