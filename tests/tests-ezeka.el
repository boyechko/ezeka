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

(define-key emacs-lisp-mode-map (kbd "C-c C-e") 'ert)

;;;=============================================================================
;;; Tests
;;;=============================================================================

(ert-deftest ezeka--create-placeholder ()
  (let* ((mdata (ezeka-metadata "a-1234"
                  'label "ψ"
                  'caption "ezeka--create-placeholder test"))
         (path (ezeka-link-path "a-1234" mdata)))
    (should (and (ezeka--create-placeholder "a-1234"
                                            mdata
                                            'quietly)
                 (file-symlink-p path)
                 (if (y-or-n-p (format "Delete placeholder `%s'?" path))
                     (delete-file path)
                   (message "Placeholder not deleted: %s" path))))))

(provide 'tests-ezeka)
;;; tests-ezeka.el ends here

