;;; ezeka-zk-hacks.el --- Ezk & Zk Integration Hacks -*- lexical-binding: t -*-

;; Copyright (C) 2022 Richard Boyechko

;; Author: Richard Boyechko <code@diachronic.net>
;; Version: 0.1
;; Package-Requires: ((emacs "25.1") (ezeka "0.8") (zk "0.4") (zk-index "0.4"))
;; Keywords: deft zettelkasten org
;; URL: https://github.com/boyechko/eclectic-zettelkasten

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

;; Hacks (advice, defalises, etc.) for Ezk & Zk integration

(require 'ezeka)
(require 'zk)
(require 'zk-index)
(require 'ezeka-zk)

;;; Code:

(defun adv--backlinks-in-entire-ezeka (func &rest args)
  "Advice around `zk-backlinks' to look in entire `ezeka-directory'.
Achieves this by lexically binding binding `zk-directory' and calling
the original FUNC with ARGS."
  (let ((zk-directory ezeka-directory))
    (apply func args)))
(advice-add 'zk-backlinks :around 'adv--backlinks-in-entire-ezeka)

(defun adv--zk-group-function (file transform)
  "Replace `zk--group-function' to better TRANSFORM the given FILE.
See `zk--group-function' for details."
  (let ((case-fold-search t))
    (if (not transform)
        "ezk"
      (string-match (zk-file-name-regexp) file)
      (let ((id (match-string 1 file))
            (title (match-string 2 file)))
        (if (string= title ".")
            (ezeka-format-metadata "{%l} %c <%K>" (ezeka-file-metadata file))
          (or title "<WRONG>"))))))
(advice-add 'zk--group-function :override 'adv--zk-group-function)

(defun ezeka-zk--file-id (file)
  "Replace `zk--file-id' for the given FILE."
  (when (ezeka-note-p file)
    (ezeka-file-name-id file)))
(when (fboundp 'zk-file-id)
  (defadvice 'zk--file-id :override 'ezeka-zk--file-id))

(defun ezeka-zk-file-name-regexp ()
  "Return the correct regexp matching Ezeka file names.
The regexp captures these groups:

Group 1 is the ezk ID.
Group 2 is the title."
  (if (string= "rumen" (ezeka-directory-kasten zk-directory))
      (concat "\\(?1:" (ezeka--id-regexp :numerus) "\\)"
              " "
              "\\(?2:[^.]+\\)"
              "\\."
              zk-file-extension
              "$")
    (concat "\\(?1:" (ezeka--id-regexp :tempus) "\\)"
            "\\(?2:\\.\\)"
            zk-file-extension
            "$")))
(defalias 'zk-file-name-regexp 'ezeka-zk-file-name-regexp)

(defalias 'zk--insert-link-and-title
  (lambda (id _)
    (ezeka-insert-link-with-metadata id :title :before)))

(provide 'ezeka-zk-hacks)
;;; ezeka-zk-hacks.el ends here
