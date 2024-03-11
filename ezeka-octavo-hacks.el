;;; ezeka-octavo-hacks.el --- Ezk & Octavo Integration Hacks -*- lexical-binding: t -*-

;; Copyright (C) 2022 Richard Boyechko

;; Author: Richard Boyechko <code@diachronic.net>
;; Version: 0.1
;; Package-Requires: ((emacs "25.1") (ezeka "0.8") (octavo "0.1") (octavo-index "0.1"))
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

;; Hacks (advice, defalises, etc.) for Ezk & Octavo integration

(require 'ezeka)
(require 'octavo)
(require 'octavo-index)
(require 'ezeka-octavo)

;;; Code:

(defun ezeka-octavo--group-function (file transform)
  "Replace `octavo--group-function' to better TRANSFORM the given FILE.
See `octavo--group-function' for details."
  (let ((case-fold-search t)
        (base (file-name-base file)))
    (cond ((not transform)
           "ezoctavo")
          ((string-match (octavo-file-name-regexp) file)
           (let ((id (match-string 1 file))
                 (title (match-string 2 file)))
             (ezeka-octavo-format-function "%i {%l} %c" id title)))
          (t
           base))))

(defun ezeka-octavo--file-id (file)
  "Replace `octavo--file-id' for the given FILE."
  (when (ezeka-file-p file)
    (ezeka-file-name-id file)))

(defun ezeka-octavo--select-file (&optional prompt files &rest _args)
  "Translate `octavo-select-file-function' arguments.
Most significantly, PROMPT and FILES are switched places to
fit `ezeka--select-file', and the rest are ignored."
  (ezeka--select-file files prompt 'require-match))

(defun ezeka-octavo-backlinks ()
  "Select from list of all notes that link to the current note.
Unlike `octavo-backlinks', search in the entire `ezeka-directory' as well
as list the note's children."
  (interactive)
  (let* ((id (ezeka-file-name-id buffer-file-name))
         (octavo-directory ezeka-directory)
         (files (octavo--grep-file-list
                 (concat "\\(parent: " id "\\|" (octavo-link-regexp id) "\\)"))))
    (if files
        (ezeka-find-file (funcall octavo-select-file-function "Backlinks: " files))
      (user-error "No backlinks found"))))

(defun ezeka-octavo-file-name-regexp ()
  "Return the correct regexp matching Ezeka file names.
The regexp captures these groups:

Group 1 is the eoctavo ID.
Group 2 is the title."
  (concat "\\(?1:"
          (ezeka--id-regexp)
          "\\)"
          "\\(?2: .+\\)*"
          "\\."
          octavo-file-extension
          "$"))

(defvar ezeka-octavo-hacks-mode nil
  "Non-nil when `ezeka-octavo-hacks-mode' is enabled.")

(define-minor-mode ezeka-octavo-hacks-mode
  "More radical customization of `octavo' than with just `ezeka-octavo'."
  :global nil
  :init-value nil
  :group 'ezeka
  :lighter " EzoH"
  (cond (ezeka-octavo-hacks-mode            ; enable the mode
         (advice-add 'octavo--group-function :override 'ezeka-octavo--group-function)
         (advice-add 'octavo--file-id :override 'ezeka-octavo--file-id)
         (advice-add 'octavo--select-file :override 'ezeka-octavo--select-file)
         (advice-add 'octavo-file-name-regexp :override 'ezeka-octavo-file-name-regexp)
         (advice-add 'octavo-backlinks :override 'ezeka-octavo-backlinks)
         (setq ezeka-octavo-hacks-mode t))
        (t                              ; disable the mode
         (advice-remove 'octavo--group-function 'ezeka-octavo--group-function)
         (advice-remove 'octavo--file-id 'ezeka-octavo--file-id)
         (advice-remove 'octavo--select-file 'ezeka-octavo--select-file)
         (advice-remove 'octavo-file-name-regexp 'ezeka-octavo-file-name-regexp)
         (advice-remove 'octavo-backlinks 'ezeka-octavo-backlinks)
         (setq ezeka-octavo-hacks-mode nil))))

(define-globalized-minor-mode global-ezeka-octavo-hacks-mode
  ezeka-octavo-hacks-mode ezeka-octavo-hacks-mode)

(provide 'ezeka-octavo-hacks)
;;; ezeka-octavo-hacks.el ends here
