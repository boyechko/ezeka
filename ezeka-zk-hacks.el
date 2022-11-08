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

(defun adv--zk-group-function (file transform)
  "Replace `zk--group-function' to better TRANSFORM the given FILE.
See `zk--group-function' for details."
  (let ((case-fold-search t)
        (base (file-name-base file)))
    (cond ((not transform)
           "ezk")
          ((string-match (zk-file-name-regexp) file)
           (let ((id (match-string 1 file))
                 (title (match-string 2 file)))
             (ezeka-zk-format-function "%i {%l} %t" id title)))
          (t
           base))))

(defun ezeka-zk--file-id (file)
  "Replace `zk--file-id' for the given FILE."
  (when (ezeka-note-p file)
    (ezeka-file-name-id file)))

(defun ezeka-zk-file-name-regexp ()
  "Return the correct regexp matching Ezeka file names.
The regexp captures these groups:

Group 1 is the ezk ID.
Group 2 is the title."
  (concat "\\(?1:" (ezeka--id-regexp :all) "\\)"
          "\\(?2: [^.]+\\)*"
          "\\."
          zk-file-extension
          "$"))

(defvar ezeka-zk-hacks--zfnr-func nil)
(defvar ezeka-zk-hacks-mode nil)

(defun ezeka-zk-hacks-mode (&optional arg)
  "Toggle my custom hacks to make zk fit my workflow.
If ARG is positive or T, enable the hacks; if negative, disable them.
This is just a pseudo mode."
  (interactive)
  (cond ((or (eq arg t)                 ; enable the mode
             (and (numberp arg) (> arg 0))
             (not ezeka-zk-hacks-mode))
         (advice-add 'zk-backlinks :around 'adv--backlinks-in-entire-ezeka)
         (advice-add 'zk--group-function :override 'adv--zk-group-function)
         (advice-add 'zk--file-id :override 'ezeka-zk--file-id)
         (advice-add 'zk--insert-link-and-title
                     :override 'ezeka-zk--insert-link-and-title)
         (setq ezeka-zk-hacks--zfnr-func (symbol-function 'zk-file-name-regexp)
               ezeka-zk-hacks-mode t)
         (defalias 'zk-file-name-regexp 'ezeka-zk-file-name-regexp)
         (message "Ezeka-zk-hacks-mode enabled"))
        (t                              ; disable the mode
         (advice-remove 'zk-backlinks 'adv--backlinks-in-entire-ezeka)
         (advice-remove 'zk--group-function 'adv--zk-group-function)
         (advice-remove 'zk--file-id 'ezeka-zk--file-id)
         (advice-remove 'zk--insert-link-and-title 'ezeka-zk--insert-link-and-title)
         (fset 'zk-file-name-regexp ezeka-zk-hacks--zfnr-func)
         (setq ezeka-zk-hacks--zfnr-func nil
               ezeka-zk-hacks-mode nil)
         (message "Ezeka-zk-hacks-mode disabled"))))

(provide 'ezeka-zk-hacks)
;;; ezeka-zk-hacks.el ends here
