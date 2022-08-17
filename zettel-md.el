;;; zettel.el --- Eclectic Zettelkasten Markdown Integration -*- lexical-binding: t -*-

;; Copyright (C) 2015-2022 Richard Boyechko

;; Author: Richard Boyechko <code@diachronic.net>
;; Version: 0.1
;; Package-Requires: ((emacs "25.1") (deft "0.8") (org "9.5"))
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

;; Markdown mode integration for zettel.el

(require 'zettel)
(require 'markdown-mode)

;; This must be enabled to have wiki links
(setq markdown-enable-wiki-links t)

;;
;; By default, `markdown-convert-wiki-link-to-filename' concatenates the
;; file extension of the current buffer's file to the link name when you
;; press C-c C-o over something like [[bib/Key2015.bib]], so it ends up
;; opening Key2015.bib.txt. The markdown-cwltf-fix-link removes the extra
;; extension, among other things.
;;
;; Unfortunately, `markdown-follow-wiki-link' also "ensure[s] that the new
;; buffer remains in `markdown-mode'", so I need yet another work-around to
;; fix that: `markdown-fwl-set-auto-mode'.
;;

(defun markdown-fwl--set-auto-mode (&rest args)
  "After advice for `markdown-follow-wiki-link'. Reverses the default
behavir of ensuring that the buffer is in markdown mode, and instead sets it
back to the mode it 'wants to be'."
  (set-auto-mode t))
(advice-add 'markdown-follow-wiki-link :after #'markdown-fwl--set-auto-mode)

(defun markdown-cwltf--fix-link (orig-fun name)
  "Advice for `markdown-convert-wiki-link-to-filename',
completely overriding the originall functionality. It combines the not
clobbering of extension, finding the right directory directory for the Zettel
so that the links can be found across multiple directories within the main
Zettelkasten, and also handling 'subkasten:' notation."
  (save-match-data
    (let ((result (or (zettel-link-file name)
                      (funcall orig-fun name))))
      (if (string-match "\\.\\w+$" name)
          (let ((orig-ext (match-string 0 name)))
            (if (string-match (concat orig-ext "\\(\\.\\w+\\)$") result)
                (replace-match orig-ext nil nil result)
              result))
        result))))

(advice-add 'markdown-convert-wiki-link-to-filename
            :around #'markdown-cwltf--fix-link)

(add-hook 'markdown-mode-hook
    (lambda ()
      (when (zettel-p buffer-file-name)
        (zettel-mode 1))))

