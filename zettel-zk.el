;;; zettel-zk.el --- Eclectic Zettelkasten & Zk Integration -*- lexical-binding: t -*-

;; Copyright (C) 2022 Richard Boyechko

;; Author: Richard Boyechko <code@diachronic.net>
;; Version: 0.1
;; Package-Requires: ((emacs "25.1") (zettel "0.8") (zk "0.4") (zk-index "0.4"))
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

;; Zk and Zk-index integration for zettel.el

(require 'zettel)
(require 'zk)
(require 'zk-index)

(defun zettel-zk-index-choose-kasten (arg new-kasten)
  "If there is an existing `zk-index-buffer-name', switches to it, otherwise
 interactively selects the deft directory from among `zettel-kaesten'. With a
prefix argument, selects new Zk directory regardless of Zk-Index buffer
status."
  (interactive
   (if (or (null (get-buffer zk-index-buffer-name))
           (equal current-prefix-arg '(4)))
       (list current-prefix-arg
             (ivy-read "Zettel kasten: " zettel-kaesten))
     (list current-prefix-arg nil)))
  (if (not new-kasten)
      (pop-to-buffer zk-index-buffer-name)
    (setq zk-directory (zettel-kasten-directory new-kasten))
    (cl-case (zettel-kasten-slug-type new-kasten)
      (:numerus
       (setq zk-id-regexp "\\([a-z]-[0-9]\\{4\\}\\)"
             zk-id-format
             (concat (cl-subseq (downcase (format-time-string "%a")) 0 1)
                     "-%H%M")))
      (:bolus
       (setq zk-id-regexp "\\([0-9]\\{3\\}-[a-z]\\{3\\}\\)"
             zk-id-format
             (concat (downcase (format-time-string "%a-%j")))))
      (t
       (setq zk-id-regexp "\\([0-9T]\\{13\\}\\)"
             zk-id-format "%Y%m%dT%H%M")))
    (setq zettel-zk-metadata-alist nil
          zk-index-mode-name (format "Zk:%s" (capitalize new-kasten)))
    (zk-index)
    (zk-index-refresh)
    (zettel-populate-categories)))

(defun zettel-zk-new-note-header (title new-id &optional orig-id)
    "Insert header in new notes with args TITLE and NEW-ID.
Optionally use ORIG-ID for backlink."
    (zettel-insert-metadata-template nil title orig-id))

(defun zettel-zk-format-function (files)
  "See `zk-new-note-header-function'."
  (let* (output)
    (dolist (file files output)
      (when (zettel-p file)
        (let* ((metadata (zettel-file-metadata file)))
          (push (format-spec zk-index-format
                             `((?i . ,(zettel-file-slug file))
                               (?t . ,(alist-get :title metadata))
                               (?c . ,(alist-get :category metadata))
                               (?k . ,(or (alist-get :citekey metadata) ""))))
                output))))))

(defun zettel-zk-index-print-header ()
  "See `zk-index-print-header-function'."
  (let ((format-string
         (replace-regexp-in-string "%\\([^[:alpha:]]*\\)[[:alpha:]]"
                                   "%\\1s"
                                   zk-index-format)))
    (insert
     (format (concat format-string "\n\n")
             "ID" "Category" "Citekey" "Title"))))

(defun zettel-zk-index-print-header ()
  "See `zk-index-print-header-function'."
  (let ((kasten (upcase (f-base zk-directory))))
    (insert (concat (propertize kasten
                                'face 'warning
                                'justification 'center)))
    (insert "\n\n")))

(defun zettel-zk-parse-file (target files)
  "See `zk-parse-file-function'."
  (let* ((files (if (listp files)
                    files
                  (list files)))
         (return
          (mapcar
           (lambda (file)
             (if (equal target 'id)
                 (zettel-file-slug file)
               (alist-get :title (zettel-file-metadata file))))
           files)))
    (if (eq 1 (length return))
        (car return)
      return)))
(setq zk-parse-file-function #'zettel-zk-parse-file)

(defvar zettel-zk-metadata-alist nil
  "An alist containing file metadata and mtime, cached by ID. Each item
has the form
(ID TITLE FILENAME MTIME METADATA).")

(defun zettel-zk-cache-update-all ()
  "Update file list and update cached information for each file. Returns
`zettel-zk-metadata-alist'."
  (setq zettel-zk-metadata-alist
    (mapcar
     (lambda (file)
       (when (zettel-p file)
         (let ((metadata (zettel-file-metadata file)))
           (list (zettel-file-slug file)
                 (alist-get :title metadata)
                 file
                 (file-attribute-modification-time (file-attributes file))
                 metadata))))
     (zk--directory-files t))))

(defun zettel-zk-alist ()
  "See `zk-alist-function'."
  (or zettel-zk-metadata-alist
      (zettel-zk-cache-update-all)))
(setq zk-alist-function #'zettel-zk-alist)
(setq zk-id-list-search-key
  #'(lambda (item)
      (or (alist-get :category (car (last item))) "")))

(provide 'zettel-zk)
