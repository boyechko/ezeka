;;; ezeka-zk.el --- Eclectic Zettelkasten & Zk Integration -*- lexical-binding: t -*-

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

;; Zk and Zk-index integration for ezeka.el

(require 'ezeka)
(require 'zk)
(require 'zk-index)

;;;###autoload
(defun ezeka-zk-index-choose-kasten (arg new-kasten)
  "If there is an existing `zk-index-buffer-name', switches to it, otherwise
 interactively selects the deft directory from among `ezeka-kaesten'. With a
prefix argument, selects new Zk directory regardless of Zk-Index buffer
status."
  (interactive
   (if (or (null (get-buffer zk-index-buffer-name))
           (equal current-prefix-arg '(4)))
       (list current-prefix-arg
             (ivy-read "Zettel kasten: " ezeka-kaesten))
     (list current-prefix-arg nil)))
  (if (not new-kasten)
      (pop-to-buffer zk-index-buffer-name)
    (setq zk-directory (ezeka-kasten-directory new-kasten))
    (cl-case (ezeka-kasten-slug-type new-kasten)
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
    (setq ezeka-zk-metadata-alist nil
          zk-index-mode-name (format "Zk:%s" (capitalize new-kasten)))
    (zk-index)
    (zk-index-refresh)))

(defun ezeka-zk-new-note-header (title new-id &optional orig-id)
  "Insert header in new notes with args TITLE and NEW-ID.
Optionally use ORIG-ID for backlink."
  (ezeka-insert-metadata-template nil title orig-id))

(defun ezeka-zk-format-function (files)
  "See `zk-new-note-header-function'."
  (let* (output)
    (dolist (file files output)
      (when (ezeka-note-p file)
        (let* ((metadata (ezeka-file-metadata file)))
          (push (format-spec zk-index-format
                             `((?i . ,(ezeka-file-slug file))
                               (?t . ,(alist-get :title metadata))
                               (?c . ,(alist-get :category metadata))
                               (?k . ,(or (alist-get :citekey metadata) ""))))
                output))))))

(defun ezeka-zk-index-print-header ()
  "See `zk-index-print-header-function'."
  (let ((format-string
         (replace-regexp-in-string "%\\([^[:alpha:]]*\\)[[:alpha:]]"
                                   "%\\1s"
                                   zk-index-format)))
    (insert
     (format (concat format-string "\n\n")
             "ID" "Category" "Citekey" "Title"))))

(defun ezeka-zk-index-print-header ()
  "See `zk-index-print-header-function'."
  (let ((kasten (upcase (f-base zk-directory))))
    (insert (concat (propertize kasten
                                'face 'warning
                                'justification 'center)))
    (insert "\n\n")))

(defun ezeka-zk-parse-file (target files)
  "See `zk-parse-file-function'."
  (let* ((files (if (listp files)
                    files
                  (list files)))
         (return
          (mapcar
           (lambda (file)
             (if (equal target 'id)
                 (ezeka-file-slug file)
               (alist-get :title (ezeka-file-metadata file))))
           files)))
    (if (eq 1 (length return))
        (car return)
      return)))
(setq zk-parse-file-function #'ezeka-zk-parse-file)

(defvar ezeka-zk-metadata-alist nil
  "An alist containing file metadata and mtime, cached by ID. Each item
has the form
(ID TITLE FILENAME MTIME METADATA).")

(defun ezeka-zk-cache-update-all ()
  "Update file list and update cached information for each file. Returns
`ezeka-zk-metadata-alist'."
  (setq ezeka-zk-metadata-alist
    (mapcar
     (lambda (file)
       (when (ezeka-note-p file)
         (let ((metadata (ezeka-file-metadata file)))
           (list (ezeka-file-slug file)
                 (alist-get :title metadata)
                 file
                 (file-attribute-modification-time (file-attributes file))
                 metadata))))
     (zk--directory-files t))))

(defun ezeka-zk-alist ()
  "See `zk-alist-function'."
  (or ezeka-zk-metadata-alist
      (ezeka-zk-cache-update-all)))
(setq zk-alist-function #'ezeka-zk-alist)
(setq zk-id-list-search-key
  #'(lambda (item)
      (or (alist-get :category (car (last item))) "")))

(provide 'ezeka-zk)
