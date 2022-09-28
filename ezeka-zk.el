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

;; For these avariables to be treated as dynamic in `ezeka-zk-index-choose-kasten',
;; need to declare them first here.
(defvar zk-directory)
(defvar zk-id-regexp)
(defvar zk-id-format)

(defmacro ezeka-zk-with-kasten (kasten &rest body)
  (declare (indent 1))
  `(let ((zk-directory (ezeka-kasten-directory ,kasten)))
     (cl-progv '(zk-id-regexp
                 zk-id-time-string-format
                 zk-directory-subdir-function)
         (cl-case (ezeka-kasten-id-type ,kasten)
           (:numerus
            '("[a-z]-[0-9]\\{4\\}"
              ,(concat (cl-subseq (downcase (format-time-string "%a")) 0 1)
                       "-%H%M")))
           (:bolus
            '("[0-9]\\{3\\}-[a-z]\\{3\\}"
              ,(concat (downcase (format-time-string "%a")) "-%j")))
           (t
            '("[0-9]\\{8\\}T[0-9]\\{4\\}"
              "%Y%m%dT%H%M")))
       ,@body)))

(defun ezeka-zk-initialize-kasten (kasten)
  "Set necessary variables for long-term work in KASTEN."
  (custom-set-variables
   `(zk-directory ,(ezeka-kasten-directory kasten)))
  (add-to-list 'global-mode-string
    (propertize (concat "Kasten:" (upcase kasten))
                'face 'bold-italic))
  (cl-case (ezeka-kasten-id-type kasten)
    (:numerus
     (setq zk-id-regexp "\\([a-z]-[0-9]\\{4\\}\\)"
           zk-id-time-string-format
           (concat (cl-subseq (downcase (format-time-string "%a")) 0 1)
                   "-%H%M")))
    (:bolus
     (setq zk-id-regexp "\\([0-9]\\{3\\}-[a-z]\\{3\\}\\)"
           zk-id-time-string-format
           (concat (downcase (format-time-string "%a-%j")))))
    (t
     (setq zk-id-regexp "\\([0-9T]\\{13\\}\\)"
           zk-id-time-string-format "%Y%m%dT%H%M"
           zk-index-format "%t [[%i]]"))))

;;;###autoload
(defun ezeka-zk-index-choose-kasten (arg new-kasten update-modified)
  "If there is an existing `zk-index-buffer-name', switches to it,
otherwise interactively selects the deft directory from among
`ezeka-kaesten'. With a \\[universal-argument] selects new Zk
directory regardless of Zk-Index buffer status."
  (interactive
   (if (or (null (get-buffer zk-index-buffer-name))
           (equal current-prefix-arg '(4)))
       (list current-prefix-arg
             (completing-read "Zettel kasten: "
                              (mapcar #'car ezeka-kaesten))
             (intern
              (completing-read "Update modification dates? "
                               '("sameday" "never" "confirm")
                               nil
                               t)))
     (list current-prefix-arg
           nil
           t)))
  (if (not new-kasten)
      (pop-to-buffer zk-index-buffer-name)
    (if-let ((buffer (get-buffer zk-index-buffer-name)))
        (kill-buffer buffer))
    (let ((with-captions (eq (ezeka-kasten-id-type new-kasten) :numerus)))
      (custom-set-variables
       '(zk-directory-subdir-function #'ezeka-subdirectory)
       `(zk-index-mode-name ,(format "Zk:%s" (capitalize new-kasten)))
       '(zk-header-title-line-regexp
         "^rubric: §?\\(?1:[^ ]+\\)\\.? \\(?2:\\(?:{\\(?3:[^ ]+\\)} \\)*\\(?4:.*\\)\\)$"
         "Regexp of the line in a zk file's header that contains the rubric.
Group 1 is the id.
Group 2 is the title with category.
Group 3 is the category.
Group 4 is the title without category.")
       `(ezeka-update-header-modified ',update-modified)
       `(zk-file-name-id-only ,(not with-captions))
       `(zk-parse-file-function (if ,with-captions
                                    #'zk-parse-file-name
                                  #'zk-parse-file-header))))
    (ezeka-zk-initialize-kasten new-kasten)
    (zk-index)
    (zk-index-refresh)))

(defun ezeka-zk-new-note-header (title new-id &optional orig-id)
  "Insert header in new notes with args TITLE and NEW-ID.
Optionally use ORIG-ID for backlink."
  (ezeka-insert-header-template new-id nil title orig-id))

(defun ezeka-zk-format-function (files)
  "See `zk-new-note-header-function'."
  (let* (output)
    (dolist (file files output)
      (when (ezeka-note-p file)
        (let* ((metadata (ezeka-file-metadata file)))
          (push (format-spec zk-index-format
                             `((?i . ,(ezeka-file-name-id file))
                               (?t . ,(alist-get :title metadata))
                               (?c . ,(alist-get :category metadata))
                               (?k . ,(or (alist-get :citekey metadata) ""))))
                output))))))

(defun ezeka-zk-format-link-and-title (id title)
  "See `zk-format-link-and-title-function'."
  (let ((file (ezeka-link-file id)))
    (when (ezeka-note-p file)
      (let* ((mdata (ezeka-file-metadata file)))
        (format-spec "%a%t [[%i]]"
                     `((?a . ,(if (alist-get :citekey mdata)
                                  (format "%s's "
                                          (substring
                                           (alist-get :citekey mdata) 1))
                                ""))
                       (?i . ,(ezeka-file-name-id file))
                       (?t . ,(alist-get :title mdata))))))))

(defun ezeka-zk-parse-file (target files)
  "See `zk-parse-file-function'."
  (let* ((files (if (listp files)
                    files
                  (list files)))
         (return
          (mapcar
           (lambda (file)
             (if (equal target 'id)
                 (ezeka-file-name-id file)
               (alist-get :title (ezeka-file-metadata file))))
           files)))
    (if (eq 1 (length return))
        (car return)
      return)))

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
           (list (ezeka-file-name-id file)
                 (alist-get :title metadata)
                 file
                 (file-attribute-modification-time (file-attributes file))
                 metadata))))
     (zk--directory-files t))))

(defun ezeka-zk-alist ()
  "See `zk-alist-function'."
  (or ezeka-zk-metadata-alist
      (ezeka-zk-cache-update-all)))

(defun ezeka-zk-zmove-all-in-desktop (kasten arg)
  "Move all files listed in the active region of deft-browser to KASTEN. With
prefix argument, confirm each move and ask about destination kasten."
  (interactive (list (completing-read "Which kasten to move to? " ezeka-kaesten)
                     current-prefix-arg))
  (let ((lines (count-lines (point-min) (point-max)))
        (moved 1)
        (zk-alist (zk--alist (zk--directory-files))))
    (goto-char (point-min))
    (while (re-search-forward zk-id-regexp nil t)
      (let* ((id (match-string-no-properties 1))
             (title (buffer-substring-no-properties
                     (point-at-bol) (match-beginning 0)))
             (file (zk--parse-id 'file-path id zk-alist)))
        (when (and file
                   (or arg
                       (y-or-n-p
                        (format "[%d/%d] Move %s [%s] to %s? "
                                moved lines id title kasten))))
          (ezeka-zmove-to-another-kasten file kasten nil t)
          (cl-incf moved))))))

(defun ezeka-zk-set-parent (filename &optional new-parent)
  "Sets the parent metadata of the FILENAME to NEW-PARENT. If
NEW-PARENT is NIL, let user choose the the Zettel."
  (interactive (list (ezeka--grab-dwim-file-target) nil))
  (let ((new-parent (or new-parent (zk--select-file))))
    (ezeka--update-metadata-values filename
                                   :parent (ezeka-file-link new-parent))))

;;;=============================================================================
;;; Mapping Across Zk-Index Buttons
;;;=============================================================================

(defun ezeka-zk-map-buttons (func &optional buffer beg end)
  "Like `widget-map-buttons' but for zk-index buttons."
  (mapc func (zk-index--current-button-list buffer beg end)))

(defun ezeka-zk-map-button-files (func &optional buffer beg end)
  "Like `widget-map-buttons' but for zk-index buttons. FUNC should be
a function accepting arguments FILE, COUNTER, TOTAL-FILES. Returns
list of files mapped across."
  (let* ((buttons (zk-index--current-button-list buffer beg end))
         (total (length buttons))
         (n 0)
         results)
    (dolist (button buttons (nreverse results))
      (let ((file (zk--triplet-file (button-get button 'zk-triplet))))
        (funcall func file n total)
        (push file results)
        (cl-incf n)))))

(defmacro define-zk-index-mapper (name func &optional docstring &rest body)
  "Define an interactive function to map a function across the files
in the active region of the current ZK-Index buffer. FUNC is the
function to map with, taking filename as the only argument. BODY is
the body of the index mapper command.

\fn(NAME (ARG) &OPTIONAL DOCSTRING &BODY BODY)"
  (declare (indent 2))
  (let ((arg (gensym "arg"))
        (beg (gensym "beg"))
        (end (gensym "end"))
        (file (gensym "file"))
        (files (gensym "files")))
    `(defun ,name (,arg ,beg ,end)
       ,docstring
       (interactive (if (region-active-p)
                        (list current-prefix-arg
                              (region-beginning) (region-end))
                      (list current-prefix-arg
                            (point-at-bol) (point-at-eol))))
       (let ((ezeka-save-after-metadata-updates t)
             (ezeka-update-modification-date 'never)
             (inhibit-read-only t))
         ,@body
         (let ((,files (nreverse (ezeka-zk-map-button-files ,func nil ,beg ,end))))
           (delete-region ,beg ,end)
           (dolist (,file ,files ,files)
             (zk-index--insert-button ,file)
             ;;(insert "\n")
             ))))))

(defvar ezeka-zk--last-genus nil
  "Used by `ezeka-zk-index-set-genus' to hold the last set genus.")

(defun ezeka-zk-index-set-genus (file)
  "Set the genus for the Zettel at point of the current Zk-Index buffer, saving
the file without asking."
  (interactive (list (ezeka--grab-dwim-file-target)))
  (let ((ezeka-save-after-metadata-updates t)
        (ezeka-update-modification-date 'never)
        (inhibit-read-only t)
        (genus (ezeka-read-genus nil nil ezeka-zk--last-genus)))
    (ezeka-set-genus file genus)
    (setq ezeka-zk--last-genus genus)
    (when zk-index-view-mode
      (with-current-buffer zk-index-buffer-name
        (when (file-equal-p (ezeka--grab-dwim-file-target) file)
          (delete-region (point-at-bol) (point-at-eol))
          (zk-index--insert-button file))))
    (zk-index-next-line)))

(define-zk-index-mapper ezeka-zk-index-mass-set-genus
    (lambda (file &rest args) (ezeka-set-genus file ezeka-zk--last-genus))
  "Set the genus for all notes in the active region and save the
files without asking."
  (setq ezeka-zk--last-genus
    (ezeka-read-genus nil "Mass-set what genus? " ezeka-zk--last-genus)))

(define-zk-index-mapper ezeka-zk-index-set-citekey
    (lambda (file &rest args) (ezeka-set-citekey file nil current-prefix-arg))
  "Set the citekey for all notes in the active region and save the
files without asking.")

(define-zk-index-mapper ezeka-zk-index-set-title
    (lambda (file &rest args) (ezeka-set-title file))
  "Set the title for all notes in the active region and save the files without
asking.")

(define-zk-index-mapper ezeka-zk-index-set-rubric
    (lambda (file &rest args)
      (let* ((metadata (ezeka-file-metadata file))
             (updated (ezeka-decode-rubric
                       (read-string "Change rubric to what? "
                                    (car (ezeka-encode-rubric metadata))))))
        (while updated
          (setf (alist-get (pop updated) metadata) (pop updated)))
        (ezeka--update-metadata-values file metadata)))
  "Set the entire rubric line for all notes in the active region and save the
files without asking.")
;;;=============================================================================
;;; Other
;;;=============================================================================

(defun ezeka-zk-insert-link-to-kasten (&optional kasten)
  "Call `zk-insert-link' after temporarily setting zk variables to be
appropriate for the particular Zettelkasten."
  (interactive (list (if current-prefix-arg
                         (completing-read "Kasten: " ezeka-kaesten)
                       "rumen")))
  (ezeka-zk-with-kasten kasten
    (call-interactively 'zk-insert-link)))

(defun ezeka-zk-find-note-in-kasten (arg &optional kasten)
  "Call `zk-find-file' after temporarily setting zk variables to be appropriate
for the particular Zettelkasten. Defaults to the Kasten set in
`zk-directory', if any. With prefix arg, ask to select Kasten."
  (interactive (list current-prefix-arg
                     (if-let ((kasten
                               (and (not current-prefix-arg)
                                    (ezeka-directory-kasten zk-directory))))
                         kasten
                       (completing-read "Kasten: " ezeka-kaesten))))
  (ezeka-zk-with-kasten kasten
    (call-interactively 'zk-find-file)))

(defun ezeka-rgrep-link-at-point (link)
  "Executes recursive grep for the ezeka link at point."
  (interactive
   (list (when (ezeka-link-at-point-p t)
           (ezeka-link-at-point))))
  (consult-grep ezeka-directory link))

(provide 'ezeka-zk)
