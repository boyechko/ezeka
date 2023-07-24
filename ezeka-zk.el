;;; ezeka-zk.el --- Eclectic Zettelkasten & Zk Integration -*- lexical-binding: t -*-

;; Copyright (C) 2022-2023 Richard Boyechko

;; Author: Richard Boyechko <code@diachronic.net>
;; Version: 0.2
;; Package-Requires: ((emacs "28.1") (ezeka "0.8") (zk "0.4") (zk-index "0.4"))
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

;; For these avariables to be treated as dynamic, need to declare them first
;; here.

;;; Code:

(defvar zk-directory)
(defvar zk-id-regexp)
(defvar zk-id-format)

(defmacro ezeka-zk-with-kasten (kasten &rest body)
  "Lexically bind variables for executing BODY in KASTEN."
  (declare (indent 1))
  `(let ((ezeka-kasten (ezeka-kasten-named ,kasten))
         (zk-directory (ezeka-kasten-directory ,kasten))
         (zk-id-regexp (ezeka--id-regexp)))
     (cl-progv '(zk-id-time-string-format
                 zk-file-name-id-only)
         (if (eq (ezeka-kasten-id-type ezeka-kasten) :numerus)
             '(,(concat (cl-subseq (downcase (format-time-string "%a")) 0 1)
                        "-%H%M")
               nil)
           '("%Y%m%dT%H%M"
             t))
       ,@body)))

(defun ezeka-zk-initialize-kasten (name)
  "Set necessary variables for long-term work in Kasten with given NAME."
  (setq zk-directory (ezeka-kasten-directory name)
        zk-id-regexp (ezeka--id-regexp))
  (let ((kasten (ezeka-kasten-named name)))
    (if (eq (ezeka-kasten-id-type kasten) :numerus)
        (setq zk-index-format "%i {%l} %c"
              zk-id-time-string-format
              (concat (cl-subseq (downcase (format-time-string "%a")) 0 1) "-%H%M"))
      (setq zk-id-time-string-format "%Y%m%dT%H%M"
            zk-index-format "{%-12l} %c [[%i]]"))))

(defcustom ezeka-zk-index-buffer-format "*Zk-Index: %k*"
  "Format string to use when creating Zk index buffers.
%k means capitalized kasten.
%K means upcased kasten."
  :type 'string)

(defun ezeka-zk--index-buffer-name (kasten)
  "Return a name for an zk index buffer for KASTEN.
The format is customizable via `ezeka-zk-index-buffer-format'."
  (format-spec ezeka-zk-index-buffer-format
               `((?k . ,(capitalize kasten))
                 (?k . ,(upcase kasten)))))

;;;###autoload
(defun ezeka-zk-index-switch-to-kasten (kasten &optional arg)
  "Create or switch to Zk-Index buffer for given KASTEN.
With \\[universal-argument] ARG, don't actually switch, just set up
the environment."
  (interactive
   (list
    (let ((active (ezeka-zk--current-active-indexes)))
      (completing-read "Switch to Kasten: "
                       (mapcar (lambda (k)
                                 (let ((name (ezeka-kasten-name k)))
                                   (if (assoc-string name active)
                                       (propertize name 'face 'bold-italic)
                                     name)))
                               ezeka-kaesten)))
    current-prefix-arg))
  (custom-set-variables
   '(zk-subdirectory-function #'ezeka-id-subdirectory)
   `(zk-index-buffer-name ,(ezeka-zk--index-buffer-name kasten)))
  (ezeka-zk-initialize-kasten kasten)
  (unless arg
    (zk-index)))

(defun ezeka-zk--current-active-indexes ()
  "Return alist of currently active Kasten and their Zk index buffers."
  (let ((regexp
         (string-replace "%k" "\\(.*\\)"
          (regexp-quote ezeka-zk-index-buffer-format))))
    (delq nil
          (mapcar (lambda (buf)
                    (when (string-match regexp (buffer-name buf))
                      (cons (downcase (match-string 1 (buffer-name buf)))
                            buf)))
                  (buffer-list)))))

(defun ezeka-zk-new-note-header (title new-id &optional orig-id)
  "Insert header in new notes with args TITLE and NEW-ID.
Optionally use ORIG-ID for backlink."
  (ezeka-insert-header-template new-id nil title orig-id))

(defun ezeka-zk-index-button-display-action (file buffer &optional same-window)
  "Function to display FILE or BUFFER on button press in Index and Desktop.
If \\[universal-argument] SAME-WINDOW is non-nil, use same window.
See `zk-index-button-display-action'."
  (let ((same-window (or same-window current-prefix-arg)))
    (cond ((one-window-p)
           (pop-to-buffer buffer
                          (display-buffer-in-direction
                           buffer
                           '((direction . top)
                             (window-height . 0.6)))))
          (same-window
           (find-file file))
          (t (find-file-other-window file)))))

(defun ezeka-zk-format-function (format id title &optional metadata)
  "Format given ID and TITLE according to FORMAT.
Control sequences %i (ID), %c (caption), and %l (link) are
supported natively. For everything else or if METADATA is
non-nil, call `ezeka-format-metadata' instead."
  (if (not (string-match-p "%[^icl0-9-]" format))
      (format-spec format
                   `((?i . ,id)
                     ,@(if (string-match "^ *{\\(.*\\)} \\(.*\\)" title)
                           `((?c . ,(match-string 2 title))
                             (?l . ,(match-string 1 title)))
                         `((?c . ,title)
                           (?l . ,id)))))
    (unless metadata
      (warn "Parsing metadata for %s (%s)" title id))
    (let ((mdata (or metadata (ezeka-file-metadata (ezeka-link-file id)))))
      (ezeka-format-metadata format mdata))))

(defun ezeka-zk-parse-file (target files)
  "Parse FILES for TARGET.
See `zk-parse-file-function'."
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
  "An alist of file metadata and mtime, cached by ID.
Each item has the form (ID TITLE FILENAME MTIME METADATA).")

(defun ezeka-zk-cache-update-all ()
  "Update file list and update cached information for each file.
Return `ezeka-zk-metadata-alist'."
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

(defun ezeka-zk-set-parent (filename &optional new-parent other-window)
  "Set parent metadata of FILENAME to NEW-PARENT (a link).
If NEW-PARENT is NIL, let user choose the the Zettel, unless
\\[universal-argument] OTHER-WINDOW is non-nil and there is something
usable in the other window, in which case set that as the new parent.
With double \\[universal-argument], clear parent metadata."
  (interactive (list (ezeka--grab-dwim-file-target)
                     nil
                     (cond ((equal current-prefix-arg '(16))
                            :none)
                           (current-prefix-arg
                            (ezeka-file-link
                             (ezeka--note-in-other-window)))
                           (t
                            nil))))
  (let ((new-parent
         (or new-parent
             other-window
             (let ((kasten (ezeka--read-kasten "Parent's Kasten? ")))
               (ezeka-file-link
                (ezeka-zk-with-kasten kasten
                  (zk--select-file "Set parent to: ")))))))
    (ezeka--update-metadata-values filename nil
      :parent (if (not (eq new-parent :none))
                  new-parent
                (message "Parent metadata cleared")
                nil))))

;;;=============================================================================
;;; Mapping Across Zk-Index Buttons
;;;=============================================================================

(defun ezeka-zk-map-buttons (func &optional buffer start end)
  "Map FUNC across zk-index buttons in BUFFER between START and END.
Like `widget-map-buttons' but for zk-index buttons."
  (mapc func (zk-index--current-button-list buffer start end)))

(defun ezeka-zk-map-button-files (func &optional buffer start end)
  "Map FUNC across zk-index buttons in BUFFER between START and END.
FUNC should be a function accepting arguments FILE, COUNTER,
TOTAL-FILES. Return list of files mapped across."
  (let* ((buttons (zk-index--current-button-list buffer start end))
         (total (length buttons))
         (n 0)
         results)
    (dolist (button buttons (nreverse results))
      (let ((file (zk--triplet-file (button-get button 'zk-triplet))))
        (funcall func file n total)
        (push file results)
        (cl-incf n)))))

(defmacro define-zk-index-mapper (name func &optional docstring &rest body)
  "Define interactive function to map FUNC across Zk-Index region.
FUNC should take filename as the only argument. BODY is the body of
the index mapper command.

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
  "Set genus for the Zettel FILE at point in the Zk-Index buffer.
Afteward, save the file without asking."
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

(define-zk-index-mapper ezeka-zk-index-entitle
    (lambda (file n out-of)
      (ezeka-entitle-file-name file
                               current-prefix-arg
                               (format "[%d/%d] New file name: " n out-of)))
  "Entitle all files in the currently active region of Zk-Index
buffer. With \\[universal-argument], don't bother editing each new
name."
  (when (ezeka-note-p (current-buffer))
    (ezeka-entitle-file-name (buffer-file-name) current-prefix-arg)
    (save-buffer)
    (user-error "Use `ezeka-entitle-file-name' for singe files")))

(defun ezeka-zk-move-all-in-region (start end kasten arg)
  "Move files between START and END of Zk Index to KASTEN.
With \\[universal-argument] ARG, confirm each move and ask about
destination kasten."
  (interactive
   (append (if (region-active-p)
               (list (region-beginning) (region-end))
             (list (point-min) (point-max)))
           (list (completing-read "Which kasten to move to? "
                                  (mapcar #'ezeka-kasten-name ezeka-kaesten))
                 current-prefix-arg)))
  (let ((lines (count-lines start end))
        (moved 1)
        (zk-alist (zk--alist (zk--directory-files))))
    (goto-char start)
    (while (re-search-forward zk-id-regexp end t)
      (let* ((id (match-string-no-properties 1))
             (title (buffer-substring-no-properties
                     (point-at-bol) (match-beginning 0)))
             (file (zk--parse-id 'file-path id zk-alist)))
        (when (and file
                   (or arg
                       (y-or-n-p
                        (format "[%d/%d] Move %s [%s] to %s? "
                                moved lines id title kasten))))
          (ezeka-move-to-another-kasten file kasten nil t)
          (cl-incf moved))))))

;;;=============================================================================
;;; Other
;;;=============================================================================

(defun ezeka-zk-insert-link (file)
  "Insert link to the Zettel FILE.
Unlike `zk-insert-link', allows editing the title in the
minibuffer according to the value of `zk-link-and-title': if it's
'ask or t, the user can edit the title before it is inserted."
  (interactive (list (zk--select-file "Insert link to: ")))
  (let ((link (ezeka-file-link file)))
    (if-let ((_ (or (eq zk-link-and-title 't)
                    (and (eq zk-link-and-title 'ask)
                         (y-or-n-p "Include (edited) title? "))))
             (mdata (ezeka-file-metadata file t)))
        (zk--insert-link-and-title
         link
         (read-string "Title: " (alist-get :title mdata)))
      (zk--insert-link file))))

(defun ezeka-zk-insert-link-to-other-window ()
  "Insert the link to the Zettel note in the other window."
  (interactive)
  (ezeka-zk-insert-link (ezeka--note-in-other-window)))

(defun ezeka-zk-insert-link-to-kasten (&optional kasten)
  "Temporarily set zk variables for KASTEN and call `zk-insert-link'."
  (interactive
   (list (if current-prefix-arg
             (completing-read "Kasten: "
                              (mapcar #'ezeka-kasten-name ezeka-kaesten))
           (ezeka-kasten-name
            (cl-find (cl-reduce #'min ezeka-kaesten :key #'ezeka-kasten-order)
                     ezeka-kaesten
                     :key #'ezeka-kasten-order)))))
  (ezeka-zk-with-kasten kasten
    (call-interactively 'ezeka-zk-insert-link)))

(defun ezeka-zk-insert-link-to-numerus ()
  "Temporarily set zk variables for numerus and call `zk-insert-link'."
  (interactive)
  (ezeka-zk-with-kasten "numerus"
    (call-interactively 'ezeka-zk-insert-link)))

(defun ezeka-zk-insert-link-to-tempus ()
  "Temporarily set zk variables for tempus and call `zk-insert-link'."
  (interactive)
  (ezeka-zk-with-kasten "tempus"
    (call-interactively 'ezeka-zk-insert-link)))

(defun ezeka-zk-insert-link-to-scriptum ()
  "Temporarily set zk variables for tempus and call `zk-insert-link'."
  (interactive)
  (ezeka-zk-with-kasten "scriptum"
    (call-interactively 'ezeka-zk-insert-link)))

(defun ezeka-zk-find-note-in-kasten (arg &optional kasten)
  "Temporarily set zk variables for KASTEN and call `zk-find-file'.
Defaults to the Kasten set in `zk-directory', if any. With
\\[universal-argument] ARG, ask to select Kasten."
  (interactive
   (list current-prefix-arg
         (if-let ((kasten
                   (and zk-directory
                        (not current-prefix-arg)
                        (ezeka-directory-kasten zk-directory))))
             kasten
           (completing-read "Kasten: "
                            (mapcar #'ezeka-kasten-name ezeka-kaesten)))))
  (ezeka-zk-with-kasten kasten
    (call-interactively 'zk-find-file)))

(defun ezeka-zk-find-note-in-numerus ()
  "Find zk note in numerus currens Kasten."
  (interactive)
  (ezeka-zk-find-note-in-kasten nil "numerus"))

(defun ezeka-zk-find-note-in-tempus ()
  "Find zk note in tempus currens Kasten."
  (interactive)
  (ezeka-zk-find-note-in-kasten nil "tempus"))

(defun ezeka-zk-find-note-in-scriptum ()
  "Find zk note in scriptum Kasten."
  (interactive)
  (ezeka-zk-find-note-in-kasten nil "scriptum"))

(defun ezeka-rgrep-link-at-point (link)
  "Execute recursive grep for the ezeka LINK at point."
  (interactive
   (list (when (ezeka-link-at-point-p t)
           (ezeka-link-at-point))))
  (consult-grep ezeka-directory link))

(defun ezeka-zk-grep-in-zettelkasten (string &optional literal)
  "Run recursive grep (`rgrep') for the given STRING across all Zettel.
If LITERAL is non-nil, search for STRING literallyl."
  (interactive "sSearch for what? ")
  (grep-compute-defaults)
  (let ((zk-directory ezeka-directory))
    (zk--grep-file-list (if literal
                            (regexp-quote string)
                          (string-replace " " ".*" string)))))

(defun ezeka-zk-replace-links (before after &optional directory confirm)
  "Replace BEFORE links to AFTER links in DIRECTORY.
DIRECTORY defaults to `ezeka-directory' if not given. If AFTER is nil,
replace the link with {{BEFORE}}. Returns a tuple of number of links
replaced in number of files. If CONFIRM (or \\[universal-argument]) is
non-nil, check with user before replacing."
  (interactive
   (let* ((before (if current-prefix-arg
                      (read-string "Replace ... ")
                    (zk--select-file "Replace links to ..." files)))
          (after (if current-prefix-arg
                     (read-string (format "Replace %s with ...: " before))
                   (zk--select-file "... with a link to ..." files))))
     (list before after nil
           (y-or-n-p "Confirm before replacing? "))))
  (let* ((bf-id (ezeka-link-id before))
         (with-links
          (let ((zk-directory (or directory ezeka-directory)))
            (zk--grep-file-list
             (format "(parent: [a-z:]*%s$|%s]])" bf-id bf-id) t)))
         (count 0))
    (if (not with-links)
        (progn (message "No links to %s found" before) nil)
      (dolist (f with-links count)
        (let ((open-buffer (get-file-buffer f)))
          (save-excursion
            (with-current-buffer (or open-buffer
                                     (find-file-noselect f))
              (if confirm (switch-to-buffer (current-buffer)))
              (let ((f-mdata (ezeka-file-metadata f))
                    (link-regexp (format "\\[\\[[a-z:]*%s]]" bf-id))
                    (replacement (if after
                                     (ezeka--format-link after)
                                   (format "{%s~}" before))))
                ;; Replace parent
                (when (and (ezeka--parent-of-p f bf-id f-mdata)
                           (or (not confirm)
                               (y-or-n-p (format "Replace parent in %s (%s)? "
                                                 (alist-get :title f-mdata)
                                                 (alist-get :id f-mdata)))))
                  ;; FIXME: Worth extending to preserve multiple parents?
                  (setf (alist-get :parent f-mdata) after)
                  (ezeka--update-file-header f f-mdata)
                  (cl-incf count))
                (goto-char (point-min))
                (if confirm
                    (query-replace-regexp link-regexp replacement)
                  (while (re-search-forward link-regexp nil t)
                    (replace-match replacement t)
                    (cl-incf count))))
              (save-buffer)
              (unless open-buffer
                (kill-buffer (current-buffer)))))))
      (message "Replaced %d link(s) in %d files" count (length with-links))
      (cons count (length with-links)))))

(defun ezeka--parent-of-p (note1 note2 &optional metadata)
  "Return non-nil if NOTE1 is a child of NOTE2.
METADATA is NOTE's metadata."
  (let* ((mdata (or metadata (ezeka-file-metadata note1)))
         (parents (alist-get :parent mdata))
         (note2-id (ezeka-link-id note2))) ; FIXME: What if it's a file?
    (cl-typecase parents
      (null   nil)
      (string (string= parents note2-id))
      (cons   (cl-find note2-id parents :test #'string=))
      (t (error "Don't know how to handle" (type-of parents))))))

(defun ezeka-zk-delete-note (link-or-file &optional change-to)
  "Delete the Zettel at LINK-OR-FILE, updating existing links with CHANGE-TO.
If CHANGE-TO is not given, use the note's parent, if set. With
\\[universal-argument], ask the user to enter CHANGE-TO."
  (interactive
   (list (ezeka--grab-dwim-file-target)
         (when current-prefix-arg
           (read-string "Change links to what? "))))
  (let* ((file (if (ezeka-link-p link-or-file)
                   (ezeka-link-file link-or-file)
                 link-or-file))
         (link (if (ezeka-link-p link-or-file)
                   link-or-file
                 (ezeka-file-link link-or-file)))
         (mdata (ezeka-file-metadata file))
         (with-links (let ((zk-directory ezeka-directory))
                       (zk--grep-file-list
                        (format "(parent: %s|%s\\]\\])" link link) t)))
         (change-to
          (or change-to
              (when (> (length with-links) 0)
                (if-let* ((parent (alist-get :parent mdata))
                          (exists (file-exists-p (ezeka-link-file parent))))
                    parent
                  (read-string (concat link "has no parent or it doesn't exist, "
                                       "replace " (length with-links) " link(s) "
                                       "with what? ")
                               (concat "{{" link "}}"))))))
         (count 0))
    (ezeka-zk-replace-links link change-to)
    (when (y-or-n-p (format "Really delete %s %s? "
                            link (alist-get :title mdata)))
      (delete-file file)
      (kill-buffer-ask (get-file-buffer file)))))

(defun ezeka-zk-insert-link-to-index ()
  "Insert link to the ID at point in `zk-index-buffer-name'."
  (interactive)
  (let ((id (with-current-buffer zk-index-buffer-name
              (zk-index--button-at-point))))
    (ezeka-insert-link-with-metadata id '(:title) :before t)))

;;;=============================================================================
;;; Entitle
;;;=============================================================================

;; FIXME: This duplicates some functionality of `ezeka-find-link'
(defun ezeka-link-entitled-file (link title)
  "Return a full file path to the Zettel LINK with the given TITLE."
  (if (ezeka-link-p link)
      (let ((kasten (ezeka-link-kasten link))
            (id (ezeka-link-id link)))
        (expand-file-name
         (ezeka--normalize-title-into-caption
          (format "%s%s%s.%s"
                  id
                  ezeka-file-name-separator
                  title
                  ezeka-file-extension))
         (expand-file-name (or (ezeka-id-subdirectory id)
                               (unless noerror
                                 (error "Link not valid: %s" link)))
                           (ezeka-kasten-directory kasten))))
    (unless noerror
      (error "This is not a proper Zettel link: %s" link))))

;; FIXME: Temporary
(defun your-read-lines (file n)
  "Return first N lines of FILE."
  (with-temp-buffer
    (insert-file-contents-literally file)
    (cl-loop repeat n
             unless (eobp)
             collect (prog1 (buffer-substring-no-properties
                             (line-beginning-position)
                             (line-end-position))
                       (forward-line 1)))))

(defun ezeka-entitle-file-name (file &optional arg prompt)
  "Rename FILE to include caption in the file name.
*Without* \\[universal-argument] ARG, edit the resulting file name
before renaming If given, use the custom PROMPT."
  (interactive (list buffer-file-name
                     current-prefix-arg))
  (let* ((link (ezeka-file-link file))
         (metadata (ezeka-file-metadata file))
         (rubric (car (ezeka-encode-rubric metadata)))
         (title (cl-subseq rubric
                           (1+
                            (cl-position (string-to-char ezeka-file-name-separator)
                                         rubric))))
         (entitled (ezeka-link-entitled-file link title)))
    (let ((buf (find-file file))
          (newname (if arg
                       (file-name-base entitled)
                     (read-string (or prompt "New file name: ")
                                  (if (ezeka-file-name-title file)
                                      (file-name-base file)
                                    (file-name-base entitled))
                                  (file-name-base entitled)))))
      (cond ((string-empty-p newname)
             (message "Empty name; not renaming"))
            ((not (ezeka-file-name-valid-p newname))
             (user-error "New file name is not valid: %s" newname))
            (t
             (rename-file-and-buffer    ; FIXME: Defined in init-utilities.el
              file
              (expand-file-name
               (file-name-with-extension newname (file-name-extension entitled))
               (file-name-directory entitled)))))
      (when arg
        (kill-buffer-if-not-modified buf)))))

;;;=============================================================================
;;; Utility
;;;=============================================================================

(defun ezeka-zk-file-id (file)
  "Return the ID of the given FILE."
  (when (string-match (zk-file-name-regexp) file)
    (match-string-no-properties 1 file)))

(defun ezeka-zk-file-title (file)
  "Return the TITLE of the given FILE."
  (when (string-match (zk-file-name-regexp) file)
    (let ((id (match-string-no-properties 1 file))
          (title (match-string-no-properties 2 file)))
      (if (string= "." title)
          (or (alist-get :title (ezeka-file-metadata file t))
            "<no title>")
        title))))

;;;=============================================================================
;;; Breadcrumbs
;;;=============================================================================

;;;###autoload
(defun ezeka-zk-desktop-drop-breadcrumbs ()
  "Add the currently-visited Zettel to today's `zk-desktop'."
  (interactive)
  (unless (buffer-live-p zk-desktop-current)
    (zk-desktop-select))
  (when (and (file-exists-p (buffer-file-name))
             (ezeka-note-p (buffer-file-name)))
    (zk-desktop-send-to-desktop (buffer-file-name))))

;; (add-hook 'ezeka-mode-hook #'ezeka-zk-desktop-drop-breadcrumbs)

(provide 'ezeka-zk)
;;; ezeka-zk.el ends here
