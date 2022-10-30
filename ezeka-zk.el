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

;; For these avariables to be treated as dynamic, need to declare them first
;; here.
(defvar zk-directory)
(defvar zk-id-regexp)
(defvar zk-id-format)

(defmacro ezeka-zk-with-kasten (kasten &rest body)
  "Lexically bind variables for executing BODY in KASTEN."
  (declare (indent 1))
  `(let ((zk-directory (ezeka-kasten-directory ,kasten))
         (zk-id-regexp (ezeka--id-regexp :all)))
     (cl-progv '(zk-id-time-string-format
                 zk-file-name-id-only)
         (if (eq (ezeka-kasten-id-type ,kasten) :numerus)
             '(,(concat (cl-subseq (downcase (format-time-string "%a")) 0 1)
                        "-%H%M")
               nil)
           '("%Y%m%dT%H%M"
             t))
       ,@body)))

(defun ezeka-zk-initialize-kasten (kasten)
  "Set necessary variables for long-term work in KASTEN."
  (setq zk-directory (ezeka-kasten-directory kasten)
        zk-id-regexp (ezeka--id-regexp :all))
  (if (eq (ezeka-kasten-id-type kasten) :numerus)
      (setq zk-id-time-string-format
            (concat (cl-subseq (downcase (format-time-string "%a")) 0 1) "-%H%M"))
    (setq zk-id-time-string-format "%Y%m%dT%H%M"
          zk-index-format "%t [[%i]]")))

(defcustom ezeka-zk-index-format "*Zk-Index: %k*"
  "Format string to use when creating Zk index buffers.
%k means capitalized kasten.
%K means upcased kasten."
  :type 'string)

(defun ezeka-zk--index-buffer-name (kasten)
  "Return a name for an zk index buffer for KASTEN.
The format is customizable via `ezeka-zk-index-format'."
  (format-spec ezeka-zk-index-format
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
                       (mapcar (lambda (kdef)
                                 (let ((k (car kdef)))
                                   (if (assoc-string k active)
                                       (propertize k 'face 'bold-italic)
                                     k)))
                               ezeka-kaesten)))
    current-prefix-arg))
  (let ((with-captions (eq (ezeka-kasten-id-type kasten) :numerus)))
    (custom-set-variables
     '(zk-directory-subdir-function #'ezeka-subdirectory)
     `(zk-index-buffer-name ,(ezeka-zk--index-buffer-name kasten))
     `(zk-file-name-id-only ,(not with-captions))
     `(zk-parse-file-function (if ,with-captions
                                  #'zk-parse-file-name
                                #'zk-parse-file-header)))
    (ezeka-zk-initialize-kasten kasten)
    (unless arg
      (zk-index))))

(defun ezeka-zk--current-active-indexes ()
  "Returns an alist of currently active kasten and their respective Zk
index buffers."
  (let ((regexp
         (replace-regexp-in-string "%[Kk]"
                                   "\\\\(.*\\\\)"
                                   (regexp-quote ezeka-zk-index-format))))
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

(defun ezeka--citaton-key-authors (key)
  "Given a citation KEY, returns a human-readable list of authors."
  (let ((case-fold-search nil))
    (when (string-match (concat "^[@&]*\\(?1:[A-Z][a-z]+\\)"
                                "\\(?:\\(?2:[A-Z][a-z]+\\)"
                                "\\(?3:EtAl\\)*\\)*\\(?4:[0-9-]+\\)*$") key)
      (let ((author1 (match-string 1 key))
            (author2 (match-string 2 key))
            (etal (match-string 3 key))
            (date (match-string 4 key)))
        (cond (etal
               (format "%s, %s, et al." author1 author2))
              (author2
               (format "%s and %s" author1 author2))
              (t
               author1))))))

(defun ezeka-zk-format-link-and-title (id title)
  "See `zk-format-link-and-title-function'."
  (let ((file (ezeka-link-file id)))
    (when (ezeka-note-p file)
      (let* ((mdata (ezeka-file-metadata file)))
        (format-spec "%a%t [[%i]]"
                     `((?a . ,(if-let ((ck (alist-get :citekey mdata)))
                                  (format "%s's " (ezeka--citaton-key-authors ck))
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
    (user-error "Use `ezeka-entitle-file-name' for singe files.")))

(defun ezeka-zk-move-all-in-region (start end kasten arg)
  "Move all files listed in the active region of zk index to KASTEN. With
prefix argument, confirm each move and ask about destination kasten."
  (interactive
   (append (if (region-active-p)
               (list (region-beginning) (region-end))
             (list (point-min) (point-max)))
           (list (completing-read "Which kasten to move to? " ezeka-kaesten)
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

(defun ezeka-zk-grep-in-zettelkasten (string)
  "Runs a recursive grep (`rgrep') for the given STRING across all Zettel."
  (interactive "sSearch for what? ")
  (grep-compute-defaults)
  (let ((zk-directory ezeka-directory))
    (zk-index-search (string-replace " " ".*" string))))

(defun ezeka-zk-replace-links (before after &optional directory)
  "Replace BEFORE links to AFTER links in DIRECTORY.
DIRECTORY defaults to `ezeka-directory' if not given. If AFTER is nil,
replace the link with {{BEFORE}}. Returns a tuple of number of links
replaced in number of files."
  (interactive
   (if current-prefix-arg
       (list (read-string "Before: ")
             (read-string "After: "))
     (let* ((zk-directory ezeka-directory)
            (files (zk--directory-files)))
       (list (ezeka-file-link (zk--select-file "Before: " files))
             (ezeka-file-link (zk--select-file "After: " files))))))
  (let ((with-links
         (let ((zk-directory (or directory ezeka-directory)))
           (zk--grep-file-list
            (format "(parent: %s$|%s]])" before before) t)))
        (count 0))
    (if (not with-links)
        (progn (message "No links to %s found" before) nil)
      (dolist (f with-links count)
        (let ((open-buffer (get-file-buffer f)))
          (save-excursion
            (with-current-buffer (or open-buffer
                                     (find-file-noselect f))
              (let ((f-mdata (ezeka-file-metadata f)))
                (when (string= (alist-get :parent f-mdata) before)
                  (setf (alist-get :parent f-mdata) after)
                  (ezeka--update-file-header f f-mdata)
                  (cl-incf count))
                (goto-char (point-min))
                (while (re-search-forward
                        (regexp-quote (ezeka-org-format-link before)) nil t)
                  (replace-match (save-match-data
                                   (if after
                                       (ezeka-org-format-link after)
                                     (format "{{%s}}" before))))
                  (cl-incf count)))
              (save-buffer)
              (unless open-buffer
                (kill-buffer (current-buffer)))))))
      (message "Replaced %d link(s) in %d files" count (length with-links))
      (cons count (length with-links)))))

(defun ezeka-zk-delete-note (link-or-file &optional change-to)
  "Delete the Zettel note with the given LINK-OR-FILE, updating any existing
links with CHANGE-TO, if given, or with the parent, if one is set."
  (interactive (list (ezeka--grab-dwim-file-target)))
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
  "Insert link in the current buffer for the button ID at point in
`zk-index-buffer-name'"
  (interactive)
  (let ((id (with-current-buffer zk-index-buffer-name
              (zk-index--button-at-point))))
    (ezeka-insert-link-with-metadata id :title :before t)))

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
         (expand-file-name (or (ezeka-subdirectory id)
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
  "Rename the given FILE to include rubric in the file name. *Without*
\\[universal-argument], edit the resulting file name before renaming."
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
;;; Advice
;;;=============================================================================

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
  (cond ((not transform) "ezk")
        ((string-match (zk-file-name-regexp) file)
         (match-string 2 file))
        (t (let ((mdata (ezeka-file-metadata file)))
             (ezeka-format-metadata "{%l} %c <%K>" mdata)))))
(advice-add 'zk--group-function :override 'adv--zk-group-function)

(defun ezeka-zk-file-name-regexp ()
  "Return the correct regexp matching zk file names.
Unlike `zk-file-name-regexp', the space and title after it are
optional. The regexp captures these groups:

Group 1 is the zk ID.
Group 2 is the title."
  (concat "\\(?1:" zk-id-regexp "\\)"
          "\\(?: \\(?2:.*\\)\\)*"
          "\\."
          zk-file-extension
          ".*"))
(defalias 'zk-file-name-regexp 'ezeka-zk-file-name-regexp)

(provide 'ezeka-zk)
