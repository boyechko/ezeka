;;; ezeka.el --- Eclectic Zettelkasten -*- lexical-binding: t -*-

;; Copyright (C) 2015-2023 Richard Boyechko

;; Author: Richard Boyechko <code@diachronic.net>
;; Version: 0.3
;; Package-Requires: ((emacs "28.1") (org "9.5"))
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

;; This package provides a very personalized implementation of Zettelkasten
;; relying on Org, Deft (now moved to ezeka-deft.el) that began on 2015-06-31.

;;; Code:

(require 'org)
(require 'format-spec)
(require 'cl-lib)
(require 'cl-generic)

;;;=============================================================================
;;; Internal Variables
;;;=============================================================================

(defun ezeka-link-regexp ()
  "Return the regular expression that matches Zettel links.

Group 1 is the ID.
Group 2 is the kasten, if specified."
  (concat "\\(?:\\(?2:[[:alpha:]]+\\):\\)*" "\\(?1:" (ezeka--id-regexp) "\\)"))

(defvar ezeka-iso8601-date-regexp
  (rx word-boundary
    (group (repeat 4 digit))
    (* "-")
    (group (repeat 2 digit))
    (* "-")
    (group (repeat 2 digit)))
  "The regular expression that matches ISO 8601-like date.
Groups 1-3 are year, month, day.")

(defvar ezeka-iso8601-time-regexp
  "T*\\([0-9]\\{2\\}\\):*\\([0-9]\\{2\\}\\)\\>"
  "The regular expression that matches ISO 8601-like time.
Groups 1-2 are hour and minute.")

(defvar ezeka-iso8601-datetime-regexp
  (concat ezeka-iso8601-date-regexp ezeka-iso8601-time-regexp)
  "The regular expression that matches ISO 8601 date and time separate with T.
Groups 1-3 are year, month, day.
Groups 4-5 are hour and minute.")

(defvar ezeka-pregenerated-numeri-file "auto/unused-numeri.dat"
  "File containing a list of unused numeri currentes.")

;;;=============================================================================
;;; User Variables
;;;=============================================================================

(defcustom ezeka-directory nil
  "The central Zettelkasten directory."
  :type 'string
  :group 'ezeka)

(defcustom ezeka-file-extension "txt"
  "Default extension for Zettel files."
  :type 'string
  :group 'ezeka)

(defcustom ezeka-kaesten nil
  "An alist of `ezeka-kasten' structs populated by `ezeka-kaesten-add'.
See that function for details; do not edit by hand."
  :type 'alist
  :group 'ezeka)

(defcustom ezeka-categories nil
  "A list of categories used for Zettel."
  :type 'list
  :group 'ezeka)

(defcustom ezeka-genera nil
  "An alist of genera used for numerus currens Zettel.
Each element should be in the form
\(LATIN-LETTER GENUS DESCRIPTION)"
  :type 'list
  :group 'ezeka)

(defcustom ezeka-keywords nil
  "A list of frequently-used keywords.
Each element should be a string beginning with #."
  :type 'list
  :group 'ezeka)

(defcustom ezeka-dynamic-keywords t
  "If non-nil, generate a dynamic list of keywords.
The list is then merged with `ezeka-keywords'."
  :type 'boolean
  :group 'ezeka)

(defcustom ezeka-create-nonexistent-links 'confirm
  "Determine how to handle links to non-existent notes.
Possible valus are t (always create), nil (never create), or
'confirm (ask user)."
  :type 'symbol
  :options '(t nil confirm)
  :group 'ezeka)

(defcustom ezeka-number-of-frames nil
  "Try to use only this many frames. Nil means single frame."
  :type 'symbol
  :options '(one two many)
  :group 'ezeka)

(defcustom ezeka-header-update-modified t
  "Whether `ezeka--update-file-header' updates the modification date.
Possible choices are ALWAYS, SAMEDAY, NEVER, or CONFIRM (same as T)."
  :type 'symbol
  :group 'ezeka)

(defcustom ezeka-save-after-metadata-updates 'confirm
  "Whether to automatically save the file after modification.
Functions affected are `ezeka-set-label', `ezeka-set-citekey', and
`ezeka-set-title-or-caption'."
  :type 'symbol
  :options '(nil t confirm)
  :group 'ezeka)

(defcustom ezeka-rename-note-keyword "#rename"
  "Keyword to add to notes that need renaming.
Because git does not keep track of renames, significantly changing the
content confuses the default rename detection. It is helpful,
therefore, to split the operations into two commits."
  :type 'string
  :group 'ezeka)

(defcustom ezeka-after-save-hook '()
  "Hook ran after an Ezeka file is saved."
  :type 'list
  :group 'ezeka)

;;;=============================================================================
;;; Kaesten
;;;=============================================================================

(cl-defstruct (ezeka-kasten (:constructor ezeka-kasten--create)
                            (:copier nil))
  name id-type id-example id-regexp default order)

(defun ezeka-kasten-named (name)
  "Return the Kasten with given NAME in `ezeka-kaesten'."
  (cl-find name ezeka-kaesten :key #'ezeka-kasten-name :test #'string=))

(defun ezeka-kaesten-add (name id-example id-regexp default order)
  "Add a new Kasten to `ezeka-kaesten' with the given options.
NAME is a unique string, ID-EXAMPLE is an example of an ID, ID-REGEXP
is used to match IDs in this Kasten, DEFAULT specifies whether this is
the default Kasten for this ID-REGEXP, and ORDER determines relative
order in various `completing-read' invocations."
  (setq ezeka-kaesten
    (cl-remove name ezeka-kaesten :test #'string= :key #'ezeka-kasten-name))
  (add-to-list 'ezeka-kaesten
    (ezeka-kasten--create :name name
                          :id-type (intern (concat ":" name))
                          :id-example id-example
                          :id-regexp id-regexp
                          :default default
                          :order order)))

(ezeka-kaesten-add "numerus" "a-1234" "[a-z]-[0-9]\\{4\\}" t 1)
(ezeka-kaesten-add "tempus" "20230404T1713" "[0-9]\\{8\\}T[0-9]\\{4\\}" t 2)
(ezeka-kaesten-add "scriptum" "a-1234~01" "[a-z]-[0-9]\\{4\\}~[0-9][0-9]" t 3)

(defun ezeka-kasten-directory (kasten)
  "Return the directory of the Kasten named KASTEN."
  (if (ezeka-kasten-named kasten)
      (file-name-as-directory (in-ezeka-dir kasten))
    (error "Unknown Kasten: %s" kasten)))

(defun ezeka--id-regexp (&optional id-type)
  "Return the regexp for the given ID-TYPE based on `ezeka-kaesten'.
If ID-TYPE is not given, return a regexp that matches all known types."
  (let ((kasten (cl-find id-type ezeka-kaesten :key #'ezeka-kasten-id-type)))
    (concat "\\(?:"
            (if kasten
                (ezeka-kasten-id-regexp kasten)
              (mapconcat (lambda (k) (ezeka-kasten-id-regexp k))
                ezeka-kaesten
                "\\|"))
            "\\)")))

;;;=============================================================================
;;; General Functions
;;;=============================================================================

(defun in-ezeka-dir (&optional relative-path)
  "Return absolute path to RELATIVE-PATH in the Zettel directory."
  (unless ezeka-directory
    (error "No `ezeka-directory' set"))
  (expand-file-name (or relative-path "") ezeka-directory))

(defun ezeka--match-entire (regexp)
  "Wrap the REGEXP in ^...$ to match the entire string."
  (concat "^" (string-trim regexp "^" "$") "$"))

(defun space-or-punct-p (character)
  "Return T if the CHARACTER is a space or punctuation."
  (when character
    (string-match-p "[[:space:][:punct:]]" (char-to-string character))))

;; TODO: More extensible way to do this without invoking other modes?
(defun ezeka--grab-dwim-file-target (&optional link-at-point)
  "Return the do-what-I-mean Zettel file from a variety of modes.
If LINK-AT-POINT is non-nil, prioritize such a link if exists."
  (cond ((and link-at-point (ezeka-link-at-point-p))
         (ezeka-link-file (ezeka-link-at-point)))
        ((and buffer-file-name
              (or ezeka-mode (ezeka-note-p buffer-file-name t)))
         buffer-file-name)
        ((eq major-mode 'magit-status-mode) ; FIXME: magit
         (magit-file-at-point))
        ((eq major-mode 'zk-index-mode) ; FIXME: zk-index
         (if-let ((button (button-at (point))))
             (zk--triplet-file (button-get button 'zk-triplet))
           (zk--select-file)))
        ((eq major-mode 'deft-mode)     ; FIXME: deft
         (if-let ((button (button-at (point))))
             (button-get button 'tag)
           (ezeka-ivy-select-link)))
        (t
         (zk--select-file))))           ; FIXME: zk

(defun ezeka--replace-pairs-in-string (replacements string)
  "Replace pairs in the REPLACEMENTS alist in STRING.
Each item in REPLACEMENTS should have the form (FROM TO REGEXP). If
REGEXP is non-nil, FROM should be a regexp string."
  (save-match-data
    (dolist (recipe replacements string)
      (setq string
       (funcall (if (caddr recipe)
                    #'replace-regexp-in-string
                  #'string-replace)
                (car recipe)
                (cadr recipe)
                string)))))

(defun ezeka--regexp-strip-named-groups (regexp)
  "Strip the named groups in the given REGEXP."
  (replace-regexp-in-string "(\\?[0-9]+:" "(" regexp))

(defun ezeka--minibuffer-edit-string (old-string &optional new-string prompt)
  "Edit NEW-STRING in minibuffer, showing it parallel to OLD-STRING.
If NEW-STRING is nil, default to OLD-STRING. If given, PROMPT is shown
as the first line."
  (let ((new-string (or new-string old-string)))
    (read-string
     (concat prompt
             " Original: " (propertize old-string 'face 'italic) "\n"
             "Change to: ")
     new-string nil new-string)))

;; See https://stackoverflow.com/a/65685019
(defun ezeka--save-buffer-read-only (file)
  "Save the FILE's buffer without running hooks."
  (with-current-buffer (get-file-buffer file)
    (if buffer-read-only
        (save-buffer)
      (read-only-mode 1)
      (save-buffer)
      (read-only-mode 0))))

(defun ezeka--rename-file (filename newname)
  "Rename the given FILENAME to NEWNAME in two steps.
If NEWNAME is relative, fill missing values from FILENAME.
The rename happens in two steps to bypass issues with
case-insensitive file systems."
  (let* ((tempname (file-name-with-extension filename "~tmp"))
         (newname (if (file-name-absolute-p newname)
                      newname
                    (expand-file-name
                     (file-name-with-extension
                      newname (file-name-extension filename))
                     (file-name-directory filename)))))
    (cond ((not (file-exists-p filename))
           (set-visited-file-name newname t t))
          ((and nil (vc-backend filename))
           (vc-rename-file filename newname))
          (t
           (rename-file filename tempname t)
           (rename-file tempname newname t)
           (set-visited-file-name newname t t)))))

;; The following is adapted from
;; https://emacs.stackexchange.com/a/46059
(defface ezeka-read-only '((t :slant italic))
  "Face for `ezeka--read-only-region'."
  :group 'ezeka)

(defun ezeka--read-only-region (begin end)
  "Make the marked region between BEGIN and END read-only.
See also `ezeka--writeable-region'.

Read-only text is given the face `ezeka-read-only'."
  (interactive "r")
  (let ((inhibit-read-only t))
    (with-silent-modifications
      (add-text-properties begin end '(read-only t))
      (let ((overlay (make-overlay begin end)))
        (overlay-put overlay 'ezeka-text-type 'read-only)
        (overlay-put overlay 'face 'ezeka-read-only)))))

(defun ezeka--writeable-region (begin end)
  "Make the marked region between BEGIN and END writeable.
See also `ezeka--read-only-region'."
  (interactive "r")
  (let ((inhibit-read-only t))
    (with-silent-modifications
      (remove-text-properties begin end '(read-only t))
      (remove-overlays begin end 'ezeka-text-type 'read-only))))

(defun ezeka--concat-strings (separator &rest elements)
  "Concatenate ELEMENTS, separating them with SEPARATOR.
Any NULLs are stripped from ELEMENTS, and everything else is fed to
FORMAT."
  (mapconcat #'(lambda (elt)
                 (if (stringp elt)
                     elt
                   (format "%s" elt)))
    (cl-remove-if #'null elements)
    separator))

;;;=============================================================================
;;; Fundamental Functions
;;;=============================================================================

(defun ezeka-note-p (file-or-buffer &optional strict)
  "Return non-NIL if the FILE-OR-BUFFER is a Zettel.
It is a Zettel if all of these conditions are met:
1) its extension is `ezeka-file-extension';
2) its filename matches `ezeka-file-name-regexp'; and, if STRICT is non-NIL,
3) the file exists;
4) the file is inside `ezeka-directory'."
  (interactive "f")
  (when file-or-buffer
    (let ((file (cl-typecase file-or-buffer
                  (buffer (buffer-file-name file-or-buffer))
                  (string (expand-file-name file-or-buffer))
                  (t
                   (signal 'type-error
                           '("FILE-OR-BUFFER can only be file or buffer"))))))
      (when file
        (and (string-equal (file-name-extension file) ezeka-file-extension)
             (string-match (concat "^" (ezeka-file-name-regexp) "$")
                           (file-name-base file))
             (if strict
                 (and (file-exists-p file)
                      (string-prefix-p ezeka-directory file)) ; FIXME: Hack
               t))))))

(defun ezeka-directory-kasten (directory)
  "Return the kasten name of the given Zettel DIRECTORY."
  ;; FIXME: This is a hack that would not work if Kasten names don't match the
  ;; directory name.
  (file-name-base (directory-file-name directory)))

(defun ezeka-file-name-valid-p (filename)
  "Return non-nil if FILENAME is a valid Zettel filename."
  (save-match-data
   (string-match (ezeka-file-name-regexp) (file-name-base filename))))

(defun ezeka--file-name-part (filename part)
  "Return given PART (:id, :label, :caption, or :citekey) of FILENAME."
  (let ((base (file-name-base filename)))
    (save-match-data
      (when (string-match (ezeka-file-name-regexp) base)
        (let ((match (match-string (cl-case part
                                     (:id      1)
                                     (:label   3)
                                     (:caption 4)
                                     (:citekey 5))
                                   base)))
          (when match
            (string-trim match)))))))

(defmacro ezeka-file-name-id (filename)
  "Return the ID part of the given Zettel FILENAME."
  `(ezeka--file-name-part ,filename :id))
(defmacro ezeka-file-name-label (filename)
  "Return the label part of the given Zettel FILENAME."
  `(ezeka--file-name-part ,filename :label))
(defmacro ezeka-file-name-caption (filename)
  "Return the caption part of the given Zettel FILENAME."
  `(ezeka--file-name-part ,filename :caption))
(defmacro ezeka-file-name-citekey (filename)
  "Return the citekey part of the given Zettel FILENAME."
  `(ezeka--file-name-part ,filename :citekey))

(defun ezeka-file-kasten (file)
  "Return the Kasten of the given Zettel FILE."
  (let ((dirs (reverse (split-string (file-name-directory file) "/" t "/"))))
    (or (cl-find-if (lambda (dir) (ezeka-kasten-named dir)) dirs)
        ;; FIXME: Hack
        (ezeka-link-kasten (ezeka-file-name-id file))
        (error "Can't figure out kasten for %s" file))))

(defun ezeka-file-link (file)
  "Return a fully qualified link to FILE."
  (let ((kasten (ezeka-file-kasten file))
        (id (ezeka-file-name-id file)))
    (if (and id kasten)
        (ezeka-make-link kasten id)
      (error "Can't get id or kasten for file %s" (file-name-base file)))))

(defun ezeka-link-p (string)
  "Return non-NIL if the STRING could be a link to a Zettel."
  (and (stringp string)
       (string-match (concat "^" (ezeka-link-regexp) "$") string)
       ;; If kasten is specified, make sure it's a valid one
       (if-let ((kasten (match-string-no-properties 2 string)))
           (ezeka-kasten-named kasten)
         t)))

(defun ezeka-link-kasten (link &optional explicit)
  "Return the Kasten part of the given LINK.
If the link does not specify a Kasten, return the default one for the
given ID type. If EXPLICIT is non-nil, return nil if Kasten is not
explicitly given."
  (if (string-match (ezeka-link-regexp) link)
      (let* ((id (match-string 1 link))
             (kasten (match-string 2 link)))
        (or kasten
            (and (not explicit)
                 (ezeka-kasten-name
                  (cl-find-if (lambda (k)
                                (and (eq (ezeka-id-type id)
                                         (ezeka-kasten-id-type k))
                                     (ezeka-kasten-default k)))
                              ezeka-kaesten)))))
    (error "Invalid link %s" link)))

(defun ezeka-link-id (link)
  "Return the ID part of the given LINK."
  (when (string-match (ezeka-link-regexp) link)
    (match-string 1 link)))

(defun ezeka-make-link (kasten id)
  "Make a new proper link to ID in KASTEN (a string)."
  (let ((kstruct (ezeka-kasten-named kasten)))
    (cond ((not (ezeka-kasten-id-type kstruct))
           (error "Kasten %s is not in `ezeka-kaesten'" kasten))
          ((not (eq (ezeka-kasten-id-type kstruct)
                    (ezeka-id-type id)))
           (error "ID doesn't match the ID type for %s Kasten" kasten))
          (t
           id))))

(defun ezeka-id-subdirectory (id)
  "Return the subdirectory relative to Kasten for the given ID, a string."
  (file-name-as-directory
   (cl-case (ezeka-id-type id)
    ;; FIXME: Hardcoded
    (:numerus (cl-subseq id 0 1)) ; first letter
    (:tempus (cl-subseq id 0 4))  ; YYYY
    (:scriptum                    ; numerus
     (cl-subseq id 0
                (length
                 (ezeka-kasten-id-example (ezeka-kasten-named "numerus"))))))))

(defun ezeka-id-directory (id kasten)
  "Return the full directory under KASTEN where ID should be."
  (file-name-as-directory
   (file-name-concat
    (ezeka-kasten-directory kasten)
    (or (ezeka-id-subdirectory id)
        (error "Cannot get subdirectory for %s" id)))))

(defvar ezeka-file-name-separator " "
  "Separator to use between ID and CAPTION in file names.")

(defun ezeka--id-kaesten (id)
  "Return all kaesten for the ID's type."
  (let ((type (ezeka-id-type id)))
    (mapcar #'car
            (cl-remove-if-not (lambda (x)
                                (eq (cadr x) type))
                              ezeka-kaesten))))

(defun ezeka-link-file (link &optional caption noerror)
  "Return a full file path to the Zettel LINK.
CAPTION can be a string (including an empty string), in
which case return a filename consisting of LINK and CAPTION
separated with `ezeka-file-name-separator'. Alternatively,
if CAPTION is anything else (e.g. 'wildcard or nil), try
wildcard expansion for the file name beginning with the ID
given in LINK. If NOERROR is non-nil, do not raise an error
if file is not found."
  (unless (ezeka-link-p link) (error "Link not valid: %s" link))
  (let* ((id (ezeka-link-id link))
         (basename (format "%s%s.%s"
                           id
                           (cond ((string-empty-p caption)
                                  "")
                                 ((stringp caption)
                                  (concat ezeka-file-name-separator caption))
                                 (t
                                  "*"))
                           ezeka-file-extension))
         (dir (ezeka-id-directory id (ezeka-link-kasten link))))
    (if (stringp caption)
        (file-truename (expand-file-name basename dir))
      (let ((matches (flatten-list
                      (file-expand-wildcards
                       (expand-file-name basename dir)))))
        (cl-case (length matches)
          (0 (if noerror
                 nil
               (error "No matching files found for link %s" link)))
          (1 (file-truename (car matches)))
          (t (error "Found multiple file matches: %s" matches)))))))

(defun ezeka-id-type (id-or-file &optional noerror)
  "Return the type of the given ID-OR-FILE based on `ezeka-kaesten`.
If NOERROR is non-nil, don't signal an error if ID doesn't match a
known type."
  (let* ((id (file-name-base id-or-file))
         (kasten (cl-find-if (lambda (k)
                               (let ((regexp (ezeka-kasten-id-regexp k)))
                                 (string-match (concat "^" regexp) id)))
                             ezeka-kaesten)))
    (if kasten
        (ezeka-kasten-id-type kasten)
      (unless noerror
        (error "ID does not match any Kasten's ID pattern")))))

(defun ezeka-encode-iso8601-datetime (string)
  "Return the internal encoded time corresponding to STRING.
STRING should be an ISO8601 date/time expression, with or without time."
  (let ((second 0) (minute 0) (hour 0) day month year)
    (when (string-match (concat "^" ezeka-iso8601-date-regexp) string)
      (setq year  (string-to-number (match-string 1 string))
            month (string-to-number (match-string 2 string))
            day   (string-to-number (match-string 3 string)))
      (when (string-match ezeka-iso8601-time-regexp string
                          (match-end 0))
        (setq hour   (string-to-number (match-string 1 string))
              minute (string-to-number (match-string 2 string))))
      (encode-time second minute hour day month year))))

(defun ezeka-file-content (file &optional header-only noerror)
  "Return content of FILE, getting it first from opened buffer.
If NOERROR is non-NIL, don't signal an error if cannot get the
content. If HEADER-ONLY is non-nil, only get the header."
  (let ((retrieve-content
         (lambda (buffer)
           "Get the content from BUFFER."
           (save-excursion
             (with-current-buffer buffer
               (save-restriction
                 (widen)
                 (if (= 0 (buffer-size))
                     (unless noerror
                       (error "Buffer %s is empty" (current-buffer)))
                   (buffer-substring-no-properties
                    (point-min)
                    (if (not header-only)
                        (point-max)
                      (goto-char (point-min))
                      (if (re-search-forward ezeka-header-separator-regexp nil t)
                          (match-beginning 0)
                        (point-max)))))))))))
    (cond ((null file)
           (unless noerror
             (signal 'type-error
                     '("FILE is nil, but should be a string"))))
          ((get-file-buffer file)
           (funcall retrieve-content (get-file-buffer file)))
          ((file-exists-p file)
           (with-temp-buffer
             (insert-file-contents file)
             (funcall retrieve-content (current-buffer))))
          (t
           (unless noerror
             (error "Cannot get content for %s" file))))))

;;;=============================================================================
;;; Metadata: Internal
;;
;; Note on terminology:
;;
;; Metadata refers to the information about the Zettel note, like its created or
;; midified time, and so on. Header, on the other hand, is the actual
;; representation of that metadata inside the Zettel note.
;;
;; Rubric is the compressed metadata information that is added to the file name
;; in numerus currens notes. Caption is the shortened title, matching the file
;; name, that is included in the rubric for redundancy.
;;;=============================================================================

(defvar ezeka-header-line-regexp
  "\\(?1:\\w+\\):\\s-+\\(?2:.*\\)"
  "The regular expression that matches a line of YAML metadata.
Group 1 is the key.
Group 2 is the value.")

(defcustom ezeka-header-separator-regexp "^$"
  "Regexp that matches the separator line between header and the note text."
  :type 'string
  :group 'ezeka)

(defcustom ezeka-header-rubric-key "rubric"
  "The header metadata key for the rubric."
  :type 'string
  :group 'ezeka)

(defcustom ezeka-header-stable-caption-mark "§"
  "Mark that signifies stable caption.
The mark is used in the rubric value to show that any differences
between caption and title values should be ignored as long as filename
and header match."
  :type 'string
  :group 'ezeka)

(defcustom ezeka-header-rubric-format "%s%i {%l} %c %k"
  "The `format-spec' string for generating the note's rubric.
See `ezeka-format-metadata' for details.
This should match `ezeka-header-rubric-regexp'."
  :type 'string
  :group 'ezeka)

(defcustom ezeka-file-name-format "%i {%l} %c %k"
  "The `format-spec' string for generating a note's file name.
See `ezeka-format-metadata' for details. This should match
`ezeka-file-name-regexp'."
  :type 'string
  :group 'ezeka)

(defun ezeka-file-name-regexp ()
  "Return regexp matching numerus currens note file names.

Group 1 is the ID.
Group 2 is the kasten.
Group 3 is the label (genus or category).
Group 4 is the caption (i.e. short title).
Group 5 is the citation key.
Group 6 is the stable caption mark."
  (concat (ezeka-link-regexp)             ; \1 and \2
          "\\(?:"                         ; everything else is optional
          "\\(?:\\.\\)*"                  ; FIXME: optional historic period
          "\\(?: {\\(?3:[^}]+\\)}\\)*"    ; \3
          "\\(?4:.+?\\)"                  ; \4
          "\\(?: \\(?5:[@&]\\S-+\\)\\)*$" ; \5
          "\\)*"                          ; end of everything else
          ))

(defun ezeka-header-rubric-regexp ()
  "Regular expression for the rubric string as found in the header.

Groups 1-5 see `ezeka-file-name-regexp'.
Group 6 is the stable caption mark."
  (concat "\\(?6:" ezeka-header-stable-caption-mark "\\)*"
          (ezeka-file-name-regexp)))

(defvar ezeka-genus-regexp "[α-ω]"
  "Regexp matching genus.")

(defun ezeka--citaton-key-authors (key)
  "Return a human-readable list of authors for citation KEY."
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

(defun ezeka-format-metadata (format-string metadata)
  "Format a string out of FORMAT-STRING and METADATA.
The format control string may contain the following %-sequences:

%a means list of cited authors.
%c means caption (i.e. short title).
%i means ID or link.
%k means citation key.
%K means kasten.
%l means label (genus or category).
%s means stable mark (see `ezeka-header-stable-caption-mark').
%t means title."
  (string-trim
   (format-spec format-string
                `((?a . ,(if-let ((ck (alist-get :citekey metadata)))
                             (format "%s's " (ezeka--citaton-key-authors ck))
                           ""))
                  (?i . ,(alist-get :id metadata))
                  (?K . ,(alist-get :kasten metadata))
                  (?l . ,(alist-get :label metadata))
                  (?c . ,(alist-get :caption metadata))
                  (?t . ,(alist-get :title metadata))
                  (?k . ,(let ((citekey (alist-get :citekey metadata)))
                           (cond ((or (not citekey)
                                      (string-empty-p citekey))
                                  "")
                                 ((string-match-p "^[@&]" citekey)
                                  citekey)
                                 (t
                                  (concat "@" citekey)))))
                  (?s . ,(if (alist-get :caption-stable metadata)
                             ezeka-header-stable-caption-mark
                           ""))))))

(defun ezeka-decode-rubric (rubric)
  "Return alist of metadata from the RUBRIC line.
If cannot decode, return NIL."
  (when (and rubric (string-match (ezeka-header-rubric-regexp) rubric))
    (let ((id          (match-string 1 rubric))
          (kasten      (match-string 2 rubric))
          (label       (match-string 3 rubric))
          (caption     (match-string 4 rubric))
          (stable      (when (match-string 6 rubric) t))
          (citekey     (match-string 5 rubric)))
      (list (cons :id id)
            (when kasten (cons :kasten (string-trim kasten)))
            (cons :type (ezeka-id-type id))
            (cons :label label)
            (when caption (cons :caption (string-trim caption)))
            (cons :caption-stable stable)
            (when citekey (cons :citekey (string-trim citekey)))))))

(defmacro ezeka-encode-rubric (metadata)
  "Return a string that encodes the given METADATA into the rubric.
The produced string is based on `ezeka-header-rubric-format'."
  (ezeka-format-metadata ezeka-header-rubric-format metadata))

(defun ezeka--header-yamlify-key (keyword)
  "Return a YAML-formatted string name of the KEYWORD symbol."
  (cl-subseq (symbol-name keyword) 1))

(defun ezeka--header-yamlify-value (value)
  "Return a YAML-formatted string for the given metadata VALUE."
  (cl-typecase value
    (string value)
    (list (concat "[ " (mapconcat #'identity value ", ") " ]"))
    (t
     (error "Not implemented for type %s" (type-of value)))))

(defun ezeka--header-deyamlify-value (value)
  "Return an elisp version of the given YAML-formatted VALUE."
  (pcase value
    ;; strip [[ ]] in wiki links
    ((rx bol "[[" (let inside (1+ anychar)) "]]" eol)
     inside)
    ;; strip [ ] in org-style timestamps
    ((rx bol "[" (let inside (seq digit (1+ anychar) digit)) "]" eol)
     inside)
    ;; remaining [ ] should be lists
    ((rx bol "[ " (let inside (1+ anychar)) " ]" eol)
     (split-string inside "," t "[[:space:]]+"))
    (_
     (string-trim value))))

(defun ezeka--header-normalize-readings (readings)
  "Normalize the value of READINGS list, returning the normalized list."
  (mapcar (lambda (instance)
            (if (string-match org-ts-regexp3 instance)
                (org-timestamp-format
                 (org-timestamp-from-string instance) "%F")
              instance))
          readings))

(defun ezeka--decode-header-make-tuple (key value)
  "Decodes the given KEY and VALUE as strings, returning a tuple."
  (cons key
        (if (eq key :readings)
            (ezeka--header-normalize-readings value)
          value)))

(defun ezeka--decode-header (header file &optional noerror)
  "Return metadata alist decoded from FILE's YAML HEADER.
They keys are converted to keywords. If NOERROR is non-nil, do not
signal an error when encountering malformed header lines."
  (let* ((metadata
          (mapcar
           (lambda (line)
             (when (> (length line) 0)
               (if (string-match ezeka-header-line-regexp line)
                   (ezeka--decode-header-make-tuple
                    (intern (concat ":" (match-string 1 line)))
                    (ezeka--header-deyamlify-value (match-string 2 line)))
                 (unless noerror
                   (error "Malformed header line: '%s'" line)))))
           (split-string header "\n")))
         (decoded (ezeka-decode-rubric (alist-get :rubric metadata))))
    (append decoded metadata)))

(defun ezeka--header-region (buffer)
  "Return a tuple of (START. END) for the header in Ezeka BUFFER."
  (if (ezeka-note-p buffer)
      (save-excursion
        (with-current-buffer buffer
          (goto-char (point-min))
          (cons (point)
                (if (re-search-forward ezeka-header-separator-regexp nil t)
                    (match-end 0)
                  (point-max)))))
    (error "Not an Ezeka note")))

(defun ezeka--make-header-read-only (buffer)
  "Make the header in the Zettel BUFFER read-only."
  (let ((beg-end (ezeka--header-region buffer)))
    (with-current-buffer buffer
      (ezeka--read-only-region (car beg-end) (cdr beg-end)))))

(defun ezeka-toggle-header-read-only ()
  "Toggle header being read-only in the current Zettel buffer."
  (interactive)
  (let ((beg-end (ezeka--header-region (current-buffer))))
    (save-excursion
      (if (cl-find-if (lambda (ol)
                        (overlay-get ol 'ezeka-text-type))
                      (overlays-at (car beg-end)))
          (ezeka--writeable-region (car beg-end) (cdr beg-end))
        (ezeka--read-only-region (car beg-end) (cdr beg-end))))))

(defun ezeka-file-metadata (file &optional noerror)
  "Return an alist of metadata for FILE.
If NOERROR is non-nil, do not signal errors. The keys are
converted to keywords."
  (if-let* ((file (expand-file-name file ezeka-directory))
            (header (ezeka-file-content file t noerror)))
      (let* ((mdata  (ezeka--decode-header header file noerror))
             ;; Fill in any missing values for :ID, :TYPE, :KASTEN, and :LINK
             (id     (or (ezeka-file-name-id file)
                         (alist-get :id mdata)))
             (type   (or (alist-get :type mdata)
                         (ezeka-id-type file)))
             (kasten (or (alist-get :kasten mdata)
                         (ezeka-file-kasten file)))
             (link   (or (ignore-errors (ezeka-file-link file))
                         (ezeka-make-link kasten id)))
             ;; TODO: Remove after full transition from v0.1 to v0.2
             (title   (or (alist-get :title mdata)
                          (alist-get :caption mdata)))
             (caption (or (alist-get :caption mdata)
                          (ezeka-file-name-caption file)
                          title)))
        (cl-mapc (lambda (key val)
                   (setf (alist-get key mdata) val))
                 '(:id :type :kasten :link :title :caption)
                 `(,id ,type ,kasten ,link ,title ,caption))
        mdata)
    (unless noerror
      (error "Cannot retrieve %s's header" file))))

;;;=============================================================================
;;; Metadata Commands
;;;=============================================================================

;; FIXME: Specify type (scalar/list) of data expected
(defconst ezeka-metadata-valid-fields
  '((:rubric)
    (:title)
    (:subtitle)
    (:author)
    (:created)
    (:modified)
    (:parent)
    (:firstborn)
    (:oldnames)
    (:readings)
    (:keywords))
  "An alist of valid metadata fields.
The format of each item should be as follows:
    (:FIELD).
The order of items will affect how the metadata is written into the
file header.")

(defun ezeka-set-metadata-value (metadata field value)
  "Set METADATA's FIELD to VALUE after doing some checking.
Return the original METADATA with the field changed."
  (setf (alist-get field metadata) value)
  metadata)

;; See https://help.dropbox.com/organize/file-names
(defun ezeka--pasturize-for-filename (title)
  "Return TITLE after making it safe to use as file caption.
The function attemps to shorten the title, and strip or replace
troublesome characters."
  (interactive (list (file-name-base buffer-file-name)))
  (let ((replacements
         '(("{β} [A-Za-z. -]+ \\<\\(?1:[A-Za-z-]+\\)'s \\(?2:[/\"_][^/\"]+[/\"_]\\) (\\(?3:[0-9]\\{4\\}\\)) \\(?4:@\\1\\3\\)"
            "{β} \\2 \\4" t)
           ("{β} [A-Za-z. -]+ \\<\\(?1:[A-Za-z-]+\\)'s \\(?2:[/\"_][^/\"]+[/\"_]\\) (\\(?3:[0-9]\\{4\\}\\)) \\(?4:@\\1.*\\)"
            "{β} \\2 (\\3) \\4" t)
           ("{π} [A-Za-z. -]+ \\<\\(?1:[A-Za-z-]+\\)'s \\(?2:[/\"_][^/\"]+[/\"_]\\) (\\(?3:[0-9]\\{4\\}\\))\\(?: [@&]\\1[0-9]+\\)"
            "{π} \\2 &\\1\\3" t)
           ("{π} [A-Za-z. -]+ \\<\\(?1:[A-Za-z-]+\\)'s \\(?2:[/\"_][^/\"]+[/\"_]\\) \\(?:(\\(?3:[0-9]\\{4\\}\\))\\)*\\(?: [@&]\\1[0-9]+\\)*"
            "{π} \\2 &\\1\\3" t)
           ("\"\\([^\"]+\\)\"" "'\\1'" t)
           ("/\\([^/]+\\)/" "_\\1_" t)
           ("(\\(ß.+\\))" "[\\1]" t)
           ("\\(?1:.*\\) \\(?2:ß.+\\): \\(?3:.*\\)" "\\1 \\3 [\\2]" t)
           ("/" "-")
           ("\\" "-")
           ("<" "[")
           (">" "]")
           (":" ",")
           ("\"" "")
           ("|" "-")
           ("?" "_")
           ("*" "_")
           ("." ""))))
    (ezeka--replace-pairs-in-string replacements title)))

(defun ezeka--set-time-of-creation (metadata)
  "Possibly update the time of creation in the METADATA.
The creation time is updated if 1) the current time is at 00:00 or is
missing and something else appears in the tempus currens, or 2) one of
the old names is a tempus currens with time."
  (let ((created
         (org-timestamp-from-string
          (concat "[" (alist-get :created metadata) "]"))))
    (when (string= (org-timestamp-format created "%H:%M") "00:00")
      (setf (alist-get :created metadata)
            (concat (org-timestamp-format created "%Y-%m-%d %a ")
                    (format-time-string
                     "%H:%M"
                     (when-let ((tempus
                                 (cl-find-if
                                  (lambda (id)
                                    (eq (ezeka-id-type id 'noerror) :tempus))
                                  (cons (alist-get :id metadata)
                                        (alist-get :oldnames metadata)))))
                       (encode-time (iso8601-parse tempus)))))))
    metadata))

(defun ezeka-toggle-update-header-modified (arg)
  "Toggle between different value of `ezeka-header-update-modified'.
With \\[universal-argument] ARG, show a list of options instead."
  (interactive "P")
  (let ((new-value
         (if arg
             (intern (completing-read
                      "When to update modification dates: "
                      '("sameday" "never" "confirm")
                      nil
                      t))
           (if-let ((result (member ezeka-header-update-modified
                                    '(#1=sameday never confirm #1#))))
               (cadr result)
             (error "Invalid current `ezeka-header-update-modified': %s"
                    ezeka-header-update-modified)))))
    (setq ezeka-header-update-modified new-value)
    (unless arg
      (message "Set `ezeka-header-update-modified' to %s" new-value))))

(defcustom ezeka-modified-updated-hook nil
  "List of functions to call after modifying the metadata header."
  :type '(or function list)
  :group 'ezeka)

(defun ezeka--maybe-update-modified (metadata)
  "Maybe update the modification time in the METADATA.
Whether to update is determined by `ezeka-update-modifaction-date'.
Return the new metadata."
  (let* ((today (format-time-string "%Y-%m-%d"))
         (now (format-time-string "%Y-%m-%d %a %H:%M"))
         (last-modified (or (alist-get :modified metadata)
                            (alist-get :created metadata)
                            (user-error "No created or modified time in %s"
                                        (alist-get :link metadata)))))
    (unless (string-equal (or last-modified "") now)
      ;; FIXME: Probably better to convert modification times to Emacs's encoded
      ;; time rather than doing it with strings.
      (when (or (equal ezeka-header-update-modified 'always)
                (and (equal ezeka-header-update-modified 'sameday)
                     (string= (cl-subseq last-modified 0 (length today)) today))
                ;; Automatic updating conditions not met; need to confirm
                (and (member ezeka-header-update-modified '(sameday confirm t))
                     (y-or-n-p
                      (format "%s was last modified at %s. Update to now? "
                              (ezeka-format-metadata "%i {%l} %t" metadata)
                              last-modified))))
        (setf (alist-get :modified metadata) now)
        (run-hooks 'ezeka-modified-updated-hook)))
    metadata))

(defun ezeka-update-modified (file)
  "Update the modification time in the current Zettel FILE's header.
This function ignores the value of `ezeka-header-update-modified',
treating it as if set to 'ALWAYS."
  (interactive (list buffer-file-name))
  (let ((ezeka-header-update-modified 'always))
    (ezeka--update-file-header file nil t)))

(defun ezeka-force-save-buffer (&optional arg)
  "Save the current buffer, even if it's unmodified.
With \\[universal-argument] ARG, don't update the modification date.
With double \\[universal-argument], update it unconditionally."
  (interactive "P")
  (let ((ezeka-header-update-modified
         (cond ((equal arg '(4)) 'never)
               ((equal arg '(16)) 'always)
               (t
                ezeka-header-update-modified)))
        (modified (buffer-modified-p)))
    (unwind-protect
        (when buffer-file-name
          (set-buffer-modified-p t)
          (ezeka--update-file-header nil nil
                                     (eq ezeka-header-update-modified 'always))
          (save-buffer))
      (set-buffer-modified-p modified))
    (set-buffer-modified-p nil)))

;; There are three different places where files can be captioned or titled:
;; 1) The file name itself might contain a caption;
;; 2) The rubric in the file header should contain caption matching the
;;    caption in the file name; and, finally,
;; 3) The title in the file header contains a nicely formatted version
;;    of the caption that is used when inserting links.

(defun ezeka--reconcile-title-and-caption (metadata)
  "Interactively reconcile title and caption in given METADATA.
Returns modifed metadata."
  (let ((caption (or (alist-get :caption metadata) ""))
        (title (or (alist-get :title metadata) "")))
    (unless (or (string= title caption)
                (alist-get :caption-stable metadata))
      (let ((choice
             (read-char-choice
              (format (concat
                       "[C]aption: %s\n"
                       "  [T]itle: %s\n"
                       "Press [c/u] or [t/l] to use that one; uppercase to edit beforehand,\n"
                       "      [n] or [q] to do noting: ")
                      (propertize caption 'face 'bold)
                      (propertize title 'face 'italic))
              '(?c ?C ?u ?U ?t ?T ?l ?L ?n ?q))))
        (pcase choice
          ((or ?c ?u) (setf (alist-get :title metadata) caption))
          ((or ?t ?l) (setf (alist-get :caption metadata) title))
          ((or ?C ?U) (setf (alist-get :title metadata)
                            (ezeka--minibuffer-edit-string caption)))
          ((or ?T ?L) (setf (alist-get :caption metadata)
                            (ezeka--minibuffer-edit-string title))))
        (setf (alist-get :caption-stable metadata) t)))
    (funcall clear-message-function)
    metadata))

(defun ezeka--replace-file-header (filename metadata)
  "Replace FILENAME's file header with METADATA."
  (let ((inhibit-read-only t))
    (save-mark-and-excursion
      (with-current-buffer (find-buffer-visiting filename)
        (save-restriction
          (widen)
          (goto-char (point-min))
          (when (re-search-forward ezeka-header-separator-regexp nil t 1)
            (narrow-to-region (point-min) (point)))
          (setf (alist-get :rubric metadata)
                (ezeka-format-metadata ezeka-header-rubric-format metadata))
          (delete-region (point-min) (point-max))
          (mapc (lambda (cons)
                  (insert (format "%s: %s\n"
                                  (ezeka--header-yamlify-key (car cons))
                                  (ezeka--header-yamlify-value (cdr cons)))))
                (let (ordered)
                  (dolist (key (mapcar #'car ezeka-metadata-valid-fields)
                               (nreverse ordered))
                    (when (alist-get key metadata)
                      (push (cons key (alist-get key metadata)) ordered)))))
          (ezeka--make-header-read-only (current-buffer)))))))

(defun ezeka--metadata-equal-p (md1 md2)
  "Return non-nil if the values of MD1 and MD2 are equal."
  (and (= (length md1) (length md2))
       (cl-every (lambda (x)
                   (equal (cdr x) (alist-get (car x) md2)))
                 md1)))

(defvar ezeka--previously-updated nil
  "Temporarily save previoius file state to avoid repeated calls.
This is a an alist of (FILENAME . CHECKSUM METADATA). Used in
`ezeka--update-file-header'.")

(defun ezeka--update-file-header (&optional filename metadata force)
  "Replace FILENAME's header with one generated from METADATA.
If METADATA is not given, get it by parsing the FILENAME's existing
header. If FORCE is non-nil, update the header even if the the file
has not changed since last update."
  (interactive (list buffer-file-name))
  (let* ((filename (or filename buffer-file-name))
         (metadata (or metadata (ezeka-file-metadata filename 'noerror)))
         (previous (assoc-string filename ezeka--previously-updated))
         (old-point (point))
         (inhibit-read-only t))
    (if (and metadata
             (or force
                 (not (string= (buffer-hash) (cadr previous)))))
        (progn
          (setq metadata
            (ezeka--set-time-of-creation
             (ezeka--maybe-update-modified
              (ezeka--reconcile-title-and-caption metadata))))
          (ezeka--replace-file-header filename metadata)
          (setf (alist-get filename ezeka--previously-updated nil nil #'string=)
                (list (buffer-hash) metadata)))
      (message "Cannot update header: can't parse metadata"))
    ;; `Save-excursion' doesn't seem to restore the point, possibly because the
    ;; file is changed, so need to do it manually.
    (goto-char old-point)))

(defun ezeka--invalid-filename-p (filename)
  "Return non-nil if FILENAME is valid (on macOS).
A valid caption is one that doesn't contain resticted characters
like slash (/) or colon (:), and is less than 255 characters long."
  (or (string-match-p "[/:*]" filename)
      (> (length filename) 255)))

(defvar ezeka--unnormalized-files-to-move nil
  "An alist of file names to rename with the caption inside metadata.
Each element should consist of the key (full file name), what it
should be renamed to (base file name), and current timestamp.")

(defun ezeka-rename-unnormalized-files (&optional confirm)
  "Rename files in `ezeka--unnormalized-files-to-move'.
If CONFIRM (\\[universal-argument]) is non-nil, confirm each rename."
  (interactive "P")
  (mapc
   (lambda (entry)
     (cl-destructuring-bind (filename caption time)
         entry
       (let ((renamed (expand-file-name
                       (file-name-with-extension caption
                                                 ezeka-file-extension)
                       (file-name-directory filename))))
         (cond ((file-exists-p renamed)
                (unless (y-or-n-p (format "File `%s' already exists. Skip? "
                                          caption))
                  (error "File `%s' already exists" caption)))
               ((and (file-exists-p filename)
                     (or (not confirm)
                         (y-or-n-p
                          (format "Move [%s]\n  to [%s]\n(saved on %s)? "
                                  (file-name-base filename)
                                  caption
                                  (format-time-string "%F" time)))))
                (ezeka--rename-file filename renamed)))
         (if (file-exists-p renamed)
             (when (y-or-n-p "Success! Remove entry? ")
               (cl-delete filename ezeka--unnormalized-files-to-move
                          :test #'string= :key #'car))
           (message "Rename failed")))))
   ezeka--unnormalized-files-to-move))

(defun ezeka-normalize-file-name (&optional filename metadata force)
  "Ensure that FILENAME's captioned name matches the METADATA.
When called interactively or FORCE is non-nil, offer to set
metadata or rename the file even if they are in agreement."
  (interactive
   (list buffer-file-name
         nil
         (prefix-numeric-value current-prefix-arg)))
  (let* ((filename (or filename buffer-file-name))
         (file-base (file-name-base filename))
         (mdata (if (null metadata)
                    (ezeka-file-metadata filename t)
                  (ezeka--update-file-header filename metadata)
                  metadata))
         (mdata-base (ezeka-format-metadata ezeka-file-name-format mdata))
         (pasturized (ezeka--pasturize-for-filename mdata-base))
         (read-user-choice
          (lambda (file-base mdata-base)
            "Prompt the user about which name to use."
            (read-char-choice
             (format (concat "Caption in filename and metadata differ:\n"
                             "[F]ilename: %s\n"
                             "[M]etadata: %s\n"
                             "Press [f/u] to set metadata from filename (uppercase to edit),\n"
                             "      [m/l] to set filename from metadata (uppercase to edit),\n"
                             "      [r] to add %s keyword for renaming later, or\n"
                             "      [n] or [q] to do noting: ")
                     (propertize file-base 'face 'bold)
                     (propertize pasturized 'face 'bold-italic)
                     (propertize ezeka-rename-note-keyword 'face 'bold))
             '(?f ?F ?u ?U ?m ?M ?l ?L ?r ?R ?n ?N ?q ?Q))))
         (keep-which
          (unless (and (not force)
                       (or (string= mdata-base file-base)
                           (member ezeka-rename-note-keyword
                                   (alist-get :keywords mdata))))
            (funcall read-user-choice file-base mdata-base))))
    (funcall clear-message-function)
    (cond ((memq keep-which '(nil ?n ?q))
           ;; do nothing
           )
          ((and (memq keep-which '(?r ?R))
                (not (member ezeka-rename-note-keyword
                             (alist-get :keywords mdata))))
           (ezeka-add-keyword filename ezeka-rename-note-keyword nil mdata)
           (ezeka--save-buffer-read-only filename))
          ((memq keep-which '(?f ?F ?u ?U))
           (when (memq keep-which '(?F ?U))
             (setq file-base (ezeka--minibuffer-edit-string file-base)))
           (setf (alist-get :id mdata)
                 (ezeka-file-name-id file-base)
                 (alist-get :label mdata)
                 (ezeka-file-name-label file-base)
                 (alist-get :caption mdata)
                 (ezeka-file-name-caption file-base)
                 (alist-get :citekey mdata)
                 (ezeka-file-name-citekey file-base)
                 (alist-get :caption-stable mdata)
                 nil)
           (ezeka--replace-file-header filename mdata)
           (ezeka--save-buffer-read-only filename)
           (apply #'run-hooks ezeka-after-save-hook))
          ((memq keep-which '(?m ?M ?l ?L))
           (setf (alist-get :keywords mdata)
                 (cl-remove ezeka-rename-note-keyword
                            (alist-get :keywords mdata)
                            :test #'string=))
           (ezeka--rename-file
            filename
            (file-name-with-extension
             (if (or (member keep-which '(?M ?L))
                     (not (string= pasturized mdata-base)))
                 (ezeka--minibuffer-edit-string pasturized)
               pasturized)
             ezeka-file-extension))
           (when t                      ; TODO check if filename changed
             (message "You might want to do `ezeka-normalize-file-name' again"))))))

;;;=============================================================================
;;; Numerus Currens
;;;=============================================================================

(defun ezeka-make-numerus (number letters)
  "Return new numerus currens ID based on NUMBER and LETTERS.
Both NUMBER and LETTERS are strings."
  (concat number "-" letters))

(defun ezeka-numerus-number (id)
  "Return the number part of the ID as a string."
  (when (string-match ezeka-numerus-currens-regexp id)
    (match-string 1 id)))

(defun ezeka-numerus-letters (id)
  "Return the letters part of the ID as a string."
  (when (string-match ezeka-numerus-currens-regexp id)
    (match-string 3 id)))

(defun ezeka-numerus-parts (id)
  "Return a list of two elements: the number and letters parts of ID.
Return NIL if the ID is not a numerus currens ID."
  (when (and (stringp id)
             (string-match ezeka-numerus-currens-regexp id))
    (list (match-string 1 id) (match-string 3 id))))

(defun abase26-letter-to-decimal (letter)
  "Return the decimal number corresponding to LETTER, a string.
Case-insensitive."
  (if (string-match "[a-zA-Z]" letter)
      (- (string-to-char (downcase letter)) ?a)
    (error "LETTER must be a string of one letter")))

(defun abase26-decimal-to-letter (n)
  "Return a string of number in abase26 corresponding decimal N."
  (if (< -1 n 26)
      (char-to-string (+ n ?a))
    (error "N must be an integer between 0 and 25")))

(defun abase26-encode (n &optional width)
  "Return string representating integer N in 'alphabetic' base 26.
If WIDTH is given, returns the string at least WIDTH wide, padded with
abase26 equivalent of 0, namely 'a'."
  (let (digits)
    (while (> n 25)
      (push (% n 26) digits)
      (setq n (/ n 26)))
    (push n digits)
    (when width
      (while (> width (length digits))
        (push 0 digits)))
    (apply #'concat (mapcar #'abase26-decimal-to-letter digits))))

(defun abase26-decode (string)
  "Return decimal integer for STRING representation in the 'alphabetic' base 26."
  (let ((n (1- (length string)))
        (total 0))
    (dolist (d (split-string string "" t))
      (setq total (+ total (* (abase26-letter-to-decimal d) (expt 26 n))))
      (setq n (1- n)))
    total))

;; TODO: Somehow make this part of `ezeka-kasten'. Function?
(defun ezeka--random-id (type)
  "Generate a random new ID of the given TYPE."
  (cl-case type
    (:tempus  (format-time-string "%Y%m%dT%H%M"))
    (:numerus (format "%s-%04d"
                      (abase26-encode (random 26))
                      (random 10000)))
    (:scriptum
     (let (project)
       (while (not project)
         ;; FIXME: Do I need a native function for this?
         (setq project
           (if (fboundp #'zk--select-file)
               (ezeka-zk-with-kasten "numerus"
                 (ezeka-file-link (zk--select-file "Select project: ")))
             (read-string "Scriptum project (numerus currens): ")))
         (unless (ezeka-link-file project)
           (setq project nil)))
       (format "%s~%02d"
               project
               (random 100))))
    (t        (error "No such ID type %s in `ezeka-kaesten'" type))))

(defun ezeka--generate-id (kasten &optional confirm)
  "Return the next unused ID for the given KASTEN.
If CONFIRM is non-nil, interactively confirm that the generated ID is
acceptable."
  (let ((type (ezeka-kasten-id-type (ezeka-kasten-named kasten)))
        (keep-checking-p
         (lambda (candidate)
           "Checks if CANDIDATE is either NIL or exists."
           (or (null candidate)
               (ignore-errors
                 (ezeka-link-file (ezeka-make-link kasten candidate))))))
        (acceptablep
         (lambda (id)
           "Check if the ID is acceptable to the user."
           (or (not confirm)
               (y-or-n-p (format "Is %s acceptable? " id)))))
        id)
    ;; TODO: Extract to a separate function
    (if (and (eq type :numerus)
             (file-exists-p (in-ezeka-dir ezeka-pregenerated-numeri-file)))
        (unwind-protect
            (with-current-buffer
                (find-file-noselect
                 (in-ezeka-dir ezeka-pregenerated-numeri-file))
              (let ((left (count-lines (point-min) (point-max))))
                (unwind-protect
                    (while (and (> left 0)
                                (funcall keep-checking-p id))
                      (setq id
                        (string-trim
                         (delete-and-extract-region
                          (point-min)
                          (search-forward-regexp "[[:space:]]" nil t))))
                      (unless (funcall acceptablep id)
                        (setq id nil)))
                  (cl-decf left)
                  (let ((inhibit-message t))
                    (basic-save-buffer)))
                (message "%d pregenerated numer%s left"
                         left
                         (if (= left 1) "us" "i")))))
      (while (funcall keep-checking-p id)
        (setq id (ezeka--random-id type))
        (unless (funcall acceptablep id)
          (setq id nil))))
    id))

;;;=============================================================================
;;; Tempus Currens
;;;=============================================================================

(defun ezeka-format-tempus-currens (&optional time)
  "Return a tempus currens ID based on the given Emacs TIME object.
If TIME is nil, default to current time."
  (format-time-string "%Y%m%dT%H%M" time))

(defun ezeka-tempus-currens-id-for (link)
  "Return a suitable tempus currens ID for the given Zettel LINK."
  (if (eq (ezeka-kasten-id-type (ezeka-kasten-named (ezeka-link-kasten link)))
          :tempus)
      ;; If already tempus currens, just return that id
      (ezeka-link-id link)
    ;; Otherwise come up with an appropriate ID based on the metadata
    (let* ((file (ezeka-link-file link))
           (mdata (ezeka-file-metadata file))
           oldname)
      (cond ((setq oldname (ezeka--resurrectable-oldname file :tempus mdata))
             ;; One of the old names was a tempus currens; just use that
             (ezeka-link-id oldname))
            ((alist-get :created mdata)
             (string-replace "T0000"    ; FIXME: A bit hacky?
                             (format-time-string "T%H%M")
                             (ezeka-format-tempus-currens
                              (ezeka-encode-iso8601-datetime
                               (alist-get :created mdata)))))
            (t
             ;; Can't figure out automatically; ask the user
             (read-string "No created metadata; make up your own name: "
                          (ezeka--generate-id (ezeka-link-kasten link))))))))

;;;=============================================================================
;;; Zettel Links
;;;=============================================================================

(defvar ezeka--new-child-plist nil
  "An alist of new children and a plist of their details.
Plist values are :parent, :title, :label, and :citekey.")

(defun ezeka-link-at-point-p (&optional freeform)
  "Return non-nil if the thing at point is a wiki link (i.e. [[XXX]]).
The first group is the link target. If FREEFORM is non-nil, also
consider Zettel links that are not enclosed in square brackets."
  (thing-at-point-looking-at
   (let ((regexp (ezeka--regexp-strip-named-groups (ezeka-link-regexp))))
     (if freeform
         (concat "\\(?1:" regexp "\\)")
       (concat "\\[\\[\\(?1:" regexp "\\)\\]\\(\\[[^]]+\\]\\)*\\]")))))

(defun ezeka-link-at-point ()
  "Return the Zettel link at point.
Needs to be called after `ezeka-link-at-point-p'."
  (match-string-no-properties 1))

;; FIXME: Relies on ace-window
(defun ezeka-find-file (file &optional same-window)
  "Edit the given FILE based on the value of `ezeka-number-of-frames'.
If SAME-WINDOW is non-NIL, open the buffer visiting the file in the
same window."
  (if same-window
      (find-file file)
    (cl-case ezeka-number-of-frames
      (two (if (< (length (frame-list)) 2)
               (find-file-other-frame file)
             (select-window (ace-select-window))
             (find-file file)))
      (one (let ((pop-up-windows t))
             (select-window (ace-select-window))
             (find-file file)))
      (nil (find-file file))
      (t (find-file-other-frame file)))))

(defun ezeka-find-link (link &optional same-window)
  "Find the given LINK.
If SAME-WINDOW is non-NIL, opens the link in the same window. Return
T if the link is a Zettel link."
  (when (ezeka-link-p link)
    (let ((existing-file (ignore-errors (ezeka-link-file link))))
      (cond ((ezeka-note-p existing-file)
             (ezeka-find-file existing-file same-window))
            ((or (eql ezeka-create-nonexistent-links t)
                 (and (eq ezeka-create-nonexistent-links 'confirm)
                      (y-or-n-p
                       (format "Link `%s' doesn't exist. Create? " link))))
             (ezeka-find-file (ezeka-link-file link "") same-window)
             (call-interactively #'ezeka-insert-header-template))
            (t
             (message "Link `%s' doesn't exist" link)
             t)))))

(defun ezeka-kill-link-or-sexp-at-point (&optional arg)
  "If there is a Zettel link at point, kill it, including square brackets.
Otherwise, call `kill-sexp', passing \\[universal-argument] ARG to it."
  (interactive "p")
  (if (ezeka-link-at-point-p)
      (let ((start (match-beginning 0))
            (end (match-end 0)))
        (kill-new (buffer-substring-no-properties start end))
        (delete-region start end)
        (let ((around (string (preceding-char) (following-char))))
          (cond ((string-match-p "\\s \\s " around)
                 (just-one-space 1))
                ((string-match-p "\\s " around)
                 (just-one-space 0)))))
    (kill-sexp arg)))

(defun ezeka--format-link (target &optional description)
  "Return a formatted org-link to TARGET with optional DESCRIPTION.
TARGET can be either a link or a filepath."
  (let* ((link (if (file-name-absolute-p target)
                   (ezeka-file-link target)
                 target)))
    (format "[[%s]%s]"
            (ezeka-link-id link)
            (if description
                (format "[%s]" description) ""))))

(defun ezeka--link-with-metadata (link &optional fields where metadata)
  "Return a string with metadata FIELD(S) at place WHERE (relative to LINK).
FIELDS defaults to :title, WHERE to :before. If WHERE is
:instead, do not include the LINK."
  (let* ((mdata (or metadata (ezeka-file-metadata (ezeka-link-file link))))
         (fields (or fields '(:title)))
         (where (or where :before))
         (value (mapconcat (lambda (f) (alist-get f mdata))
                  fields " ")))
    (concat (if (eq where :before)
                (concat value " ")
              "")
            (if (eq where :instead)
                ""
              (ezeka--format-link
               link
               (when (eq where :description)
                 value)))
            (if (eq where :after)
                (concat " " value)
              ""))))

(defun ezeka-insert-with-spaces (str)
  "Insert STR at point surrounded by spaces as appropriate."
  (insert (if (or (bolp) (space-or-punct-p (char-before))) "" " ")
          (string-trim str)
          (if (or (eolp) (space-or-punct-p (char-after))) "" " ")))

(defun ezeka-insert-link-with-metadata (link &optional fields where confirm)
  "Insert the Zettel LINK, optionally adding metadata FIELD(S).
WHERE (:BEFORE, :AFTER, or in :DESCRIPTION) determines where
the fields are added. FIELDS can be a list. If CONFIRM is
non-NIL, ask for confirmation before inserting metadata."
  (let* ((fields (or fields
                     (when (called-interactively-p 'any)
                       (list
                        (intern-soft
                         (completing-read
                          "Which metadata field? "
                          '(":none" ":title" ":citekey" ":label")))))))
         (where (or where
                    (when fields
                      (intern-soft
                       (completing-read "Where? "
                                        '(":before" ":after"))))))
         (file (or (ezeka-link-file link nil t)
                   (when (cl-find-if #'(lambda (buf)
                                         (string-match link (buffer-name buf)))
                                     (buffer-list))))))
    (ezeka-insert-with-spaces
     (if (and file
              (or (not confirm)
                  (let ((mdata (ezeka-file-metadata file)))
                    ;; Pressing return just defaults to NO rather than quit
                    (define-key query-replace-map [return] 'act)
                    (y-or-n-p (format (if (eq where :description)
                                          "Insert [%s] in the link %s? "
                                        "Insert [%s] %s the link? ")
                                      (mapconcat (lambda (f)
                                                   (alist-get f mdata))
                                        fields
                                        " ")
                                      where)))))
         (ezeka--link-with-metadata link fields where mdata)
       (ezeka--format-link link)))))

(defun ezeka--select-file (files &optional prompt require-match)
  "Select from among Zettel FILES, presenting optional PROMPT.
If REQUIRE-MATCH is non-nil, require match, otherwise treat entered
text as a Zettel link."
  (let* ((table (ezeka-completion-table files))
         (file (when table
                 (completing-read (or prompt "Select Zettel: ")
                                  table
                                  nil
                                  require-match))))
    (cdr (assoc-string file table))))

(defun ezeka-insert-link-to-visiting (arg)
  "Insert a link to another Zettel being currently visited.
With \\[universal-argument] ARG offers a few options for including
Zettel metadata. If the user selects a Zettel that does not exist in
the list, just insert the link to what was selected. If the cursor in
already inside a link, replace it instead."
  (interactive "P")
  (let ((link (ezeka-file-link
               (ezeka--select-file (ezeka-visiting-buffer-list)
                                   "Insert link to: " t))))
    (if link
        (if (not (ezeka-link-at-point-p))
            (if arg
                (funcall-interactively #'ezeka-insert-link-with-metadata link)
              (ezeka-insert-link-with-metadata link '(:title) :before t))
          ;; When replacing, don't including anything
          (delete-region (match-beginning 0) (match-end 0))
          (insert (ezeka--format-link link)))
      (message "No visiting Zettel"))))

(defun ezeka--note-in-other-window ()
  "Return the file name to the Zettel note in the other window.
If the file is not a Zettel note, return nil."
  (when-let* ((other-win (cond ((one-window-p t 'visible)
                                (user-error "There are no other windows"))
                               ((and (> (count-windows nil 'visible) 2)
                                     (featurep 'ace-window))
                                (aw-select " Ace - Window"))
                               ((> (count-windows nil 'all-frames) 2)
                                (user-error "There are more than one `other-window's"))
                               (t
                                (other-window-for-scrolling))))
              (other-buf (window-buffer other-win))
              (file (or (buffer-file-name other-buf)
                        (with-current-buffer other-buf
                          (ezeka--grab-dwim-file-target))))
              (_ (ezeka-note-p file)))
    file))

(defun ezeka-insert-link-to-other-window (&optional link-only)
  "Insert the link to the Zettel note in the other window.
With \\[universal-argument] LINK-ONLY, insert just the link, otherwise
also include the title."
  (interactive "P")
  (if link-only
      (ezeka-insert-link-with-metadata (ezeka--note-in-other-window))
    (ezeka-insert-link-with-metadata (ezeka--note-in-other-window)
                                     '(:author :title) :before t)))

(defun ezeka-insert-link-to-bookmark (arg)
  "Insert a link to a bookmark.
With \\[universal-argument] ARG, offer a few options for including
Zettel metadata. If the user selects a Zettel that does not exist in
the list, just insert the link to what was selected. If the cursor in
already inside a link, replace it instead."
  (interactive "P")
  (let* ((table (mapcar (lambda (item)
                          (let ((link (cdr (cl-find 'filename
                                                    (cdr item)
                                                    :key #'car))))
                            (when (ezeka-link-p link)
                              (cons (car item)
                                    (ezeka-link-file link)))))
                        bookmark-alist))
         (link (when table
                 (ezeka-file-link
                  (cdr (assoc-string
                        (completing-read "Insert link to: " table nil t) table))))))
    (if link
        (if (not (ezeka-link-at-point-p))
            (if arg
                (funcall-interactively #'ezeka-insert-link-with-metadata link)
              (ezeka-insert-link-with-metadata link '(:title) :before t))
          ;; When replacing, don't including anything
          (delete-region (match-beginning 0) (match-end 0))
          (insert (ezeka--format-link link)))
      (message "No visiting Zettel"))))

(defun ezeka-insert-link-from-clipboard (arg)
  "Insert link with metadata to the LINK in the OS clipboard.
See `ezeka-insert-link' for details. With \\[universal-argument] ARG,
insert just the link itself."
  (interactive "P")
  (let ((link (gui-get-selection 'CLIPBOARD))
        (backlink (when buffer-file-name
                    (ezeka-file-link buffer-file-name))))
    (when (ezeka-link-p link)
      (if arg
          (ezeka-insert-link-with-metadata link)
        (ezeka-insert-link-with-metadata link '(:title) :before t))
      (when backlink
        (gui-set-selection 'CLIPBOARD backlink)
        (message "Backlink to %s copied to clipboard" backlink)))))

(defun ezeka-kill-ring-save-link-and-title (arg)
  "Save the link and title to kill ring and system clipboard.
If the point is at Zettel link, use that; otherwise, the current
buffer. With \\[universal-argument] ARG, save just the title.
With double \\[universal-argument], save caption."
  (interactive "P")
  (let* ((file (ezeka--grab-dwim-file-target))
         (link (ezeka-file-link file)))
    (when file
      (let* ((mdata (ezeka-file-metadata file))
             (result (cond ((equal arg '(4))
                            (alist-get :title mdata))
                           ((equal arg '(16))
                            (alist-get :rubric mdata))
                           (t
                            (ezeka-format-metadata "%i {%l} %t" mdata)))))
        (kill-new result)
        (unless select-enable-clipboard
          (gui-set-selection 'CLIPBOARD result))
        (message "Saved [%s] in the kill ring" result)))))

(defun ezeka-kill-ring-save-link-or-filename (arg)
  "Save in kill ring the Zettel link at point or in Zettel buffer.
With \\[universal-argument] ARG, save the file name relative to
`ezeka-directory' instead. With two prefix arguments, open the file in
Finder with it selected."
  (interactive "p")
  (let ((file (ezeka--grab-dwim-file-target t)))
    (when file
      (let ((link (if (= arg 4)
                      (file-relative-name (file-truename file)
                                          (file-truename ezeka-directory))
                    (ezeka-file-link file))))
        (if select-enable-clipboard
            (kill-new link)
          (gui-set-selection 'CLIPBOARD link))
        (message "Saved [%s] in the kill ring" link)
        (when (= arg 16)
          (shell-command (format "open -R '%s' &" file)))))))

(defun ezeka-kill-ring-save-next-link ()
  "Save the first link at or after point (but before EOL)."
  (interactive)
  (save-excursion
    (let ((link (if (ezeka-link-at-point-p)
                    (ezeka-link-at-point)
                  (let ((eol (save-excursion (end-of-visual-line) (point))))
                    (when (re-search-forward (ezeka-link-regexp) eol t)
                      (match-string-no-properties 0))))))
      (when link
        (kill-new link)
        (message "Saved [%s] to kill ring" link)))))

(defun ezeka-links-to (link)
  "Run a recursive grep (`rgrep') to find references LINK.
Called interactively, get the LINK at point or to current Zettel."
  (interactive (list (ezeka-file-link (ezeka--grab-dwim-file-target t))))
  (grep-compute-defaults)
  (rgrep link "*.txt" (file-name-as-directory ezeka-directory) nil))

(defun ezeka-rgrep (string)
  "Run a recursive grep (`rgrep') for the given STRING across all Zettel."
  (interactive "sSearch for what? ")
  (grep-compute-defaults)
  (rgrep (string-replace " " ".*" string)
         "*.txt" (file-name-as-directory ezeka-directory) nil))

;; To show the beginning of Zettel title in the mode-line,
;; add the following to the user configuration:
;;
;; (add-hook 'ezeka-mode-hook 'ezeka-show-title-in-mode-line)
(defun ezeka-show-title-in-mode-line ()
  "Change `mode-line-misc-info' to show Zettel's title from metadata."
  (interactive)
  (when (and (ezeka-note-p buffer-file-name)
             (not (zerop (buffer-size))))
    (save-excursion
      (save-restriction
        (widen)
        (goto-char (point-min))
        (let ((metadata
               (ezeka-decode-rubric
                (buffer-substring-no-properties
                 (or (re-search-forward ezeka-header-rubric-key nil t) (point-min))
                 (point-at-eol)))))
          (when metadata
            (let ((words (split-string (alist-get :title metadata))))
              (setq-local mode-line-misc-info
                          (replace-regexp-in-string
                           "/" "" (mapconcat #'identity
                                    (cl-subseq words 0 (min 5 (length words)))
                                    " "))))))))))

(defun ezeka-update-link-prefix-title (&optional delete-title)
  "Replace text from point to next Zettel link with that Zettel's title.
With \\[universal-argument] DELETE-TITLE, delete the text instead."
  (interactive "P")
  (save-excursion
    ;; if already inside a link, go to the start
    (when (string= "link" (car (org-thing-at-point)))
      (re-search-backward "\\[\\["))
    ;; if char under cursor is start of link, back up to BOF
    (while (or (char-equal (following-char) ?\[)
               (= (preceding-char) 0))  ; BOF
      (backward-char))
    (unless (char-equal (preceding-char) ? ) (insert " "))
    (let ((start (point)))
      ;; Cannot use `org-next-link', since it ignores links in comments
      (when (re-search-forward "\\[\\[")
        (goto-char (match-beginning 0)))
      (when-let* ((_ (ezeka-link-at-point-p))
                  (link (ezeka-link-at-point))
                  (file (ezeka-link-file link))
                  (mdata (ezeka-file-metadata file))
                  (title (alist-get :title mdata)))
        (delete-region start (point))
        (unless delete-title
          (insert title " "))))))

;;;=============================================================================
;;; Link hints via overlays
;;;=============================================================================

(defcustom ezeka-make-help-echo-overlays t
  "Make help echo overlays with link's filename."
  :group 'ezeka
  :type 'boolean)

(defun ezeka--make-help-echo-overlay (&optional pos context)
  "Make an overlay at POS (or `point') with help-echo.
CONTEXT is the result of `org-context'."
  (save-excursion
    (goto-char (or pos (point)))
    (when-let* ((context (or context (org-context)))
                (link (cl-find :link context :key #'car))
                (_ (ezeka-link-at-point-p))
                (overlay (make-overlay (cadr link) (caddr link))))
      (overlay-put overlay 'type 'ezeka-help-echo)
      (overlay-put overlay 'face '((t (:underline "purple"))))
      (overlay-put overlay 'help-echo
                   (file-name-base (ezeka-link-file (ezeka-link-at-point)))))))

(defun ezeka--make-help-echo-overlays (&optional buffer)
  "Make help echo overlays in BUFFER (or `current-buffer')."
  (interactive)
  (let ((buffer (or buffer (current-buffer))))
    (save-excursion
      (with-current-buffer buffer
        (remove-overlays (point-min) (point-max) 'type 'ezeka-help-echo)
        (goto-char (point-min))
        (when ezeka-make-help-echo-overlays
          (while (re-search-forward (ezeka-link-regexp) nil t)
            (ezeka--make-help-echo-overlay (point) (org-context))))))))

;;;=============================================================================
;;; Genealogical
;;;=============================================================================

(defun ezeka-trace-genealogy (file-or-link &optional degree)
  "Return FILE-OR-LINK's next genealogical link.
If cannot figure it out, return NIL. With the optional DEGREE, try to
find the Nth link (i.e. grandparent if DEGREE is 2, child if DEGREE is
-1, an so on), returning the most remote link that could be found."
  (let ((degree (or degree 1)))
    (if (= (abs degree) 0)
        file-or-link
      (ezeka-trace-genealogy (alist-get (if (> degree 0)
                                            :parent
                                          :firstborn)
                                        (ezeka-file-metadata
                                         (if (ezeka-link-p file-or-link)
                                             (ezeka-link-file file-or-link)
                                           file-or-link)))
                             (if (> degree 0)
                                 (1- degree)
                               (1+ degree))))))

(defun ezeka-find-ancestor (n &optional same-window)
  "Open the current Zettel's immediate ancestor.
With a prefix argument, try to find the Nth ancestor. With
\\[universal-argument] or SAME-WINDOW non-nil, open in the same
window."
  (interactive (list (if (integerp current-prefix-arg)
                         current-prefix-arg
                       1)
                     (equal current-prefix-arg '(4))))
  (when (ezeka-note-p buffer-file-name)
    (let ((ancestor (ezeka-trace-genealogy buffer-file-name n)))
      (if ancestor
          (ezeka-find-link ancestor same-window)
        (message "No ancestor of degree %d found" n)))))

(defun ezeka-find-descendant (n)
  "Open the current Zettel's immediate descendant.
With a prefix argument, try to find the Nth ancestor."
  (interactive "p")
  (when (ezeka-note-p buffer-file-name)
    (let ((descendant (ezeka-trace-genealogy buffer-file-name (- n))))
      (if descendant
          (ezeka-find-link descendant)
        (message "No descendant found")))))

(defun ezeka-insert-ancestor-link (arg)
  "Insert a link with title to the ancestor of the current Zettel.
With a numerical prefix ARG'ument, try to find Nth ancestor. With a
universal argument, ask for confirmation before inserting."
  (interactive "P")
  (let* ((degree (if (integerp arg) arg 1))
         (link (ezeka-trace-genealogy buffer-file-name degree)))
    (if link
        (ezeka-insert-link-with-metadata link '(:title) :before (not arg))
      (message "Could not find such ancestor"))))

(defun ezeka--generate-new-child (parent &optional kasten id)
  "Generate a new child in the same Kasten as PARENT link.
If KASTEN is given, use that kasten instead. Return a fully qualified
link to the new child. If ID is non-nil, use that instead of
generating one."
  (let* ((kasten (or kasten (ezeka-link-kasten parent)))
         (child-link (ezeka-make-link
                      kasten (or id (ezeka--generate-id kasten 'confirm)))))
    (when parent
      (add-to-list 'ezeka--new-child-plist
        (list child-link :parent parent)))
    child-link))

(defun ezeka-new-note-or-child (kasten &optional parent noselect manual)
  "Create a new note in KASTEN as an orphan or with optional PARENT.
If NOSELECT (or \\[universal-argument]) is given, don't open the new
note. If MANUAL is non-nil (or double \\[universal-argument]) is
given, allow the user to enter the ID manually. Return link to the
note."
  (interactive
   (list (ezeka--read-kasten)
         (when (ezeka-note-p buffer-file-name t)
           (ezeka-file-link buffer-file-name))
         (equal current-prefix-arg '(4))
         (when (equal current-prefix-arg '(16))
           (read-string "ID for the new note: "))))
  (let ((link (if parent
                  (ezeka--generate-new-child parent kasten manual)
                (ezeka-make-link kasten (or manual
                                            (ezeka--generate-id kasten))))))
    (unless noselect
      (ezeka-find-link link))
    link))

(defun ezeka--read-kasten (&optional prompt)
  "Read a valid Kasten with `completing-read' and given PROMPT, if any."
  (completing-read
   (or prompt "Kasten: ")
   (if (listp ezeka-kaesten)
       (mapcar #'ezeka-kasten-name ezeka-kaesten)
     (error "No `ezeka-kaesten' defined"))))

(defun ezeka--possible-new-note-title ()
  "Return a possible title for a new Zettel note based on context."
  (interactive)
  (if (region-active-p)
      (buffer-substring-no-properties (region-beginning) (region-end))
    (let ((start (point)))
      (save-excursion
        (beginning-of-line)
        ;; FIXME: Might be good to have some limit to prevent
        ;; killing whole paragraphs worth of text with soft
        ;; newlines.
        (string-trim-left
         (buffer-substring-no-properties (point) (max (point-min) (1- start)))
         "[ +*-]*")))))

(defun ezeka-insert-new-child-with-title (arg title &optional id)
  "Create a new child with given TITLE, inserting its link at point.
If TITLE is not given, use text on the current line before point.
With \\[universal-argument] ARG, create the child in the same Kasten
as the current note. With double \\[universal-argument], ask for ID."
  (interactive
   (list current-prefix-arg
         (org-trim
          (read-from-minibuffer "Title for the child: "
                                (ezeka--possible-new-note-title)))
         (when (equal current-prefix-arg '(16))
           (read-from-minibuffer "ID for the child: "))))
  (let* ((parent-link (ezeka-file-link buffer-file-name))
         (citekey (alist-get :citekey (ezeka-file-metadata buffer-file-name)))
         (child-link (ezeka--generate-new-child
                      parent-link
                      (unless (equal arg '(4))
                        (ezeka--read-kasten "Kasten for new child: "))
                      id))
         (plist (cdr (assoc-string child-link ezeka--new-child-plist))))
    (setf (alist-get child-link ezeka--new-child-plist nil nil #'string=)
          (plist-put (plist-put plist :citekey citekey) :title title))
    (insert (ezeka--format-link child-link))
    (ezeka-find-link child-link)))

;;;=============================================================================
;;; Buffers and Frames
;;;=============================================================================

(defun ezeka-visiting-buffer-list (&optional skip-current modified-only)
  "Return a list of Zettel files that are currently being visited.
If SKIP-CURRENT is non-nil, remove the current buffer. If
MODIFIED-ONLY is non-nil, only list modified buffers."
  (nreverse
   (mapcar #'buffer-file-name
           (cl-remove-if-not (lambda (buf)
                               (and (ezeka-note-p (buffer-file-name buf))
                                    (or (not modified-only)
                                        (buffer-modified-p buf))))
                             (remove (when skip-current
                                       (current-buffer))
                                     (buffer-list))))))

(defun ezeka-kill-visiting-buffers (arg)
  "Allow kill currently visited Zettel buffers one-by-one.
With \\[universal-argument] ARG, just kill all visiting Zettel."
  (interactive "P")
  (let (;; Disabling sorting preserves the same order as with `switch-to-buffer'
        ;; FIXME: How to do this without relying on vertico?
        (vertico-sort-function nil))
    (if arg
        (mapc (lambda (file)
                (kill-buffer (get-file-buffer file)))
              (ezeka-visiting-buffer-list t))
      (while t
        (let* ((table (ezeka-completion-table (ezeka-visiting-buffer-list t)))
               (choice (completing-read "Kill buffer: " table nil t)))
          (kill-buffer (get-file-buffer (cdr (assoc-string choice table)))))))))

(defun ezeka-formatted-frame-title ()
  "Return string suitable for `frame-title-format'.
This is a way to consistently format the frame title with useful
information for Zettelkasten work."
  (interactive)
  (concat (if (ezeka-note-p buffer-file-name)
              (let ((metadata (ezeka-file-metadata buffer-file-name)))
                (format "%s §%s@%s"
                        (alist-get :title metadata)
                        (alist-get :id metadata)
                        (alist-get :kasten metadata)))
            "%b")))

;; Add the following hook to enact:
;;
;; (add-hook 'post-command-hook 'ezeka-show-tooltip-with-link-title)
;;
;; Set absolute values for tooltip location
;; (add-to-list 'tooltip-frame-parameters '(top . 1015))
;; (add-to-list 'tooltip-frame-parameters '(left . 560))
(defun ezeka-show-tooltip-with-link-title ()
  "If the cursor is at a Zettel link, show a tooltip with its title."
  (while-no-input
    (redisplay)
    (when-let* ((link (and (ezeka-link-at-point-p)
                           (ezeka-link-at-point)))
                (position (window-absolute-pixel-position))
                (metadata (ezeka-file-metadata (ezeka-link-file link) t)))
      (tooltip-show
       (format "%s%s%s" (alist-get :title metadata)
               (if (alist-get :citekey metadata) " " "")
               (or (alist-get :citekey metadata) ""))))))

;; Add the following hook to enact:
;;
;; (add-hook 'post-command-hook 'ezeka-show-link-title-in-mode-line)
;; (remove-hook 'post-command-hook 'ezeka-show-link-title-in-mode-line)
(defun ezeka-show-link-title-in-mode-line ()
  "If the cursor is at a Zettel link, show the title in the mode line."
  (while-no-input
    (redisplay)
    (if-let* ((link (and (ezeka-link-at-point-p)
                         (ezeka-link-at-point)))
              (metadata (ezeka-file-metadata (ezeka-link-file link) t)))
        (setq mode-line-misc-info
          (propertize
           (format "%s%s%s" (alist-get :title metadata)
                   (if (alist-get :citekey metadata) " " "")
                   (or (alist-get :citekey metadata) ""))
           'face '(:slant italic :height 0.9)))
      (setq mode-line-misc-info zk-index-mode-line-orig))))

(defun ezeka-completion-table (files)
  "Turn list of FILES into completion table suitable for `completing-read'."
  ;;                  * ID  LABEL  TITLE  CITEKEY
  (let* ((iw 14) (lw 10) (kw 25)
         (tw (- (frame-width) (+ iw lw kw 5)))
         (fmt (format "%%s%%-%ds %%-%ds %%-%ds %%-15s" iw lw tw kw)))
    (mapcar (lambda (file)
              (when (ezeka-note-p file)
                (let* ((metadata (ezeka-file-metadata file t))
                       (title (alist-get :title metadata))
                       (buf (get-file-buffer file)))
                  (cons (format fmt
                                (if (and buf (buffer-modified-p buf)) "*" " ")
                                (or (alist-get :id metadata)
                                    (file-name-base file))
                                (alist-get :label metadata)
                                (when title
                                  (cl-subseq title 0 (min (length title)
                                                          tw)))
                                (or (alist-get :citekey metadata) ""))
                        file))))
            files)))

(defun ezeka-switch-to-buffer (&optional modified-only other-window)
  "Quickly switch to other open Zettel buffers.
If MODIFIED-ONLY (or \\[universal-argument]) is non-nil, show only
modified buffers. If OTHER-WINDOW is non-nil (or double
\\[universal-argument]), open buffer in other window."
  (interactive
   (list (equal current-prefix-arg '(4))
         (equal current-prefix-arg '(16))))
  (let* ((buffers (nreverse (ezeka-visiting-buffer-list t modified-only)))
         (table (ezeka-completion-table buffers))
         ;; Disabling sorting preserves the same order as with `switch-to-buffer'
         ;; FIXME: How to do this without relying on vertico?
         (vertico-sort-function nil))
    (if (null buffers)
        (read-buffer-to-switch
         (format "No %sZettel buffers. Switch to regular buffer: "
                 (if modified-only "modified " " ")))
      (funcall (if other-window
                   'switch-to-buffer-other-window
                 'switch-to-buffer)
               (get-file-buffer
                (cdr (assoc-string
                      (completing-read "Visit Zettel buffer: " table nil t)
                      table)))))))

;;;=============================================================================
;;; Labels
;;
;; A label is either a genus (for numerus currens notes) or category (for tempus
;; currens notes). By default, it is the value shown between curly brackets
;; {...} in the note's rubric.
;;;=============================================================================

(defun ezeka--read-category (&optional prompt custom sort-fn)
  "Use `completing-read' to select a category from `ezeka-categories'.
Optional PROMPT allows customizing the prompt. If CUSTOM is non-nil,
asks the user to type in the category directly. If SORT-FN is given,
use that to sort the list first."
  (let ((prompt (or prompt "Category: "))
        (categories (if (not (functionp sort-fn))
                        ezeka-categories
                      (let ((cats-copy (cl-copy-list ezeka-categories)))
                        (cl-sort cats-copy sort-fn)))))
    (if custom
        (read-string prompt)
      (completing-read prompt categories))))

(defun ezeka--read-genus (&optional prompt verbose default)
  "Read a genus as defined in `ezeka-genera'.
Return a string containing the genus letter. If PROMPT is non-nil, use
that prompt instead of the default. If VERBOSE is non-nil, show a list
of choices with explantions. DEFAULT is the genus used if user just
presses [return]."
  (cl-flet ((--completing-read ()
                               (let ((table (mapcar (lambda (genus)
                                                      (cl-destructuring-bind (lt gk desc)
                                                          genus
                                                        (cons
                                                         (format "%s (%s) ⇒ %s" lt gk desc)
                                                         lt)))
                                                    ezeka-genera)))
                                 (cdr (assoc-string (completing-read (or prompt "Genus: ")
                                                                     table nil t)
                                                    table)))))
    (let (item)
      (while (null item)
        (let ((result
               (if verbose
                   (--completing-read)
                 (read-char
                  (concat (or prompt "Genus")
                          " (Latin character, or RETURN for \"" (or default "x")
                          "\"): ")))))
          (cond ((and (characterp result) (eq result ?\C-m))
                 (setq result
                   (car (cl-rassoc default ezeka-genera
                                   :key #'car :test #'string=))))
                ((characterp result)
                 (setq result (char-to-string result)))
                ((stringp result)
                 result)
                (t
                 (signal 'wrong-type-argument '(or character string))))
          (setq item (assoc-string result ezeka-genera))
          (cond ((string= result "?") (setq verbose t))
                ((not item) (setq prompt "No such genus; try again. ")))))
      (cadr item))))

(defun ezeka--read-label (file-or-link &optional arg prompt default)
  "Interactively read label for the given FILE-OR-LINK.
Pass ARG, PROMPT, and DEFAULT to the appropriate function."
  (if (eq :numerus (ezeka-id-type file-or-link))
      (ezeka--read-genus prompt arg default)
    (ezeka--read-category prompt arg)))

(defun ezeka--update-metadata-values (filename metadata &rest args)
  "Update FILENAME's header, replacing METADATA values with new ones.
Afterwards, save the file while ignoring its read only status. If
METADATA is not given, read it from file first. The rest of the ARGS
should consist of KEY and VALUE pairs.

\(fn FILENAME METADATA &REST KEY VAL KEY VAL ...)"
  (declare (indent 2))
  (when (/= (logand (length args) 1) 0)
    (signal 'wrong-number-of-arguments (list 'setf (length args))))
  (let ((already-open (get-file-buffer filename)))
    (save-excursion
      (unwind-protect
          (with-current-buffer (or already-open (find-file-noselect filename))
            (let ((already-modified (buffer-modified-p))
                  (metadata (or metadata (ezeka-file-metadata filename))))
              (while args
                (setq metadata
                  (ezeka-set-metadata-value metadata (pop args) (pop args))))
              (ezeka--replace-file-header filename metadata)
              (when (and (not already-modified)
                         (if (eq ezeka-save-after-metadata-updates 'confirm)
                             (y-or-n-p "Save? ")
                           ezeka-save-after-metadata-updates))
                (save-buffer))))
        (let ((buf (get-file-buffer filename)))
          (unless already-open
            (kill-buffer-if-not-modified buf)))))))

(defun ezeka--add-change-log-entry (filename entry &optional section)
  "Make a change log ENTRY in FILENAME's SECTION.
If SECTION is nil, default to `Change Log'."
  (save-excursion
    (let* ((section (or section "Change Log"))
           (headline (org-find-exact-headline-in-buffer section)))
      (if headline
          (progn
            (goto-char headline)
            (end-of-line))
        (goto-char (point-max))
        (org-insert-heading nil nil 'top)
        (insert section))
      (insert "\n\n")
      (org-insert-item)
      (insert
       (format "- %s :: %s"
               (format-time-string (org-time-stamp-format nil t))
               entry))
      (org-fill-element))))

(defun ezeka-set-title-or-caption (filename &optional new-val set-title set-caption)
  "Update the title in FILENAME's header to NEW-VAL.
With \\[universal-argument], change the caption instead;
with double \\[universal-argument], change both the title
and the caption. Non-interactively, non-nil SET-TITLE and
SET-CAPTION determine which fields to change."
  (interactive (let* ((arg (prefix-numeric-value current-prefix-arg))
                      (set-title (not (eq arg 4)))
                      (set-caption (member arg '(4 16))))
                 (list (buffer-file-name) nil set-title set-caption)))
  (when (ezeka-note-p filename)
    (let* ((mdata (ezeka-file-metadata filename))
           (change-what (cond ((and (not set-title) set-caption) "the caption")
                              ((and set-title (not set-caption)) "the title")
                              ((and set-title set-caption) "both title and caption")
                              (t "nothing (huh?)")))
           (new-val (or new-val
                        (read-string (format "Change %s to what? " change-what)
                                     (alist-get (if set-title :title :caption)
                                                mdata)))))
      (when (and set-title set-caption
                 (y-or-n-p "Record the change in the change log? "))
        (ezeka--add-change-log-entry
         filename
         (format "Rename from \"%s\" to \"%s.\""
                 (alist-get :title mdata) new-val)))
      (when set-title
        (setf (alist-get :title mdata) new-val))
      (when set-caption
        (setf (alist-get :caption mdata) (ezeka--pasturize-for-filename new-val))
        (setf (alist-get :caption-stable mdata) nil))
      (ezeka--update-metadata-values filename mdata))))

(defun ezeka-set-label (filename label arg)
  "Set LABEL (genus or category) in Zettel FILENAME.
With \\[universal-argument], either show genera verbosely or type
custom category."
  (interactive
   (let ((target (ezeka--grab-dwim-file-target)))
     (list target
           (ezeka--read-label target current-prefix-arg)
           current-prefix-arg)))
  (if (not (ezeka-note-p filename))
      (error "Not a Zettel note")
    (ezeka--update-metadata-values filename nil :label label)
    (when (eq :tempus (ezeka-id-type filename))
      (cl-pushnew label ezeka-categories))))

(defun ezeka-set-citekey (filename &optional citekey degree)
  "Set CITEKEY in the Zettel note in FILENAME.
If CITEKEY is not given, get it from the parent unless it's
\\[universal-argument], in which case let the user enter the citekey
no matter what. With DEGREE, traces genealogy further than parent."
  (interactive (list (buffer-file-name)
                     current-prefix-arg
                     (if (integerp current-prefix-arg)
                         current-prefix-arg
                       1)))
  (if (not (ezeka-note-p filename))
      (error "Not a Zettel note")
    (let* ((ancestor (ezeka-trace-genealogy filename degree))
           (citekey (or (and (equal citekey '(4))
                             (read-string "New citekey: "))
                        (and ancestor
                             (alist-get :citekey
                               (ezeka-file-metadata (ezeka-link-file ancestor) t)))
                        (read-string "New citekey: "))))
      (ezeka--update-metadata-values
          filename nil
        :citekey (if (and citekey
                          (string-match-p "^[^@&]" citekey))
                     (concat "@" citekey)
                   citekey)))))

(defun ezeka-set-citekey (filename &optional citekey degree)
  "Set CITEKEY in the Zettel note in FILENAME.
If CITEKEY is not given, get it from the parent, letting the
user edit beforehand. With DEGREE (or numerical prefix
argument), trace genealogy farther than parent."
  (interactive (list (buffer-file-name)
                     nil
                     (if (integerp current-prefix-arg)
                         current-prefix-arg
                       1)))
  (unless (ezeka-note-p filename)
    (user-error "Not a Zettel note"))
  (let* ((ancestor (ezeka-trace-genealogy filename degree))
         (initial
          (or (alist-get :citekey
                (ezeka-decode-rubric (file-name-base filename)))
              (alist-get :citekey
                (ezeka-decode-rubric
                 (file-name-base (ezeka-link-file ancestor))))))
         (citekey (or citekey (read-string "New citekey: " initial))))
    (ezeka--update-metadata-values filename nil
      :citekey (if (and citekey
                        (string-match-p "^[^@&]" citekey))
                   (concat "@" citekey)
                 citekey))))

(defun ezeka-set-author (filename author)
  "Set the AUTHOR metadata in Zettel FILENAME."
  (interactive
   (let ((target (ezeka--grab-dwim-file-target)))
     (list target
           (read-string "Set author (family, given) to: "
                        (ezeka-file-name-citekey target)))))
  (if (not (ezeka-note-p filename))
      (error "Not a Zettel note")
    (ezeka--update-metadata-values filename nil :author author)))

(defvar ezeka--dynamic-keywords-cache nil
  "A list of keywords present in the current `ezeka-directory'.
This list is generated once per session and then just referenced.")

(defun ezeka--all-keywords ()
  "Return `ezeka-keywords' with optional dynamic keywords.
See `ezeka-dynamic-keywords'."
  (when (and ezeka-dynamic-keywords
             (null ezeka--dynamic-keywords-cache))
    (let* ((zk-directory ezeka-directory)
           (tag-list (zk--grep-tag-list)))
      (setq ezeka--dynamic-keywords-cache tag-list)))
  (cl-union ezeka--dynamic-keywords-cache ezeka-keywords))

(defun ezeka-add-keyword (filename keyword &optional replace metadata)
  "Add the given KEYWORD to the Zettel note in FILENAME.
When KEYWORD is nil (or \\[universal-argument]), clear any
existing keywords. When REPLACE is non-nil (or double
\\[universal-argument]), replace them with KEYWORD. Keywords
are interactively selected based on `ezeka-keywords' and
`ezeka-dynamic-keywords'. If METADATA is supplied, used
that."
  (interactive
   (list (ezeka--grab-dwim-file-target)
         (pcase current-prefix-arg
           ('(4) nil)
           ('(16) (completing-read "Replace with keyword: " (ezeka--all-keywords)))
           (_ (completing-read "Add keyword: " (ezeka--all-keywords))))
         (equal current-prefix-arg '(16))))
  (let ((keyword (cond ((null keyword) nil)
                       ((string-match-p "^#\\w+$" keyword)
                        keyword)
                       ((string-match-p "^\\w+$" keyword)
                        (concat "#" keyword))
                       (t
                        (user-error "Keywords must consist of \\w characters")))))
    (if (not (ezeka-note-p filename))
        (user-error "This command can only be used on Zettel notes")
      (let ((mdata (or metadata (ezeka-file-metadata filename))))
        (ezeka--update-metadata-values filename mdata
          :keywords (cond (replace (list keyword))
                          (keyword (cons keyword (alist-get :keywords mdata)))
                          (t nil)))))))

(defun ezeka-add-reading (filename &optional date)
  "Add DATE to the FILENAME's readings."
  (interactive (list (ezeka--grab-dwim-file-target)
                     (org-read-date t nil nil nil nil nil t)))
  (let ((mdata (ezeka-file-metadata filename)))
    (ezeka--update-metadata-values filename mdata
      :readings (cons date (alist-get :readings mdata)))))

;;;=============================================================================
;;; Populating Files
;;;=============================================================================

(defvar ezeka--citekey-history nil)

(defun ezeka-insert-header-template (&optional link label title parent citekey)
  "Insert header template into the current buffer.
If given, populate the header with the LINK, LABEL, TITLE, PARENT, and
CITEKEY."
  (interactive
   (let* ((link (if buffer-file-name
                    (ezeka-file-link buffer-file-name)
                  ;; FIXME: Rewrite with completing-read
                  (read-string "Insert template into note with this link: ")))
          (nondir (file-name-nondirectory buffer-file-name))
          (mdata (when (string-match (ezeka-zk-file-name-regexp) nondir)
                   (ezeka-decode-rubric
                    (concat (match-string 1 nondir)
                            (match-string 2 nondir)))))
          (plist (cdr (assoc link ezeka--new-child-plist))))
     (list
      link
      (or (alist-get :label mdata) (ezeka--read-label link))
      (read-string "Title: " (or (plist-get plist :title)
                                 (alist-get :caption mdata)))
      (read-string "Parent? " (plist-get plist :parent))
      (read-string
       "Citekey? " (or (plist-get plist :citekey)
                       (when (string-match "[@&][^\\s]+$"
                                           (file-name-base buffer-file-name))
                         (match-string 0 (file-name-base buffer-file-name))))
       'ezeka--citekey-history))))
  (let* ((id (ezeka-link-id link))
         (caption (ezeka--pasturize-for-filename title))
         (inhibit-read-only t)
         (content (buffer-substring-no-properties (point-min) (point-max))))
    (erase-buffer)
    (goto-char (point-min))
    (insert
     (concat ezeka-header-rubric-key
             ": "
             (ezeka-format-metadata ezeka-header-rubric-format
                                    `((:id . ,(ezeka-link-id link))
                                      (:caption . ,caption)
                                      (:label . ,label)
                                      (:citekey . ,citekey)))))
    (insert "\ntitle: " title)
    (insert "\ncreated: "
            ;; Insert creation time, making it match a tempus currens filename
            (format-time-string
             "%Y-%m-%d %a %H:%M"
             (let ((today (format-time-string "%Y%m%d")))
               (if (and (eq :tempus (ezeka-id-type id))
                        (not (string-match-p (regexp-quote today) id))
                        (not
                         (when (called-interactively-p 'any)
                           (y-or-n-p "Past tempus currens; set created time to now? "))))
                   (ezeka-encode-iso8601-datetime id)
                 nil)))
            "\n")                       ; i.e. current time
    (when (and parent (not (string-empty-p parent)))
      (insert "parent: " (ezeka--format-link parent) "\n"))
    (insert "\n")
    (insert content)))

;; FIXME: `rb-rename-file-and-buffer' is not local
(defun ezeka-incorporate-file (file kasten &optional arg)
  "Move FILE (defaults to one in current buffer) to KASTEN.
With \\[universal-argument] ARG, asks for a different name."
  (interactive (list (buffer-file-name)
                     (completing-read "Zettel kasten: " ezeka-kaesten)
                     current-prefix-arg))
  (rb-rename-file-and-buffer
   (if (not arg)
       (ezeka-link-file
        (ezeka-make-link kasten (file-name-base file)))
     (call-interactively #'rb-rename-file-and-buffer))))

;;;=============================================================================
;;; Org-Mode Intergration
;;;=============================================================================

(defun ezeka-org-set-todo-properties ()
  "Set the FROM, CREATED, and ID properties for the current org heading."
  (interactive)
  (org-set-property "ID" (org-id-get-create))
  (org-set-property "FROM"
                    (ezeka-insert-link-with-metadata
                     buffer-file-name '(:title) :after))
  (org-set-property "CREATED"
                    ;; FIXME: Surely there is a better function to do this, no?
                    (format-time-string
                     (format "[%s]"
                             (cl-subseq (cdr org-time-stamp-formats) 1 -1)))))

(defun ezeka-org-interactive-tempus ()
  "Use org-mode's `org-time-stamp' command to insert a tempus currens."
  (interactive)
  (insert (ezeka--format-link
           (ezeka-format-tempus-currens (org-read-date t t)))))

(defvar ezeka--org-timestamp-regexp
  (rx (seq
       (optional (or "[" "<"))
       (= 4 digit) "-" (= 2 digit) "-" (= 2 digit)
       " "
       (= 3 letter)
       " "
       (= 2 digit) ":" (= 2 digit)
       (optional (or "]" ">"))))
  "Regexp matching Org timestamp, either with or without time.")

(defun ezeka-dwim-with-this-timestring (&optional beg end)
  "Do What I Mean with the timestring at point or between BEG and END.
If the timestring is IS8601, make it into an org-time-stamp, and
vice-versa. If it's something else, try to make it into
org-time-stamp. Return the result of the conversion."
  (interactive (if (region-active-p)
                   (list (region-beginning) (region-end))
                 (list)))
  (when beg (goto-char beg))
  (if (or (thing-at-point-looking-at ezeka--org-timestamp-regexp)
          (thing-at-point-looking-at ezeka-iso8601-datetime-regexp)
          (thing-at-point-looking-at ezeka-iso8601-date-regexp))
      (setq beg (match-beginning 0)
            end (match-end 0))
    (error "Can't find any time strings here"))
  (let* ((text (buffer-substring-no-properties beg end))
         timestamp)
    ;; if the region was surrounded by parentheses, remove those
    (save-excursion
      (goto-char (1- beg))
      (when (re-search-forward (format "(%s)" text) (point-at-eol) t)
        (replace-match text)
        (setq beg (- beg 1)
              end (- end 1))))
    (cond ((iso8601-valid-p text)       ; ISO-8601 -> Org timestamp
           (let ((timestamp (iso8601-parse text)))
             (delete-region beg end)
             (org-insert-time-stamp (iso8601--encode-time timestamp)
                                    (integerp (car timestamp)) t)
             org-last-inserted-timestamp))
          ((setq timestamp              ; org timestamp -> ISO-8601
             (org-timestamp-from-string (if (string-match-p "[[<].*[]>]" text)
                                            text
                                          (format "[%s]" text))))
           (let ((iso8601 (org-timestamp-format timestamp "%Y%m%dT%H%M")))
             (delete-region beg end)
             (insert iso8601)
             iso8601))
          ((integerp (car (parse-time-string text))) ; datetime -> org timestamp
           (delete-region beg end)
           (org-insert-time-stamp (encode-time (parse-time-string text)) t t)
           org-last-inserted-timestamp)
          ((integerp (nth 4 (setq parsed (parse-time-string text)))) ; date -> ISO-8601
           (setf (decoded-time-second parsed) 0
                 (decoded-time-minute parsed) 0
                 (decoded-time-hour   parsed) 0)
           (let ((timestamp (format-time-string "%F" (encode-time timestamp))))
             (when (y-or-n-p
                    (format "Is %s same as %s? " text timestamp))
               (delete-region beg end)
               (insert timestamp)
               timestamp)))
          (t
           (signal 'wrong-type-argument (list text))))))

(defun ezeka-org-export-as-new-note (&optional kasten)
  "Create new Zettel in KASTEN (a string) from the current org subtree.
With \\[universal-argument], ask to select the KASTEN."
  (interactive (list (when current-prefix-arg
                       (ezeka--read-kasten "Zettel Kasten: "))))
  (let* ((parent-file buffer-file-name)
         (parent-link (ezeka-file-link buffer-file-name))
         (kasten (or kasten (ezeka-file-kasten buffer-file-name)))
         (kstruct (ezeka-kasten-named kasten))
         (new-title "")
         new-link
         new-file)
    (save-excursion
      (save-restriction
        (org-narrow-to-subtree)
        (goto-char (point-min))
        (let ((title (nth 4 (org-heading-components)))
              timestamp)
          (cond ((string-match "\\(.*\\) \\([[<].*[]>]\\)" title)
                 (list (match-string 1 title) (match-string 2 title))
                 (setq timestamp
                   (save-match-data
                     (org-timestamp-from-string (match-string 2 title))))
                 (setq new-title
                   (ezeka--minibuffer-edit-string
                    (match-string 1 title)
                    nil
                    "Title for new note: ")))
                ((org-get-scheduled-time nil)
                 (setq timestamp
                   (org-timestamp-from-time (org-get-scheduled-time nil) t)))
                (t
                 (setq new-title
                   (ezeka--minibuffer-edit-string
                    (buffer-substring-no-properties (point-at-bol) (point-at-eol))
                    nil
                    "Title for new note: "))
                 (setq timestamp
                   (org-timestamp-from-string
                    (read-string "No timestamp found. Enter it here: ")))))
          (setq new-link (ezeka-make-link
                          kasten
                          (if (eq (ezeka-kasten-id-type kstruct) :numerus)
                              (ezeka--random-id :numerus)
                            (if (org-timestamp-has-time-p timestamp)
                                (org-timestamp-format timestamp "%Y%m%dT%H%M")
                              (concat (org-timestamp-format timestamp "%Y%m%dT")
                                      (format-time-string "%H%M")))))
                new-file (ezeka-link-file new-link ""))
          (let* ((content (org-get-entry)))
            (if (file-exists-p new-file)
                (message "Aborting, file already exists: %s" new-file)
              ;; New file buffer
              (with-current-buffer (get-buffer-create new-file)
                (ezeka-insert-header-template new-link
                                              (ezeka--read-label new-file)
                                              new-title parent-link)
                (insert "\n" content)
                (set-visited-file-name new-file t)
                (basic-save-buffer))
              ;; Back in original buffer
              (with-current-buffer (get-file-buffer (file-truename parent-file))
                (org-cut-subtree)
                (insert (ezeka--format-link new-link)
                        " "
                        (alist-get :title (ezeka-file-metadata new-file)))))))))))

(defun ezeka-open-link-at-point (&optional arg)
  "Open a Zettel link at point even if it's not formatted as a link.
With \\[universal-argument] ARG, ignore `ezeka-number-of-frames' and
open the link in the same window."
  (interactive "P")
  (when (or (ezeka-link-at-point-p)
            (ezeka-link-at-point-p t))
    ;; FIXME: Is it okay to check like this for prefix arg "upstream"?
    (ezeka-find-link (ezeka-link-at-point) (or arg current-prefix-arg))
    ;; This function is later added to `org-open-at-point-functions', so "must
    ;; return t if they identify and follow a link at point. If they don’t find
    ;; anything interesting at point, they must return nil."
    t))

(defun ezeka-open-link-at-mouse (ev)
  "Open a Zettel link at mouse point (determined from EV)."
  (interactive "e")
  (mouse-set-point ev)
  (ezeka-open-link-at-point t))

;; Org links
(eval-after-load "org"
  '(progn
     ;; Try to resolve "fuzzy" links (i.e. without explicit protocol). This is
     ;; all that is needed to handle links in the form [[ZETTEL-LINK]].
     (push #'ezeka-find-link org-open-link-functions)

     ;; Do the same for Zettel links that lack even the link markup. This is
     ;; useful for following parents/children.
     (push #'ezeka-open-link-at-point org-open-at-point-functions)

     ;; This allows following links as part of #+INCLUDE statements.
     ;; TODO: Add a function to follow #+INCLUDE links
     ))

;;;-----------------------------------------------------------------------------
;;; Org-Mode: Inserting Snippets
;;;-----------------------------------------------------------------------------

(defcustom ezeka-insert-snippet-summary nil
  "Non-nil means insert the snippet summary."
  :type 'boolean
  :group 'ezeka)

(defcustom ezeka-insert-snippet-footnotes t
  "Non-nil means insert footnotes."
  :type 'boolean
  :group 'ezeka)

(defun ezeka--org-nth-link-on-line (&optional n)
  "Return the Nth link on the current line.
With a negative N, count from the end. If there is no such link,
return NIL."
  (let ((n (or n 1)))
    (save-excursion
      (save-restriction
        (narrow-to-region (point-at-bol) (point-at-eol))
        (cond ((> n 0)
               (beginning-of-line)
               (dotimes (i n)
                 (org-next-link)))
              ((< n 0)
               (end-of-line)
               (dotimes (i (abs n))
                 (org-previous-link))))
        (when (ezeka-link-at-point-p)
          (ezeka-link-at-point))))))

(defun ezeka--initialize-org-id-locations ()
  "Initialize the org-id locations."
  (interactive)
  (unless (listp org-id-extra-files)
    (setq org-id-extra-files '()))
  (dolist (file
           (directory-files-recursively
            ;; FIXME: hardcoded
            (ezeka-kasten-directory "scriptum") ".*\\.txt"))
    (cl-pushnew file org-id-extra-files)))

(defun ezeka--org-move-after-properties ()
  "Move point after the properties drawer, if any."
  (when (org-get-property-block)
    (goto-char (cdr (org-get-property-block)))
    ;; `org-get-property-block' ends on :END:
    (unless (zerop (forward-line 2))
      (insert "\n\n"))))

;;; TODO:
;;; - if region is active, narrow to it rather than to subtree (allows # lines!)
;;; - don't copy subtrees marked with COMMENT
;;; - quickly scan through all the headings and see if any need updating?
(defun ezeka-insert-snippet-text (arg link)
  "Insert snippet text from the given LINK into the current buffer.
By default, only update the text if the modification time is
different. With \\[universal-argument] ARG, forces update."
  (interactive
   (list current-prefix-arg
         (or (org-entry-get (point) "SNIP_SOURCE")
             (ezeka--org-nth-link-on-line -1)
             (error "Insert a link to the snippet source first"))))
  (save-excursion
    (save-restriction
      (org-back-to-heading)
      (let* ((file (ezeka-link-file link))
             ;; Get the metadata and most recent modification
             (mdata (ezeka-file-metadata file))
             (modified-mdata (format "[%s]"
                                     (or (alist-get :modified mdata)
                                         (alist-get :created mdata))))
             (modified-prop (org-entry-get (point) "SNIP_MODIFIED"))
             (current? (string= modified-mdata modified-prop))
             (local? (string-match-p "local"
                                     (or (org-entry-get nil "TAGS")
                                         "")))
             (org-id (org-id-get-create)))
        (org-narrow-to-subtree)
        (unless 'disabled
          (ezeka--writeable-region (point-min) (point-max)))
        (when local?
          (error "There are local changes (or at least :local: tag)"))
        (when (looking-at org-outline-regexp)
          (replace-regexp (regexp-quote (elt (org-heading-components) 4))
                          (ezeka-format-metadata "%t [[%i]]" mdata)))
        (unless (string= link (org-entry-get (point) "SNIP_SOURCE"))
          (org-entry-put (point) "SNIP_SOURCE" link))
        (org-set-tags (cl-remove "CHANGED" (org-get-tags) :test #'string=))
        (if (and current? (null arg))
            (message "Snippet is up to date; leaving alone")
          (org-entry-put (point) "SNIP_MODIFIED" modified-mdata)
          (when (or t (y-or-n-p "Update the text? "))
            ;; If current line is a comment, create a heading after it
            (when (org-at-comment-p)
              (org-insert-subheading nil))
            ;; Delete existing text
            (ezeka--org-move-after-properties)
            (let ((start (point))
                  (comments-removed 0)
                  (footnotes-removed 0)
                  (content '()))
              (delete-region start (point-max))
              ;; Get the Summary and Snippet subtrees from snippet file
              (with-current-buffer (find-file-noselect file)
                ;; Include Summary section if present
                (when (and ezeka-insert-snippet-summary
                           (org-find-exact-headline-in-buffer "Summary"))
                  (goto-char (org-find-exact-headline-in-buffer "Summary"))
                  (forward-line)
                  (let ((summary-start (point)))
                    (org-end-of-subtree)
                    (mapcar (lambda (line)
                              (push (concat "# " line "\n") content))
                            (split-string
                             (buffer-substring-no-properties summary-start (point))
                             "\n" t))))
                (goto-char
                 (or (org-find-exact-headline-in-buffer "Snippet")
                     (org-find-exact-headline-in-buffer "Content")
                     (error "Can't find the Snippet or Content section")))
                (if (not org-id)
                    (warn "No org-id added to file %s" file)
                  (org-entry-add-to-multivalued-property (point)
                                                         "USED_IN+"
                                                         (format "id:%s" org-id)))
                (basic-save-buffer)
                (ezeka--org-move-after-properties)
                (let ((content-start (point)))
                  (org-end-of-subtree)
                  (push (buffer-substring-no-properties content-start (point))
                        content)))
              ;; Insert the copied subtrees and remove its headings and comments
              (apply #'insert (nreverse content))
              (goto-char start)
              (while (re-search-forward "^[*]+ " nil t) ; remove headings
                (goto-char (match-beginning 0))
                (ezeka--org-move-after-properties)
                (kill-region (match-beginning 0) (point)))
              ;; Remove <<tags>>
              (goto-char start)
              (while (re-search-forward "<<[^>]+>>\n*" nil t)
                (replace-match ""))
              ;; Remove Zettel links
              (goto-char start)
              (while (re-search-forward " ?\\[\\[[^]]+]]" nil t)
                (replace-match ""))
              ;; Remove inline @@...@@ and {...~} comments, but not {{...}}
              (goto-char start)
              (while (re-search-forward " ?\\(@@\\|{[^{]\\).*\\(@@\\|~}\\)\n*" nil t)
                (cl-incf comments-removed)
                (replace-match ""))
              ;; Remove footnotes if need be
              (unless ezeka-insert-snippet-footnotes
                (goto-char start)
                (while (re-search-forward "^\\[fn:.+?\\].*?$" nil t)
                  (goto-char (match-beginning 0))
                  (kill-paragraph 1)
                  (cl-incf footnotes-removed)))
              (org-indent-region (point-min) (point-max))
              (goto-char (point-max))
              (insert "\n")
              (message "Removed %d comments and %d footnotes"
                       comments-removed footnotes-removed)
              (rb-collapse-blank-lines)
              t)))
        (when nil                     ; FIXME: Remove?
          (ezeka--read-only-region (point-min) (point-max)))))))

(defun ezeka--update-inserted-snippet ()
  "Update the snippet in the current note wherever it is used."
  (let ((current (current-buffer)))
    (save-excursion
      (when-let ((pos (or (org-find-exact-headline-in-buffer "Snippet")
                          (org-find-exact-headline-in-buffer "Content"))))
        (goto-char pos)
        (when-let ((used-in (org-entry-get (point) "USED_IN+"))
                   (used-list
                    (split-string
                     (replace-regexp-in-string "\\(id:\\|\\[id:\\)" "" used-in)
                     nil t " \n")))
          (dolist (org-id used-list)
            (org-id-goto org-id)
            (org-back-to-heading t)
            (org-set-tags (cl-union '("CHANGED") (org-get-tags) :test #'string=)))))
      (switch-to-buffer current))))
(add-hook 'ezeka-modified-updated-hook #'ezeka--update-inserted-snippet)

(defun ezeka-find-inserted-snippet ()
  "Find source of snippet inserted with `ezeka-insert-snippet-text'.
The point should be within the org entry. If called from the heading
with :USED_IN: property, perform the reverse action."
  (interactive)
  (let ((line-at-point (thing-at-point 'line t)))
    (if-let ((used-in (org-entry-get (point) "USED_IN+")))
        (progn
          (org-id-goto (string-trim used-in "\\(id:\\|\\[id:\\)" "]"))
          (org-back-to-heading t))
      (if-let ((source-link (org-entry-get (point) "SNIP_SOURCE")))
          (ezeka-find-link source-link)
        (org-back-to-heading)
        (org-next-link)
        (org-open-at-point)
        (goto-char (point-min))))
    (when (search-forward (string-trim line-at-point) nil t)
      (goto-char (match-beginning 0)))))

(defun ezeka-transclude-snippet (link)
  "Insert `#+transclude' statement from LINK."
  (interactive
   ;; Assume the file is the last link on the current line
   (list (save-excursion
           (org-back-to-heading)
           (end-of-line)
           (org-previous-link)
           (when (ezeka-link-at-point-p)
             (ezeka-link-at-point)))))
  ;; Get the metadata and most recent modification
  (save-excursion
    (save-restriction
      (let* ((file (ezeka-link-file link))
             (metadata (ezeka-file-metadata file)))
        (org-narrow-to-subtree)
        ;; Update the heading title
        (org-edit-headline
         (format "%s [[%s]]" (alist-get :title metadata) link))
        ;; Delete existing text
        (org-back-to-heading t)
        (delete-region (point-at-bol 2) (org-end-of-subtree t))
        ;; Insert the transclusion line
        (insert (format "\n#+transclude: [[%s::begin]] :lines 2- :end \"end\""
                        (file-relative-name file)))))))

(defcustom ezeka-snippet-heading "Snippet"
  "The text of the snippet heading."
  :type 'string)

(defun ezeka-org-footnote-action-maybe-local (&optional arg)
  "Place footnotes locally in snippets, ignoring `org-footnote-section'.
This is a wrapper around `org-footnote-action' to be used in the
transcluded snippets. Footnotes are placed locally if the current
heading matches `ezeka-snippet-heading' or if the command was called
with \\[universal-argument] ARG. With double \\[universal-argument],
offer additional options."
  (interactive "P")
  (let ((snippet? (string= ezeka-snippet-heading
                          (save-excursion
                            (org-back-to-heading-or-point-min t)
                            (and (org-context)
                                 (nth 4 (org-heading-components)))))))
    (if (or snippet? arg)
        (let ((org-footnote-section nil)
              (org-footnote-auto-label nil)
              (org-footnote-define-inline nil)
              (org-footnote-auto-adjust 'sort)
              (context (org-context)))
          (org-element-cache-reset)
          (kill-new (format-time-string "%H%M"))
          (org-footnote-new))
      (org-footnote-action (equal arg '(16))))))

;;;=============================================================================
;;; Bookmark Integration
;;;=============================================================================

(defun bookmark-make-record-ezeka ()
  "Set the bookmark's filename property to the Zettel link.
This is the Bookmark record function for Zettel files."
  (list (cons 'filename (ezeka-file-link buffer-file-name))
        (cons 'handler 'bookmark-ezeka-handler)))

(defun bookmark-ezeka-handler (bmk-record)
  "Handle bookmark records for Zettel bookmark in BMK-RECORD."
  (find-file (ezeka-link-file (cdr (assoc 'filename bmk-record)))))

;; Use the special ezeka bookmark handler in Zettel buffers
(add-hook 'ezeka-mode-hook
  (lambda ()
    (setq-local bookmark-make-record-function 'bookmark-make-record-ezeka)))

;;;=============================================================================
;;; Maintenance
;;;=============================================================================

(defvar ezeka--move-log-file "auto/move.log"
  "Path, relative to `ezeka-directory', to the log file for recording moves.")

(defun ezeka--add-to-move-log (link1 link2)
  "Log the move from LINK1 to LINK2 in `ezeka--move-log-file'."
  ;; FIXME: Hardcoded
  ;; ("SOURCE" "TARGET" "TIME")
  (write-region (format "\n%S" (list link1 link2 (format-time-string "%FT%T")))
                nil
                (expand-file-name ezeka--move-log-file ezeka-directory)
                'append))

(defun ezeka-link-moved-p (link &optional confirm)
  "Check whether LINK appears in the `ezeka--move-log-file'.
If CONFIRM is non-nil, confirm the link to check."
  (interactive
   (let ((link (when (ezeka-link-at-point-p t)
                 (ezeka-link-at-point))))
     (list (if (or current-prefix-arg (null link))
               (read-string "Check which link? " link)
             link))))
  (with-temp-buffer
    (insert-file-contents ezeka--move-log-file)
    (goto-char (point-min))
    (if-let* ((_ (re-search-forward
                  (concat ".*" link ".*") nil t)))
        (cl-destructuring-bind (source target time)
            (read (match-string 0))
          (kill-new target)
          (if (y-or-n-p
               (format "%s moved to %s on %s. Open? " source target time))
              (find-file-other-window (ezeka-link-file target))
            t))
      (message "No record of moving %s" link))))

(defun ezeka--move-note (link1 link2 &optional confirm)
  "Move Zettel note from LINK1 to LINK2.
With CONFIRM, confirm before move."
  (let ((path1 (ezeka-link-file link1 'try-wild))
        (path2 (ezeka-link-file link2 "")))
    (when (or (not confirm)
              (y-or-n-p (format "Move %s to %s? " link1 link2)))
      (unless (file-exists-p (file-name-directory path2))
        (make-directory (file-name-directory path2)))
      (ezeka--rename-file path1 path2)
      (let* ((mdata (ezeka-file-metadata path2))
             (oldnames (alist-get :oldnames mdata)))
        (ezeka--add-to-move-log link1 link2)
        ;; Put original name in oldnames, unless link1 == link2, and remove
        ;; link2 from oldnames just in case we're resurrecting an oldname.
        (unless (string= (ezeka-link-id link1) (ezeka-link-id link2))
          (setf (alist-get :oldnames mdata)
                (cl-union (list link1) (remove link2 oldnames))))
        (ezeka--update-file-header path2 mdata t)
        (when-let ((buf (get-file-buffer path2)))
          (with-current-buffer buf
            (save-buffer)))
        ;; Replace links
        (message "Replacing links: %s with %s" link1 link2)
        (let ((replaced (ezeka-zk-replace-links link1 link2)))
          (message "Moved %s to %s, replacing %d links in %d files"
                   link1 link2
                   (or (car replaced) 0) (or (cdr replaced) 0)))))))

(defun ezeka--resurrectable-oldname (source-file id-type &optional metadata)
  "Check SOURCE-FILE's oldnames for an oldname of ID-TYPE.
ID-TYPE should be a keyword matching an ID type in
`ezeka-kaesten'. If METADATA is non-nil, use that rather
than parsing the file again. If successful, return the
appropriate oldname."
  (when-let* ((mdata (or metadata (ezeka-file-metadata source-file)))
              (cadaver (cl-find-if (lambda (link)
                                     (when (ezeka-link-p link)
                                       (eq (ezeka-id-type link t) id-type)))
                                   (alist-get :oldnames mdata))))
    (unless (ezeka-link-file cadaver nil t)
      cadaver)))

(defun ezeka-move-to-another-kasten (source-file kasten &optional target-link noselect)
  "Move SOURCE-FILE Zettel to a generated link in KASTEN.
With \\[universal-argument], asks for an explicit TARGET-LINK instead.
Open (unless NOSELECT is non-nil) the target link and returns it."
  (interactive
   (let ((target (when (equal current-prefix-arg '(4))
                   (read-string "Enter target link: "))))
     (list (ezeka--grab-dwim-file-target)
           (if target
               (ezeka-link-kasten target)
             (completing-read "Which kasten to move to? "
                              (mapcar #'ezeka-kasten-name ezeka-kaesten)))
           target)))
  (let* ((ezeka-header-update-modified 'never)  ; FIXME: Hardcoded
         (source-link (ezeka-file-link source-file))
         (id-type (ezeka-kasten-id-type (ezeka-kasten-named kasten)))
         (target-link
          (or target-link
              (ezeka--resurrectable-oldname source-file id-type)
              (if (eq id-type :tempus)          ; FIXME: Hardcoded
                  (ezeka-tempus-currens-id-for source-link)
                (ezeka--generate-id kasten 'confirm)))))
    (if (not target-link)
        (error "Don't know where to move %s" source-link)
      (save-some-buffers nil (lambda () (ezeka-note-p buffer-file-name t)))
      (ezeka--move-note source-link target-link)
      (cond ((string= source-file buffer-file-name)
             (kill-this-buffer)
             (unless (> (length (frame-list))
                        (cl-case ezeka-number-of-frames
                          (one 1)
                          (two 2)))
               (delete-frame)))
            ((eq major-mode 'magit-status-mode)
             (magit-refresh)))
      (unless noselect
        (ezeka-find-link target-link t)))))

(defun ezeka-generate-n-new-ids (how-many type)
  "Generate HOW-MANY new IDs of TYPE, making sure there are no dulicates."
  (interactive
   (list (read-number "How many? " 10)
         (completing-read "Which type? "
                          (mapcar #'ezeka-kasten-type
                                  ezeka-kaesten))))
  (goto-char (point-max))
  (let (ids)
    (dotimes (n how-many)
      (push (ezeka--random-id type) ids))
    (mapc (lambda (s)
            (insert s "\n"))
          (delete-dups ids))
    (delete-duplicate-lines (point-min) (point-max))))

;; Based on https://stackoverflow.com/a/30328255
;;
;; Add the following to emacs config file:
;;
;; (add-hook 'post-command-hook 'ezeka--magit-show-title-in-mode-line)
;; (add-hook 'magit-mode-hook
;;   (lambda ()
;;     (setq ezeka--original-mode-line mode-line-misc-info)
;;     (add-hook 'post-command-hook 'ezeka--magit-show-title-in-mode-line nil t)))

(defvar ezeka--original-mode-line nil
  "Value of `mode-line-misc-info' before we override it.")

(defun ezeka--magit-show-title-in-mode-line ()
  "Display Zettel title of the file under cursor in the mode line."
  (while-no-input
    (redisplay)
    (when-let* ((file
                 (cl-case major-mode
                   (magit-status-mode (magit-file-at-point))
                   (dired-mode (dired-file-name-at-point))
                   (wdired-mode (dired-file-name-at-point))
                   (t (setq mode-line-misc-info ezeka--original-mode-line)
                      nil)))
                (line (magit-file-line file))
                (mdata
                 (ezeka-decode-rubric
                  (when (string-match ezeka-header-line-regexp line)
                    (match-string 2 line)))))
      (setq mode-line-misc-info
        (format "%s: %s"
                (propertize (alist-get :id mdata)
                            'face '(:weight bold))
                (propertize (alist-get :title mdata)
                            'face '(:slant italic)))))))

;;;=============================================================================
;;; Mode
;;;=============================================================================

;; Define a minor mode for working with Zettel files
(define-minor-mode ezeka-mode
  "Make the keymap ezeka-mode-map active."
  :lighter " Ezeka"
  :keymap
  (mapcar (lambda (tuple)
            (cons (if (stringp (car tuple)) (kbd (car tuple)) (car tuple))
                  (cdr tuple)))
          ;;------------------------------------------------------------------
          ;; According to key binding conventions, the only bindings reserved
          ;; for minor modes are "Sequences consisting of C-c followed by any
          ;; other punctuation character" than {, }, <, >, : or ; that are
          ;; reserved for major modes, leaving the following:
          ;;
          ;; ` ~ ! @ # $ % ^ & * ( ) - _ = + [ ] | \ ' " , . / ?
          ;; X X   X X X X X           X X X X X X X X X X X X X
          ;;------------------------------------------------------------------
          '(
            ("C-c `" . ezeka-toggle-header-read-only) ; `org-table-edit-field'
            ("C-c ~" . ezeka-set-title-or-caption) ; `org-table-create-with-table\.el'
            ;; ("C-c !" . ) ; `org-time-stamp-inactive'
            ("C-c @" . ezeka-set-citekey)
            ("C-c #" . ezeka-add-keyword)
            ("C-c $" . ezeka-kill-ring-save-link-at-point)
            ("C-c %" . ezeka-kill-ring-save-link-or-filename)
            ("C-c ^" . ezeka-find-ancestor)
            ;; ("C-c &" . ) ; yasnippet
            ;; ("C-c *" . ) ; `org-ctrl-c-star'
            ("C-c (" . ezeka-normalize-file-name)
            ("C-c )" . ezeka-set-title-or-caption)
            ;; ("C-c -" . ) ; `org-ctrl-c-minus' that turns region into list
            ("C-c _" . ezeka-find-descendant)
            ("C-c =" . ezeka-kill-ring-save-link-and-title) ; `org-table-eval-formula'
            ("C-c +" . ezeka-dwim-with-this-timestring)
            ("C-c [" . ezeka-update-link-prefix-title) ; `org-agenda-file-to-front'
            ("C-c ]" . ezeka-add-reading)
            ("C-c |" . ezeka-toggle-update-header-modified) ; `org-table-create-or-convert-from-region'
            ("C-c '" . ezeka-set-label)                     ; `org-edit-special'
            ("C-c \"" . ezeka-insert-ancestor-link)
            ("C-c ," . ezeka-insert-new-child-with-title)
            ("C-c ." . ezeka-insert-link-from-clipboard) ; `org-table-eval-formula'
            ("C-c /" . ezeka-set-author)                 ; `org-sparse-tree'
            ("C-c ?" . ezeka-links-to)  ; `org-table-field-info'

            ;; shadows `org-open-at-mouse', but allows opening in same window with C-u
            ([C-down-mouse-1] . ezeka-open-link-at-mouse)
            ;;
            ;; Unsafe: reserved for major modes
            ;;
            ;; Shadows `org-schedule'
            ;; ("C-c C-s" . ezeka-select-and-find-link)
            ;; Shadows `kill-sexp' in global-map
            ;; ("C-M-k" . ezeka-kill-link-or-sexp-at-point)
            ;; Shadows `org-ctrl-c-tab'
            ;; ("C-c C-i" . 'ezeka-org-include-cached-file)
            ;; Shadows `org-set-property-and-value'
            ("C-c C-x F" . ezeka-org-set-todo-properties)
            ("C-c C-x z" . ezeka-move-to-another-kasten)
            ))                          ; end of :keymap
  (cond (ezeka-mode
         (when (or (ezeka-note-p (current-buffer))
                   (y-or-n-p "This doesn't look like an Ezeka note. Still enable `ezeka-mode'? "))
           (ezeka--make-header-read-only (current-buffer))

           (add-hook 'before-save-hook 'ezeka--update-file-header nil t)
           (add-hook 'before-save-hook 'ezeka-normalize-file-name nil t)

           ;; Treat : (colon) as part of the word, allowing
           ;; forward/backward-word over full Zettel links.
           (modify-syntax-entry ?: "w")))
        (t
         (remove-hook 'before-save-hook 'ezeka--update-file-header t)
         (remove-hook 'before-save-hook 'ezeka-normalize-file-name t)

         (modify-syntax-entry ?: "."))))

(provide 'ezeka)
;;; ezeka.el ends here
