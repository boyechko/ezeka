;;; ezeka.el --- Eclectic Zettelkasten -*- lexical-binding: t -*-

;; Copyright (C) 2015-2022 Richard Boyechko

;; Author: Richard Boyechko <code@diachronic.net>
;; Version: 0.1
;; Package-Requires: ((emacs "25.1") (org "9.5"))
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

(require 'org)
(require 'format-spec)

;;;=============================================================================
;;; Internal Variables
;;;=============================================================================

(defun in-ezeka-dir (&optional relative-path)
  "Returns absolute pathname of the given pathspec relative to
the Zettel directory."
  (expand-file-name (or relative-path "") ezeka-directory))

;; FIXME: temporary
(defvar ezeka-regexp-bolus-currens
  "\\([0-9]\\{3\\}\\)-\\([a-z]\\{3\\}\\)"
  "The regular expression that matches bolus currens like abc-123.")

(defvar ezeka-regexp-numerus-currens
  "\\([a-z]\\)-\\([0-9]\\{4\\}\\)"
  "The regular expression that matches numerus currens like d-0503.")

(defvar ezeka-regexp-tempus-currens
  "\\([0-9]\\{4\\}\\)\\([0-9][0-9]\\)\\([0-9][0-9]\\)T\\([0-9][0-9]\\)\\([0-9][0-9]\\)"
  "The regular expression that matches the basic (but not extended) ISO 8601
timestamp.
Groups 1-3 are year, month, day.
Groups 4-5 are hour, minute.")

;; FIXME: Is this or the individually-named variables redundant?
(defvar ezeka-id-type-regexp-alist
  `((:bolus   . ,ezeka-regexp-bolus-currens) ; FIXME: temporary
    (:numerus . ,ezeka-regexp-numerus-currens)
    (:tempus  . ,ezeka-regexp-tempus-currens))
  "An alist of type and its regular expressions for the various ID types.")

(defvar ezeka-id-type-example-alist
  '((:bolus   . "123-abc") ; FIXME: temporary
    (:numerus . "a-1234")
    (:tempus  . "20210123T1234"))
  "An alist of type and an example of what it looks like for the various ID
types.")

(defvar ezeka-regexp-id
  ;; Strip the groups in the component regexps
  (concat "\\("
          (replace-regexp-in-string "\\\\[()]" "" ezeka-regexp-numerus-currens)
          "\\|"
          (replace-regexp-in-string "\\\\[()]" "" ezeka-regexp-tempus-currens)
          ;; FIXME: temporary
          "\\|"
          (replace-regexp-in-string "\\\\[()]" "" ezeka-regexp-bolus-currens)
          "\\)")
  "A generalized regexp that matches any ID, whatever its ID type.")

(defvar ezeka-regexp-link
  (concat "\\(?:\\(?2:[[:alpha:]]+\\):\\)*\\(?1:" ezeka-regexp-id "\\)")
  "The regular expression that matches Zettel links.

Group 1 is the ID.
Group 2 is the kasten, if specified.")

(defvar ezeka-regexp-link-simplified
  (concat "\\(?:\\(?2:[[:alpha:]]+\\):\\)*\\(?1:[0-9a-zT-]+\\)")
  "The regular expression that matches Zettel links but without matching
`ezeka-regexp-id' precisely.

Group 1 is the ID.
Group 2 is the kasten, if specified.")

(defvar ezeka-regexp-iso8601-date
  "\\<\\([0-9]\\{4\\}\\)-*\\([0-9]\\{2\\}\\)-*\\([0-9]\\{2\\}\\)"
  "The regular expression that matches ISO 8601-like date.
Groups 1-3 are year, month, day.")

(defvar ezeka-regexp-iso8601-time
  "T*\\([0-9]\\{2\\}\\):*\\([0-9]\\{2\\}\\)\\>"
  "The regular expression that matches ISO 8601-like time.
Groups 1-2 are hour and minute.")

(defvar ezeka-regexp-iso8601-datetime
  (concat ezeka-regexp-iso8601-date ezeka-regexp-iso8601-time)
  "The regular expression that matches ISO 8601 date and time separate with T.
Groups 1-3 are year, month, day.
Groups 4-5 are hour and minute.")

(defvar ezeka-pregenerated-numeri-file "auto/unused-numeri.dat"
  "List of unused numri curentes to use for creating new numerus currens
Zettel in rumen when Emacs cannot check the list of existing files.")

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

(defcustom ezeka-kaesten
  ;; name | directory | ID type | list-order
  `(("os"         :tempus  1)
    ("rumen"      :numerus 2)
    ("esophagus"  :bolus   3)
    ("omasum"     :tempus  4)
    ("abomasum"   :tempus  5)
    ("rectum"     :tempus  6)
    ("fabula"     :tempus  7)
    ("machina"    :tempus  8))
  "An alist containing the names and ID types of kaesten."
  :type 'alist
  :group 'ezeka)

(defcustom ezeka-kaesten-aliases nil
  "An alist of any other aliases for the `ezeka-kaesten'. This is an alist of
the actual name followed by the alias."
  :type 'alist
  :group 'ezeka)

(defcustom ezeka-default-kasten
  ;; ID type | kasten name
  `((:numerus . "rumen")
    (:bolus . "esophagus")              ; FIXME: temporary
    (:tempus . "omasum"))
  "An alist of default Kasten (i.e. not requiring fully qualified link) for
each ID type."
  :type 'alist
  :group 'ezeka)

(defcustom ezeka-categories nil
  "A list of categories used for Zettel."
  :type 'list
  :group 'ezeka)

(defcustom ezeka-genera nil
  "An alist of genera used for numerus currens Zettel.
Each element should be in the form
(LATIN-LETTER GENUS DESCRIPTION)"
  :type 'list
  :group 'ezeka)

(defcustom ezeka-keywords nil
  "A list of frequently-used keywords. Each element should be a string
beginning with #."
  :type 'list
  :group 'ezeka)

(defcustom ezeka-number-of-frames nil
  "Try to use only this many frames. Nil means single frame."
  :type 'symbol
  :options '(one two many)
  :group 'ezeka)

(defcustom ezeka-update-header-modified t
  "Determines whether `ezeka--update-file-header' updates the
modification date. Possible choices are ALWAYS, SAMEDAY, NEVER, or
CONFIRM (same as T)."
  :type 'symbol
  :group 'ezeka)

(defcustom ezeka-save-after-metadata-updates 'confirm
  "Determines whether `ezeka-set-label', `ezeka-set-title', and
`ezeka-set-citekey' will automatically save the file after
modification."
  :type 'symbol
  :options '(nil t confirm)
  :group 'ezeka)

;;;=============================================================================
;;; General Functions
;;;=============================================================================

(defun space-or-punct-p (char)
  "Returns T if the character is a space or punctuation."
  (when char
    (string-match-p "[[:space:][:punct:]]" (char-to-string char))))

;; TODO: More extensible way to do this without invoking other modes?
(defun ezeka--grab-dwim-file-target (&optional link-at-point)
  "Returns the do-what-I-mean Zettel file from a variety of modes. If
LINK-AT-POINT is non-nil, prioritize such a link if exists."
  (cond ((and link-at-point (ezeka-link-at-point-p))
         (ezeka-link-file (ezeka-link-at-point) t))
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
  "Replace pairs in the REPLACEMENTS alist in STRING. Each item in
REPLACEMENTS should have the form

(FROM TO REGEXP)

If REGEXP is non-nil, FROM should be a regexp string."
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

(defun ezeka--read-parallel-change (old-string new-string)
  "Use `read-string' to change OLD-STRING to NEW-STRING, showing them
in parallel."
  (read-string
   (concat "Change: " (propertize old-string 'face 'italic) "\n"
           "    to: ")
   new-string nil new-string))

;; See https://stackoverflow.com/a/65685019
(defun ezeka--save-buffer-read-only (buffer)
  "Save the given BUFFER after enabling `read-only-mode'
and therefore preventing hooks from running."
  (if buffer-read-only
      (save-buffer)
    (read-only-mode 1)
    (save-buffer)
    (read-only-mode 0)))

(defun ezeka--rename-file (filename newname)
  "Rename the given FILENAME to NEWNAME. If NEWNAME is relative, fill
missing values from FILENAME."
  (let ((newname (if (file-name-absolute-p newname)
                     newname
                   (expand-file-name
                    (file-name-with-extension
                     newname (file-name-extension filename))
                    (file-name-directory filename)))))
    (cond ((not (and filename (file-exists-p filename)))
           (set-visited-file-name newname))
          ((vc-backend filename)
           (vc-rename-file filename newname))
          (t
           (rename-file filename newname t)
           (set-visited-file-name newname t t)))))

;;;=============================================================================
;;; Fundamental Functions
;;;=============================================================================

(defun ezeka-note-p (file-or-buffer &optional strict)
  "Returns non-NIL if the file or buffer is a Zettel. It is a Zettel if all
of these conditions are met:
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
             (string-match ezeka-file-name-regexp (file-name-base file))
             (if strict
                 (and (file-exists-p file)
                      (string-prefix-p ezeka-directory file)) ; FIXME: Hack
               t))))))

(defun ezeka-kasten-directory (kasten)
  "Returns the directory of the given KASTEN."
  (if (assoc kasten ezeka-kaesten)
      (file-name-as-directory (in-ezeka-dir (ezeka-kasten-truename kasten)))
    (error "Unknown Kasten: %s" kasten)))

(defun ezeka-directory-kasten (directory)
  "Returns the kasten name of the given Zettel directory."
  ;; FIXME: This is a hack that would not work if Kasten names don't match the
  ;; directory name.
  (file-name-base (directory-file-name directory)))

(defun ezeka-kasten-truename (kasten)
  "Returns the true name of the given KASTEN."
  (or (cdr (assoc kasten ezeka-kaesten-aliases))
      (car (assoc kasten ezeka-kaesten))))

(defun ezeka-file-name-valid-p (filename)
  "Returns non-nil if the given FILENAME is a valid Zettel filename."
  (save-match-data
   (string-match ezeka-file-name-regexp (file-name-base filename))))

(defun ezeka--file-name-part (filename part)
  "Returns the given PART (:id, :label, :caption, or :citekey) of the
FILENAME."
  (let ((base (file-name-base filename)))
    (save-match-data
      (when (string-match ezeka-file-name-regexp base)
        (let ((match (match-string (cl-case part
                                     (:id      1)
                                     (:label   3)
                                     (:caption 4)
                                     (:citekey 5))
                                   base)))
          (when match
            (string-trim match)))))))

(defmacro ezeka-file-name-id (filename)
  "Returns the ID part of the given Zettel FILENAME."
  `(ezeka--file-name-part ,filename :id))
(defmacro ezeka-file-name-label (filename)
  "Returns the label part of the given Zettel FILENAME."
  `(ezeka--file-name-part ,filename :label))
(defmacro ezeka-file-name-caption (filename)
  "Returns the caption part of the given Zettel FILENAME."
  `(ezeka--file-name-part ,filename :caption))
(defmacro ezeka-file-name-citekey (filename)
  "Returns the citekey part of the given Zettel FILENAME."
  `(ezeka--file-name-part ,filename :citekey))

;; FIXME: Relies on the fact that the Kasten directory is 2nd from the last.
(defun ezeka-file-kasten (file &optional noerror)
  "Returns the Kasten of the given Zettel file. If NOERROR is non-nil,
simply returns nil if cannot figure out the Kasten."
  (let ((dirs (reverse (split-string (file-name-directory file) "/" t "/"))))
    (cond ((assoc (car dirs) ezeka-kaesten)
           (ezeka-kasten-truename (car dirs)))
          ((assoc (cadr dirs) ezeka-kaesten)
           (ezeka-kasten-truename (cadr dirs)))
          (t
           (unless noerror
             (error "Can't figure out kasten for %s" file))))))

(defun ezeka-file-link (file &optional noerror)
  "Given the path to a Zettel FILE, returns a fully qualified link to
it. If NOERROR is non-nil, do not signal an error if cannot figiure
out a proper link, just return nil."
  (let ((kasten (ezeka-file-kasten file noerror))
        (id (ezeka-file-name-id file)))
    (cond ((or (null kasten) (null id))
           (unless noerror
             (error "Can't get id or kasten for file %s" (file-name-base file))))
          ((equal kasten (alist-get (ezeka-id-type id) ezeka-default-kasten))
           id)
          (t
           (concat kasten ":" (ezeka-file-name-id file))))))

(defun ezeka-link-p (string)
  "Returns non-NIL if the string could be a link to a Zettel."
  (and (stringp string)
       (string-match (concat "^" ezeka-regexp-link "$") string)
       ;; If kasten is specified, make sure it's a valid one
       (if (match-string-no-properties 2 string)
           (or (assoc (match-string-no-properties 2 string) ezeka-kaesten)
               (assoc (match-string-no-properties 2 string) ezeka-kaesten-aliases))
         t)))

(defun ezeka-link-kasten (link)
  "Returns the kasten part of the given LINK. If no kasten is explicitly
specified, asks the user to resolve the ambiguity."
  (when (string-match ezeka-regexp-link link)
    (let* ((kasten (match-string 2 link))
           (id (match-string 1 link))
           (type (ezeka-id-type id)))
      (or kasten
          (if-let ((default (alist-get type ezeka-default-kasten)))
              default
            (call-interactively #'ezeka-set-default-kasten)
            (ezeka-link-kasten link))))))

(defun ezeka-set-default-kasten (type kasten)
  "Interactively set the default kasten for the given ID type. See
`ezeka-default-kasten' for valid types."
  (interactive
   (list (intern (completing-read "Set default for which type of Zettel? "
                           (mapcar #'first ezeka-default-kasten)))
         (completing-read "Set the default to what Kasten? "
                   (if (listp ezeka-kaesten)
                       (mapcar #'first ezeka-kaesten)
                     (error "No Zettelkästen defined")))))
  (setf (alist-get type ezeka-default-kasten) kasten))

(defun ezeka-kasten-id-type (kasten)
  "Returns the Zettel ID naming type for the given kasten based on
`ezeka-kaesten'."
  (cadr (assoc kasten ezeka-kaesten #'string=)))

(defun ezeka-link-id (link)
  "Returns the ID part of the given LINK."
  (when (string-match ezeka-regexp-link link)
    (match-string 1 link)))

(defun ezeka-make-link (kasten id)
  "Make a new proper link to ID in KASTEN."
  (let ((id-type (ezeka-kasten-id-type kasten)))
    (cond ((not id-type)
           (error "Unknown kasten: %s" kasten))
          ((not
            (string-match-p (alist-get id-type ezeka-id-type-regexp-alist) id))
           (error "ID doesn't match the ID type for %s kasten" kasten))
          ((rassoc kasten ezeka-default-kasten)
           id)
          (t
           (concat kasten ":" id)))))

(defun ezeka-subdirectory (id)
  "Returns the relative subdirectory for the given ID, a string."
  (cl-case (ezeka-id-type id)
    (:numerus (file-name-as-directory (cl-subseq id 0 1)))
    (:tempus (file-name-as-directory (match-string 1 id)))
    (:bolus (file-name-as-directory
             (format "%c00-%c99" (elt id 0) (elt id 0))))))

(defun ezeka-link-file (link &optional noerror rubric)
  "Return a full file path to the Zettel LINK. If RUBRIC is nil (so
return a file name consisting just the LINK), 'wild (find existing
file beginning with LINK), or a string (returns a filename consisting
of LINK, `ezeka-file-name-separator', and RUBRIC). If NOERROR is
non-NIL, don't signal an error if the link is invalid."
  (or (catch 'invalid
        (when (ezeka-link-p link)
          (let* ((kasten (ezeka-link-kasten link))
                 (id (ezeka-link-id link))
                 (filename
                  (expand-file-name
                   (format "%s%s.%s"
                           id
                           (cond ((null rubric) "*")
                                 ((eq rubric 'wild) "*")
                                 ((stringp rubric)
                                  (concat ezeka-file-name-separator rubric))
                                 (t
                                  (signal 'wrong-type-argument
                                          '(rubric (or nil 'wild stringp)))))
                           ezeka-file-extension)
                   (file-name-concat (ezeka-kasten-directory kasten)
                                     (or (ezeka-subdirectory id)
                                         (throw 'invalid nil))))))
            (if (eq rubric 'wild)
                (car (file-expand-wildcards filename))
              (or (car (file-expand-wildcards filename))
                  (string-replace "*" "" filename)))))) ; FIXME: Hackish
      (unless noerror
        (error "Link not valid: %s" link))))

(defun ezeka-id-type (id-or-file)
  "Returns the type of the given ID or file: :NUMERUS or :TEMPUS. Modifies
match data after matching against the appropriate ID type regexp."
  (let ((id (file-name-base id-or-file)))
    (cond ((string-match ezeka-regexp-tempus-currens id)
           :tempus)
          ((string-match ezeka-regexp-numerus-currens id)
           :numerus)
          ;; FIXME: Temporary
          ((string-match ezeka-regexp-bolus-currens id)
           :bolus)
          (t
           ;; Anything else is not a Zettel
           nil))))

(defun ezeka-encode-iso8601-datetime (string)
  "Returns the internal encoded time given the ISO8601 date/time
expression, with or without time."
  (let ((second 0) (minute 0) (hour 0) day month year)
    (when (string-match (concat "^" ezeka-regexp-iso8601-date) string)
      (setq year  (string-to-number (match-string 1 string))
            month (string-to-number (match-string 2 string))
            day   (string-to-number (match-string 3 string)))
      (when (string-match ezeka-regexp-iso8601-time string
                          (match-end 0))
        (setq hour   (string-to-number (match-string 1 string))
              minute (string-to-number (match-string 2 string))))
      (encode-time second minute hour day month year))))

(defun ezeka-file-content (file &optional header-only noerror)
  "Returns the content of the FILE, getting it either from an opened buffer
or the file itself. If NOERROR is non-NIL, don't signal an error if cannot
get the content. If HEADER-ONLY is non-nil, only get the header."
  (cl-flet ((retrieve-content ()
              "Get the content from `current-buffer'."
              (widen)
              (if (= 0 (buffer-size))
                  (unless noerror
                    (error "Buffer %s is empty" (current-buffer)))
                (buffer-substring-no-properties
                 (point-min)
                 (if (not header-only)
                     (point-max)
                   (goto-char (point-min))
                   (if (re-search-forward "\n\n" nil t)
                       (match-beginning 0)
                     (unless noerror
                       (error "Cannot separate header in %s" file))))))))
    (cond ((get-file-buffer file)
           (save-excursion
             (save-restriction
               (with-current-buffer (get-file-buffer file)
                 (retrieve-content)))))
          ((file-exists-p file)
           (with-temp-buffer
             (insert-file-contents file)
             (retrieve-content)))
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

(defvar ezeka-regexp-header-line
  "\\(?1:\\w+\\):\\s-+\\(?2:.*\\)"
  "The regular expression that matches a line of YAML metadata.
Group 1 is the key.
Group 2 is the value.")

(defcustom ezeka-regexp-header-separator "^$"
  "Regexp that matches the separator line between header and the note text."
  :type 'string
  :group 'ezeka)

(defcustom ezeka-header-rubric-key "rubric"
  "The header metadata key for the rubric."
  :type 'string
  :group 'ezeka)

(defcustom ezeka-header-stable-caption-mark "§"
  "A mark used in the rubric value to signify that any differences
between caption and title values should be ignored as long as filename
and header match."
  :type 'string
  :group 'ezeka)

(defcustom ezeka-header-rubric-format "%s%i {%l} %c %k"
  "The format-spec string for generating the note's rubric to be used
in the note's header.
See `ezeka-format-metadata' for details.
This should match `ezeka-regexp-header-rubric'."
  :type 'string
  :group 'ezeka)

(defcustom ezeka-file-name-format "%i {%l} %c %k"
  "The format-spec string for generating a numerus currens note's file name.
See `ezeka-format-metadata' for details.
This should match `ezeka-file-name-regexp'."
  :type 'string
  :group 'ezeka)

(defcustom ezeka-file-name-regexp
  (concat ezeka-regexp-link             ; \1 and \2
          "\\(?:"                       ; everything else is optional
          "\\(?:\\.\\)*"                               ; FIXME: optional historic period
          "\\(?: {\\(?3:[^}]+\\)}\\)*"                 ; \3
          "\\(?4:.+?\\)"                               ; \4
          "\\(?: \\(?5:[@&]\\S-+\\)\\)*$"              ; \5
          "\\)*"                        ; end of everything else
          )
  "Regexp matching numerus currens note file names.

Group 1 is the ID.
Group 2 is the kasten.
Group 3 is the label (genus or category).
Group 4 is the caption (i.e. short title).
Group 5 is the citation key.
Group 6 is the stable caption mark.")

(defcustom ezeka-regexp-header-rubric
  (concat "\\(?6:" ezeka-header-stable-caption-mark "\\)*"
          ezeka-file-name-regexp)
  "Regular expression for the rubric string as found in the header.

Groups 1-5 see `ezeka-file-name-regexp'.
Group 6 is the stable caption mark."
  :type 'regexp
  :group 'ezeka)

(defvar ezeka-regexp-genus "[α-ω]"
  "Regexp matching genus.")

(defun ezeka-format-metadata (format-string metadata)
  "Format a string out of format-string and METADATA.
The format control string may contain the following %-sequences:

%i means ID or link.
%l means label (genus or category).
%c means caption (i.e. short title).
%k means citation key.
%s means stable mark (see `ezeka-header-stable-caption-mark')."
  (string-trim
   (format-spec format-string
                `((?i . ,(alist-get :link metadata))
                  (?l . ,(alist-get :label metadata))
                  (?c . ,(alist-get :caption metadata))
                  (?k . ,(or (alist-get :citekey metadata) ""))
                  (?s . ,(if (alist-get :caption-stable metadata)
                             ezeka-header-stable-caption-mark
                           ""))))))

(defun ezeka-decode-rubric (rubric file)
  "Returns an alist of metadata from the RUBRIC in FILE. If cannot
decode, returns NIL."
  (when (and rubric (string-match ezeka-regexp-header-rubric rubric))
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
            (cons :caption (string-trim caption))
            (cons :caption-stable stable)
            (when citekey (cons :citekey (string-trim citekey)))))))

(defmacro ezeka-encode-rubric (metadata)
  "Returns a string that encodes the given METADATA into the rubric
according to `ezeka-header-rubric-format'."
  (ezeka-format-metadata ezeka-header-rubric-format metadata))

(defun ezeka--header-yamlify-key (keyword)
  "Returns a YAML-formatted string that is the name of the KEY, a keyword
symbol."
  (cl-subseq (symbol-name keyword) 1))

(defun ezeka--header-yamlify-value (value)
  "Returns a YAML-formatted string for the given metadata VALUE."
  (cl-typecase value
    (string value)
    (list (concat "[ " (mapconcat #'identity value ", ") " ]"))
    (t
     (error "Not implemented for type %s" (type-of value)))))

(defun ezeka--header-deyamlify-value (value)
  "Returns an elisp version of the given YAML-formatted VALUE."
  (pcase value
    ((rx bol "[" (let inside (1+ anychar)) "]" eol)
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
  "Returns an alist of metadata decoded from the given YAML header of
FILE. They keys are converted to keywords."
  (let* ((metadata
          (mapcar
           (lambda (line)
             (when (> (length line) 0)
               (if (string-match ezeka-regexp-header-line line)
                   (ezeka--decode-header-make-tuple
                    (intern (concat ":" (match-string 1 line)))
                    (ezeka--header-deyamlify-value (match-string 2 line)))
                 (unless noerror
                   (error "Malformed header line: '%s'" line)))))
           (split-string header "\n")))
         (decoded (ezeka-decode-rubric (alist-get :rubric metadata) file)))
    (append decoded metadata)))

(defun ezeka-file-metadata (file &optional noerror)
  "Returns an alist of metadata for the given FILE based on the most current
content of the FILE. They keys are converted to keywords."
  (if-let ((header (ezeka-file-content file t noerror)))
      (let* ((mdata  (ezeka--decode-header header file noerror))
             ;; Fill in any missing values for :ID, :TYPE, :KASTEN, and :LINK
             (id     (or (alist-get :id mdata)
                         (file-name-base file)))
             (type   (or (alist-get :type mdata)
                         (ezeka-id-type file)))
             (kasten (or (alist-get :kasten mdata)
                         (alist-get type ezeka-default-kasten)))
             (link   (or (ezeka-file-link file noerror)
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

;; See https://help.dropbox.com/organize/file-names
(defun ezeka--normalize-title-into-caption (title)
  "Returns TITLE after shortening it and stripping or replacing
troublesome characters for it to be used safely as file caption."
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
           ("?" "+")
           ("*" "+"))))
    (ezeka--replace-pairs-in-string replacements title)))

;; TODO: Extend to check for any tempus currens oldnames
(defun ezeka--set-time-of-creation (metadata)
  "Update the time of creation in the METADATA based on the time
encoded in tempus currens ID."
  (if (eq (alist-get :type metadata) :tempus)
      (let ((id (alist-get :id metadata)))
        (setf (alist-get :created metadata)
              (format-time-string "%Y-%m-%d %a %H:%M"
                                  (encode-time (iso8601-parse id))))
        metadata)
    metadata))

(defun ezeka-toggle-update-header-modified (arg)
  "Toggle between different value of `ezeka-update-header-modified'.
With \\[universal-argument], show a list of options instead."
  (interactive "P")
  (let ((new-value
         (if arg
             (intern (completing-read
                      "When to update modification dates: "
                      '("sameday" "never" "confirm")
                      nil
                      t))
           (if-let ((result (member ezeka-update-header-modified
                                    '(#1=sameday never confirm #1#))))
               (cadr result)
             (error "Invalid current `ezeka-update-header-modified': %s"
                    ezeka-update-header-modified)))))
    (setq ezeka-update-header-modified new-value)
    (unless arg
      (message "Set `ezeka-update-header-modified' to %s" new-value))))

(defun ezeka--maybe-update-modifed (metadata)
  "Updates the modification time in the METADATA according to the
value of `ezeka-update-modifaction-date', returning the new metadata."
  (let* ((today (format-time-string "%Y-%m-%d"))
         (now (format-time-string "%Y-%m-%d %a %H:%M"))
         (last-modified (or (alist-get :modified metadata)
                            (alist-get :created metadata))))
    (unless (string-equal (or last-modified "") now)
      ;; FIXME: Probably better to convert modification times to Emacs's encoded
      ;; time rather than doing it with strings.
      (when (or (equal ezeka-update-header-modified 'always)
                (and (equal ezeka-update-header-modified 'sameday)
                     (string= (cl-subseq last-modified 0 (length today)) today))
                ;; Automatic updating conditions not met; need to confirm
                (and (member ezeka-update-header-modified '(sameday confirm t))
                     (y-or-n-p
                      (format "%s was last modified at %s. Update to now? "
                              (alist-get :id metadata) last-modified))))
        (setf (alist-get :modified metadata) now)))
    metadata))

(defun ezeka-update-modified (file)
  "Update the modification time in the current Zettel file's header,
ignoring the value of `ezeka-update-header-modified'."
  (interactive (list buffer-file-name))
  (let ((ezeka-update-header-modified 'always))
    (ezeka--update-file-header file)))

;; There are three different places where files can be captioned or titled:
;; 1) The file name itself might contain a caption;
;; 2) The rubric in the file header should contain caption matching the
;;    caption in the file name; and, finally,
;; 3) The title in the file header contains a nicely formatted version
;;    of the caption that is used when inserting links.

(defun ezeka--reconcile-title-and-caption (metadata)
  "Interactively reconcile the title and caption in the given
METADATA. Returns modifed metadata."
  (let ((caption (or (alist-get :caption metadata) ""))
        (title (or (alist-get :title metadata) "")))
    (unless (or (string= title caption)
                (alist-get :caption-stable metadata))
      (let ((choice
             (read-char-choice
              (format (concat
                       "Caption in metadata: %s\n"
                       "  Title in metadata: %s\n"
                       "Press [c] or [u] to use caption for title\n"
                       "      [t] or [l] to use title for caption,\n"
                       "      [n] or [q] to do noting: ")
                      (propertize caption 'face 'bold)
                      (propertize title 'face 'italic))
              '(?c ?C ?u ?U ?t ?T ?l ?L ?n ?q))))
        (pcase choice
          ((or ?c ?u) (setf (alist-get :title metadata) caption))
          ((or ?t ?l) (setf (alist-get :caption metadata) title))
          ((or ?C ?U) (setf (alist-get :title metadata)
                            (ezeka--read-parallel-change caption caption)))
          ((or ?T ?L) (setf (alist-get :caption metadata)
                            (ezeka--read-parallel-change title title))))
        (setf (alist-get :caption-stable metadata) t)))
    (funcall clear-message-function)
    metadata))

(defun ezeka--update-file-header (&optional filename metadata inhibit-read-only)
  "Replaces the FILENAME's header with one generated from the given
METADATA or by parsing the FILENAME's existing header. If
INHIBIT-READ-ONLY is non-nil, write new header even if the buffer is
read only."
  (interactive (list buffer-file-name))
  (let* ((filename (or filename buffer-file-name))
         (metadata (or metadata (ezeka-file-metadata filename t)))
         (old-point (point))
         (inhibit-read-only inhibit-read-only))
    (save-mark-and-excursion
      (with-current-buffer (get-file-buffer filename)
        (save-restriction
          (goto-char (point-min))
          (when (re-search-forward ezeka-regexp-header-separator nil t 1)
            (narrow-to-region (point-min) (point)))
          (setq metadata
            (ezeka--set-time-of-creation
             (ezeka--maybe-update-modifed
              (ezeka--reconcile-title-and-caption metadata))))
          (setf (alist-get :rubric metadata)
                (ezeka-format-metadata ezeka-header-rubric-format metadata))
          (delete-region (point-min) (point-max))
          (mapc (lambda (cons)
                  (insert (format "%s: %s\n"
                                  (ezeka--header-yamlify-key (car cons))
                                  (ezeka--header-yamlify-value (cdr cons)))))
                (let (ordered)
                  (dolist (key '(:rubric
                                 :title :subtitle :author
                                 :created :modified
                                 :parent :firstborn :oldnames
                                 :readings :keywords)
                               (nreverse ordered))
                    (when (alist-get key metadata)
                      (push (cons key (alist-get key metadata)) ordered))))))))
    ;; `Save-excursion' doesn't seem to restore the point, possibly because the
    ;; file is changed, so need to do it manually.
    (goto-char old-point)))

(defun ezeka-normalize-file-name (&optional filename metadata)
  "Ensure that FILENAME's captioned name matches the METADATA."
  (interactive (list buffer-file-name))
  (let ((filename (or filename buffer-file-name)))
    (when (eq :numerus (or (alist-get :type metadata)
                           (and (ezeka-file-kasten filename t)
                                (ezeka-kasten-id-type
                                 (ezeka-file-kasten filename)))))
      (let* ((base (file-name-base filename))
             (mdata (if (null metadata)
                        (ezeka-file-metadata filename t)
                      (ezeka--update-file-header filename metadata)
                      metadata))
             (mname (ezeka-format-metadata ezeka-file-name-format mdata))
             (keep-which
              (unless (string= mname base)
                (downcase
                 (read-char-choice
                  (format (concat "Caption in filename and metadata differ:\n"
                                  "[F]ilename: %s\n"
                                  "[M]etadata: %s\n"
                                  "Press [f/u] to set metadata from filename,\n"
                                  "      [m/l] to set filename from metadata, or\n"
                                  "      [n] or [q] to do noting: ")
                          (propertize base 'face 'bold)
                          (propertize mname 'face 'bold-italic))
                  '(?f ?F ?u ?U ?m ?M ?l ?L ?n ?N ?q ?Q))))))
        (funcall clear-message-function)
        (cond ((or (string= mname base)
                   (member keep-which '(?n ?q)))
               ;; do nothing
               )
              ((member keep-which '(?f ?u))
               (setf (alist-get :label mdata) (ezeka-file-name-label base)
                     (alist-get :caption mdata) (ezeka-file-name-caption base)
                     (alist-get :citekey mdata) (ezeka-file-name-citekey base))
               (ezeka--update-file-header filename mdata)
               (ezeka--save-buffer-read-only filename))
              ((member keep-which '(?m ?l))
               (ezeka--rename-file
                filename (ezeka--read-parallel-change base mname))))))))

;;;=============================================================================
;;; Numerus Currens
;;;=============================================================================

(defun ezeka-make-numerus (number letters)
  "Returns a new numerus currens ID composed of the NUMBER and LETTERS,
both of which are strings."
  (concat number "-" letters))

(defun ezeka-numerus-number (id)
  "Returns the number part of the ID as a string."
  (when (string-match ezeka-regexp-numerus-currens id)
    (match-string 1 id)))

(defun ezeka-numerus-letters (id)
  "Returns the letters part of the ID as a string."
  (when (string-match ezeka-regexp-numerus-currens id)
    (match-string 3 id)))

(defun ezeka-numerus-parts (id)
  "Returns NIL if the ID is not a numerus currens id, and otherwise
returns a list of two elements: the number and letters parts of the id."
  (when (and (stringp id)
             (string-match ezeka-regexp-numerus-currens id))
    (list (match-string 1 id) (match-string 3 id))))

(defun abase26-letter-to-decimal (letter)
  "Returns the decimal number corresponding to the given character-as-string.
Case-insensitive."
  (if (string-match "[a-zA-Z]" letter)
      (- (string-to-char (downcase letter)) ?a)
    (error "LETTER must be a string of one letter")))

(defun abase26-decimal-to-letter (n)
  "Returns a string of the number in abase26 corresponding to the given
decimal."
  (if (< -1 n 26)
      (char-to-string (+ n ?a))
    (error "N must be an integer between 0 and 25")))

(defun abase26-encode (n &optional width)
  "Returns a string representation of the integer in the 'alphabetic' base
26. If WIDTH is given, returns the string at least WIDTH wide, padded with
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
  "Returns the integer for the given string representation in the
'alphabetic' base 26."
  (let ((n (1- (length string)))
        (total 0))
    (dolist (d (split-string string "" t))
      (setq total (+ total (* (abase26-letter-to-decimal d) (expt 26 n))))
      (setq n (1- n)))
    total))

(defun ezeka-generate-new-id (type)
  "Generates a random new ID of the given type."
  (cl-case type
    (:tempus (format-time-string "%Y%m%dT%H%M"))
    (:bolus  (format "%03d-%s"
                     (random 1000)
                     (abase26-encode (random (expt 26 3)) 3)))
    (:numerus (format "%s-%04d"
                      (abase26-encode (random 26))
                      (random 10000)))
    (:index   (format "%02d-%s"
                      (random 100)
                      (abase26-encode (random 26))))
    (t        (error "Unknown Zettel type"))))

(defun ezeka-next-unused-id (kasten)
  "Returns the next unused ID for the given KASTEN from `ezeka-kaesten'."
  (let ((type (ezeka-kasten-id-type kasten))
        id)
    (cl-flet ((exists? ()
                "Checks if ID is either NIL or exists."
                (or (null id)
                    (file-exists-p
                     (ezeka-link-file (ezeka-make-link kasten id))))))
      (cond ((eq type :tempus)
             (while (exists?)
               (setq id (ezeka-generate-new-id type))))
            ((and (eq type :numerus)
                  (file-exists-p (in-ezeka-dir ezeka-pregenerated-numeri-file)))
             (unwind-protect
                 (with-current-buffer
                     (find-file-noselect
                      (in-ezeka-dir ezeka-pregenerated-numeri-file))
                   (let ((left (count-lines (point-min) (point-max))))
                     (unwind-protect
                         (while (and (> left 0) (exists?))
                           (setq id
                             (string-trim
                              (delete-and-extract-region
                               (point-min)
                               (search-forward-regexp "[[:space:]]" nil t))))
                           (cl-decf left))
                       (let ((inhibit-message t))
                         (basic-save-buffer)))
                     (message "%d pregenerated numer%s left"
                              left
                              (if (= left 1) "us" "i"))))))
            (t
             (while (exists?)
               (setq id (ezeka-generate-new-id type))))))
    id))

;;;=============================================================================
;;; Tempus Currens
;;;=============================================================================

(defun ezeka-decode-time-into-tempus-currens (time)
  "Returns a tempus currens ID based on the given Emacs time object."
  (format-time-string "%Y%m%dT%H%M" time))

(defun ezeka-tempus-currens-id-for (link)
  "Returns a suitable tempus currens ID for the given Zettel link."
  (if (eq (ezeka-kasten-id-type (ezeka-link-kasten link)) :tempus)
      ;; If already tempus currens, just return that id
      (ezeka-link-id link)
    ;; Otherwise come up with an appropriate ID based on the metadata
    (let ((metadata (ezeka-file-metadata (ezeka-link-file link)))
          oldname)
      (cond ((setq oldname
               (cl-find-if
                (lambda (l)
                  (eq (ezeka-kasten-id-type (ezeka-link-kasten l))
                      :tempus))
                (alist-get :oldnames metadata)))
             ;; One of the old names was a tempus currens; just use that
             (ezeka-link-id oldname))
            ((alist-get :created metadata)
             (string-replace "T0000"    ; FIXME: A bit hacky?
                             (format-time-string "T%H%M")
                             (ezeka-decode-time-into-tempus-currens
                              (ezeka-encode-iso8601-datetime
                               (alist-get :created metadata)))))
            (t
             ;; Can't figure out automatically; ask the user
             (read-string "No created metadata; make up your own name: "
                          (ezeka-next-unused-id (ezeka-link-kasten link))))))))

;;;=============================================================================
;;; Zettel Links
;;;=============================================================================

(defun ezeka-link-at-point-p (&optional freeform)
  "Returns non-nil if the thing at point is a wiki link (i.e. [[XXX]]). The
first group is the link target. If FREEFORM is non-nil, also consider Zettel
links that are not enclosed in square brackets."
  (thing-at-point-looking-at
   (let ((regexp (ezeka--regexp-strip-named-groups ezeka-regexp-link)))
     (if freeform
         (concat "\\(?1:" regexp "\\)")
       (concat "\\[\\[\\(?1:" regexp "\\)\\]\\(\\[[^]]+\\]\\)*\\]")))))

(defun ezeka-link-at-point ()
  "Return the Zettel link at point. Needs to be called after
`ezeka-link-at-point-p'."
  (match-string-no-properties 1))

;; FIXME: Relies on ace-window
(defun ezeka-find-file (file &optional same-window)
  "Edit the given file based on the value of `ezeka-number-of-frames'.
If SAME-WINDOW is non-NIL, opens the buffer visiting the file in the
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
  "Attempts to find the given Zettel link based on the value of
`ezeka-number-of-frames'. If SAME-WINDOW is non-NIL, opens the link in
the same window. Returns T if the link is a Zettel link."
  (when (ezeka-link-p link)
    (let ((existing-file (ezeka-link-file link t 'wild)))
        (cond ((ezeka-note-p existing-file)
               (ezeka-find-file existing-file same-window))
              ((y-or-n-p "Link doesn't exist. Create? ")
               (let ((caption (plist-get (assoc-string link ezeka--new-child-plist)
                                         :caption)))
                 (ezeka-find-file (ezeka-link-file link t caption) same-window)
                 (call-interactively #'ezeka-insert-header-template)))
              (t
               (message "Link to non-existant note"))))))

(defun ezeka-kill-link-or-sexp-at-point (&optional arg)
  "If there is a Zettel link at point, kill it, including the square
brackets. Otherwise, call `kill-sex'."
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

(defun ezeka-org-format-link (target &optional description)
  "Returns a formatted org-link to TARGET, which can be either a link or a filepath."
  (let* ((link (if (file-name-absolute-p target)
                   (ezeka-file-link target)
                 target)))
    (format "[[%s]%s]"
            link
            (if description
                (format "[%s]" description) ""))))

(defun ezeka--link-with-metadata (link &optional field where metadata)
  "Returns a string containing the metadata FIELD (:title by default)
at place WHERE (:before by default) in relation to the LINK."
  (let* ((mdata (or metadata (ezeka-file-metadata (ezeka-link-file link))))
         (field (or field :title))
         (where (or where :before))
         (value (alist-get field mdata)))
    (concat (if (eq where :before)
                (concat value " ")
              "")
            (ezeka-org-format-link
             link
             (when (eq where :description)
               value))
            (if (eq where :after)
                (concat " " value)
              ""))))

(defun ezeka-insert-link-with-metadata (link &optional field where confirm)
  "Inserts the Zettel link, optionally adding a metadata FIELD put
WHERE (:BEFORE, :AFTER, or in :DESCRIPTION). If CONFIRM is non-NIL,
ask for confirmation before inserting metadata."
  (let* ((field (or field
                    (when (called-interactively-p 'any)
                      (intern-soft
                       (completing-read
                        "Which metadata field? "
                        '(":none" ":title" ":citekey" ":label"))))))
         (where (or where
                    (when field
                      (intern-soft
                       (completing-read "Where? "
                                        '(":before" ":after")))))))
    (let ((mdata (ezeka-file-metadata (ezeka-link-file link))))
      (insert (if (or (bolp) (space-or-punct-p (char-before))) "" " ")
              (if (or (not confirm)
                      (progn
                        ;; Pressing return just defaults to NO rather than quit
                        (define-key query-replace-map [return] 'act)
                        (y-or-n-p (format (if (eq where :description)
                                              "Insert [%s] in the link %s? "
                                            "Insert [%s] %s the link? ")
                                          (alist-get field mdata) where))))
                  (ezeka--link-with-metadata link field where mdata)
                (ezeka-org-format-link link))
              (if (or (eolp) (space-or-punct-p (char-after))) "" " ")))))

(defun ezeka-insert-link-to-visiting (arg)
  "Inserts a link to another Zettel being currently visited. With
\\[universal-argument] offers a few options for including Zettel
metadata. If the user selects a Zettel that does not exist in the
list, just insert the link to what was selected. If the cursor in
already inside a link, replace it instead."
  (interactive "P")
  (let* ((table (ezeka-completion-table
                 (ezeka-visiting-buffer-list t arg)))
         (link (when table
                 (ezeka-file-link
                  (cdr (assoc-string
                        (completing-read "Insert link to: " table nil t) table))))))
    (if link
        (if (not (ezeka-link-at-point-p))
            (if arg
                (funcall-interactively #'ezeka-insert-link-with-metadata link)
              (ezeka-insert-link-with-metadata link :title :before t))
          ;; When replacing, don't including anything
          (delete-region (match-beginning 0) (match-end 0))
          (insert (ezeka-org-format-link link)))
      (message "No visiting Zettel"))))

(defun ezeka-insert-link-to-other-window (arg)
  "Like `ezeka-insert-link' but adds the link to file in the other
window. With \\[universal-argument], insert just the link."
  (interactive "P")
  (let ((other-buf (window-buffer (other-window-for-scrolling))))
    (when-let* ((file (or (buffer-file-name other-buf)
                          (with-current-buffer other-buf
                            (ezeka--grab-dwim-file-target))))
                (link (ezeka-file-link file)))
      (if arg
          (ezeka-insert-link-with-metadata link)
        (ezeka-insert-link-with-metadata link :title :before t)))))

(defun ezeka-insert-link-to-bookmark (arg)
  "Inserts a link to a bookmark. With \\[universal-argument] offers a
few options for including Zettel metadata. If the user selects a
Zettel that does not exist in the list, just insert the link to what
was selected. If the cursor in already inside a link, replace it
instead."
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
              (ezeka-insert-link-with-metadata link :title :before t))
          ;; When replacing, don't including anything
          (delete-region (match-beginning 0) (match-end 0))
          (insert (ezeka-org-format-link link)))
      (message "No visiting Zettel"))))

(defun ezeka-insert-link-from-clipboard (arg)
  "Like `ezeka-insert-link' but attempts to get the link ID from OS
clipboard, inserting it with metadata. With prefix argument, insert just the
link itself."
  (interactive "P")
  (let ((link (gui-get-selection 'CLIPBOARD))
        (backlink (when buffer-file-name
                    (ezeka-file-link buffer-file-name))))
    (when (ezeka-link-p link)
      (if arg
          (ezeka-insert-link-with-metadata link)
        (ezeka-insert-link-with-metadata link :title :before t))
      (when backlink
        (gui-set-selection 'CLIPBOARD backlink)
        (message "Backlink to %s copied to clipboard" backlink)))))

(defun ezeka-kill-ring-save-link-title (arg)
  "Save the title to the kill ring and system clipboard of either the
Zettel link at point or, if there is none, the current buffer. With
\\[universal-argument], also includes the link."
  (interactive "P")
  (let* ((file (ezeka--grab-dwim-file-target))
         (link (ezeka-file-link file)))
    (when file
      (let* ((mdata (ezeka-file-metadata file))
             (title (if arg
                        (ezeka--link-with-metadata link :title :before mdata)
                      (ezeka-format-metadata ezeka-header-rubric-format mdata)
                      (alist-get :title mdata))))
        (kill-new title)
        (unless select-enable-clipboard
          (gui-set-selection 'CLIPBOARD title))
        (message "Saved [%s] in the kill ring" title)))))

(defun ezeka-kill-ring-save-link (arg)
  "Save the Zettel link at point or the buffer base filename in the kill ring
to be used as a wiki link elsewhere. With prefix argument, save the file name
relative to `ezeka-directory' instead. With two prefix arguments, open the
file in Finder with it selected."
  (interactive "p")
  (let ((file (ezeka--grab-dwim-file-target t)))
    (when file
      (let ((link (if (= arg 4)
                      (file-relative-name file ezeka-directory)
                    (ezeka-file-link file))))
        (if select-enable-clipboard
            (kill-new link)
          (gui-set-selection 'CLIPBOARD link))
        (message "Saved [%s] in the kill ring" link)
        (when (= arg 16)
          (shell-command (format "open -R %s &" file)))))))

(defun ezeka-kill-ring-save-next-link ()
  "Save the first link at or after point (but before EOL)."
  (interactive)
  (save-excursion
    (let ((link (if (ezeka-link-at-point-p)
                    (ezeka-link-at-point)
                  (let ((eol (save-excursion (end-of-visual-line) (point))))
                    (when (re-search-forward ezeka-regexp-link eol t)
                      (match-string-no-properties 0))))))
      (when link
        (kill-new link)
        (message "Saved [%s] to kill ring" link)))))

(defun ezeka-links-to (link)
  "Runs a recursive grep (`rgrep') to find references to the link at point or
to current Zettel."
  (interactive (list (ezeka-file-link (ezeka--grab-dwim-file-target t))))
  (grep-compute-defaults)
  (rgrep link "*.txt" (f-slash ezeka-directory) nil))

(defun ezeka-rgrep (string)
  "Runs a recursive grep (`rgrep') for the given STRING across all Zettel."
  (interactive "sSearch for what? ")
  (grep-compute-defaults)
  (rgrep (string-replace " " ".*" string)
         "*.txt" (f-slash ezeka-directory) nil))

;; To show the beginning of Zettel title in the mode-line,
;; add the following to the user configuration:
;;
;; (add-hook 'ezeka-mode-hook 'ezeka-show-title-in-mode-line)
(defun ezeka-show-title-in-mode-line ()
  "Change `mode-line-misc-info' to show the Zettel note's title from
metadata."
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
                 (point-at-eol))
                buffer-file-name)))
          (when metadata
            (let ((words (split-string (alist-get :title metadata))))
              (setq-local mode-line-misc-info
                          (replace-regexp-in-string
                           "/" "" (mapconcat #'identity
                                    (cl-subseq words 0 (min 5 (length words)))
                                    " "))))))))))

(defun ezeka-update-link-prefix-title (arg)
  "Kills text from point to the next Zettel link, replacing it with
that Zettel's title. With \\[universal-argument] kill text from point
to the next link."
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
      (org-next-link)
      (when (ezeka-link-at-point-p)
        (let* ((file (ezeka-link-file (ezeka-link-at-point)))
               (title (alist-get :title (ezeka-file-metadata file))))
          (delete-region start (1- (point)))
          (unless arg
            (backward-char)
            (insert title)))))))

;;;=============================================================================
;;; Genealogical
;;;=============================================================================

(defun ezeka-trace-genealogy (file-or-link &optional degree)
  "Returns the FILE-OR-LINK's next genealogical link, or NIL if could not
figure out. With the optional DEGREE, try to find the Nth link (i.e.
grandparent if DEGREE is 2, child if DEGREE is -1, an so on), returning the
most remote link that could be found."
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

(defun ezeka-find-ancestor (n)
  "Opens the current Zettel's immediate ancestor. With a prefix argument, try
to find the Nth ancestor."
  (interactive "p")
  (when (ezeka-note-p buffer-file-name)
    (let ((ancestor (ezeka-trace-genealogy buffer-file-name n)))
      (if ancestor
          (ezeka-find-link ancestor)
        (message "No ancestor found")))))

(defun ezeka-find-descendant (n)
  "Opens the current Zettel's immediate descendant. With a prefix argument,
try to find the Nth ancestor."
  (interactive "p")
  (when (ezeka-note-p buffer-file-name)
    (let ((descendant (ezeka-trace-genealogy buffer-file-name (- n))))
      (if descendant
          (ezeka-find-link descendant)
        (message "No descendant found")))))

(defun ezeka-insert-ancestor-link (arg)
  "Insert a link to the ancestor of the current Zettel, adding its title (if
available) before the link. With a numerical prefix argument, try to find Nth
ancestor. With a universal argument, ask for confirmation before inserting."
  (interactive "P")
  (let* ((degree (if (integerp arg) arg 1))
         (link (ezeka-trace-genealogy buffer-file-name degree)))
    (if link
        (ezeka-insert-link-with-metadata link :title :before (not arg))
      (message "Could not find such ancestor"))))

(defvar ezeka--new-child-plist nil
  "An alist of new children and a plist of their details (:parent,
:title, :label, etc.).")

(defun ezeka-insert-new-child (parent &optional arg noselect)
  "Inserts a link to a new Zettel in the same Kasten as the PARENT,
which can be a file or a link. With \\[universal-argument] allows the
user to select a different Kasten. With double \\[universal-argument]
asks for the full link. If NOSELECT is non-nil, don't visit the
created child. Returns link to the new child."
  (interactive
   (list (when (ezeka-note-p buffer-file-name t) buffer-file-name)
         current-prefix-arg))
  (let ((parent-link (if (or (null parent)
                             (ezeka-link-p parent))
                         parent
                       (ezeka-file-link parent)))
        child-link)
    (if (equal arg '(16))
        (while (not child-link)
          (setq child-link (read-string "Enter link for new child: "))
          (when (file-exists-p (ezeka-link-file child-link))
            (message "This Zettel already exists; try again")))
      (let ((kasten (if (or (equal arg '(4))
                            (null parent-link))
                        (completing-read
                         "Kasten: "
                         (if (listp ezeka-kaesten)
                             (mapcar #'car ezeka-kaesten)
                           (error "No `ezeka-kaesten' defined")))
                      (ezeka-link-kasten parent-link))))
        (setq child-link
          (ezeka-make-link kasten (ezeka-next-unused-id kasten)))
        (when parent-link
          (add-to-list 'ezeka--new-child-plist
            (list child-link :parent parent-link)))))
    ;; Don't worry if insert fails
    (ignore-errors
      (insert (ezeka-org-format-link child-link)))
    (unless noselect
      (ezeka-find-link child-link))
    child-link))

(defun ezeka--possible-new-note-title ()
  "Returns a possible title for a new Zettel note based on context."
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

(defun ezeka-insert-new-child-with-title (arg title)
  "Wrapper around `ezeka-insert-new-child' that creates a new child
with given TITLE (defaults to text before point). Passes the prefix
argument to `ezeka-insert-new-child'."
  (interactive (list current-prefix-arg
                     (read-from-minibuffer "Title for the child: "
                                           (ezeka--possible-new-note-title))))
  (let* ((parent-link (ezeka-file-link buffer-file-name))
         (citekey (alist-get :citekey
                    (ezeka-file-metadata buffer-file-name)))
         (title (org-trim (or title (ezeka--possible-new-note-title))))
         (child-link (ezeka-insert-new-child parent-link arg t))
         (plist (cdr (assoc-string child-link ezeka--new-child-plist))))
    (setf (alist-get child-link ezeka--new-child-plist nil nil #'string=)
          (plist-put (plist-put plist :citekey citekey)
                     :title title))
    (ezeka-find-link child-link)))

;;;=============================================================================
;;; Buffers and Frames
;;;=============================================================================

(defun ezeka-visiting-buffer-list (&optional skip-current modified-only)
  "Returns a list of Zettel files that are currently being visited. If
SKIP-CURRENT is non-nil, remove the current buffer. If MODIFIED-ONLY
is non-nil, only list modified buffers."
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
  "Allows the user to easily kill Zettel that are being currently
visited. With \\[universal-argument], kill all visiting Zettel."
  (interactive "P")
  (let (;; Disabling sorting preserves the same order as with `switch-to-buffer'
        ;; FIXME: How to do this without relying on vertico?
        (vertico-sort-function nil))
    (if arg
        (mapc (lambda (file)
                (kill-buffer (get-file-buffer file)))
              (ezeka-visiting-buffer-list t))
      (while t
        (let ((table
               (ezeka-completion-table
                (ezeka-visiting-buffer-list t))))
          (kill-buffer
           (get-file-buffer
            (cdr (assoc-string (completing-read "Visit buffer: " table nil t)
                               table)))))))))

(defun ezeka-formatted-frame-title ()
  "Returns a string suitable for `frame-title-format' as a way to
consistently format the frame title with useful information for
Zettelkasten work."
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
                (metadata (ezeka-file-metadata (ezeka-link-file link t) t)))
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
              (metadata (ezeka-file-metadata (ezeka-link-file link t) t)))
      (setq mode-line-misc-info
        (propertize
         (format "%s%s%s" (alist-get :title metadata)
                 (if (alist-get :citekey metadata) " " "")
                 (or (alist-get :citekey metadata) ""))
         'face '(:slant italic :height 0.9)))
      (setq mode-line-misc-info zk-index-mode-line-orig))))

(defun ezeka-completion-table (files)
  "Given a list of Zettel files, returns a nicely formatted list of choices
suitable for passing to `completing-read' as collection."
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

(defun ezeka-switch-to-buffer (arg)
  "Quickly switch to other open Zettel buffers. With
\\[universal-argument], show only modified buffers. With double
\\[universal-argument], open buffer in other window."
  (interactive "P")
  (let ((table (ezeka-completion-table
                (nreverse (ezeka-visiting-buffer-list t (equal arg '(4))))))
        ;; Disabling sorting preserves the same order as with `switch-to-buffer'
        ;; FIXME: How to do this without relying on vertico?
        (vertico-sort-function nil))
    (funcall (if (equal arg '(16)) 'find-file-other-window 'find-file)
             (if table
                 (cdr (assoc-string
                       (completing-read "Visit buffer: " table nil t) table))
               (switch-to-buffer
                (read-buffer-to-switch
                 "No opened Zettel. Switch to regular buffer: "))))))

;;;=============================================================================
;;; Labels
;;
;; A label is either a genus (for numerus currens notes) or category (for tempus
;; currens notes). By default, it is the value shown between curly brackets
;; {...} in the note's rubric.
;;;=============================================================================

(defun ezeka--read-category (&optional prompt custom sort-fn)
  "Uses `completing-read' to select a category from
`ezeka-categories'. If CUSTOM is non-nil, asks the user to type in the
category directly. If SORT-FN is given, use that to sort the list
first."
  (let ((prompt (or prompt "Category: "))
        (categories (if (not (functionp sort-fn))
                        ezeka-categories
                      (let ((cats-copy (cl-copy-list ezeka-categories)))
                        (cl-sort cats-copy sort-fn)))))
    (if custom
        (read-string prompt)
      (completing-read prompt categories))))

(defun ezeka--read-genus (&optional prompt verbose default)
  "Read a genus, a single Latin character as defined in
`ezeka-genera'. Returns a string containing the genus letter. If
VERBOSE is non-nil, show a list of choices with explantions. DEFAULT
is the genus used if user just presses [return]."
  (let (item)
    (while (null item)
      (let ((result
             (if verbose
                 (completing-read (or prompt "Genus: ") ezeka-genera nil t)
               (read-char
                (concat (or prompt "Genus")
                        " (Latin character, or RETURN for \"" (or default "x")
                        "\"): ")))))
        (cond ((and (characterp result) (eq result ?\C-m))
               (setq result (car
                             (cl-rassoc default ezeka-genera
                                        :key #'car :test #'string=))))
              ((characterp result)
               (setq result (char-to-string result)))
              ((stringp result))
              (t (signal 'wrong-type-argument '(or character string))))
        (unless (setq item (assoc-string result ezeka-genera))
          (setq prompt "No such genus; try again. "))))
    (cadr item)))

(defun ezeka--read-label (file-or-link &optional arg prompt default)
  "Interactively read the label for the given FILE-OR-LINK. Passes ARG
to the appropriate function."
  (if (eq :numerus (ezeka-id-type file-or-link))
      (ezeka--read-genus prompt arg default)
    (ezeka--read-category prompt arg)))

(defun ezeka--update-metadata-values (filename metadata &rest args)
  "Update FILENAME's header, replacing metadata values with new ones,
and saves the file while ignoring its read only status. If METADATA is
not given, read it from file first.

\(fn FILENAME METADATA &REST KEY VAL KEY VAL ...)"
  (when (/= (logand (length args) 1) 0)
    (signal 'wrong-number-of-arguments (list 'setf (length args))))
  (let ((already-open (get-file-buffer filename)))
    (save-excursion
      (unwind-protect
          (with-current-buffer (or already-open (find-file-noselect filename))
            (let ((already-modified (buffer-modified-p))
                  (metadata (or metadata (ezeka-file-metadata filename)))
                  sets)
              (while args
                (setf (alist-get (pop args) metadata) (pop args)))
              (ezeka--update-file-header filename metadata t)
              (when (and (not already-modified)
                         (if (eq ezeka-save-after-metadata-updates 'confirm)
                             (y-or-n-p "Save? ")
                           ezeka-save-after-metadata-updates))
                (save-buffer))))
        (let ((buf (get-file-buffer filename)))
          (unless already-open
            (kill-buffer-if-not-modified buf)))))))

;; TODO: Also ask about updating the filename
(defun ezeka-set-title (&optional filename new-title)
  "Updates the title in FILENAME's header to NEW-TITLE."
  (interactive)
  (let ((filename (or filename (buffer-file-name))))
    (when (ezeka-note-p filename)
      (let ((metadata (ezeka-file-metadata filename)))
        (setf (alist-get :title metadata)
              (read-string "Change title to what? "
                           (alist-get :title metadata))
              (alist-get :caption-stable metadata)
              nil)
        (ezeka--update-metadata-values filename metadata)))))

(defun ezeka-set-label (filename label arg)
  "Set the appropriate label (genus or category) in the Zettel note
with given FILENAME (defaults to `buffer-file-name'). With
\\[universal-argument], either show genera verbosely or type custom
category."
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
  "Set the CITEKEY in the Zettel note in FILENAME. If CITEKEY is not
given, get it from the parent unless it's \\[universal-argument], in
which case let the user enter the citekey no matter what. With DEGREE,
traces genealogy further than parent."
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
                        (read-string "New citekey: ")))
           (citekey (when (and citekey (not (string-empty-p citekey)))
                      (string-trim-left citekey "@"))))
      (ezeka--update-metadata-values filename nil
                                     :citekey (when citekey
                                                (concat "@" citekey))))))

(defun ezeka-add-keyword (filename keyword &optional arg)
  "Adds the given KEYWORD to the Zettel note in FILE. With
\\[universal-argument], clear keywords first."
  (interactive (list (ezeka--grab-dwim-file-target)
                     (completing-read "Add keyword: " (cons "#" ezeka-keywords) nil nil "#")
                     current-prefix-arg))
  (if (not (ezeka-note-p filename))
      (error "Not a Zettel note")
    (let* ((metadata (ezeka-file-metadata filename))
           (keywords (unless (equal arg '(4))
                       (alist-get :keywords metadata))))
      (unless (or (string-empty-p keyword)
                  (string= keyword "#"))
        (add-to-list 'keywords keyword t))
      (ezeka--update-metadata-values filename metadata :keywords keywords))))

;;;=============================================================================
;;; Populating Files
;;;=============================================================================

(defun ezeka-insert-header-template (&optional link label title parent citekey)
  "Inserts the header template into the current buffer, populating the
header with the LINK, LABEL, TITLE, PARENT, and CITEKEY."
  (interactive
   (let* ((link (if buffer-file-name
                    (ezeka-file-link buffer-file-name)
                  ;; FIXME: Rewrite with completing-read
                  (read-string "Insert template into note with this link: ")))
          (plist (cdr (assoc link ezeka--new-child-plist))))
     (list
      link
      (ezeka--read-label link)
      (read-string "Title: " (plist-get plist :title))
      (read-string "Parent? " (plist-get plist :parent))
      (read-string "Citekey? " (plist-get plist :citekey)))))
  (let* ((file (ezeka-link-file link))
         (caption (ezeka--normalize-title-into-caption title)))
    (goto-char (point-min))
    (insert
     (concat ezeka-header-rubric-key
             ": "
             (ezeka-format-metadata ezeka-header-rubric-format
                                    `((:link . ,link)
                                      (:caption . ,caption)
                                      (:label . ,label)
                                      (:citekey . ,citekey)))))
    (insert "\ntitle: " title)
    (insert "\ncreated: "
            ;; Insert creation time, making it match a tempus currens filename
            (format-time-string
             "%Y-%m-%d %a %H:%M"
             (let ((today (format-time-string "%Y%m%d")))
               (if (and (eq :tempus (ezeka-id-type file))
                        (not (string-match-p (regexp-quote today) file))
                        (not
                         (when (called-interactively-p 'any)
                           (y-or-n-p "Past tempus currens; set created time to now? "))))
                   (ezeka-encode-iso8601-datetime file)
                 nil)))
            "\n")                     ; i.e. current time
    (when (and parent (not (string-empty-p parent)))
      (insert "parent: " parent "\n"))
    (insert "\n")))

(defun ezeka-incorporate-file (file kasten &optional arg)
  "Moves the file in the current buffer to the appropriate Zettelkasten. With
prefix argument, asks for a different name."
  (interactive (list (buffer-file-name)
                     (completing-read "Zettel kasten: " ezeka-kaesten)
                     current-prefix-arg))
  (rename-file-and-buffer
   (if (not arg)
       (ezeka-link-file
        (ezeka-make-link kasten (file-name-base file)))
     (call-interactively #'rename-file-and-buffer))))

;;;=============================================================================
;;; Org-Mode Intergration
;;;=============================================================================

(defun ezeka-org-set-todo-properties ()
  "Set the FROM, CREATED, and ID properties for the current heading to
facilitate refiling."
  (interactive)
  (org-set-property "ID" (org-id-get-create))
  (org-set-property "FROM" (ezeka-insert-link-with-metadata
                            buffer-file-name :title :after))
  (org-set-property "CREATED"
                    ;; FIXME: Surely there is a better function to do this, no?
                    (format-time-string
                     (format "[%s]"
                             (cl-subseq (cdr org-time-stamp-formats) 1 -1)))))

(defun ezeka-org-interactive-tempus ()
  "Inserts a tempus currens link after having the user select the date using
org-mode's interactive `org-time-stamp' command."
  (interactive)
  (let ((datetime
         (with-temp-buffer
           (let ((start (point)))
             (org-time-stamp '(4) t)
             (org-timestamp-format (org-timestamp-from-string
                                    (delete-and-extract-region start (point)))
                                   "%Y%m%dT%H%M")))))
    (insert (ezeka-org-format-link datetime))))

(defun ezeka-dwim-with-this-timestring (beg end)
  "Do What I Mean with the timestring in the region. If the timestring is
IS8601, make it into an org-time-stamp, and vice-versa. If it's something
else, try to make it into org-time-stamp."
  (interactive
   (cond ((org-at-timestamp-p t)
          (list (match-beginning 0) (match-end 0)))
         ((or (thing-at-point-looking-at ezeka-regexp-iso8601-datetime)
              (thing-at-point-looking-at ezeka-regexp-iso8601-date))
          (list (match-beginning 0) (match-end 0)))
         ((region-active-p)
          (list (region-beginning) (region-end)))
         ((user-error "Region not active"))))
  (let ((text (buffer-substring-no-properties beg end))
        timestamp)
    ;; if the region was surrounded by parentheses, remove those
    (save-excursion
      (goto-char (1- beg))
      (when (re-search-forward (format "(%s)" text) (point-at-eol) t)
        (replace-match text)
        (setq beg (- beg 1)
              end (- end 1))))
    (cond ((iso8601-valid-p text)       ; ISO-8601 -> Org timestamp
           (let ((parsed (iso8601-parse text)))
             (delete-region beg end)
             (org-insert-time-stamp (iso8601--encode-time parsed)
                                    (integerp (car parsed)) t)))
          ((setq timestamp              ; org timestamp -> ISO-8601
             (org-timestamp-from-string (if (string-match-p "[[<].*[]>]" text)
                                            text
                                          (format "[%s]" text))))
           (delete-region beg end)
           (insert
            (org-timestamp-format timestamp "[[%Y%m%dT%H%M]]")))
          ((integerp (car (parse-time-string text))) ; otherwise -> org timestamp
           (delete-region beg end)
           (org-insert-time-stamp (encode-time (parse-time-string text)) t t))
          (t
           (signal 'wrong-type-argument (list text))))))

(defun ezeka-org-export-as-new-note (&optional kasten)
  "Creates a new Zettel in the current Kasten (with prefix argument,
ask for the Kasten) from the current org subtree."
  (interactive (list (when current-prefix-arg
                       (completing-read "Zettel kasten: " ezeka-kaesten))))
  (let ((parent-file buffer-file-name)
        (parent-link (ezeka-file-link buffer-file-name))
        (kasten (or kasten (ezeka-file-kasten buffer-file-name)))
        tempus-currens
        new-title
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
                 (setq timestamp (save-match-data (org-timestamp-from-string
                                                   (match-string 2 title)))
                       new-title (match-string 1 title)))
                ((org-get-scheduled-time nil)
                 (setq timestamp
                   (org-timestamp-from-time (org-get-scheduled-time nil) t)))
                (t
                 (error "Could not get the timestamp for new Zettel")))
          (setq tempus-currens (if (org-timestamp-has-time-p timestamp)
                                   (org-timestamp-format timestamp "%Y%m%dT%H%M")
                                 (concat (org-timestamp-format timestamp "%Y%m%dT")
                                         (format-time-string "%H%M")))
                new-link (ezeka-make-link kasten tempus-currens)
                new-file (ezeka-link-file new-link))
          (let* ((content (org-get-entry)))
            (if (file-exists-p new-file)
                (message "Aborting, file already exists: %s" new-file)
              ;; New file buffer
              (with-current-buffer (get-buffer-create new-file)
                (ezeka-insert-header-template new-link
                                              (ezeka--read-label new-file)
                                              new-title parent-link)
                (insert "\n" content)
                (basic-save-buffer))
              ;; Back in original buffer
              (with-current-buffer (get-file-buffer (file-truename parent-file))
                (org-cut-subtree)
                (insert (ezeka-org-format-link new-link)
                        " "
                        (alist-get :title (ezeka-file-metadata new-file)))))))))))

(defun ezeka-open-link-at-point (&optional arg)
  "Open a Zettel link at point even if it's not formatted as a link. With a
prefix argument, ignore `ezeka-number-of-frames' and open the link in the
same window."
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
  "Open a Zettel link at mouse point."
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
  :type 'boolean)

(defcustom ezeka-insert-snippet-footnotes nil
  "Non-nil means insert footnotes."
  :type 'boolean)

;;; TODO:
;;; - implement some kind of checksum check for keeping draft up to date
;;; - if region is active, narrow to it rather than to subtree (allows # lines!)
;;; - don't copy subtrees marked with COMMENT
;;; - update the snippet title in the heading while I'm at it
;;; - command to edit the current heading in situ and locate same text point
;;; - quickly scan through all the headings and see if any need updating?
;;; - add marker that I'm including text from the Zettel; define a new org block?
(defun ezeka-insert-snippet-text (arg file)
  "Inserts the combination of Summary and Snippet sections from the given
snippet FILE into the current buffer. With prefix argument, forces update."
  (interactive
   ;; Assume the file is the last link on the current line
   (list current-prefix-arg
         (save-excursion
           (end-of-line)
           (org-previous-link)
           (when (ezeka-link-at-point-p)
             (ezeka-link-file (ezeka-link-at-point))))))
  ;; Get the metadata and most recent modification
  (save-excursion
    (save-restriction
      (let* ((metadata (ezeka-file-metadata file))
             (modified (format "[%s]" (or (alist-get :modified metadata)
                                          (alist-get :created metadata))))
             current?)
        ;; Update the timestamp if modification time is more recent
        (end-of-line)
        (if (org-at-timestamp-p 'inactive)
            (if (string= (org-element-property :raw-value (org-element-context))
                         modified)
                (setq current? t)       ; we still have most recent text
              ;; Need to repeat `org-at-timestamp-p' for match data
              (when (org-at-timestamp-p 'inactive)
                (replace-match modified)))
          (just-one-space)
          (insert modified))
        (if (and current? (null arg))
            (message "Snippet is up to date; leaving alone")
          (when (y-or-n-p "Update the text? ")
            ;; If current line is a comment, create a heading after it
            (when (org-at-comment-p)
              (org-insert-subheading nil))
            ;; Delete existing text
            (org-narrow-to-subtree)
            (forward-line 1)
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
                  (let ((copy-from (point)))
                    (org-end-of-subtree)
                    (mapcar (lambda (line)
                              (push (concat "# " line "\n") content))
                            (split-string
                             (buffer-substring-no-properties copy-from (point))
                             "\n" t))))
                (goto-char (or (org-find-exact-headline-in-buffer "Snippet")
                               (org-find-exact-headline-in-buffer "Content")
                               (error "Can't find the Snippet or Content section")))
                (let ((copy-from (point)))
                  (org-end-of-subtree)
                  (push (buffer-substring copy-from (point)) content)))
              ;; Insert the copied subtrees and remove its headings and comments
              (insert "\n")
              (apply #'insert (nreverse content))
              (goto-char start)
              (while (re-search-forward "^[*]+ " nil t) ; remove headings
                (goto-char (match-beginning 0))
                (kill-line 1))
              ;; Remove my notes in {...} and Zettel links
              (goto-char start)
              (while (re-search-forward (rx (optional blank)
                                            (group
                                             (or (and "[[" (+? (not space)) "]]")
                                                 (and "{" (+? anything) "}"))))
                                        nil t)
                (when (eq (elt (match-string 1) 0) ?{)
                  (incf comments-removed))
                (replace-match ""))
              ;; Remove footnotes if need be
              (unless ezeka-insert-snippet-footnotes
                (goto-char start)
                (while (re-search-forward "^\\[fn:.+?\\].*?$" nil t)
                  (goto-char (match-beginning 0))
                  (kill-paragraph 1)
                  (incf footnotes-removed)))
              (org-indent-region (point-min) (point-max))
              (goto-char (point-max))
              (insert "\n")
              (rb-collapse-blank-lines)
              (message "Removed %d comments and %d footnotes"
                       comments-removed footnotes-removed)
              t)))))))

(defun ezeka-find-inserted-snippet ()
  "While the point is within the org entry, find the source of the snippet
inserted through `ezeka-insert-snippet-text'."
  (interactive)
  (save-excursion
    (org-back-to-heading t)
    (org-next-link)
    (org-open-at-point)))

(defun ezeka-transclude-snippet (link)
  "Inserts the transclusion statement from given snippet LINKE into the
current buffer."
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
  "This is a wrapper around `org-footnote-action' to be used in the
transcluded snippets, making sure that the footnotes are placed
locally rather in whatever `org-footnote-section' is set to. Footnotes
are placed locally if the current heading matches
`ezeka-snippet-heading' or if the command was called with
\\[universal-argument]. With double \\[universal-argument], offer
additional options."
  (interactive "P")
  (let ((snippet? (string= ezeka-snippet-heading
                          (save-excursion
                            (org-back-to-heading-or-point-min t)
                            (and (org-context)
                                 (nth 4 (org-heading-components)))))))
    (if (or snippet? arg)
        (let ((org-footnote-section nil)
              (context (org-context)))
          (org-element-cache-reset)
          ;; taken from `org-footnote-action'
          (if (not (and context
                        (> (point)
	                       (save-excursion
                             ;; FIXME: Broken, since :END property is nil?
		                     (goto-char (org-element-property :end context))
		                     (skip-chars-backward " \t")
		                     (point)))))
              (org-footnote-action (equal arg '(16)))
            (kill-new (format-time-string "%H%M"))
            (org-footnote-new)))
      (org-footnote-action (equal arg '(16))))))

;;;=============================================================================
;;; Bookmark Integration
;;;=============================================================================

(defun bookmark-make-record-ezeka ()
  "Bookmark record function for Zettel bookmarks, setting the
bookmark's filename property to the Zettel link."
  (list (cons 'filename (ezeka-file-link buffer-file-name))
        (cons 'handler 'bookmark-ezeka-handler)))

(defun bookmark-ezeka-handler (bmk-record)
  "Bookmark record handler for Zettel bookmarks."
  (find-file (ezeka-link-file (cdr (assoc 'filename bmk-record)))))

;; Use the special ezeka bookmark handler in Zettel buffers
(add-hook 'ezeka-mode-hook
  (lambda ()
    (setq-local bookmark-make-record-function 'bookmark-make-record-ezeka)))

;;;=============================================================================
;;; Maintenance
;;;=============================================================================

(defvar ezeka--move-log-file "auto/zmove.log"
  "Path, relative to `ezeka-directory', to the log file for recording
  moves.")

(defun ezeka--add-to-move-log (link1 link2)
  "Log the move from LINK1 to LINK2 in `ezeka--move-log-file'."
  (write-region (format "\n%S" (list link1 link2 (format-time-string "%FT%T")))
                nil
                (expand-file-name ezeka--move-log-file ezeka-directory)
                t))

(defun ezeka--move-note (link1 link2 &optional confirm)
  "Move Zettel note from LINK1 to LINK2. With CONFIRM, confirm before
move."
  (let ((path1 (ezeka-link-file link1))
        (path2 (ezeka-link-file link2)))
    (when (or (not confirm)
              (y-or-n-p (format "Move %s to %s? " link1 link2)))
      (unless (file-exists-p (file-name-directory path2))
        (make-directory (file-name-directory path2)))
      (vc-rename-file path1 path2)
      (let* ((mdata (ezeka-file-metadata path2))
             (oldnames (alist-get :oldnames mdata)))
        ;; add to log
        (ezeka--add-to-move-log link1 link2)
        ;; put original name in oldnames, update rubric
        (unless (string= (ezeka-link-id link1) (ezeka-link-id link2))
          (setf (alist-get :oldnames mdata)
                (cons link1 oldnames)))
        (ezeka--update-file-header path2 mdata t)
        (when-let ((buf (get-file-buffer path2)))
          (with-current-buffer buf
            (save-buffer)))
        ;; replace links
        (let ((replaced (ezeka-zk-replace-links link1 link2)))
          (message "Moved %s to %s, replacing %d links in %d files"
                   link1 link2
                   (or (car replaced) 0) (or (cdr replaced) 0)))))))

(defun ezeka-move-to-another-kasten (source-file kasten &optional target-link noselect)
  "Moves the current Zettel to another KASTEN. With
\\[universal-argument], asks for a target link instead. Opens (unless
NOSELECT is non-nil) the target link and returns it."
  (interactive
   (let ((target (when (equal current-prefix-arg '(4))
                   (read-string "Enter target link: "))))
     (list (ezeka--grab-dwim-file-target)
           (if target
               (ezeka-link-kasten target)
            (completing-read "Which kasten to move to? " ezeka-kaesten))
           target)))
  (let* ((source-link (ezeka-file-link source-file))
         (kasten (or kasten
                     ))
         (target-link (or target-link
                          (ezeka-make-link
                           kasten
                           (cl-case (cadr (assoc kasten ezeka-kaesten #'string=))
                             (:numerus (ezeka-next-unused-id kasten))
                             (:tempus (ezeka-tempus-currens-id-for source-link))
                             (t
                              (error "Don't know how to handle this")))))))
    (if (not target-link)
        (error "Don't know where to move %s" source-link)
      ;; Offer to save buffers, since zmove tries to update links
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
  "Generates a bunch of new IDs, making sure there are no dulicates."
  (interactive
   (list (read-number "How many? " 10)
         (intern (completing-read "Which type? "
                           (mapcar #'first ezeka-default-kasten)))))
  (goto-char (point-max))
  (let (ids)
    (dotimes (n how-many)
      (push (ezeka-generate-new-id type) ids))
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
  "Displays Zettel title of the file under cursor in the mode line."
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
                  (when (string-match ezeka-regexp-header-line line)
                    (match-string 2 line))
                  file)))
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
  :lighter " Zettel"
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
          ;;   X   X X X X X           X X X X X X X X X X X X X
          ;;------------------------------------------------------------------
          '(
            ;; ("C-c `" . ) ; `org-table-edit-field'
            ("C-c ~" . ezeka-set-title) ; `org-table-create-with-table\.el'
            ;; ("C-c !" . ) ; `org-time-stamp-inactive'
            ("C-c @" . ezeka-set-citekey)
            ("C-c #" . ezeka-add-keyword)
            ("C-c $" . ezeka-kill-ring-save-link-at-point)
            ("C-c %" . ezeka-kill-ring-save-link)
            ("C-c ^" . ezeka-find-ancestor)
            ;; ("C-c &" . ) ; yasnippet
            ;; ("C-c *" . ) ; `org-ctrl-c-star'
            ;; ("C-c (" . ) ;
            ;; ("C-c )" . ) ;
            ;; ("C-c -" . ) ;
            ("C-c _" . ezeka-find-descendant)
            ("C-c =" . ezeka--update-file-header) ; `org-table-eval-formula'
            ("C-c +" . ezeka-dwim-with-this-timestring)
            ("C-c [" . ezeka-update-link-prefix-title) ; `org-agenda-file-to-front'
            ("C-c ]" . ezeka-set-title) ;
            ("C-c |" . ezeka-toggle-update-header-modified) ; `org-table-create-or-convert-from-region'
            ("C-c '" . ezeka-set-label) ; `org-edit-special'
            ("C-c \"" . ezeka-insert-ancestor-link)
            ("C-c ," . ezeka-insert-new-child)
            ("C-c ." . ezeka-insert-link-from-clipboard) ; `org-table-eval-formula'
            ("C-c /" . ezeka-kill-ring-save-link-title)
            ("C-c ?" . ezeka-links-to)                   ; `org-table-field-info'

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
            ))

  ;; Treat : (colon) as part of the word, allowing forward/backward-word over full
  ;; Zettel links.
  (add-hook 'ezeka-mode-hook
    (lambda ()
      (modify-syntax-entry ?: "w"))))

;; On save, update modificate date and normalize file name
(add-hook 'ezeka-mode-hook
  (lambda ()
    (add-hook 'before-save-hook 'ezeka--update-file-header nil t)
    (add-hook 'after-save-hook 'ezeka-normalize-file-name nil t)))

(provide 'ezeka)
