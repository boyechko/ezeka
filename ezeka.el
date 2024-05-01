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
(require 'seq)

;;;=============================================================================
;;; Variables: General
;;;=============================================================================

(defcustom ezeka-directory nil
  "The central Zettelkasten directory."
  :type 'string
  :group 'ezeka)

(defcustom ezeka-file-extension "txt"
  "Default extension for Zettel files."
  :type 'string
  :group 'ezeka)

(defcustom ezeka-categories nil
  "A list of categories used for tempus currens Zettel."
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

(defcustom ezeka-rename-note-keyword "#rename"
  "Keyword to add to notes that need renaming.
Because git does not keep track of renames, significantly changing the
content confuses the default rename detection. It is helpful,
therefore, to split the operations into two commits."
  :type 'string
  :group 'ezeka)

(defcustom ezeka-note-moving-keyword "#moving"
  "Keyword signifying that the note is being moved.
See `ezeka-move-to-another-kasten' for details."
  :type 'string
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

(defcustom ezeka-after-save-hook '()
  "Hook ran after an Ezeka file is saved."
  :type 'list
  :group 'ezeka)

(defcustom ezeka-find-file-functions '()
  "List of functions to call when finding Ezeka files.
Each function should accept two arguments: the file to find,
and optionally, the source from where the function was
called. TARGET should be a filename; SOURCE can be either a
filename or a symbol describing the source."
  :type 'list
  :group 'ezeka)

;; For our purposes, a "timestamp" is a string representation of "time," the
;; default Emacs time value.
(defcustom ezeka-timestamp-formats
  '("%F %a" . "%F %a %R")
  "A cons cell of date-only and full timestamp formats.
These timestamps are used for created and modified metadata,
reading dates, and change log entries.

See `format-time-string' for details about format string."
  :type 'cons
  :group 'ezeka)

(defcustom ezeka-new-numerus-currens-method 'auto
  "Method for generating new numeri currentes.
Possible choices:
- 'ASK           = ask the user every time
- 'AUTO (or NIL) = distribute evenly among subdirectories
- 'MANUAL        = enter new ID manually
- 'RANDOM        = randomly
- 'SELECTIVE     = selected letter, random number
- string         = get from file of pregenerated IDs

If the value is a string, it should be a file name composed
of pregenerated numeri currentes, one per line."
  :type '(choice (const :tag "Ask" ask)
                 (const :tag "Automatic" auto)
                 (const :tag "Manual" manual)
                 (const :tag "Random" random)
                 (const :tag "Selective" selective)
                 (file :tag "Pregenerated numeri currentes"))
  :group 'ezeka)

;;;-----------------------------------------------------------------------------
;;; Variables: Metadata and Headers
;;
;; Metadata refers to the information about the Zettel note, like its created or
;; midified time, and so on. Header, on the other hand, is the actual
;; representation of that metadata inside the Zettel note.
;;
;; Rubric is the compressed metadata information (including the caption, which
;; is a pasteurized or shortened title) that serves as the file name for the
;; note. This is included in the rubric for redundancy.
;;;-----------------------------------------------------------------------------

;; TODO Replace "link" with "id," reserving "link" term for actual links
(defmacro ezeka-link-regexp (&optional match-entire)
  "Return the regular expression that matches Zettel links.
If MATCH-ENTIRE is non-nil, the regexp matches the entire
string.

Group 1 is the ID.
Group 2 is the kasten, if specified."
  `(rx ,(if match-entire 'string-start 'word-start)
       (optional (group-n 2 (one-or-more alpha)) ":")
       (group-n 1 (regexp ,(ezeka--id-regexp)))
       ,(if match-entire 'string-end 'word-end)))

(defcustom ezeka-file-name-format "%i {%l} %c %k"
  "The `format-spec' string for generating a note's file name.
At the minimum, it must start with \"%i\"; everything else
is optional. See `ezeka-format-metadata' for details. This
should match `ezeka-file-name-regexp'."
  :type 'string
  :group 'ezeka)

(defcustom ezeka-file-name-separator " "
  "String separating the ID from the rest of the file name.
For example, with `ezeka-file-name-format' set to \"%i-%c\"")

;; TODO See `ezeka-header-rubric-regexp' about how to unhardcode this.
(defmacro ezeka-file-name-regexp ()     ; HARDCODED
  "Return regexp matching Zettel base file names (i.e. rubrics).
It should match the result of `ezeka-file-name-format`.

Group 1 is the ID.
Group 2 is the kasten.
Group 3 is the label (genus or category).
Group 4 is the caption (i.e. short title).
Group 5 is the citation key.
Group 6 is the stable caption mark."
  `(concat ,(ezeka-link-regexp)          ; \1 and \2
           ,ezeka-file-name-separator
           "\\(?:"                      ; everything else is optional
           "\\(?:\\.\\)*"               ; FIXME: optional historic period
           "\\(?:{\\(?3:[^}]+\\)} \\)*" ; \3
           "\\(?4:.+?\\)"               ; \4
           "\\(?: \\(?5:[@&]\\S-+\\)\\)*$" ; \5
           "\\)*"                          ; end of everything else
           ))

(defcustom ezeka-header-separator-regexp "^$"
  "Regexp that matches the separator line between header and the note text."
  :type 'string
  :group 'ezeka)

(defcustom ezeka-header-rubric-key "rubric"
  "The header metadata key for the rubric."
  :type 'string
  :group 'ezeka)

(defcustom ezeka-header-rubric-stable-mark "§"
  "Mark that signifies that the caption is stable.
The mark is used in the rubric to show that any differences
between caption and title values should be ignored as long
as filename and rubric match."
  :type 'string
  :group 'ezeka)

(defcustom ezeka-header-update-modified t
  "Whether `ezeka--update-file-header' updates the modification date.
Possible choices are ALWAYS, SAMEDAY, NEVER, or CONFIRM (same as T)."
  :type 'symbol
  :group 'ezeka)

(defcustom ezeka-header-read-only t
  "When non-nil, the Ezeka file headers are made read-only."
  :type 'boolean
  :group 'ezeka)

(defcustom ezeka-save-after-metadata-updates 'confirm
  "Whether to automatically save the file after modification.
Functions affected are `ezeka-set-label', `ezeka-set-citekey', and
`ezeka-set-title-or-caption'."
  :type 'symbol
  :options '(nil t confirm)
  :group 'ezeka)

;;;=============================================================================
;;; Kaesten
;;;=============================================================================

(cl-defstruct
    (ezeka-id-style (:constructor nil)
                    (:constructor ezeka-id-style--create))
  (description nil :type 'string)
  (regexp      nil :type 'string)
  (min-length  nil :type 'integer)
  (max-length  nil :type 'integer))

(cl-defstruct
    (ezeka-kasten (:constructor nil)
                  (:constructor ezeka-kasten--create)
                  (:copier nil))
  (name
   nil
   :documentation "The name of the Kasten"
   :type 'string
   :read-only t)
  (id-style
   nil
   :documentation "ID style (`ezeka-id-style') used in the Kasten"
   :type 'keyword)
  (directory
   nil
   :documentation "Kasten's directory under `ezeka-directory'"
   :type 'string)
  (subdir-func
   nil
   :documentation "Return subdirectory based on given ID"
   :type 'function)
  (order
   nil
   :documentation "Priority order (1 = highest)"
   :type 'integer))

(defun ezeka-kasten-id-type (kasten)
  "Return a keyword naming KASTEN's ID style."
  (intern (concat ":" (ezeka-kasten-name kasten))))

(defun ezeka-kasten-id-min-length (kasten)
  "Return `ezeka-id-style-min-length' for KASTEN's `ezeka-kasten-id-style'."
  (ezeka-id-style-min-length (ezeka-kasten-id-style kasten)))

(defun ezeka-kasten-id-regexp (kasten)
  "Return `ezeka-id-style-regexp' for KASTEN's `ezeka-kasten-id-style'."
  (ezeka-id-style-regexp (ezeka-kasten-id-style kasten)))

(defvar ezeka--kaesten nil
  "A list of `ezeka-kasten' structs populated by `ezeka-kaesten-new'.
The list is ordered by the Kasten's `order' field.")

(cl-defun ezeka-kasten-new (name &key id-regexp minimal-id directory subdir-func)
  "Create a new `ezeka-kasten' with NAME and register it.
See defstruct above about ID-REGEXP, MINIMAL-ID, DIRECTORY,
and SUBDIR-FUNC. DIRECTORY, if relative, will be expanded in
`ezeka-directory'."
  (let* ((directory (or directory ""))
         (directory (if (file-name-absolute-p directory)
                        directory
                      (file-name-as-directory
                       (expand-file-name directory ezeka-directory))))
         (id-style (ezeka-id-style--create
                    :description (format "Used in %s Kasten; ex. %s" name minimal-id)
                    :regexp id-regexp
                    :min-length (length minimal-id)))
         (kasten (ezeka-kasten--create :name name
                                       :id-style id-style
                                       :directory directory
                                       :subdir-func subdir-func
                                       :order (1+ (length ezeka--kaesten)))))
    (setq ezeka--kaesten
      (cl-sort (cons kasten
                     (cl-remove name ezeka--kaesten
                                :test #'string= :key #'ezeka-kasten-name))
               #'<
               :key #'ezeka-kasten-order))
    kasten))

(defun ezeka-kaesten ()
  "Return a list of registered Kästen."
  (copy-sequence ezeka--kaesten))

(cl-defgeneric ezeka-kasten (thing)
  "Return the `ezeka-kasten' appropriate for THING."
  (:method ((thing string))
   (cl-find thing (ezeka-kaesten) :key #'ezeka-kasten-name :test #'string=))
  (:method ((thing integer))
   (cl-find thing (ezeka-kaesten) :key #'ezeka-kasten-order))
  (:method ((thing ezeka-kasten))
   thing))

(setq ezeka--kaesten nil)
(ezeka-kasten-new "numerus"
                  :id-regexp "[a-z]-[0-9]\\{4\\}"
                  :minimal-id "a-1234"
                  :directory "numerus"
                  :subdir-func (lambda (id) (substring id 0 1)))
(ezeka-kasten-new "tempus"
                  :id-regexp "[0-9]\\{8\\}T[0-9]\\{4\\}"
                  :minimal-id "20230404T1713"
                  :directory "tempus"
                  :subdir-func (lambda (id) (substring id 0 4)))
(ezeka-kasten-new "scriptum"
                  :id-regexp "[a-z]-[0-9]\\{4\\}~[0-9][0-9]"
                  :minimal-id "a-1234~01"
                  :directory "scriptum"
                  :subdir-func (lambda (id)
                                 (car (split-string id "~"))))
(ezeka-kasten-new "v1"
                  :id-regexp "[0-9]\\{3\\}\\(-[A-Z]\\(-[0-9][0-9]\\)*\\)*"
                  :minimal-id "123")
(ezeka-kasten-new "v2"
                  :id-regexp "[0-9]\\{3\\}\\(-[a-z]+\\)*"
                  :minimal-id "123")
(ezeka-kasten-new "v3"
                  :id-regexp "[0-9]\\{3\\}-[a-z]\\{3\\}"
                  :minimal-id "123-abc")

(defun ezeka--id-regexp (&optional id-type match-entire)
  "Return the regexp for the given ID-TYPE based on `ezeka-kaesten'.
If ID-TYPE is not given, return a regexp that matches all known types.
If MATCH-ENTIRE is non-nil, enclose the regexp in string boundaries."
  (let ((kasten (cl-find id-type (ezeka-kaesten) :key #'ezeka-kasten-id-type)))
    (concat (when match-entire "\\`")
            "\\(?:"
            (if kasten
                (ezeka-kasten-id-regexp kasten)
              (mapconcat #'ezeka-kasten-id-regexp
                         (cl-sort (ezeka-kaesten) #'>
                                  :key #'ezeka-kasten-id-min-length)
                         "\\|"))
            "\\)"
            (when match-entire "\\'"))))

(defun ezeka-id-valid-p (id &optional id-type)
  "Return non-nil if ID matches the ID-TYPE.
If ID-TYPE is not given, check ID against all known types."
  (let ((kasten (if id-type
                    (cl-find id-type (ezeka-kaesten) :key #'ezeka-kasten-id-type)
                  (cl-find id (ezeka-kaesten)
                           :key #'ezeka-kasten-id-regexp
                           :test #'string-match-p))))
    (and (stringp id)
         (string-match-p (ezeka--id-regexp id-type 'match-entire) id))))

(ert-deftest ezeka-id-valid-p ()
  (should-not (ezeka-id-valid-p "goggly-gook"))
  (should (ezeka-id-valid-p "a-1234"))
  (should (ezeka-id-valid-p "327-C-02-A")))

(defvar ezeka--read-id-history nil
  "History of IDs that the user entered manually.")

(defun ezeka--read-id (prompt &optional id-type initial-input required)
  "Use `read-string' with PROMPT to read an ID.
If ID-TYPE is given, make sure the entered ID is valid for
that specific type. INITIAL-INPUT is passed to
`read-string', which see. If REQUIRED is non-nil, keep
asking until a valid ID is entered."
  (let ((message "")
        id)
    (while (null id)
      (setq id (read-string (concat message prompt)
                            initial-input
                            'ezeka--read-id-history))
      (when (or (and (string-empty-p id) required)
                (and (not (string-empty-p id))
                     (not (ezeka-id-valid-p id id-type))))
        (setq id nil
              message (format "That is not a valid %sID; try again.\n"
                              (if id-type
                                  (concat (symbol-name id-type) " ")
                                "")))))
    id))

;;;=============================================================================
;;; General Functions
;;;=============================================================================

(define-error 'ezeka-error
  "Something went wrong: %s"
  'error)

(define-error 'ezeka-unknown-id-type-error
  "Unknown type of ID: `%s'"
  'ezeka-error)

(defun in-ezeka-dir (&optional relative-path)
  "Return absolute path to RELATIVE-PATH in the Zettel directory."
  (unless ezeka-directory
    (signal 'ezeka-error "No `ezeka-directory' set"))
  (expand-file-name (or relative-path "") ezeka-directory))

(defun ezeka--space-or-punct-p (character)
  "Return T if the CHARACTER is a space or punctuation."
  (when character
    ;; [:punct:] is too permissive, since includes (), [], {}, etc.
    (string-match-p "[[:space:].,;:'\"]" (char-to-string character))))

(defmacro ezeka--with-space (value &optional where)
  "If VALUE is non-nil, return a string with specified space.
If VALUE is nil, return an empty string. VALUE is evaluated
only once. WHERE can be 'before, 'after, or 'both."
  (let ((sym (gensym)))
    `(let ((,sym ,value))
       (if ,sym
           (concat (if (member ,where '(before both)) " " "")
                   ,sym
                   (if (member ,where '(after both)) " " ""))
         ""))))

(defun ezeka--ordinal-suffix (n)
  "Ordinal suffix for N, a number or string.
\(That is, `st', `nd', `rd', or `th', as appropriate.)
This function is based on `diary-ordinal-suffix'."
  (let ((n (round (if (numberp n) n (string-to-number n)))))
    (if (or (memq (% n 100) '(11 12 13))
            (< 3 (% n 10)))
        "th"
      (aref ["th" "st" "nd" "rd"] (% n 10)))))

(defun ezeka--grab-dwim-file-target (&optional link-at-point interactive)
  "Return the do-what-I-mean Zettel file from a variety of modes.
If LINK-AT-POINT is non-nil, prioritize such a link if
exists. If INTERACTIVE is non-nil, have the user select a
file interactively."
  (cond ((and link-at-point (ezeka-link-at-point-p))
         (ezeka-link-file (ezeka-link-at-point)))
        ((and buffer-file-name
              (or ezeka-mode (ezeka-file-p buffer-file-name t)))
         buffer-file-name)
        ((eq major-mode 'magit-status-mode)
         (magit-file-at-point))
        ((eq major-mode 'xref--xref-buffer-mode)
         (when-let ((xref (xref--item-at-point)))
           (xref-file-location-file (xref-item-location xref))))
        ((eq major-mode 'deft-mode)
         (when-let ((button (button-at (point))))
           (button-get button 'tag)))
        ((eq major-mode 'octavo-index-mode)
         (or (button-get (button-at (point)) 'button-data)
             (octavo--select-file)))
        ((and (featurep 'octavo)
              interactive)
         ;; FIXME Rather than calling `octavo--select-file' directly, need a native
         ;; function that would at least allow selecting Kasten.
         (octavo--select-file))))

(defun ezeka--replace-in-string (string &rest replacements)
  "Replace in STRING all the regexp/replacement pairs in REPLACEMENTS.
Each item in REPLACEMENTS is in the form (FROM TO) for simple
string replacements or (FROM TO 'regexp [&rest ARGS]) for regexp,
where ARGS is the argument list to `replace-regexp-in-string'
 after STRING. Each replacement pair is processed in turn."
  (declare (indent 1))
  (save-match-data
    (dolist (recipe replacements string)
      (setq string
        (if (memq 'regexp recipe)
            (apply #'replace-regexp-in-string
                   (car recipe)
                   (cadr recipe)
                   string
                   (cdr (memq 'regexp recipe)))
          (string-replace (car recipe) (cadr recipe) string))))))

(defun ezeka--regexp-strip-named-groups (regexp)
  "Strip the named groups in the given REGEXP."
  (replace-regexp-in-string "(\\?[0-9]+:" "(" regexp))

(defun ezeka--minibuffer-edit-string (old-string &optional new-string prompt history)
  "Edit NEW-STRING in minibuffer, showing it in parallel to OLD-STRING.
If NEW-STRING is nil, default to OLD-STRING. If given,
PROMPT is a string shown as the first line. HISTORY should
be a variable name passed to `read-string'."
  (let* ((old-string (or old-string ""))
         (new-string (or new-string old-string))
         (prompt (or prompt "Original: ")))
    (condition-case nil
        (read-string
         (format (format "%%s%%s\n%%%-ds" (length prompt))
                 prompt (propertize old-string 'face 'italic)
                 "Edited: ")
         new-string
         history)
      (minibuffer-quit old-string))))

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
  (let* ((tempname (file-name-with-extension filename "tmp"))
         (newname (if (file-name-absolute-p newname)
                      newname
                    (expand-file-name
                     newname
                     (file-name-directory filename)))))
    (when (or (not (file-exists-p filename)) ; filename not saved yet
              (progn
                (rename-file filename tempname t)
                (rename-file tempname newname t)
                (file-exists-p newname)))
      (set-visited-file-name newname t t)
      (ezeka--add-to-system-log 'rename nil
        'old-name (file-relative-name filename ezeka-directory)
        'new-name (file-relative-name newname ezeka-directory))
      newname)))

(defun ezeka--make-symbolic-link (target linkname)
  "Make a symbolic link to TARGET from LINKNAME.
This is a wrapper around `make-symbolic-link' that also adds
an entry into the system log. Both TARGET and LINKNAME
should be files."
  (interactive
   (list buffer-file-name
         (read-file-name "Symbolic link to: "
                         ezeka-directory
                         (file-name-nondirectory buffer-file-name)
                         nil
                         (file-name-nondirectory buffer-file-name))))
  (condition-case nil
      (make-symbolic-link target linkname)
    (file-already-exists (warn "File already exists"))
    (:success
     (ezeka--add-to-system-log 'symlink nil
       'target (file-name-base target)
       'link (file-name-base linkname)))))

(ert-deftest ezeka--make-symbolic-link ()
  (let ((target (make-temp-file "ezeka-target"))
        (linkname (expand-file-name "ezeka-symlink" (temporary-file-directory))))
    (ezeka--make-symbolic-link target linkname)
    (should (and (file-exists-p linkname) (file-symlink-p linkname)))
    (should-not (when (file-symlink-p linkname)
                  (delete-file linkname)
                  (file-exists-p linkname)))))

;; The following is adapted from
;; https://emacs.stackexchange.com/a/46059
(defface ezeka-read-only '((t :slant italic))
  "Face for `ezeka--read-only-region'."
  :group 'ezeka)

(defun ezeka--read-only-region (begin end)
  "Make the marked region between BEGIN and END read-only.
The text property is nonsticky in the front and the rear.
See also `ezeka--writeable-region'.

Read-only text is given the face `ezeka-read-only'."
  (interactive "r")
  (let ((inhibit-read-only t))
    (with-silent-modifications
      (add-text-properties begin end
                           ;; If the ‘rear-nonsticky’ property is a list,
                           ;; properties are rear-sticky _unless_ their names
                           ;; are in the list.
                           '(read-only t rear-nonsticky (read-only)))
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
  (declare (indent 1))
  (mapconcat #'(lambda (elt)
                 (if (stringp elt)
                     elt
                   (format "%s" elt)))
    (cl-remove-if #'null elements)
    separator))

;;;=============================================================================
;;; Fundamental Functions
;;;=============================================================================

(defun ezeka-file-p (file-or-buffer &optional strict)
  "Return non-NIL if the FILE-OR-BUFFER is a Zettel.
It is a Zettel if all of these conditions are met:
1) its extension is `ezeka-file-extension';
2) its filename matches `ezeka-file-name-regexp'; and, if STRICT is non-NIL,
3) the file exists;
4) the file is inside `ezeka-directory'."
  (interactive "f")
  (when-let ((_ file-or-buffer)
             (file (cond ((bufferp file-or-buffer)
                          (buffer-file-name file-or-buffer))
                         ((stringp file-or-buffer)
                          (expand-file-name file-or-buffer))
                         (strict
                          (signal 'wrong-type-argument
                                  (list 'file-or-buffer-p file-or-buffer)))
                         (t nil))))
    (and (string-equal (file-name-extension file) ezeka-file-extension)
         (string-match (concat "^" (ezeka-file-name-regexp) "$")
                       (file-name-base file))
         (or (not strict)
             (and (file-exists-p file)
                  (string-prefix-p ezeka-directory file)))))) ; FIXME: Hack

(defun ezeka-directory-kasten (directory)
  "Return the kasten name of the given Zettel DIRECTORY."
  ;; FIXME: This is a hack that would not work if Kasten names don't match the
  ;; directory name.
  (file-name-base (directory-file-name directory)))

(defun ezeka-file-name-valid-p (filename)
  "Return non-nil if FILENAME is a valid Zettel filename."
  (save-match-data
    (string-match (ezeka-file-name-regexp) (file-name-base filename))))

(defun ezeka--file-name-fields (filename)
  "Parse FILENAME, returning its matched fields an an alist."
  (let* ((base (file-name-base filename)))
    (save-match-data
      (when (string-match (ezeka-file-name-regexp) base)
        (mapcar (lambda (key-val)
                  (cons (car key-val)
                        (when (cdr key-val) (string-trim (cdr key-val)))))
                `((id . ,(match-string 1 base))
                  (label . ,(match-string 3 base))
                  (caption . ,(match-string 4 base))
                  (citekey . ,(match-string 5 base))))))))

(defun ezeka--file-name-part (filename part)
  "Return given PART (id, label, caption, or citekey) of FILENAME."
  (let ((base (file-name-base filename)))
    (save-match-data
      (when-let ((_ (string-match (ezeka-file-name-regexp) base))
                 (match (match-string (pcase part
                                        ('id 1)
                                        ('label 3)
                                        ('caption 4)
                                        ('citekey 5))
                                      base)))
        (string-trim match)))))

(defmacro ezeka-file-name-id (filename)
  "Return the ID part of the given Zettel FILENAME."
  `(ezeka--file-name-part ,filename 'id))
(defmacro ezeka-file-name-label (filename)
  "Return the label part of the given Zettel FILENAME."
  `(ezeka--file-name-part ,filename 'label))
(defmacro ezeka-file-name-caption (filename)
  "Return the caption part of the given Zettel FILENAME."
  `(ezeka--file-name-part ,filename 'caption))
(defmacro ezeka-file-name-citekey (filename)
  "Return the citekey part of the given Zettel FILENAME."
  `(ezeka--file-name-part ,filename 'citekey))

;; TODO Rewrite using rubric
(defun ezeka-format-file-name (format-string filename)
  "Format FILENAME according to FORMAT-STRING.
Unlike `ezeka-format-metadata', this function relies purely
on the filename.

The format control string can contain the following
%-sequences:

%c means caption (i.e. short title).
%e means file extension.
%f means base filename (i.e. without extension).
%i means ID or link.
%k means citation key.
%K means kasten.
%l means label (genus or category).

Unknown %-sequences are left intact."
  (let-alist (ezeka--file-name-fields filename)
    (let* ((case-fold-search nil)
           (basic (format-spec format-string
                               `((?e . ,(file-name-extension filename))
                                 (?f . ,(file-name-base filename))
                                 (?i . ,.id)
                                 (?l . ,.label)
                                 (?c . ,.caption)
                                 (?K . ,(ezeka-kasten-name
                                         (ezeka-file-kasten filename)))
                                 (?k . ,(or .citekey "")))
                               'ignore))) ; leave unknown %-sequences
      basic)))

(defun ezeka-file-kasten (file)
  "Return the Kasten of the given Zettel FILE."
  (when-let ((id (ezeka-file-name-id file)))
    (cl-find-if (lambda (re) (string-match-p (rx bos (regexp re) eos) id))
                (ezeka-kaesten)
                :key #'ezeka-kasten-id-regexp)))

(defun ezeka-file-link (file)
  "Return a fully qualified link to FILE or nil."
  (when (and (stringp file)
             (not (string-empty-p file)))
    (if (ezeka-link-p file)
        file
      (when-let ((kasten (ezeka-file-kasten file))
                 (id (ezeka-file-name-id file)))
        (ezeka-make-link kasten id)))))

(defun ezeka-same-file-p (file1 file2)
  "Return non-nil if FILE1 and FILE2 point to same Zettel."
  (cond ((eq file1 file2) t)
        ((or (symbolp file1) (symbolp file2)) nil)
        ((string= (ezeka-file-link file1)
                  (ezeka-file-link file2)))))

(defun ezeka-link-p (string)
  "Return non-NIL if the STRING could be a link to a Zettel."
  (when (stringp string)
    (string-match-p (ezeka-link-regexp 'match-entire) string)))

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
                  (cl-find (ezeka-id-type id)
                           (ezeka-kaesten)
                           :key #'ezeka-kasten-id-type)))))
    (signal 'wrong-type-argument (list 'ezeka-link-p link))))

(defun ezeka-link-id (link)
  "Return the ID part of the given LINK."
  (when (string-match (ezeka-link-regexp) link)
    (match-string 1 link)))

(defun ezeka-make-link (kasten id)
  "Make a new proper link to ID in KASTEN (string or `ezeka-kasten')."
  (let ((e-k (pcase kasten
               ((pred stringp) (ezeka-kasten kasten))
               ((pred ezeka-kasten-p) kasten)
               (_ (signal 'wrong-type-argument (list 'ezeka-kasten-p kasten))))))
    (cond ((not e-k)
           (signal 'ezeka-error
                   (list "Unknown Kasten `%s'; register with `ezeka-kasten-new' first"
                         kasten)))
          ((not (eq (ezeka-kasten-id-type e-k) (ezeka-id-type id)))
           (signal 'ezeka-error
                   (list "ID `%s' doesn't match the ID type for `%s' Kasten"
                         id
                         (ezeka-kasten-name e-k))))
          (t
           id))))

(defun ezeka-id-directory (id &optional kasten)
  "Return the full directory under KASTEN where ID should be."
  (let ((eka (ezeka-kasten (or kasten (ezeka-link-kasten id)))))
    (file-name-as-directory
     (file-name-concat (ezeka-kasten-directory eka)
                       (or (funcall (ezeka-kasten-subdir-func eka) id)
                           "")))))

(defun ezeka--id-kaesten (id)
  "Return all kaesten for the ID's type."
  (let ((type (ezeka-id-type id)))
    (mapcar #'car
            (cl-remove-if-not (lambda (x)
                                (eq (cadr x) type))
                              (ezeka-kaesten)))))

(defun ezeka-link-file (link)
  "Return a full file path to the Zettel LINK.
To do that, try wildcard expansion for the file name
beginning with LINK, returning it if found, or NIL
otherwise."
  (save-match-data
    (when-let* ((link (and (stringp link) (substring-no-properties link)))
                (id (ezeka-link-id link))
                (basename (file-name-with-extension
                           (concat id "*")
                           ezeka-file-extension))
                (dir (ezeka-id-directory id))
                (matches (flatten-list
                          (file-expand-wildcards
                           (expand-file-name basename dir))))
                (files (or (cl-remove-if-not
                            (lambda (f)
                              (null (file-attribute-type (file-attributes f))))
                            matches)
                           t)))
      (cond ((zerop (length matches)) nil)
            ((not (cdr matches)) (car matches))
            ((not (cdr files)) (car files))
            ((eq t files)
             (warn "Found multiple symbolic links for `%s':\n    %s"
                   link
                   (mapcar #'file-name-base matches))
             (ezeka--select-file matches
                                 "Multiple matches found. Select one: "
                                 'require-match))
            (t
             (warn "Found multiple matches for `%s':\n%s"
                   link
                   (mapconcat (lambda (m)
                                (list (file-name-base m)
                                      (pcase (file-attribute-type (file-attributes m))
                                        ((pred stringp) 'symlink)
                                        ((pred null) 'file)
                                        (_ 'directory))))
                              matches
                              "\n- "))
             (ezeka--select-file matches
                                 "Multiple matches found. Select one: "
                                 'require-match))))))

(defun ezeka-link-path (link &optional metadata)
  "Return a full file path to the Zettel LINK.
Unlike `ezeka-link-file', no attempts are made to check if
the file actually exists; its file path is simply computed
based on LINK and METADATA (if present)."
  (let ((mdata (append metadata
                       (ezeka-metadata link
                         'label "nil"
                         'caption "nil"))))
    (expand-file-name (file-name-with-extension
                       (if mdata
                           (ezeka-format-metadata ezeka-file-name-format mdata)
                         (ezeka-link-id link))
                       ezeka-file-extension)
                      (ezeka-id-directory (ezeka-link-id link)))))

(defun ezeka-id-type (id-or-file)
  "Return the type of the given ID-OR-FILE based on `ezeka-kaesten`."
  (if-let* ((id (or (ezeka-file-name-id id-or-file)
                    (file-name-base id-or-file))) ; HACK
            (kasten (cl-find-if (lambda (k)
                                  (string-match-p
                                   (rx bos (regexp (ezeka-kasten-id-regexp k)) eos)
                                   id))
                                (ezeka-kaesten))))
      (ezeka-kasten-id-type kasten)
    (signal 'ezeka-unknown-id-type-error (cons id kasten))))

(defun ezeka-file-content (file &optional header-only)
  "Return content of FILE, getting it first from opened buffer.
If HEADER-ONLY is non-nil, only get the header."
  (let ((_retrieve-content
         (lambda ()
           "Retrieve content from current buffer."
           (buffer-substring-no-properties
            (point-min)
            (if (not header-only)
                (point-max)
              (goto-char (point-min))
              (if (re-search-forward ezeka-header-separator-regexp nil t)
                  (match-beginning 0)
                (point-max)))))))
    (if (get-file-buffer file)
        (save-excursion
          (with-current-buffer (get-file-buffer file)
            (save-restriction
              (widen)
              (funcall _retrieve-content))))
      (with-temp-buffer
        (insert-file-contents file)
        (funcall _retrieve-content)))))

;;;=============================================================================
;;; Parsing and Manipulating Time
;;;=============================================================================

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

(defun ezeka--parse-time-string (string)
  "Return the internal encoded time corresponding to STRING.
It should be an ISO8601 date/time expression or an
`org-mode' timestamp, with or without time. Cannot use
`parse-time-string' because it always expects time."
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

(defun ezeka--iso8601-time-string (&optional time)
  "Return a string with full ISO8601 representation of TIME (or now)."
  (format-time-string "%FT%T%z" time))

(defun ezeka--complete-time (time1 &optional time2)
  "Complete TIME1 from TIME2, returning time value.
If TIME2 is not given, use current time."
  (let* ((dt1 (decode-time time1))
         (dt2 (decode-time time2))
         (dt-now (decode-time)))
    (when (and (zerop (decoded-time-hour dt1))
               (zerop (decoded-time-minute dt1)))
      (setf (decoded-time-hour dt1)
            (if (zerop (decoded-time-hour dt2))
                (decoded-time-hour dt-now)
              (decoded-time-hour dt2))
            (decoded-time-minute dt1)
            (if (zerop (decoded-time-minute dt2))
                (decoded-time-minute dt-now)
              (decoded-time-minute dt2))))
    (encode-time dt1)))

(defun ezeka-timestamp (&optional time full brackets noweekday)
  "Return Emacs TIME formatted according to `ezeka-timestamp-formats'.
If FULL is non-nil, include hour and minute. If BRACKETS is
non-nil, surround the timestamp with square brackets. If
NOWEEKDAY is non-nil, do not include the abbreviated weekday
in date-only timestamp."
  (let ((fmt (funcall (if full #'cdr #'car)
                      ezeka-timestamp-formats)))
    (format (if brackets "[%s]" "%s")
            (format-time-string (if (and (not full) noweekday)
                                    (string-replace " %a" "" fmt)
                                  fmt)
                                time))))

(defun ezeka-dwim-with-this-timestring (&optional beg end)
  "Do What I Mean with the timestring at point or between BEG and END.
If the timestring is IS8601, make it into an org-time-stamp, and
vice-versa. If it's something else, try to make it into
org-time-stamp. Return the result of the conversion."
  (interactive (if (region-active-p)
                   (list (region-beginning) (region-end))
                 (list)))
  (unless (and beg end)
    (when beg (goto-char beg))
    (if (or (thing-at-point-looking-at ezeka--org-timestamp-regexp)
            (thing-at-point-looking-at ezeka-iso8601-datetime-regexp)
            (thing-at-point-looking-at ezeka-iso8601-date-regexp))
        (setq beg (match-beginning 0)
              end (match-end 0))
      (user-error "Can't figure out time string; try selecting region?")))
  (let* ((text (buffer-substring-no-properties beg end))
         timestamp)
    ;; if the region was surrounded by parentheses or brackets, remove those
    (save-excursion
      (re-search-backward (rx space) (point-at-bol) 'noerror)
      (when (re-search-forward (rx (1+ (syntax open-parenthesis))
                                   (literal text)
                                   (1+ (syntax close-parenthesis)))
                               (point-at-eol) t)
        (replace-match text)
        (setq beg (match-beginning 0)
              end (point))))
    (cond
     ;; ISO-8601 -> org timestamp ==============================================
     ((iso8601-valid-p text)
      (let ((timestamp (iso8601-parse text)))
        (delete-region beg end)
        (org-insert-time-stamp (iso8601--encode-time timestamp)
                               (integerp (car timestamp)) t)
        org-last-inserted-timestamp))
     ;; org timestamp -> ISO-8601 ==============================================
     ((setq timestamp
        (org-timestamp-from-string (if (string-match-p "[[<].*[]>]" text)
                                       text
                                     (format "[%s]" text))))
      (let ((iso8601 (org-timestamp-format timestamp "%Y%m%dT%H%M")))
        (delete-region beg end)
        (insert iso8601)
        iso8601))
     ;; datetime -> org timestamp ==============================================
     ((integerp (car (parse-time-string text)))
      (delete-region beg end)
      (org-insert-time-stamp (encode-time (parse-time-string text)) t t)
      org-last-inserted-timestamp)
     ;; date -> ISO-8601 =======================================================
     ((integerp (nth 4 (setq parsed (parse-time-string text))))
      (setf (decoded-time-second parsed) 0
            (decoded-time-minute parsed) 0
            (decoded-time-hour parsed) 0)
      (let ((timestamp (format-time-string "%F" (encode-time timestamp))))
        (when (y-or-n-p
               (format "Is %s same as %s? " text timestamp))
          (delete-region beg end)
          (insert timestamp)
          timestamp)))
     ;; something else =========================================================
     (t
      (display-warning 'ezeka-dwim-with-this-timestring
                       "`%s' doesn't look like a timestring" text)))))

(defun ezeka-insert-or-convert-timestamp (&optional beg end)
  "Insert or convert timestamp at point or between BEG and END.
If there is no timestamp at point, insert one. Otherwise, if
it's IS8601, make it into an org-time-stamp, and vice-versa.
If it's something else, try to make it into org-time-stamp.
Return the result of the conversion."
  (interactive (if (region-active-p)
                   (list (region-beginning) (region-end))
                 (list)))
  (cond ((and beg end)
         (ezeka-dwim-with-this-timestring beg end))
        ((or (thing-at-point-looking-at ezeka--org-timestamp-regexp)
             (thing-at-point-looking-at ezeka-iso8601-datetime-regexp)
             (thing-at-point-looking-at ezeka-iso8601-date-regexp))
         (ezeka-dwim-with-this-timestring (match-beginning 0) (match-end 0)))
        (t
         (insert (ezeka-timestamp nil 'full 'brackets)))))

;;;=============================================================================
;;; Metadata: Internal
;;;=============================================================================

(defconst ezeka-metadata-fields
  '((id        :format ?i :hidden t :predicate ezeka-id-valid-p)
    (link      :format ?L :hidden t :predicate ezeka-link-p)
    (path      :format ?P :hidden t)
    (filename  :format ?F :hidden t)
    (rubric    :format ?R)
    (category  :format ?l :hidden t)
    (label     :format ?l :hidden t)
    (caption   :format ?c :hidden t)
    (citekey   :format ?k :hidden t)
    (successor :format ?s :predicate ezeka-id-valid-p)
    (title     :format ?t)
    (subtitle  :format ?T)
    (author    :format ?a)
    (created   :format ?C :predicate ezeka--timep)
    (modified  :format ?M :predicate ezeka--timep)
    (parent    :format ?p :predicate ezeka-id-valid-p)
    (firstborn :format ?f :predicate ezeka-id-valid-p)
    (oldnames  :format ?o :predicate (ezeka-id-valid-p))
    (readings  :format ?r :predicate listp)
    (keywords  :format ?k :predicate listp))
  "An alist of valid metadata fields and their properties.
The format of each item should be as follows:
    (FIELD [:HIDDEN T] [:PREDICATE <type>]).

If HIDDEN is T, the field is not added to the note header.
YAML collections are returned as Emacs lists and strings
that look like ISO 8601 or `org-mode' timestamps are
converted to Emacs encoded time values. Everything else is
assumed to be scalars, which result in Emacs strings. If
given, PREDICATE is a function that must be satisfied by the
de-YAMLified value.

The order of items will affect how the metadata is written
into the file header.")

(defun ezeka--read-metadata-field (&optional prompt default)
  "Read a metadata field from the user after PROMPT.
The result is returned as a symbol. If there is no input,
return DEFAULT."
  (intern-soft
   (completing-read (or prompt "Whic field? ")
                    ezeka-metadata-fields
                    nil
                    'require-match
                    nil
                    nil
                    default)))

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
%C means creation timestamp.
%F means filename.
%i means ID or link.
%k means citation key.
%K means kasten.
%l means label (genus or category).
%M means modification timestamp (empty if not set).
%p means parent.
%R means rubric.
%s means stable mark (see `ezeka-header-rubric-stable-mark').
%t means title."
  (let ((_format-time
         (lambda (time-string)
           (if (and (stringp time-string)
                    (not (string-match-p ezeka-iso8601-date-regexp time-string)))
               "UNKNOWN"
             (format-time-string (cdr ezeka-timestamp-formats) time-string)))))
    (save-match-data
      (let-alist metadata
        (string-trim
         (format-spec format-string
                      `((?a . ,(if-let ((ck .citekey))
                                   (format "%s's " (ezeka--citaton-key-authors ck))
                                 ""))
                        (?c . ,(or .caption (ezeka--pasteurize-file-name .title)))
                        (?C . ,(funcall _format-time .created))
                        (?F . ,.filename)
                        (?i . ,.id)
                        (?k . ,(cond ((or (not .citekey)
                                          (string-empty-p .citekey))
                                      "")
                                     ((string-match-p "^[@&]" .citekey)
                                      .citekey)
                                     (t
                                      (concat "@" .citekey))))
                        (?K . ,.kasten)
                        (?l . ,.label)
                        (?M . ,(funcall _format-time .modified))
                        (?p . ,.parent)
                        (?R . ,.rubric)
                        (?s . ,(if .caption-stable
                                   ezeka-header-rubric-stable-mark
                                 ""))
                        (?t . ,.title))))))))

(defun ezeka-metadata (file-or-link &rest values)
  "Create a metadata object from pairs of VALUES.
FILE-OR-LINK identifies the Zettel these metadata belong to.
VALUES should be a list of keyword and value properties
corresponding to metadata fields."
  (declare (indent 1))
  (if (and values (zerop (mod (length values) 2)))
      (let* ((link (if (ezeka-link-p file-or-link)
                       file-or-link
                     (ezeka-file-link file-or-link)))
             (mdata `((link . ,link)
                      (id . ,(ezeka-link-id link))
                      (file . ,(unless (ezeka-link-p file-or-link)
                                 file-or-link))
                      (_metadata . created))))
        (while values
          (push (cons (car values) (cadr values)) mdata)
          (setq values (cddr values)))
        mdata)
    (signal 'wrong-type-argument (list 'key-value-pairs-p values))))

(defmacro ezeka-encode-rubric (metadata &optional stable-mark)
  "Return a string that encodes the given METADATA into the rubric.
The rubric consist of `ezeka-file-name-format', preceded by
`ezeka-header-rubric-stable-mark' if STABLE-MARK is given."
  `(concat (when ,stable-mark ezeka-header-rubric-stable-mark)
           (ezeka-format-metadata ezeka-file-name-format ,metadata)))

;; TODO It would be useful to generate the regexp based on
;; `ezeka-encode-rubric' output, perhaps by passing METADATA consisting
;; of %-sequences from `ezeka-format-metadata'. The fields in
;; `ezeka-metadata-fields' could, then, provide their own particular regexp.
(defmacro ezeka-header-rubric-regexp ()
  "Regular expression for the rubric string as found in the header.

Groups 1-5 see `ezeka-file-name-regexp'.
Group 6 is the stable rubric mark."
  `(concat "\\(?6:" ,ezeka-header-rubric-stable-mark "\\)*"
           ,(ezeka-file-name-regexp)))

(defun ezeka-decode-rubric (rubric)
  "Return alist of metadata from the RUBRIC line.
If cannot decode, return NIL."
  (when (and rubric (string-match (ezeka-header-rubric-regexp) rubric))
    (let ((id           (match-string 1 rubric))
          (kasten       (match-string 2 rubric))
          (label        (match-string 3 rubric))
          (caption      (match-string 4 rubric))
          (stable       (when (match-string 6 rubric) t))
          (citekey      (match-string 5 rubric)))
      `((id . ,id)
        (kasten . ,(when kasten (string-trim kasten)))
        (type . ,(ezeka-id-type id))
        (label . ,(unless (string= "nil" label) ; HACK for new notes
                    (ezeka--validate-label label)))
        (caption . ,(when caption (string-trim caption)))
        (caption-stable . ,stable)
        (citekey . ,(when citekey (string-trim citekey)))
        (_metadata . from-rubric)))))

(defun ezeka--header-yamlify-key (key)
  "Return a YAML-formatted string name of the KEY symbol."
  (symbol-name key))

(defun ezeka--timep (time)
  "If TIME is a Lisp time value then return TIME, else return nil.
This is a copy of `timep' from `type-break'."
  (condition-case nil
      (and (float-time time) time)
    (error nil)))

(defun ezeka--full-time-p (time)
  "Return non-NIL if TIME contains non-zero hour, minute, or second."
  (cl-notevery (lambda (x) (or (null x) (zerop x)))
               (cl-subseq (decode-time time) 0 3)))

(defun ezeka--header-yamlify-value (value)
  "Return a YAML-formatted string for the given metadata VALUE."
  (pcase value
    ((pred stringp)
     value)
    ((and (pred ezeka--timep)
          (pred ezeka--full-time-p))
     (ezeka-timestamp value 'full))
    ((pred ezeka--timep)
     (ezeka-timestamp value nil nil 'noweekday))
    ((pred listp)
     (concat "[ "
             (mapconcat #'ezeka--header-yamlify-value
                        value
                        ", ")
             " ]"))
    (_
     (signal 'wrong-type-argument (list (type-of value) value)))))

(defun ezeka--header-deyamlify-value (value)
  "Return an elisp version of the given YAML-formatted VALUE."
  (pcase value
    ;; strip [[ ]] around wiki links
    ((rx bol "[[" (let inside (1+ anychar)) "]]" eol)
     inside)
    ;; convert ISO 8601 and/or `org-mode' timestamps to time values
    ((rx bol
         (* "[")
         (let timestamp
           (seq (= 4 digit) "-" (= 2 digit) "-" (= 2 digit) (* anychar)))
         (* "]")
         eol)
     (ezeka--parse-time-string timestamp))
    ;; remaining [ ] should be lists
    ((rx bol "[ " (let inside (1+ anychar)) " ]" eol)
     (mapcar #'ezeka--header-deyamlify-value
             (split-string inside "," t "[[:space:]]+")))
    (_
     (string-trim value))))

(defvar ezeka-header-line-regexp
  "\\(?1:\\w+\\):\\s-+\\(?2:.*\\)"
  "The regular expression that matches a line of YAML metadata.
Group 1 is the key.
Group 2 is the value.")

(defvar ezeka--validate-metadata-field-history nil
  "History variable for `ezeka--validate-metadata-field'.")

(defun ezeka--validate-metadata-field (field value &optional predicate)
  "Return VALUE if it is valid for metadata FIELD.
PREDICATE, if given, overrides `ezeka-metadata-fields'."
  (let ((pred (or predicate
                  (plist-get (alist-get field ezeka-metadata-fields)
                             :predicate)
                  #'identity)))
    (cond ((and (functionp pred) (funcall pred value))
           value)
          ((functionp pred)
           (ezeka--validate-metadata-field
            field
            (ezeka--header-deyamlify-value
             (read-string
              (format "`%s' does not satisfy predicate `%s'; fix it: " value pred)
              value
              'ezeka--validate-metadata-field-history))
            pred))
          ((listp pred)
           (mapcar (lambda (v)
                     (ezeka--validate-metadata-field field v (car pred)))
                   value))
          (t
           (signal 'wrong-type-argument (cons 'functionp-or-listp pred))))))

(defun ezeka--decode-header (header &optional file)
  "Return metadata alist decoded from FILE's HEADER.
They keys are converted to symbols."
  (let* ((metadata
          (mapcar
           (lambda (line)
             (when (> (length line) 0)
               (if (string-match ezeka-header-line-regexp line)
                   (let* ((key (intern (match-string 1 line)))
                          (val (match-string 2 line))
                          (deval (ezeka--validate-metadata-field
                                  key
                                  (ezeka--header-deyamlify-value val))))
                     (cons key deval))
                 (signal 'ezeka-error (list "Malformed header line: '%s'" line)))))
           (split-string header "\n" 'omit-nulls "[ ]+")))
         (rubric (ezeka-decode-rubric (alist-get 'rubric metadata))))
    (append rubric metadata)))

(defun ezeka--header-region (buffer)
  "Return a tuple of (START. END) for the header in Ezeka BUFFER."
  (save-excursion
    (with-current-buffer buffer
      (goto-char (point-min))
      (cons (point)
            (if (re-search-forward ezeka-header-separator-regexp nil t)
                (match-end 0)
              (point-max))))))

(defun ezeka--make-header-read-only (buffer)
  "Make the header in the Zettel BUFFER read-only."
  (when ezeka-header-read-only
    (let ((beg-end (ezeka--header-region buffer)))
      (with-current-buffer buffer
        (ezeka--read-only-region (car beg-end) (cdr beg-end))))))

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

(defun ezeka-file-metadata (file)
  "Return an alist of metadata for FILE."
  (save-match-data
    (if-let* ((file (expand-file-name file ezeka-directory))
              (header (ezeka-file-content file 'just-header))
              (mdata (ezeka--decode-header header file))
              (rubric (ezeka-decode-rubric (file-name-base file))))
        (let-alist mdata
          (setf (alist-get '_metadata mdata)
                'from-file)
          (setf (alist-get 'id mdata)
                (or \.id
                    (ezeka-file-name-id file)
                    (file-name-base file))) ; HACK
          (setf (alist-get 'type mdata)
                (or \.type
                    (ezeka-id-type file)))
          (setf (alist-get 'kasten mdata)
                (or \.kasten
                    (ezeka-file-kasten file)))
          (setf (alist-get 'filename mdata)
                (file-name-base file))
          (setf (alist-get 'path mdata)
                (file-relative-name file ezeka-directory))
          (setf (alist-get 'link mdata)
                (or (ignore-errors (ezeka-file-link file))
                    (ignore-errors (ezeka-make-link kasten id))
                    (ezeka--file-name-part file 'id)))
          (setf (alist-get 'caption mdata)
                (or \.caption
                    (ezeka-file-name-caption file)
                    \.title))
          (setf (alist-get 'title mdata)
                (or \.title
                    \.caption))
          (setf (alist-get 'created mdata)
                (or (ezeka--parse-time-string (alist-get 'id rubric))
                    \.created))
          (setf (alist-get 'rubric mdata)
                (ezeka-encode-rubric mdata))
          mdata)
      (signal 'file-error file))))

(defun ezeka-point-at-bob ()
  "Return point at the beginning of the body (BoB)."
  (save-excursion
    (save-match-data
      (goto-char (point-min))
      (if (re-search-forward ezeka-header-separator-regexp nil t)
          (1+ (match-end 0))
        (signal 'ezeka-error "Cannot find where the body begins")))))

;;;=============================================================================
;;; Metadata: Commands
;;;=============================================================================

(defun ezeka-set-metadata-value (metadata field value)
  "Set METADATA's FIELD to VALUE after doing some checking.
Return the original METADATA with the field changed."
  (setf (alist-get field metadata) value)
  metadata)

(defun ezeka--add-oldname (metadata oldname)
  "Add OLDNAME to METADATA, returning the modified metadata."
  (setf (alist-get 'oldnames metadata)
        (remove (alist-get 'id metadata)
                (cl-union (list oldname)
                          (alist-get 'oldnames metadata))))
  metadata)

;; See [[https://help.dropbox.com/organize/file-names]]
(defvar ezeka--pasturize-characters
  '(("/" "_")
    ("\\" "-")
    ("<" "[")
    (">" "]")
    (":" ",")
    ("\"" "")
    ("|" "-")
    ("?" "¿")
    ("*" "_")
    ("." ""))
  "A list of pasturizing replacements.
This list is then passed to `ezeka--replace-in-string'.")

;; by NickD [https://emacs.stackexchange.com/a/75531/41826]
(defun ezeka--unaccent-string (s)
  "Decomposes accented characters in S to produce an ASCII equivalent."
  (let ((_decompose
         (lambda (char)
           (let ((dec (get-char-code-property char 'decomposition)))
            (while (cdr dec)
              (setq dec
                (if (fixnump (car dec))
                    (get-char-code-property (car dec) 'decomposition)
                  (cdr dec))))))))
    (mapconcat (lambda (c)
                 (char-to-string (or (funcall _decompose c) c)))
               s "")))

;; Any way to make this more general without re-implementing half of BibTeX?
(defun ezeka--citekey-from-note-title (title)
  "Parse note's TITLE into a citekey suggestion or NIL."
  (save-match-data
    (let* ((given-name '(1+ (in "A-Z" "a-z" "." "-" " ")))
           (family-name '(1+ (in "A-Za-z-")))
           (title-delimiters '(in "/\"'_"))
           (work-title `(seq ,title-delimiters
                             (1+ anychar)
                             ,title-delimiters))
           (date '(>= 4 (in "0-9" "-" ",")))
           ;; Common patterns
           (first-edition
            (rx-to-string `(seq ,given-name
                                " " word-start
                                (group-n 1 ,family-name)
                                "'s "
                                (group-n 2 ,work-title)
                                " "
                                "(" (group-n 3 ,date) ")")
                          'no-group))
           (republished
            (rx-to-string `(seq ,given-name
                                " " word-start
                                (group-n 1 ,family-name)
                                "'s "
                                (group-n 2 ,work-title)
                                "(" (group-n 3 ,date) ")")
                          'no-group)))
      (when (or (string-match first-edition title)
                (string-match republished title))
        (concat (match-string 1 title) (match-string 3 title))))))

;; See https://help.dropbox.com/organize/file-names
(defun ezeka--pasteurize-file-name (title)
  "Return TITLE after making it safe to use as file caption.
The function attemps to shorten the title, and strip or replace
troublesome characters."
  (interactive (list (file-name-base buffer-file-name)))
  (let* ((given-name '(1+ (in "A-Z" "a-z" "." "-" " ")))
         (family-name '(1+ (in "A-Za-z-")))
         (title-delimiters '(in "/\"'_"))
         (work-title `(seq ,title-delimiters
                           (1+ anychar)
                           ,title-delimiters))
         (date '(>= 4 (in "0-9" "-" ",")))
         (one (list
               (rx-to-string `(seq ,given-name
                                   " " word-start
                                   (group-n 1 ,family-name)
                                   "'s "
                                   (group-n 2 ,work-title)
                                   " "
                                   "(" (group-n 3 ,date) ")"
                                   (0+ " " (group-n 4 (seq (any "&@")
                                                           (1+ word)
                                                           (backref 3)))))
                             'no-group)
               "\\2\\4"
               'regexp))
         (two (list
               (rx-to-string `(seq ,given-name
                                   " " word-start
                                   (group-n 1 ,family-name)
                                   "'s "
                                   (group-n 2 ,work-title)
                                   "(" (group-n 3 ,date) ")"
                                   (0+ " " (group-n 4 (any "&@") (1+ word) (1+ digit))))
                             'no-group)
               "\\2 (\\3) \\4"
               'regexp))
         (complex-replacements (list one two))
         (simple-replacements
          '(("\"\\<\\([^\"]+\\)\\>\"" "'\\1'" regexp)
            ("\\</\\([^/]+\\)/\\>" "_\\1_" regexp)
            ("(\\(ß.+\\))" "[\\1]" regexp)
            ("\\(?1:.*\\) \\(?2:ß.+\\): \\(?3:.*\\)" "\\1 \\3 [\\2]" regexp))))
    (string-trim (apply #'ezeka--replace-in-string
                        (ezeka--unaccent-string title)
                        (append complex-replacements
                                simple-replacements
                                ezeka--pasturize-characters)))))

(defun ezeka--depasturize-for-title (caption)
  "Return CAPTION after trying to reverse `ezeka--pasteurize-file-name'."
  (ezeka--replace-in-string caption
    '("\\<'\\(.*\\)'\\>" "\"\\1\"" regexp)
    '("\\<_\\(.*\\)_\\>" "/\\1/" regexp)))

(defun ezeka--normalize-metadata-timestamps (metadata)
  "Normalize the creation and modification times in METADATA.
The times are modified if either is missing time value (i.e.
just has date) or the time is 00:00. The created time is
taken from tempus currens, if there is a record of one.
Otherwise, use current time. The modification time is set to
current time."
  (let* ((created  (alist-get 'created metadata))
         (modified (alist-get 'modified metadata))
         (tempus (cl-find-if
                  (lambda (id)
                    (eq (ezeka-id-type id) :tempus))
                  (cons (alist-get 'id metadata)
                        (alist-get 'oldnames metadata)))))
    (setf (alist-get 'created metadata)
          (ezeka--complete-time created
                                (if tempus
                                    (ezeka--parse-time-string tempus)
                                  (current-time))))
    (setf (alist-get 'modified metadata)
          (when modified
            (ezeka--complete-time modified)))
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
             (user-error "Invalid current `ezeka-header-update-modified': %s"
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
  (let* ((mdata (ezeka--normalize-metadata-timestamps metadata))
         (now (current-time))
         (last-modified (or (alist-get 'modified mdata)
                            (alist-get 'created mdata)
                            (user-error "No created or modified time in %s"
                                        (alist-get 'link mdata))))
         (elapsed (- (time-to-seconds now) (time-to-seconds last-modified))))
    (unless (<= elapsed 60)             ; less than a minute
      (when (or (eq ezeka-header-update-modified 'always)
                (and (eq ezeka-header-update-modified 'sameday)
                     (= (time-to-days last-modified)
                        (time-to-days (current-time))))
                ;; Automatic updating conditions not met; need to confirm
                (and (member ezeka-header-update-modified '(sameday confirm t))
                     (y-or-n-p
                      (format "%s was last modified at %s. Update to now? "
                              (ezeka-format-metadata "%i {%l} %t" mdata)
                              (ezeka-timestamp last-modified 'full 'brackets)))))
        (setf (alist-get 'modified mdata) now)
        (ezeka--add-to-system-log 'update-modified nil
          'note (ezeka-encode-rubric metadata))
        (run-hooks 'ezeka-modified-updated-hook)))
    mdata))

(defun ezeka-update-modified (file)
  "Update the modification time in the current Zettel FILE's header.
This function ignores the value of `ezeka-header-update-modified',
treating it as if set to 'ALWAYS."
  (interactive (list buffer-file-name))
  (let ((ezeka-header-update-modified 'always))
    (ezeka--update-file-header file nil t)))

(defun ezeka-force-save-buffer (&optional update-modified)
  "Save the current buffer, even if it's unmodified.
Unless specified with UPDATE-MODIFIED, respect the value of
`ezeka-update-modified'. If called interactively with \\[universal-argument], however,
don't update the modification date. With \\[universal-argument] \\[universal-argument],
update it unconditionally."
  (interactive
   (let ((prefix (prefix-numeric-value current-prefix-arg)))
     (list (cond ((= prefix 4) 'never)
                 ((= prefix 16) 'always)
                 (t ezeka-header-update-modified)))))
  (let ((ezeka-header-update-modified update-modified)
        (before-save-hook
         (remove 'ezeka--update-file-header before-save-hook))
        (modified (buffer-modified-p)))
    (unwind-protect
        (when (ezeka-file-p buffer-file-name)
          (set-buffer-modified-p t)
          (ezeka--update-file-header nil nil
                                     (eq ezeka-header-update-modified 'always))
          (save-buffer)
          (shell-command (format "git add -f \"%s\"" buffer-file-name)))
      (set-buffer-modified-p modified))
    (set-buffer-modified-p nil)))

;; There are three different places where files can be captioned or titled:
;; 1) The file name itself might contain a caption;
;; 2) The rubric in the file header should contain caption matching the
;;    caption in the file name; and, finally,
;; 3) The title in the file header contains a nicely formatted version
;;    of the caption that is used when inserting links.

(defun ezeka--reconcile-title-and-caption (file &optional metadata force)
  "Interactively reconcile title and caption in FILE's METADATA.
Returns modifed metadata. If FORCE is non-nil, attempt
reconciling even if CAPTION-STABLE is true."
  (let* ((mdata (or metadata (ezeka-file-metadata file)))
         (caption (or (alist-get 'caption mdata) ""))
         (title (or (alist-get 'title mdata)
                    (alist-get 'rubric mdata)))
         (_capitalp (lambda (c) (and (= ?w (char-syntax c)) (= c (upcase c))))))
    (unless (or (string= (ezeka--pasteurize-file-name title) caption)
                (and (not force)
                     (alist-get 'caption-stable mdata)))
      (let ((choice (read-char-choice
                     (format
                      (concat
                       "Caption: %s\n"
                       "  Title: %s\n"
                       "Press [c/u] to edit caption, [t/l] to edit title;\n"
                       "      [C/U] and [T/L] to use that one for the other;\n"
                       "      [n] or [q] to leave alone as is.")
                      (propertize caption 'face :bold)
                      (propertize title 'face :italic))
                     '(?c ?u ?t ?l ?C ?U ?T ?L ?n ?q))))
        (pcase choice
          ((or ?c ?u ?T ?L) (setf (alist-get 'caption mdata)
                                  (ezeka--minibuffer-edit-string
                                   (ezeka--pasteurize-file-name
                                    (if (funcall _capitalp choice)
                                        title
                                      caption)))))
          ((or ?t ?l ?C ?U) (setf (alist-get 'title mdata)
                                  (ezeka--minibuffer-edit-string
                                   (if (funcall _capitalp choice)
                                       (ezeka--depasturize-for-title caption)
                                     title))))))
      (setf (alist-get 'caption-stable mdata) t))
    (funcall clear-message-function)
    mdata))

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
          (setf (alist-get 'rubric metadata)
                (ezeka-encode-rubric metadata 'stable-mark))
          (delete-region (point-min) (point-max))
          (mapc (lambda (cons)
                  (when cons
                    (insert (format "%s: %s\n"
                                    (ezeka--header-yamlify-key (car cons))
                                    (ezeka--header-yamlify-value (cdr cons))))))
                (mapcar (lambda (field)
                          (when (and (not (plist-get (cdr field) :hidden))
                                     (alist-get (car field) metadata))
                            (cons (car field)
                                  (alist-get (car field) metadata))))
                        ezeka-metadata-fields))
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
  (interactive (list buffer-file-name nil current-prefix-arg))
  (let* ((fname (or filename buffer-file-name))
         (mdata (or metadata (ezeka-file-metadata fname)))
         (force (or force current-prefix-arg))
         (prev (assoc-string fname ezeka--previously-updated))
         (old-point (point))
         (inhibit-read-only t))
    (cond ((not mdata)
           (signal 'ezeka-error '("Cannot update header: can't parse metadata")))
          ((and (not force)
                (string= (buffer-hash) (cadr prev)))
           (message "The header is unchanged"))
          (t
           (setq mdata
             (ezeka--normalize-metadata-timestamps
              (ezeka--maybe-update-modified
               (ezeka--reconcile-title-and-caption fname mdata force))))
           (ezeka--replace-file-header fname mdata)
           (setf (alist-get fname ezeka--previously-updated nil nil #'string=)
                 (list (buffer-hash) mdata))))
    ;; `Save-excursion' doesn't seem to restore the point, possibly because the
    ;; file is changed, so need to do it manually.
    (goto-char old-point)))

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
                  (signal 'ezeka-error (list "File `%s' already exists" caption))))
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

(defvar ezeka--marked-for-rename nil
  "List of files that are marked for rename.")

(defun ezeka--mark-for-rename (filename &optional metadata)
  "Mark FILENAME (with METADATA) for rename."
  (interactive (list buffer-file-name))
  (let ((mdata (or metadata (ezeka-file-metadata filename))))
    (let-alist mdata
      (ezeka-add-keyword filename ezeka-rename-note-keyword nil mdata)
      (when (y-or-n-p "Create a symbolic link meanwhile? ")
        (ezeka--make-symbolic-link filename (ezeka-link-path \.id mdata)))
      (cl-pushnew (ezeka-file-name-id filename)
                  ezeka--marked-for-rename
                  :test #'string=))))

(defcustom ezeka-harmonize-file-name-preference 'ask
  "Can be 'ASK, 'FILENAME, or 'METADATA."
  :type 'symbol
  :options '(ask filename metadata)
  :group 'ezeka)

(defun ezeka-harmonize-file-name (&optional filename metadata force preference)
  "Ensure that FILENAME's captioned name matches the METADATA
When called interactively with \\[universal-argument], or
FORCE is non-nil, offer to set metadata or rename the file
even if they are in agreement. If given, PREFERENCE
overrides `ezeka-harmonize-file-name-preference'."
  (interactive
   (list buffer-file-name
         nil
         current-prefix-arg
         'ask))
  (let* ((filename (or filename buffer-file-name))
         (file-base (file-name-base filename))
         (mdata (if (null metadata)
                    (ezeka-file-metadata filename)
                  (ezeka--update-file-header filename metadata)
                  metadata))
         (mdata-base (ezeka-format-metadata ezeka-file-name-format mdata))
         (preference (or preference ezeka-harmonize-file-name-preference))
         (_read-user-choice
          (lambda ()
            "Prompt the user about which name to use."
            (read-char-choice
             (format (concat "Caption in filename and metadata differ:\n"
                             "[F]ilename: %s\n"
                             "[M]etadata: %s\n"
                             "Press [f/u] to set metadata from filename (uppercase to edit),\n"
                             "      [m/l] to set filename from metadata (uppercase to edit),\n"
                             "      [r] to add %s keyword for renaming later, or\n"
                             "      [n] or [q] to do noting:")
                     (propertize file-base 'face 'bold)
                     (propertize mdata-base 'face 'italic)
                     (propertize ezeka-rename-note-keyword 'face 'bold))
             '(?f ?F ?u ?U ?m ?M ?l ?L ?r ?R ?n ?N ?q ?Q))))
         (keep-which
          (cond ((eq preference 'metadata) ?m)
                ((eq preference 'filename) ?f)
                ((or force
                     (and (not (string= (ezeka--unaccent-string file-base)
                                        (ezeka--unaccent-string mdata-base)))
                          (not (member ezeka-rename-note-keyword
                                       (alist-get 'keywords mdata)))))
                 (funcall _read-user-choice)))))
    (funcall clear-message-function)
    (cond ((memq keep-which '(nil ?n ?q))
           ;; do nothing
           )
          ;; rename
          ((and (memq keep-which '(?r ?R))
                (not (member ezeka-rename-note-keyword
                             (alist-get 'keywords mdata))))
           (ezeka--mark-for-rename filename mdata)
           (ezeka--save-buffer-read-only filename))
          ;; set from filename
          ((memq keep-which '(?f ?F ?u ?U))
           (when (memq keep-which '(?F ?U))
             (setq file-base (ezeka--minibuffer-edit-string file-base)))
           (setf (alist-get 'id mdata)
                 (ezeka-file-name-id file-base)
                 (alist-get 'label mdata)
                 (ezeka-file-name-label file-base)
                 (alist-get 'caption mdata)
                 (ezeka-file-name-caption file-base)
                 (alist-get 'citekey mdata)
                 (ezeka-file-name-citekey file-base)
                 (alist-get 'caption-stable mdata)
                 nil)
           (ezeka--replace-file-header filename mdata)
           (ezeka--save-buffer-read-only filename)
           (run-hooks 'ezeka-after-save-hook))
          ;; set from metadata
          ((memq keep-which '(?m ?M ?l ?L))
           (let ((pasteurized
                  (ezeka--pasteurize-file-name
                   (ezeka-format-metadata ezeka-file-name-format mdata))))
             (setf (alist-get 'keywords mdata)
                   (cl-remove ezeka-rename-note-keyword
                              (alist-get 'keywords mdata)
                              :test #'string=)
                   (alist-get 'caption mdata)
                   pasteurized)
             (ezeka--rename-file filename
                                 (file-name-with-extension
                                  (if (or (member keep-which '(?M ?L))
                                          (not (string= file-base mdata-base)))
                                      (ezeka--minibuffer-edit-string pasteurized)
                                    pasteurized)
                                  ezeka-file-extension))
             (run-hooks 'ezeka-after-save-hook)
             (when t                    ; TODO check if filename changed
               (message "You might want to do `ezeka-harmonize-file-name' again")))))))

;;;=============================================================================
;;; Numerus Currens
;;;=============================================================================

(defun ezeka-make-numerus (letter numbers)
  "Return new numerus currens ID based on LETTER and NUMBERS.
Both LETTER and NUMBERS are strings."
  (let ((result (concat letter "-" numbers)))
    (if (string-match-p (ezeka--id-regexp :numerus) result)
        result
      (signal 'ezeka-error (list "Invalid numerus: %s" result)))))

(defun ezeka-numerus-letter (id)
  "Return the letter part of the ID as a string."
  (when (string-match-p (ezeka--id-regexp :numerus) id)
    (cl-subseq id 2)))

(defun ezeka-numerus-numbers (id)
  "Return the numbers part of the ID as a string."
  (when (string-match-p (ezeka--id-regexp :numerus) id)
    (cl-subseq id 0 1)))

(defun ezeka-numerus-parts (id)
  "Return a list of two elements: the letter and numbers parts of ID.
Return NIL if the ID is not a numerus currens ID."
  (when (string-match-p (ezeka--id-regexp :numerus) id)
    (split-string id "-")))

(defun abase26-letter-to-decimal (letter)
  "Return the decimal number corresponding to LETTER, a string.
Case-insensitive."
  (if (string-match "[a-zA-Z]" letter)
      (- (string-to-char (downcase letter)) ?a)
    (signal 'wrong-type-argument (list 'stringp letter))))

(defun abase26-decimal-to-letter (n)
  "Return a string of number in abase26 corresponding decimal N."
  (if (< -1 n 26)
      (char-to-string (+ n ?a))
    (signal 'wrong-type-argument (list '0-to-25-p n))))

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

(defun ezeka--numerus-subdir-counts (&optional sort-test sort-key)
  "Return an alist of numerus subdirs and number of notes in each.
SORT-TEST is the predicate for sorting based on that SORT-
KEY; the default is #'> (i.e. ascending). SORT-KEY is a
function to access either `car' (the default) or `cdr' of
the tuples in the form (LETTER . COUNT)."
  (let ((letters (mapcar (lambda (letter)
                           (cons letter
                                 (length
                                  (directory-files
                                   (ezeka-id-directory
                                    (ezeka-make-numerus (string letter) "0000"))
                                   nil
                                   (concat "\\." ezeka-file-extension "$")))))
                         (number-sequence ?a ?z))))
    (cl-sort letters (or sort-test #'<) :key (or sort-key #'car))))

(defun ezeka-numerus-subdirectory-distribution (&optional sort-by-count)
  "Displays distribution of notes in the numerus Kasten.
With SORT-BY-COUNT (or \\[universal-argument]), sort by number of notes
in ascending order."
  (interactive "P")
  (with-current-buffer (get-buffer-create "*Numerus Distribution*")
    (erase-buffer)
    (apply 'insert (mapcar (lambda (x) (format "%c: %d note(s)\n" (car x) (cdr x)))
                           (ezeka--numerus-subdir-counts
                            (when sort-by-count '<)
                            (when sort-by-count 'cdr))))
    (view-mode))
  (pop-to-buffer "*Numerus Distribution*"))

(defun ezeka--scantest-numerus-subdirs (&optional n counts)
  "Return a list of numerus subdirs with Nth fewest number of notes.
N can is an integer between 0 (fewest notes) and, depending
on actual counts, some ceiling between 0 (every subdir has
same number of notes) and M, total number of subdirs less
one (every subdir has unique number of notes). If N is not
an integer or is outside of 0..M range, return the subdirs
with most notes. COUNTS are from `ezeka--numerus-subdir-counts'."
  (let* ((counts (if counts
                     (cl-sort (copy-sequence counts) #'< :key #'cdr)
                   (ezeka--numerus-subdir-counts #'< #'cdr)))
         (unique (cl-remove-duplicates (mapcar 'cdr counts)))
         (n (cond ((not n) 0)
                  ((and (integerp n) (< -1 n (length unique))) n)
                  (t (1- (length unique))))))
    (cl-remove-if-not
     (lambda (x) (= (nth n unique) (cdr x)))
     counts)))

(defun ezeka--pregenerated-numerus (filename)
  "Retrieve next numerus from FILENAME."
  (with-current-buffer (find-file-noselect filename)
    (let ((left (count-lines (point-min) (point-max)))
          result)
      (unwind-protect
          (string-trim
           (delete-and-extract-region
            (point-min)
            (search-forward-regexp "[[:space:]]" nil t)))
        (let ((inhibit-message t))
          (basic-save-buffer))
        (message "%d pregenerated numer%s left"
                 left
                 (if (= left 1) "us" "i"))
        nil))))

(defun ezeka--read-new-numerus-currens-method ()
  "Ask the user how to generate the new numeri currentes."
  (pcase (completing-read "Generate a new numerus currens ..."
                          '("automatically" "manually" "randomly" "selectively")
                          nil
                          t)
    ("automatically" 'auto)
    ("randomly" 'random)
    ("manually" 'manual)
    ("selectively" 'selective)))

(defun ezeka-new-numerus-currens (&optional method)
  "Return the next unused numerus currens.
METHOD, if given, overrides `ezeka-new-numerus-currens-method'."
  (interactive (list (ezeka--read-new-numerus-currens-method)))
  (let* ((method (or method ezeka-new-numerus-currens-method))
         (method (if (eq method 'ask)
                     (ezeka--read-new-numerus-currens-method)
                   method))
         (method-desc (format "%s" method))
         (error-msg "")
         (auto-elevate 0)
         (auto-counts (when (eq method 'auto) (ezeka--numerus-subdir-counts)))
         auto-scantest
         (_prompt (lambda (prompt)
                    "Assemble a full prompt based on PROMPT."
                    (format "%s%sMethod: %s\n%s"
                            error-msg
                            (if (string-empty-p error-msg) "" "\n")
                            method-desc prompt)))
         (_already-exists-p
          (lambda (candidate)
            "Returns non-nil if CANDIDATE does not already exists."
            (ignore-errors
              (ezeka-link-file (ezeka-make-link "numerus" candidate)))))
         (_acceptablep
          (lambda (candidate)
            "Check if the CANDIDATE is unique and acceptable to the user."
            (let ((choice
                   (read-char-choice
                    (funcall _prompt
                             (format "Is %s acceptable? (y or n, N to elevate) "
                                     candidate))
                    '(?y ?Y ?n ?N))))
              (cond ((member choice '(?y ?Y)) candidate)
                    ((= choice ?N)
                     (cl-incf auto-elevate)
                     nil)
                    (t nil)))))
         (_random-numerus
          (lambda (char)
            "Generate a random numerus currens starting with CHAR."
            (ezeka-make-numerus (string char) (format "%04d" (random 10000))))))
    (catch 'success
      (while t
        (setq id (pcase method
                   ((pred stringp)
                    (setq method-desc "pregenerated")
                    (ezeka--pregenerated-numerus method))
                   ('manual
                    (ezeka--read-id (funcall _prompt "New numerus currens: ")
                                    :numerus))
                   ('selective
                    (funcall _random-numerus
                             (read-char-choice (funcall _prompt "Starting letter (a-z): ")
                                               (number-sequence ?a ?z))))
                   ('auto
                    (setq auto-scantest (cl-union
                                         auto-scantest
                                         (ezeka--scantest-numerus-subdirs
                                          auto-elevate auto-counts))
                          method-desc (format "auto [%s]"
                                              (mapconcat (lambda (c)
                                                           (string (car c)))
                                                         auto-scantest
                                                         ", ")))
                    (funcall _random-numerus
                             (car (elt auto-scantest
                                       (random (length auto-scantest))))))
                   ('random
                    (funcall _random-numerus
                             (seq-random-elt (number-sequence ?a ?z))))
                   (_
                    (signal 'ezeka-error
                            (list "Don't know how to handle METHOD: %s" method)))))
        (cond ((not (ezeka-id-valid-p id :numerus))
               (setq error-msg
                 (format "`%s' is not a valid numerus currens ID." id)))
              ((funcall _already-exists-p id)
               (setq error-msg
                 (format "A file with ID `%s' already exists." id)))
              ((funcall _acceptablep id)
               (throw 'success id)))))))

;; TODO: Somehow make this part of `ezeka-kasten'. Function?
(defun ezeka--generate-id (kasten &optional batch)
  "Return the next unused ID for the given KASTEN.
If BATCH is non-nil, assume that the user cannot respond to
interactive prompts."
  (let ((type (ezeka-kasten-id-type (ezeka-kasten kasten))))
    (pcase type
      (':tempus (ezeka-tempus-currens))
      (':numerus (ezeka-new-numerus-currens
                  (when (and batch
                             (member ezeka-new-numerus-currens-method
                                     '(ask selective)))
                    'auto)))
      (':scriptum (ezeka-scriptum-id))
      (_ (signal 'ezeka-error (list "Not implemented for ID type `%s'" type))))))

;;;=============================================================================
;;; Tempus Currens
;;;=============================================================================

(defun ezeka-tempus-currens (&optional time)
  "Return a tempus currens ID based on the given Emacs TIME object.
If TIME is nil, default to current time."
  (format-time-string "%Y%m%dT%H%M" time))

(defun ezeka-tempus-currens-id-for (link-or-file &optional interactive)
  "Return tempus currens ID for the given Zettel LINK-OR-FILE.
INTERACTIVE is non-NIL when called interactively."
  (interactive
   (list (ezeka-file-link (ezeka--grab-dwim-file-target))
         (prefix-numeric-value current-prefix-arg)))
  (let ((link (if (ezeka-link-p link-or-file)
                  link-or-file
                (ezeka-file-link link-or-file)))
        (file (if (file-exists-p link-or-file)
                  link-or-file
                (ezeka-link-file link-or-file))))
    (if (eq (ezeka-kasten-id-type (ezeka-kasten (ezeka-link-kasten link)))
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
              ((alist-get 'created mdata)
               (ezeka-tempus-currens
                (ezeka--complete-time (alist-get 'created mdata))))
              (t
               ;; Can't figure out automatically; ask the user
               (ezeka--read-id "No created metadata; make up your own name: "
                               :tempus
                               (ezeka--generate-id (ezeka-link-kasten link)
                                                   interactive))))))))

;;;=============================================================================
;;; Scriptum
;;;=============================================================================

(defun ezeka-scriptum-id (&optional project time)
  "Return a scriptum ID based on PROJECT and Emacs TIME object.
If TIME is nil, default to current time."
  (let ((_scriptum-id (lambda (project n)
                       "Return scriptum ID as a string based on PROJECT and N."
                       (format "%s~%02d" project n))))
    (while (not project)
      ;; FIXME: Do I need a native function for this?
      (setq project
        (if (fboundp #'octavo--select-file)
            (ezeka-octavo-with-kasten "numerus"
              (ezeka-file-link (octavo--select-file "Select project: ")))
          (ezeka--read-id "Scriptum project (numerus currens): " :numerus)))
      (unless (ezeka-link-file project)
        (setq project nil)))
    ;; TODO: If this is first entry in scriptum project, create a project
    ;; heading <numerus>~00 with caption for the project? Or a symbolic link
    ;; to numerus?
    (funcall _scriptum-id
             project
             (cl-find-if-not (lambda (n)
                               (ezeka-link-file
                                (funcall _scriptum-id project n)))
                             (number-sequence 1 99)))))

;;;=============================================================================
;;; Zettel Links
;;;=============================================================================

(defvar ezeka--new-child-metadata nil
  "An alist of new children and their metadata.")

;; TODO Metadata really should be a `defstruct'
(defun ezeka--set-new-child-metadata (link metadata &rest plist)
  "Set the metadata property values for LINK.
If METADATA is given, set the child metadata to that with
modificationbs specified in PLIST."
  (declare (indent 2))
  (cond ((not (null metadata)))
        ((and plist (zerop (mod (length plist) 2)))
         (while plist
           (push (cons (car plist) (cadr plist)) metadata)
           (setq plist (cddr plist))))
        (t
         (signal 'wrong-type-argument (list 'key-value-pairs-p plist))))
  (setf (alist-get link ezeka--new-child-metadata nil nil #'string=)
        metadata))

(defun ezeka--new-child-metadata (link)
  "Return metadata alist for child LINK."
  (alist-get link ezeka--new-child-metadata nil nil #'string=))

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
  (let ((truename (file-truename file)))
    (run-hook-with-args 'ezeka-find-file-functions
                        file
                        (if (ezeka-file-p buffer-file-name)
                            buffer-file-name
                          'find-file))
    (if same-window
        (find-file truename)
      (cl-case ezeka-number-of-frames
        (two (if (< (length (frame-list)) 2)
                 (find-file-other-frame truename)
               (when (featurep 'ace-window)
                 (select-window (ace-select-window)))
               (find-file truename)))
        (one (let ((pop-up-windows t))
               (when (featurep 'ace-window)
                 (select-window (ace-select-window)))
               (find-file truename)))
        (nil (find-file truename))
        (t (find-file-other-frame truename))))))

(defun ezeka-find-file-replace-placeholder (file &optional same-window)
  "Find the given FILE, offering to replace it if it's a placeholder.
If SAME-WINDOW is non-NIL, open the buffer visiting the file
in the same window."
  (if (or (not (file-symlink-p file))
          (and (file-symlink-p file)
               (string= "follow"
                        (completing-read "What to do with this symbolic link? "
                                         '(follow create)
                                         nil 'require-match nil nil "follow"))))
      (ezeka-find-file file same-window)
    (when (y-or-n-p "Delete symlink and create a new note? ")
      (ezeka-replace-placeholder file))))

(defun ezeka-find-link (link &optional same-window)
  "Find the given LINK.
If SAME-WINDOW (or \\[universal-argument]) is non-NIL, opens
the link in the same window. Return T if the link is a
Zettel link."
  (interactive (list (ezeka--read-id "Link to find: ")
                     current-prefix-arg))
  (when (ezeka-link-p link)
    (let ((file (ezeka-link-file link))
          (buf (cl-find link (buffer-list)
                        :key #'buffer-name
                        :test #'string-match-p)))
      (cond (file (ezeka-find-file-replace-placeholder file same-window))
            (buf  (if same-window
                      (pop-to-buffer-same-window buf)
                    (pop-to-buffer buf)))
            ((ezeka-note-moved-p link nil 'ask))
            ((or (eq ezeka-create-nonexistent-links t)
                 (and (eq ezeka-create-nonexistent-links 'confirm)
                      (y-or-n-p
                       (format "Link `%s' doesn't exist. Create? " link))))
             (when-let ((_ (ezeka-file-p buffer-file-name))
                        (parent (ezeka-file-link buffer-file-name)))
               (ezeka--set-new-child-metadata link nil 'parent parent))
             (ezeka-find-file
              (ezeka-link-path link (ezeka--new-child-metadata link))
              same-window)
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

;; TODO Remove, since not used anywhere
(defun ezeka--link-with-metadata (link &optional fields where metadata)
  "Return a string with metadata FIELD(S) at place WHERE (relative to LINK).
WHERE can be any of :before (default), :after, :instead, and
:description. FIELDS defaults to 'title, WHERE to :before.
If WHERE is :instead, do not include the LINK."
  (let* ((mdata (or metadata (ezeka-file-metadata (ezeka-link-file link))))
         (fields (or fields '(title)))
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

(defun ezeka-insert-with-spaces (&rest strings)
  "Insert STRINGS at point surrounded by spaces as appropriate.
STRINGS are themselves concatenated with spaces in between."
  (insert (if (or (bolp) (ezeka--space-or-punct-p (char-before))) "" " ")
          (string-trim (mapconcat #'identity strings " "))
          (if (or (eolp) (ezeka--space-or-punct-p (char-after))) "" " ")))

(defvar ezeka-insert-link-hook nil
  "Normal hook that is run after `ezeka-insert-link' and friends.")

(defun ezeka--insert-link-with-spaces (link &rest strings)
  "Insert LINK at point.
If STRINGS is non-nil, LINK is not actually inserted, just
assumed to be contained in some of them."
  (let* ((breadcrumbs
          (save-excursion
            (re-search-forward
             ezeka-breadcrumbs-trail-headline nil 'noerror)))
         (already-linked
          (cond ((save-excursion (re-search-backward link nil 'noerror))
                 'above)
                ((save-excursion (re-search-forward link breadcrumbs 'noerror))
                 'below))))
    (when (or (not already-linked)
              (y-or-n-p (format "There's already a link there %s. Insert anyway? "
                                already-linked)))
      (if strings
          (apply #'ezeka-insert-with-spaces strings)
        (ezeka-insert-with-spaces (ezeka--format-link link)))
      (run-hooks 'ezeka-insert-link-hook))))

(defun ezeka-insert-link-with-metadata (link &optional fields where noedit)
  "Insert the Zettel LINK, optionally adding metadata FIELD(S).
WHERE (:before, :after, or in :description) determines where
the fields are added. FIELDS can be a list. If NOEDIT is
non-nil, insert the link without allowing the user to
interactively edit the text."
  (interactive
   (list (ezeka--read-id "Link to insert: ")
         ;; FIXME Where argument is completely ignored
         (list (ezeka--read-metadata-field "Which field? "))
         (intern-soft (completing-read "Where? " '(":before" ":after")))))
  (if-let* ((_ (car fields))
            (desc-fmt (mapconcat
                       (lambda (f)
                         (format "%%%c"
                                 (plist-get (alist-get f ezeka-metadata-fields)
                                            :format)))
                       fields
                       " "))
            (file (or (ezeka-link-file link)
                      (cl-find-if #'(lambda (buf)
                                      (string-match link (buffer-name buf)))
                                  (buffer-list))))
            (mdata (if (file-symlink-p file)
                       (ezeka-decode-rubric (file-name-base file))
                     (ezeka-file-metadata file)))
            (desc-string (ezeka-format-metadata desc-fmt mdata)))
      (ezeka--insert-link-with-spaces link
                                      (if noedit
                                          desc-string
                                        (read-string "Insert: " desc-string))
                                      (ezeka--format-link link))
    (ezeka--insert-link-with-spaces link)))

(defun ezeka--select-file (files &optional prompt require-match)
  "Select from among Zettel FILES, presenting optional PROMPT.
If REQUIRE-MATCH is non-nil, require match, otherwise treat entered
text as a Zettel link."
  (when files
    (let* ((table (ezeka-completion-table files))
           (_collection (lambda (string predicate action)
                          (if (eq action 'metadata)
                              '(metadata (category . ezeka-file))
                            (complete-with-action
                             action table string predicate)))))
      (gethash (completing-read (or prompt "Select Zettel: ")
                                _collection
                                nil
                                require-match)
               table))))

(defun ezeka-insert-link-to-visiting (arg)
  "Insert a link to another Zettel being currently visited.
With \\[universal-argument] ARG offers a few options for including
Zettel metadata. If the user selects a Zettel that does not exist in
the list, just insert the link to what was selected. If the cursor in
already inside a link, replace it instead."
  (interactive "P")
  (let ((link (ezeka-file-link
               (ezeka--select-file (ezeka-visiting-files-list)
                                   "Insert link to: " t))))
    (if link
        (if (not (ezeka-link-at-point-p))
            (if arg
                (funcall-interactively #'ezeka-insert-link-with-metadata link)
              (ezeka-insert-link-with-metadata link '(title) :before))
          ;; When replacing, don't including anything
          (delete-region (match-beginning 0) (match-end 0))
          (ezeka--insert-link-with-spaces link))
      (message "No visiting Zettel"))))

(defun ezeka--note-in-other-window ()
  "Return the file name to the Zettel note in the other window.
If there is no other window or the file is not a Zettel
note, return nil."
  (when-let* ((other-win (cond ((one-window-p t 'visible)
                                nil)
                               ((and (> (count-windows nil 'visible) 2)
                                     (featurep 'ace-window))
                                (aw-select " Ace - Window"))
                               ((> (count-windows nil 'visible) 2)
                                (user-error "There are more than one `other-window's"))
                               (t
                                (other-window-for-scrolling))))
              (other-buf (window-buffer other-win))
              (file (or (buffer-file-name other-buf)
                        (with-current-buffer other-buf
                          (ezeka--grab-dwim-file-target))))
              (_ (ezeka-file-p file)))
    file))

(defun ezeka-insert-link-to-other-window (&optional link-only rubric)
  "Insert the link to the Zettel note in the other window.
By default, include the title. With LINK-ONLY (or
\\[universal-argument]), insert just the link; with RUBRIC,
(or \\[universal-argument] \\[universal-argument]) insert
the rubric instead."
  (interactive
   (list (equal current-prefix-arg '(4))
         (equal current-prefix-arg '(16))))
  (let ((other-file (ezeka--note-in-other-window)))
    (cond (link-only
           (ezeka--insert-link-with-spaces other-file))
          (rubric
           (ezeka-insert-link-with-metadata other-file '(rubric) :before))
          (t
           (ezeka-insert-link-with-metadata
            other-file
            (list (ezeka--read-metadata-field "Which field? ")))))))

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
              (ezeka-insert-link-with-metadata link '(title) :before t))
          ;; When replacing, don't including anything
          (delete-region (match-beginning 0) (match-end 0))
          (ezeka--insert-link-with-spaces link))
      (message "No visiting Zettel"))))

(defun ezeka-insert-link-from-clipboard (arg)
  "Insert link with metadata to the LINK in the OS clipboard.
See `ezeka-insert-link-with-metadata' for details. With \\[universal-argument] ARG,
insert just the link itself."
  (interactive "P")
  (let ((link (gui-get-selection 'CLIPBOARD))
        (backlink (when buffer-file-name
                    (ezeka-file-link buffer-file-name))))
    (when (ezeka-link-p link)
      (if arg
          (ezeka-insert-link-with-metadata link)
        (ezeka-insert-link-with-metadata link '(title) :before t))
      (when backlink
        (gui-set-selection 'CLIPBOARD backlink)
        (message "Backlink to %s copied to clipboard" backlink)))))

(defun ezeka--kill-ring-clipboard-save (text)
  "Save TEXT to kill ring and GUI clipboard."
  (kill-new text)
  (unless select-enable-clipboard
    (gui-set-selection 'CLIPBOARD text))
  (message "Saved [%s] in the kill ring & clipboard" text))

(defvar ezeka--krsmf-time-format-history nil
  "History variable for `ezeka-kill-ring-save-metadata-field'.")

(defun ezeka-kill-ring-save-metadata-field (field &optional time-format)
  "Save the given metadata FIELD to kill ring and system clipboard.
FIELD should be one of `ezeka-metadata-fields'. If the point
is at Zettel link, use that; otherwise, the current buffer.
With non-nil TIME-FORMAT, format time accordingly."
  (interactive
   (list (intern-soft
          (completing-read "Which field? "
                           ezeka-metadata-fields
                           nil
                           t))
         nil
         current-prefix-arg))
  (when-let* ((file (ezeka--grab-dwim-file-target))
              (link (ezeka-file-link file))
              (mdata (ezeka-file-metadata file))
              (value (alist-get field mdata))
              (formatted
               (pcase value
                 ((pred stringp)
                  value)
                 ((pred ezeka--timep)
                  (let ((time-format
                         (or (when (called-interactively-p 'any)
                               (read-string "How to format time values? "
                                            (concat "[" (cdr ezeka-timestamp-formats) "]")
                                            'ezeka--krsmf-time-format-history))
                             (cdr ezeka-timestamp-formats))))
                    (format-time-string time-format value)))
                 (_ (format "%s" value)))))
    (ezeka--kill-ring-clipboard-save formatted)
    (when insert (insert formatted))))

(defun ezeka-kill-ring-save-link (arg)
  "Save in kill ring the Zettel link at point or in Zettel buffer.
With \\[universal-argument] ARG, save the file name relative to `ezeka-directory'.
With \\[universal-argument] \\[universal-argument], open the file in Finder with it selected."
  (interactive "p")
  (let ((file (ezeka--grab-dwim-file-target t)))
    (when file
      (let ((link (if (= arg 4)
                      (file-relative-name (file-truename file)
                                          (file-truename ezeka-directory))
                    (ezeka-file-link file))))
        (ezeka--kill-ring-clipboard-save link)
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
        (ezeka--kill-ring-clipboard-save link)))))

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
  (when (and (ezeka-file-p buffer-file-name)
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
            (let ((words (split-string (alist-get 'title metadata))))
              (setq-local mode-line-misc-info
                          (replace-regexp-in-string
                           "/" "" (mapconcat #'identity
                                    (cl-subseq words 0 (min 5 (length words)))
                                    " "))))))))))

(defun ezeka-update-link-description (&optional field delete)
  "Replace text from point to next Zettel link with its title.
If given (or called with \\[universal-argument]), FIELD specifies a different
field (see `ezeka-metadata-fields'). With DELETE (or \\[universal-argument] \\[universal-argument]), delete
the text instead."
  (interactive
   (list (if (equal current-prefix-arg '(4))
             (ezeka--read-metadata-field))
         (equal current-prefix-arg '(16))))
  (save-excursion
    ;; if already inside a link, go to the start
    (when (eq :link (caar (org-context)))
      (re-search-backward "\\[\\[" (point-at-bol) 'noerror))
    ;; if char under cursor is start of link, back up to BOF
    (while (or (char-equal (following-char) ?\[)
               (= (preceding-char) 0))  ; BOF
      (backward-char))
    (let ((start (point))
          (eop (save-excursion
                 (forward-paragraph)
                 (point))))
      ;; Cannot use `org-next-link', since it ignores links in comments
      (when (re-search-forward "\\[\\[" eop 'noerror)
        (goto-char (match-beginning 0)))
      (when-let* ((_ (ezeka-link-at-point-p))
                  (link (ezeka-link-at-point))
                  (file (ezeka-link-file link))
                  (text (alist-get (or field 'title) (ezeka-file-metadata file))))
        (delete-region start (point))
        (unless delete
          (ezeka-insert-with-spaces text " "))))))

;;;=============================================================================
;;; Link hints via overlays
;;;=============================================================================

(defcustom ezeka-make-help-echo-overlays t
  "Make help echo overlays with link's filename."
  :group 'ezeka
  :type 'boolean)

(defun ezeka--make-help-echo-overlay-at-pos (&optional pos)
  "Make a help-echo overlay at POS (or `point')."
  (save-match-data
    (save-excursion
      (goto-char (or pos (point)))
      (when-let* ((_ (or (thing-at-point-looking-at (ezeka-link-regexp))
                         (and (backward-to-word 1)
                              (thing-at-point-looking-at (ezeka-link-regexp)))))
                  (file (ezeka-link-file (match-string 1)))
                  (overlay (make-overlay (match-beginning 1) (match-end 1))))
        (overlay-put overlay 'type 'ezeka-help-echo)
        (overlay-put overlay 'face '(:underline "purple"))
        (overlay-put overlay 'help-echo (file-name-base file))))))
;; (add-hook 'ezeka-octavo-insert-link-hook 'ezeka--make-help-echo-overlay-at-pos)

(defun ezeka--make-help-echo-overlay (match-data)
  "Make a help-echo overlay for Zettel ID based on MATCH-DATA."
  (save-match-data
    (set-match-data match-data)
    (when-let* ((file (ignore-errors
                        (ezeka-link-file (match-string-no-properties 1))))
                (overlay (make-overlay (match-beginning 1) (match-end 1))))
      (overlay-put overlay 'type 'ezeka-help-echo)
      (overlay-put overlay 'face '(:underline "purple"))
      (overlay-put overlay 'help-echo (file-name-base file)))))

(defun ezeka--make-help-echo-overlays (&optional buffer)
  "Make help echo overlays in BUFFER (or `current-buffer')."
  (interactive)
  (with-current-buffer (or buffer (current-buffer))
    (save-excursion
      (remove-overlays (point-min) (point-max) 'type 'ezeka-help-echo)
      (goto-char (point-min))
      (let ((overlayable
             (rx (or (group-n 5 (seq "[[" (regexp (ezeka-link-regexp)) "]]"))
                     (group-n 5 (seq bol "parent: " (regexp (ezeka-link-regexp))))))))
        (when ezeka-make-help-echo-overlays
          (while (re-search-forward overlayable nil t)
            (ezeka--make-help-echo-overlay (match-data))))))))

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
                                            'parent
                                          'firstborn)
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
  (when (ezeka-file-p buffer-file-name)
    (let ((ancestor (ezeka-trace-genealogy buffer-file-name n)))
      (if ancestor
          (ezeka-find-link ancestor same-window)
        (message "No ancestor of degree %d found" n)))))

(defun ezeka-find-descendant (n)
  "Open the current Zettel's immediate descendant.
With a prefix argument, try to find the Nth ancestor."
  (interactive "p")
  (when (ezeka-file-p buffer-file-name)
    (let ((descendant (ezeka-trace-genealogy buffer-file-name (- n))))
      (if descendant
          (ezeka-find-link descendant)
        (message "No descendant found")))))

(defun ezeka-insert-ancestor-link (arg)
  "Insert a link with title to the ancestor of the current Zettel.
With a numerical prefix ARG'ument, try to find Nth ancestor. With a
\\[universal-argument], insert with title without confirmation."
  (interactive "P")
  (let* ((degree (if (integerp arg) arg 1))
         (link (ezeka-trace-genealogy buffer-file-name degree)))
    (if link
        (if arg
            (ezeka-insert-link-with-metadata link '(title) :before 'noedit)
          (ezeka-insert-link-with-metadata link '(title) :before))
      (message "Could not find such ancestor"))))

(defun ezeka--generate-new-child (parent &optional kasten id)
  "Generate a new child in the same Kasten as PARENT link.
If KASTEN is given, use that kasten instead. Return a fully qualified
link to the new child. If ID is non-nil, use that instead of
generating one."
  (let* ((kasten (or kasten (ezeka-link-kasten parent)))
         (child-link
          (ezeka-make-link kasten
                           (or id (ezeka--generate-id kasten)))))
    (when parent
      (ezeka--set-new-child-metadata child-link nil 'parent parent))
    child-link))

(defun ezeka-new-note-or-child (kasten &optional parent noselect manual)
  "Create a new note in KASTEN as an orphan or with optional PARENT.
If NOSELECT (or \\[universal-argument]) is given, don't open the new
note. If MANUAL is non-nil (or double \\[universal-argument]) is
given, allow the user to enter the ID manually. Return link to the
note."
  (interactive
   (let ((manual (when (equal current-prefix-arg '(16))
                   (ezeka--read-id "ID for the new note: ")))
         (noselect (equal current-prefix-arg '(4))))
     (list (if manual (ezeka-link-kasten manual) (ezeka--read-kasten))
           (when (ezeka-file-p buffer-file-name t)
             (ezeka-file-link buffer-file-name))
           noselect
           manual)))
  (let ((link (if parent
                  (ezeka--generate-new-child parent kasten manual)
                (ezeka-make-link kasten
                                 (or manual
                                     (ezeka--generate-id kasten)))))
        (ezeka-create-nonexistent-links t))
    (unless noselect
      (ezeka-find-link link))
    link))

(defun ezeka--possible-new-note-title ()
  "Return a possible title for a new Zettel note based on context."
  (interactive)
  (if (region-active-p)
      (buffer-substring-no-properties (region-beginning) (region-end))
    (let ((context (buffer-substring-no-properties
                    (point)
                    (max (point-min)
                         (save-excursion
                           (forward-line -1)
                           (point))))))
      (string-trim-left
       (replace-regexp-in-string "[\t\n\r]+" " " context)
       "[ +*-]*"))))

(defun ezeka-insert-new-child-with-title (arg title &optional id)
  "Create a new child with given TITLE, inserting its link at point.
If TITLE is not given, use text on the current line before point.
With \\[universal-argument] ARG, create the child in the same Kasten
as the current note. With \\[universal-argument] \\[universal-argument], ask for ID."
  (interactive
   (list current-prefix-arg
         (ezeka--read-title "Title for the child: "
                            (ezeka--possible-new-note-title))
         (when (equal current-prefix-arg '(16))
           (ezeka--read-id "ID for the child: "))))
  (let* ((parent (ezeka-file-name-id buffer-file-name))
         (citekey (ezeka--read-citekey
                   "Citekey? "
                   (ezeka--citekey-from-note-title title)))
         (kasten (cond (id
                        (ezeka-link-kasten id))
                       ((equal arg '(4))
                        (ezeka-file-kasten buffer-file-name))
                       (t
                        (ezeka--read-kasten "Kasten for new child: "))))
         (child-link (ezeka--generate-new-child parent kasten id))
         (metadata `((id . ,child-link)
                     (kasten . ,kasten)
                     (title . ,title)
                     (caption . ,(ezeka--pasteurize-file-name title))
                     (parent . ,parent)
                     (citekey . ,citekey))))
    (ezeka--insert-link-with-spaces child-link)
    (ezeka--set-new-child-metadata child-link metadata)
    (ezeka--add-to-system-log 'new-child nil
      'child (ezeka-encode-rubric metadata)
      'parent parent)
    (if-let* ((_ (eq 'placeholder
                     (intern (completing-read
                              "Create a new note or just a placeholder? "
                              '(new-note placeholder) nil 'require))))
              (genus (ezeka--read-genus "Genus" 'verbose "ψ" 'require-match))
              (child-path (ezeka-link-path
                           child-link
                           (cons `(label . ,genus) metadata)))
              (link-to (ezeka--read-id "Symbolic link to: " nil parent 'required))
              (link-target (file-relative-name
                            (ezeka-link-file link-to)
                            (file-name-directory child-path))))
        (progn
          (ezeka--add-to-move-log child-link
                                  link-to
                                  (alist-get 'caption metadata)
                                  "Placeholder")
          (ezeka--make-symbolic-link link-target child-path))
      (ezeka-find-link child-link))))

;;;=============================================================================
;;; Buffers and Frames
;;;=============================================================================

(defun ezeka-visiting-files-list (&optional skip-current modified-only)
  "Return a list of Zettel files that are currently being visited.
If SKIP-CURRENT is non-nil, remove the current buffer. If
MODIFIED-ONLY is non-nil, only list modified buffers."
  (nreverse
   (mapcar #'buffer-file-name
           (cl-remove-if-not (lambda (buf)
                               (and (ezeka-file-p (buffer-file-name buf))
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
              (ezeka-visiting-files-list t))
      (while t
        (let* ((table (ezeka-completion-table (ezeka-visiting-files-list t)))
               (choice (completing-read "Kill buffer: " table nil t)))
          (kill-buffer (get-file-buffer (gethash choice table))))))))

(defun ezeka-formatted-frame-title ()
  "Return string suitable for `frame-title-format'.
This is a way to consistently format the frame title with useful
information for Zettelkasten work."
  (interactive)
  (concat (if (ezeka-file-p buffer-file-name)
              (let ((metadata (ezeka-file-metadata buffer-file-name)))
                (format "%s §%s@%s"
                        (alist-get 'title metadata)
                        (alist-get 'id metadata)
                        (alist-get 'kasten metadata)))
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
       (format "%s%s%s" (alist-get 'title metadata)
               (if (alist-get 'citekey metadata) " " "")
               (or (alist-get 'citekey metadata) ""))))))

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
           (format "%s%s%s" (alist-get 'title metadata)
                   (if (alist-get 'citekey metadata) " " "")
                   (or (alist-get 'citekey metadata) ""))
           'face '(:slant italic :height 0.9)))
      (setq mode-line-misc-info octavo-index-mode-line-orig))))

(defun ezeka-completion-table (files &optional get-metadata)
  "Turn list of FILES into completion table suitable for `completing-read'.
If given, GET-METADATA specifies whether to get each file's
metadata, which can be expensive with many FILES, or rely
purely on the file name."
  (when files
    (let* ((n (length files))
           (table (make-hash-table :test #'equal :size n)))
      (dolist (f files table)
        (puthash (if get-metadata
                     (ezeka-encode-rubric (ezeka-file-metadata f))
                   (ezeka-format-file-name "%f" f))
                 f
                 table)))))

(defun ezeka--select-buffer (&optional skip-current modified-only prompt)
  "Select an open Zettel buffer, returning its filename.
If SKIP-CURRENT is non-nil, skip current buffer. If
MODIFIED-ONLY is non-nil, show only modified buffers.
PROMPT, if specified, replaces the default one."
  (let* ((files (nreverse (ezeka-visiting-files-list skip-current modified-only)))
         (table (ezeka-completion-table files 'use-metadata))
         (prompt (or prompt "Select Zettel buffer: "))
         ;; Disabling sorting preserves the same order as with `switch-to-buffer'
         ;; FIXME: How to do this without relying on vertico?
         (vertico-sort-function nil))
    (when files
      (get-file-buffer
       (gethash (completing-read prompt table nil t) table)))))

(defun ezeka-switch-to-buffer (&optional modified-only)
  "Select and switch to another open Zettel buffer.
If MODIFIED-ONLY (or \\[universal-argument]) is non-nil, show only
modified buffers."
  (interactive "P")
  (if-let ((buf (ezeka--select-buffer 'skip-current modified-only)))
      (progn
        (ezeka-breadcrumbs-drop (buffer-file-name buf) buffer-file-name)
        (switch-to-buffer buf))
    (user-error "There are no open Zettel buffers")))

(defun ezeka-switch-to-buffer-other-window (&optional modified-only)
  "Select and switch to another open Zettel buffer in the other window.
If MODIFIED-ONLY (or \\[universal-argument]) is non-nil, show only
modified buffers."
  (interactive "P")
  (let ((buf (ezeka--select-buffer 'skip-current modified-only)))
    (ezeka-breadcrumbs-drop (buffer-file-name buf) buffer-file-name)
    (switch-to-buffer-other-window buf)))

(defun ezeka-switch-to-other-buffer (buffer-or-name &optional norecord force-same-window)
  "Like `switch-to-buffer', but only list non-Zettel buffers.
See `switch-to-buffer' for details about BUFFER-OR-NAME,
NORECORD, and FORCE-SAME-WINDOW. With \\[universal-argument], switch only
to buffers visiting files."
  (interactive
   (let ((force-same-window
          (unless switch-to-buffer-obey-display-actions
            (cond
             ((window-minibuffer-p) nil)
             ((not (eq (window-dedicated-p) t)) 'force-same-window)
             ((pcase switch-to-buffer-in-dedicated-window
                ('nil
                 (user-error "Cannot switch buffers in a dedicated window"))
                ('prompt
                 (if (y-or-n-p
                      (format "Window is dedicated to %s; undedicate it?"
                              (window-buffer)))
                     (progn
                       (set-window-dedicated-p nil nil)
                       'force-same-window)
                   (user-error
                    "Cannot switch buffers in a dedicated window")))
                ('pop nil)
                (_ (set-window-dedicated-p nil nil) 'force-same-window))))))
         (our-buffers (mapcar #'get-file-buffer (ezeka-visiting-files-list))))
     (list (read-buffer "Switch to non-Zettel buffer: "
                        (other-buffer (current-buffer))
                        (confirm-nonexistent-file-or-buffer)
                        (lambda (bname)
                          "Return T if BNAME is not an Ezeka buffer."
                          (let ((bname (if (stringp bname) bname (car bname))))
                            (and (not (member (get-buffer bname) our-buffers))
                                 (buffer-file-name (get-buffer bname))))))
           nil force-same-window)))
  (switch-to-buffer buffer-or-name norecord force-same-window))

;;;=============================================================================
;;; Labels
;;
;; A label is either a genus (for numerus currens notes) or category (for tempus
;; currens notes). By default, it is the value shown between curly brackets
;; {...} in the note's rubric.
;;;=============================================================================

(defun ezeka--validate-label (label)
  "Return the validated LABEL when it is, or NIL otherwise."
  (rx-let ((genus (eval (cons 'any (mapcar #'cadr ezeka-genera)))))
    (when (string-match-p (rx string-start
                              (or genus (one-or-more alpha))
                              string-end)
                          label)
      label)))

(defvar ezeka--read-category-history nil
  "History of manually entered categories.")

(defun ezeka--read-category (&optional prompt custom default sort-fn)
  "Use `completing-read' to select a category from `ezeka-categories'.
Optional PROMPT allows customizing the prompt, while DEFAULT
specifies initial input. If CUSTOM is non-nil, asks the user
to type in the category directly. If SORT-FN is given, use
that to sort the list first."
  (let* ((prompt (or prompt "Category: "))
         (cats (if (not (functionp sort-fn))
                   ezeka-categories
                 (cl-sort (cl-delete-duplicates ezeka-categories :test #'string=)
                          sort-fn)))
         (cat (if custom
                  (read-string prompt default 'ezeka--read-category-history)
                (completing-read prompt cats nil nil default
                                 'ezeka--read-category-history))))
    (cl-pushnew cat ezeka-categories)
    cat))

(defun ezeka--completing-read-char (prompt choices &optional choice-format)
  "Use `completing-read' to read one of CHOICES after PROMPT.
CHOICES should be an alist of (CHARACTER [ZERO-OR-MORE
FIELDS]) elements. CHOICE-FORMAT is applied to `format' with
the full CHOICES element. Return one of the characters."
  (let ((table
         (mapcar (lambda (x)
                   (cons (apply #'format (or choice-format "%s") x)
                         (pcase (car x)
                           ((pred stringp) (car x))
                           ((pred characterp) (string (car x)))
                           (_
                            (signal 'wrong-type-argument
                                    (list 'string-or-character-p x))))))
                 choices)))
    (elt (cdr
          (assoc-string (completing-read prompt table nil t)
                        table))
         0)))

(defun ezeka--read-genus (&optional prompt verbose default require-match)
  "Read a genus as defined in `ezeka-genera'.
Return a string containing the genus letter or an empty
string (unless REQUIRE-MATCH is non-nil). If PROMPT is non-
nil, use that prompt instead of the default. If VERBOSE is
'CHOICES, show a list of choices with explantions, if T just
offer basic info after PROMPT, and NIL not to show anything.
DEFAULT is the genus used if user just presses [return]."
  (catch 'done
    (while t
      (let ((result
             (if (eq verbose 'choices)
                 (ezeka--completing-read-char (or prompt "Genus: ")
                                              ezeka-genera
                                              "%c (%c) ⇒ %s")
               (read-char-choice
                (concat (or prompt "Genus")
                        (if verbose
                            (format " (Latin character, `?' to list choices%s): "
                                    (cond ((and verbose (stringp default))
                                           (format ", or RETURN for \"%s\"" default))
                                          ((and verbose (not require-match))
                                           ", or RETURN to leave blank")
                                          (t "")))
                          (unless prompt ": ")))
                (append '(?? ?\C-m) (mapcar #'car ezeka-genera))))))
        (cond ((and (= result ?\C-m) (stringp default))
               (throw 'done default))
              ((or (= result ??)
                   (and (= result ?\C-m) require-match))
               (setq verbose 'choices))
              ((assq result ezeka-genera)
               (throw 'done
                      (char-to-string (cadr (assq result ezeka-genera)))))
              (t
               (setq prompt "No such genus; try again. ")))))))

(defun ezeka--read-label (kasten &optional special prompt default)
  "Interactively read label for the given KASTEN.
Pass SPECIAL, PROMPT, and DEFAULT to the appropriate
function."
  (if (eq :numerus (ezeka-kasten-id-type (ezeka-kasten kasten)))
      (ezeka--read-genus prompt special default)
    (ezeka--read-category prompt special default)))

(defun ezeka--update-metadata-values (filename metadata &rest args)
  "Update FILENAME's header, replacing METADATA values with new ones.
Afterwards, save the file while ignoring its read only status. If
METADATA is not given, read it from file first. The rest of the ARGS
should consist of KEY and VALUE pairs.

\(fn FILENAME METADATA &REST KEY VAL KEY VAL ...)"
  (declare (indent 2))
  (when (/= (logand (length args) 1) 0)
    (signal 'wrong-number-of-arguments (list 'setf (length args))))
  (let* ((already-open (get-file-buffer filename))
         (buf (or already-open (find-file-noselect filename))))
    (save-excursion
      (unwind-protect
          (with-current-buffer buf
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
        (unless already-open
          (kill-buffer-if-not-modified buf))))))

(defun ezeka--skip-line-and-insert (&rest args)
  "Insert a blank line and insert ARGS."
  (let ((pos (point)))
    (when (re-search-backward "^[^\n]" nil 'noerror)
      (end-of-line)
      (delete-region (point) pos))
    (apply #'insert "\n\n" args)))

(defun ezeka-add-change-log-entry (filename entry &optional section)
  "Add a change log ENTRY in FILENAME's SECTION.
If SECTION is nil, default to `Change Log'."
  (declare (indent 1))
  (interactive (list buffer-file-name nil nil))
  (let* ((section (or section "Change Log"))
         (headline (org-find-exact-headline-in-buffer section))
         (date-item (format "- [%s] :: " (ezeka-timestamp)))
         entry-pos)
    (save-restriction
      (save-excursion
        (if headline
            (progn
              (goto-char headline)
              (end-of-line))
          (goto-char (point-max))
          (org-insert-heading nil nil 'top)
          (insert section))
        (org-narrow-to-subtree)
        (org-back-to-heading-or-point-min)
        (if (search-forward date-item nil 'noerror)
            (let ((item-start (match-beginning 0)))
              (org-end-of-item)         ; actually moves pt to next item
              (when (re-search-backward "\\.\\(?1:\"\\)*" item-start t)
                (replace-match "\\1"))
              (insert "; ")
              (when entry
                (setq entry (concat (downcase (cl-subseq entry 0 1))
                                    (cl-subseq entry 1)))))
          (ezeka--skip-line-and-insert date-item))
        (if (null entry)
            (setq entry-pos (point))
          (insert entry)
          (org-fill-element))))
    (when (numberp entry-pos)
      (goto-char entry-pos))))

(defun ezeka--demote-quotes (string)
  "Demote double quotes in STRING to single quotes.
Only ASCII double and single quotes are touched."
  (string-replace "\"" "'" string))

;; TODO: Rewrite into two separate functions! title-and-caption could be
;; separate function.
(defun ezeka-set-title-or-caption (filename &optional new-val set-title set-caption metadata)
  "Update the title in FILENAME's header to NEW-VAL.
With \\[universal-argument], change the caption instead;
with double \\[universal-argument], change both the title
and the caption. Non-interactively, non-nil SET-TITLE and
SET-CAPTION determine which fields to change. METADATA
allows working with different metadata values than currently
exist in FILENAME."
  (interactive (let* ((arg (prefix-numeric-value current-prefix-arg))
                      (set-title (not (eq arg 4)))
                      (set-caption (member arg '(4 16))))
                 (list (buffer-file-name) nil set-title set-caption)))
  (when (ezeka-file-p filename)
    (let* ((mdata (or metadata (ezeka-file-metadata filename)))
           (caption (alist-get 'caption mdata))
           (change-what (cond ((and (not set-title) set-caption) "caption")
                              ((and set-title (not set-caption)) "title")
                              ((and set-title set-caption) "both title and caption")
                              (t "nothing (huh?)")))
           (new-val (or new-val
                        (ezeka--read-title
                         (format "Change \"%s\" to what? " change-what)
                         (if set-title
                             (alist-get 'title mdata)
                           caption)))))
      (when (and set-caption (y-or-n-p "Record the change in the change log? "))
        (ezeka-add-change-log-entry
            filename
          (cond ((string-match (regexp-quote caption) new-val)
                 (when-let ((addition (ezeka--demote-quotes
                                       (string-trim (replace-match "" nil nil new-val)
                                                    "[ ,]+"))))
                   (format "Add \"%s\" to caption." addition)))
                ((string-match (regexp-quote new-val) caption)
                 (when-let ((deletion (ezeka--demote-quotes
                                       (string-trim (replace-match "" nil nil caption)
                                                    "[ ,]+"))))
                   (format "Remove \"%s\" from caption." deletion)))
                (t
                 (format "Change caption from \"%s\" to \"%s.\""
                         (ezeka--demote-quotes (alist-get 'caption mdata))
                         (ezeka--demote-quotes new-val))))))
      (when set-title
        (setf (alist-get 'title mdata) new-val))
      (when set-caption
        (setf (alist-get 'caption mdata) (ezeka--pasteurize-file-name new-val))
        (setf (alist-get 'caption-stable mdata) nil))
      (ezeka--update-metadata-values filename mdata))))

(defun ezeka-set-subtitle (filename subtitle)
  "Set the SUBTITLE metadata in Zettel FILENAME."
  (interactive (list (ezeka--grab-dwim-file-target) nil))
  (when (ezeka-file-p filename)
    (let* ((mdata (ezeka-file-metadata filename))
           (subtitle (or subtitle
                         (ezeka--read-title
                          (if (alist-get 'subtitle mdata)
                              (format "Change `%s' to what? "
                                      (alist-get 'subtitle mdata))
                            "Subtitle: ")
                          (alist-get 'subtitle mdata)))))
      (setf (alist-get 'subtitle mdata) subtitle)
      (ezeka--update-metadata-values filename mdata))))

(defun ezeka-set-successor (filename successor)
  "Add a SUCCESSOR header field in FILENAME."
  (interactive
   (list (ezeka--grab-dwim-file-target)
         (ezeka--note-in-other-window)))
  (when (ezeka-file-p filename)
    (let* ((mdata (ezeka-file-metadata filename))
           (successor (or (ezeka-file-link successor)
                          (ezeka--read-id
                           (if (alist-get 'successor mdata)
                               (format "Change `%s' to what? "
                                       (alist-get 'successor mdata))
                             "Successor: ")
                           (alist-get 'successor mdata)))))
      (setf (alist-get 'successor mdata) successor)
      (ezeka--update-metadata-values filename mdata))))

(defun ezeka-set-parent (filename &optional new-parent)
  "Set parent metadata of FILENAME to NEW-PARENT (a link).
If called interactively, ask user to select the parent,
offering to use the note in the other window (if available).
If called with \\[universal-argument], read the parent ID manually."
  (interactive
   (list (ezeka--grab-dwim-file-target)
         (if current-prefix-arg
             (ezeka--read-id "New parent: "
                             nil
                             (ezeka-file-link (ezeka--note-in-other-window)))
           (ezeka-file-link (ezeka--select-file
                             (ezeka-octavo-with-kasten "numerus"
                               (octavo--directory-files 'full))
                             "Select new parent: " 'require-match)))))
  (ezeka--update-metadata-values filename nil
    'parent (if new-parent
                (ezeka-file-link new-parent)
              (message "Parent metadata cleared")
              nil)))

(defun ezeka-set-label (filename label &optional special)
  "Set LABEL (genus or category) in Zettel FILENAME.
With non-nil SPECIAL (or \\[universal-argument]), either
show genera verbosely or type custom category."
  (interactive
   (let ((target (ezeka--grab-dwim-file-target)))
     (list target
           (ezeka--read-label (ezeka-file-kasten target) current-prefix-arg)
           current-prefix-arg)))
  (if (not (ezeka-file-p filename))
      (user-error "Not a Zettel note")
    (let ((old-val (ezeka-file-name-label filename)))
      (ezeka--update-metadata-values filename nil 'label label)
      (when (eq :tempus (ezeka-id-type filename))
        (cl-pushnew label ezeka-categories))
      (when (y-or-n-p "Add a change log entry? ")
        (ezeka-add-change-log-entry
            filename
          (format "Change label from {%s} to {%s}." old-val label))))))

(defun ezeka--validate-citekey (citekey)
  "Return validated version of the CITEKEY or NIL.
If CITEKEY is a string that does not start with @ or &,
prepend @ to it."
  (save-match-data
    (when (string-match "\\`\\(?1:[@&]\\)*\\(?2:[A-Za-z0-9-]+\\)\\'" citekey)
      (concat (or (match-string 1 citekey) "@")
              (match-string 2 citekey)))))

(defun ezeka-set-citekey (filename &optional citekey degree)
  "Set CITEKEY in the Zettel note in FILENAME.
If CITEKEY is not given, get it from the parent, leting the
user edit it before setting. With DEGREE, trace genealogy
further than parent."
  (interactive (list (buffer-file-name)
                     nil
                     (if (integerp current-prefix-arg)
                         current-prefix-arg
                       1)))
  (if (not (ezeka-file-p filename))
      (user-error "Not a Zettel note")
    (let* ((ancestor (ezeka-trace-genealogy filename degree))
           (old-citekey (ezeka-file-name-citekey filename))
           (citekey (or citekey
                        (and ancestor
                             (ezeka-file-name-citekey (ezeka-link-file ancestor)))))
           (citekey (ezeka--minibuffer-edit-string
                     old-citekey citekey nil
                     'ezeka--read-citekey-history)))
      (ezeka--update-metadata-values filename nil
        'citekey (ezeka--validate-citekey citekey))
      (when (y-or-n-p "Record the change in the change log? ")
        (ezeka-add-change-log-entry
         filename
         (cond ((and old-citekey (string-empty-p citekey))
                (format "Remove citekey %s." old-citekey))
               ((not old-citekey)
                (format "Add citekey %s." citekey))
               ((not (string= old-citekey citekey))
                (format "Change citekey from %s to %s." old-citekey citekey))
               (t
                (format "Old citekey: %s, new citekey: %s" old-citekey citekey))))))))

(defun ezeka-set-author (filename author)
  "Set the AUTHOR metadata in Zettel FILENAME."
  (interactive
   (let ((target (ezeka--grab-dwim-file-target)))
     (list target
           (read-string "Set author (family, given) to: "
                        (ezeka-file-name-citekey target)))))
  (if (not (ezeka-file-p filename))
      (user-error "Not a Zettel note")
    (ezeka--update-metadata-values filename nil 'author author)))

(defvar ezeka--dynamic-keywords-cache nil
  "A list of keywords present in the current `ezeka-directory'.
This list is generated once per session and then just referenced.")

(defun ezeka--all-keywords ()
  "Return `ezeka-keywords' with optional dynamic keywords.
See `ezeka-dynamic-keywords'."
  (if ezeka-dynamic-keywords
      (setq ezeka--dynamic-keywords-cache
        (cl-union ezeka--dynamic-keywords-cache
                  ezeka-keywords
                  :test #'string=))
    ezeka-keywords))

(defun ezeka--keyword-list (keys)
  "Return a proper list of keywords from KEYS.
KEYS can be a string of space and/or comma-separated keys,
or a list of strings that are then assumed to be keywords."
  (mapcar (lambda (s)
            (if (= ?# (elt s 0))
                s
              (concat "#" s)))
          (pcase keys
            ((pred stringp)
             (split-string keys "[ ,]+" 'omit-nulls "[^a-z0-9-]+"))
            ((pred listp)
             keys)
            (_
             (signal 'wrong-type-argument (list 'string-or-list-p keys))))))

(defun ezeka--add-to-keywords-cache (keys)
  "Add keywords from KEYS to keyword history.
If given, the KEYS is sanitized and split to get clean
keywords, which are then added to `ezeka--dynamic-keywords-cache.'
The last keyword from KEYS is returned."
  (let ((keywords (ezeka--keyword-list keys)))
    (dolist (word keywords (last keywords))
      (cl-pushnew word ezeka--dynamic-keywords-cache :test #'string=))))

(defun ezeka-add-keyword (filename keyword &optional replace metadata)
  "Add the given KEYWORD to the Zettel note in FILENAME.
Select keyword interactively from `ezeka--all-keywords'.
When KEYWORD is nil (or \\[universal-argument]), clear any existing keywords.
When REPLACE is non-nil (or double \\[universal-argument]), replace them with
KEYWORD. Use METADATA if supplied."
  (interactive
   (list (ezeka--grab-dwim-file-target)
         (pcase current-prefix-arg
           ('(4) nil)
           ('(16) (completing-read "Replace with keyword: "
                                   (ezeka--all-keywords)
                                   nil nil nil 'ezeka--keyword-history))
           (_ (completing-read "Add keyword: "
                               (ezeka--all-keywords)
                               nil nil nil 'ezeka--keyword-history)))
         (equal current-prefix-arg '(16))))
  (let ((keyword (cond ((null keyword) nil)
                       ((string-match-p "^#\\w+$" keyword)
                        keyword)
                       ((string-match-p "^\\w+$" keyword)
                        (concat "#" keyword))
                       (t
                        (user-error "Keywords must consist of \\w characters")))))
    (if (not (ezeka-file-p filename))
        (user-error "Not a Zettel note")
      (let ((mdata (or metadata (ezeka-file-metadata filename))))
        (ezeka--update-metadata-values filename mdata
          'keywords (cond (replace
                           (list keyword))
                          (keyword
                           (ezeka--add-to-keywords-cache keyword)
                           (cl-remove-duplicates
                            (cons ezeka-rename-note-keyword (alist-get 'keywords mdata))
                            :test #'string=))
                          (t nil)))))))

(defvar ezeka--keyword-history nil
  "History variable for editing keywords.")

(defun ezeka-edit-keywords (filename &optional metadata clear)
  "Interactively edit FILENAME's keywords.
If given, use METADATA, otherwise read it from the file. If
CLEAR is non-nil (or called interactively with \\[universal-argument]), simply
clear the keywords without attempting to edit them."
  (interactive (list (ezeka--grab-dwim-file-target)
                     nil
                     current-prefix-arg))
  (if (not (ezeka-file-p filename))
      (user-error "Not a Zettel note")
    (let* ((mdata (or metadata (ezeka-file-metadata filename)))
           (keystring (string-join (alist-get 'keywords mdata) " "))
           (new-keys (unless clear
                       (ezeka--keyword-list
                        (if (string-empty-p keystring)
                            (completing-read "Add keyword: "
                                             (ezeka--all-keywords)
                                             nil nil nil 'ezeka--keyword-history)
                          (push keystring ezeka--keyword-history)
                          (ezeka--minibuffer-edit-string
                           keystring nil
                           "Edit keywords: " '(ezeka--keyword-history . 1)))))))
      (ezeka--add-to-keywords-cache new-keys)
      (ezeka--update-metadata-values filename mdata 'keywords new-keys))))

(defun ezeka-add-reading (filename &optional date)
  "Add DATE to the FILENAME's readings."
  (interactive (list (ezeka--grab-dwim-file-target)
                     (org-read-date nil nil nil nil nil nil 'inactive)))
  (let ((mdata (ezeka-file-metadata filename)))
    (ezeka--update-metadata-values filename mdata
      'readings (cons date (alist-get 'readings mdata)))))

(defun ezeka-add-oldname (filename &optional link)
  "Add LINK to the FILENAME's oldnames."
  (interactive (list (ezeka--grab-dwim-file-target)
                     (ezeka--read-id "What is the old name? ")))
  (let* ((mdata (ezeka-file-metadata filename))
         (oldnames (alist-get 'oldnames mdata)))
    (cl-pushnew link oldnames)
    (ezeka--update-metadata-values filename mdata
      'oldnames oldnames)))

;;;=============================================================================
;;; Populating Files
;;;=============================================================================

(defvar ezeka--read-title-history nil
  "History variable for Zettel titles.")

(defun ezeka--read-title (&optional prompt initial-input)
  "Read title or caption via minibuffer.
PROMPT and INITIAL-INPUT are passed to `read-string'."
  (read-string (or prompt "Title: ") initial-input 'ezeka--read-title-history))

(defvar ezeka--read-citekey-history nil
  "History variable for Zettel citekeys.")

(defun ezeka--read-citekey (&optional prompt initial-input)
  "Read a citekey, returning either a string or nil.
PROMPT and INITIAL-INPUT are passed to `read-string'."
  (ezeka--validate-citekey
   (read-string (or prompt "Citekey: ")
                initial-input
                'ezeka--read-citekey-history)))

(defun ezeka--read-kasten (&optional prompt)
  "Read a valid Kasten with `completing-read' and given PROMPT, if any."
  (completing-read
   (or prompt "Kasten: ")
   (if (listp (ezeka-kaesten))
       (mapcar #'ezeka-kasten-name (ezeka-kaesten))
     (signal 'ezeka-error (list "No `ezeka-kaesten' defined")))))

(defun ezeka-insert-header-template
    (&optional link label title parent citekey metadata)
  "Insert header template into the current buffer.
If given, populate the header with the LINK, LABEL, TITLE, PARENT, and
CITEKEY. Anything not specified is taken from METADATA, if available."
  (interactive
   (let* ((link (if buffer-file-name
                    (ezeka-file-link buffer-file-name)
                  (user-error "This command can only be called from a Zettel")))
          (mdata (or (ezeka--new-child-metadata link)
                     (ezeka-decode-rubric (file-name-base buffer-file-name)))))
     (let-alist mdata
       (list
        link
        (or .label
            (setf (alist-get 'label mdata)
                  (ezeka--read-label (ezeka-link-kasten link))))
        (setf (alist-get 'title mdata)
              (ezeka--read-title "Title: " (or .title .caption)))
        (setf (alist-get 'parent mdata)
              (ezeka--read-id "Parent? " nil .parent))
        (setf (alist-get 'citekey mdata)
              (ezeka--read-citekey "Citekey? " .citekey))
        (prog1 mdata
          (ezeka--set-new-child-metadata link mdata))))))
  (let* ((link (or link (alist-get 'link metadata)))
         (mdata (ezeka-metadata link
                  'link link
                  'id (or (alist-get 'id metadata) (ezeka-link-id link))
                  'label (or label (alist-get 'label metadata))
                  'title (or title (alist-get 'title metadata))
                  'parent (or parent (alist-get 'parent metadata))
                  'citekey (or citekey (alist-get 'citekey metadata))))
         (inhibit-read-only t)
         (content (buffer-substring-no-properties (point-min) (point-max))))
    (let-alist mdata
      (erase-buffer)
      (goto-char (point-min))
      (insert
       (concat ezeka-header-rubric-key
               ": "
               (ezeka-encode-rubric mdata)))
      (insert "\ntitle: " .title)
      (insert (format "\ncreated: %s\n"
                      ;; Insert creation time, making it match a tempus currens filename
                      (ezeka-timestamp (when (eq :tempus (ezeka-id-type .id))
                                         (ezeka--parse-time-string .id))
                                       'full)))
      (when (and .parent (not (string-empty-p .parent)))
        (insert "parent: " (ezeka--format-link .parent) "\n"))
      (insert "\n" content)
      (add-hook 'after-save-hook 'ezeka--run-3f-hook nil 'local))))

(defun ezeka--run-3f-hook ()
  "Run hooks in `ezeka-find-file-functions'."
  (let* ((mdata (ezeka-file-metadata buffer-file-name))
         (parent (alist-get 'parent mdata)))
    (run-hook-with-args 'ezeka-find-file-functions
                        buffer-file-name
                        (if parent
                            (ezeka-link-file parent)
                          this-command))))

;; FIXME: `rb-rename-file-and-buffer' is not local
(defun ezeka-incorporate-file (file kasten &optional arg)
  "Move FILE (defaults to one in current buffer) to KASTEN.
With \\[universal-argument] ARG, asks for a different name."
  (interactive (list (buffer-file-name)
                     (completing-read "Zettel kasten: " (ezeka-kaesten))
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
  (org-set-property "FROM" (ezeka-insert-link-with-metadata
                            buffer-file-name '(title) :after))
  (org-set-property "CREATED"
                    (ezeka-timestamp nil 'full 'brackets)))

(defun ezeka-org-interactive-tempus ()
  "Use org-mode's `org-time-stamp' command to insert a tempus currens."
  (interactive)
  (ezeka--insert-link-with-spaces (ezeka-tempus-currens (org-read-date t t))))

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

(defun ezeka-org-export-as-new-note (&optional kasten)
  "Create new Zettel in KASTEN (a string) from the current org subtree.
With \\[universal-argument], use the current KASTEN without asking."
  (interactive (list (unless current-prefix-arg
                       (ezeka--read-kasten "Zettel Kasten: "))))
  (let* ((parent-file buffer-file-name)
         (kasten (or kasten (ezeka-file-kasten buffer-file-name)))
         (kstruct (ezeka-kasten kasten))
         mdata)
    (setf (alist-get 'parent mdata)
          (ezeka-file-link buffer-file-name))
    (save-excursion
      (save-restriction
        (org-narrow-to-subtree)
        (goto-char (point-min))
        (let* ((head-level (nth 1 (org-heading-components)))
               (head-title (nth 4 (org-heading-components)))
               timestamp)
          (cond ((string-match "\\(.*\\) \\([[<].*[]>]\\)" head-title)
                 (setf (alist-get 'title mdata)
                       (ezeka--minibuffer-edit-string
                        (match-string-no-properties 1 head-title)
                        nil
                        "Title for new note: "))
                 (setf (alist-get 'created mdata)
                       (save-match-data
                         (org-timestamp-to-time
                          (org-timestamp-from-string (match-string 2 head-title))))))
                ((org-get-scheduled-time nil)
                 (setf (alist-get 'created mdata)
                       (org-get-scheduled-time nil)))
                (t
                 (setf (alist-get 'title mdata)
                       (ezeka--minibuffer-edit-string
                        (buffer-substring-no-properties (point-at-bol) (point-at-eol))
                        nil
                        "Title for new note: "))
                 (setf (alist-get 'created mdata)
                       (parse-time-string
                        (read-string "No timestamp found. Enter it here: ")))))
          (setf (alist-get 'link mdata)
                (ezeka-make-link
                 kasten
                 (cond ((eq (ezeka-kasten-id-type kstruct) :tempus)
                        (ezeka-tempus-currens
                         (ezeka--complete-time (alist-get 'created mdata))))
                       (t
                        (ezeka--generate-id kasten)))))
          (setf (alist-get 'path mdata)
                (ezeka-link-path (alist-get 'link mdata)))
          (if (file-exists-p (alist-get 'path mdata))
              (user-error "Aborting, file already exists: %s" (alist-get 'path mdata))
            (let ((entry-pt (point))
                  (content (org-copy-subtree)))
              (with-current-buffer (get-buffer-create (alist-get 'path mdata))
                ;; New file buffer
                (ezeka-insert-header-template
                 nil (ezeka--read-label kasten) nil nil nil mdata)
                (insert "\n" org-subtree-clip)
                (set-visited-file-name (alist-get 'path mdata) t)
                (basic-save-buffer)
                (ezeka-add-change-log-entry (alist-get 'path mdata)
                  (format "Extract from %s." (alist-get 'parent mdata))))
              (with-current-buffer (get-file-buffer (file-truename parent-file))
                ;; Back in original buffer
                (goto-char entry-pt)
                (org-cut-subtree)
                (insert (make-string head-level ?*)
                        " "
                        head-title
                        " "
                        (ezeka--format-link (alist-get 'link mdata)))))))))))

(defun ezeka-open-link-at-point (&optional same-window)
  "Open a Zettel link at point even if it's not formatted as a link.
If SAME-WINDOW is non-nil, or the command is called with \\[universal-argument],
ignore `ezeka-number-of-frames' and open the link in the same window."
  (interactive "p")
  (if-let ((_ (or (ezeka-link-at-point-p)
                  (ezeka-link-at-point-p t)))
           (link (ezeka-link-at-point)))
      ;; This function is later added to `org-open-at-point-functions', so "must
      ;; return t if they identify and follow a link at point. If they don’t
      ;; find anything interesting at point, they must return nil."
      (and (ezeka-find-link link same-window)
           t)
    (message "Could not find any links at point")
    nil))

(defun ezeka-open-link-at-mouse (ev &optional same-window)
  "Open a Zettel link at mouse point (determined from EV).
With non-nil SAME-WINDOW (or \\[universal-argument]), open in the same window."
  (interactive "e\nP")
  (mouse-set-point ev)
  (ezeka-open-link-at-point same-window))

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

(defcustom ezeka-snippet-heading "Snippet"
  "The text of the snippet heading."
  :type 'string)

(defcustom ezeka-snippet-modified-property "MODIFIED"
  "Name of the snippet heading's last-modified property."
  :type 'string)

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

(defun ezeka--org-move-after-properties ()
  "Move point after the properties drawer, if any.
Return the resulting point."
  (when (org-get-property-block)
    (goto-char (cdr (org-get-property-block)))
    ;; `org-get-property-block' ends on :END:
    (unless (zerop (forward-line 2))
      (insert "\n\n"))
    (point)))

(defun ezeka--find-snippet-heading ()
  "Go to the first snippet heading in the current buffer.
Return the new position; otherwise, nil."
  (let (pos)
    (save-excursion
      (goto-char (point-min))
      ;; HARDCODED Match the entire org-mode heading line
      (when (re-search-forward (concat "^\\* .*"
                                       ezeka-snippet-heading
                                       ".*$"))
        (setq pos (match-end 0))))
    (when pos
      (goto-char pos))))

(defun ezeka-set-modified-property (&optional time)
  "Add :MODIFIED: property to the current heading.
If given, set it to TIME, otherwise the latest recorded
modification date."
  (cond ((not (org-at-heading-p))
         (user-error "Move to the org heading first"))
        (t
         (if-let ((mdata (ezeka-file-metadata buffer-file-name)))
             (org-set-property ezeka-snippet-modified-property
                               (alist-get 'created mdata))))))

;;; TODO:
;;; - if region is active, narrow to it rather than to subtree (allows # lines!)
;;; - don't copy subtrees marked with COMMENT
;;; - quickly scan through all the headings and see if any need updating?
(defun ezeka-insert-snippet-text (link &optional force summary)
  "Insert snippet text from the given LINK into the current buffer.
By default, only update the text if the modification time is
different. With \\[universal-argument] FORCE, forces update. With SUMMARY
\(or \\[universal-argument] \\[universal-argument]),
insert the summary before the content."
  (interactive
   (list (or (org-entry-get (point) "SNIP_SOURCE")
             (save-excursion
               (org-back-to-heading)
               (ezeka--org-nth-link-on-line -1))
             (user-error "Insert a link to the snippet source first"))
         (equal current-prefix-arg '(4))
         (equal current-prefix-arg '(16))))
  (save-excursion
    (save-restriction
      (org-back-to-heading)
      (let* ((snip-file (ezeka-link-file link))
             ;; Get the metadata and most recent modification
             (snip-mdata (ezeka-file-metadata snip-file))
             (their-modified (or (alist-get 'modified snip-mdata)
                                 (alist-get 'created snip-mdata)))
             (our-modified
              (when-let ((snip-modified (org-entry-get (point) "SNIP_MODIFIED")))
                (ezeka--parse-time-string snip-modified)))
             (current? (time-equal-p their-modified our-modified))
             (local? (string-match-p "local"
                                     (or (org-entry-get nil "TAGS")
                                         "")))
             (org-id (org-id-get-create)))
        (org-narrow-to-subtree)
        (when local?
          (user-error "There are local changes (or at least :local: tag)"))
        (when (looking-at org-outline-regexp)
          (replace-regexp (regexp-quote (elt (org-heading-components) 4))
                          (ezeka-format-metadata "%t [[%i]]" snip-mdata)))
        (unless (string= link (org-entry-get (point) "SNIP_SOURCE"))
          (org-entry-put (point) "SNIP_SOURCE" link))
        (org-set-tags (cl-remove "CHANGED" (org-get-tags) :test #'string=))
        (if (and current? (not force))
            (message "Snippet is up to date; leaving alone")
          (org-entry-put (point)
                         "SNIP_MODIFIED"
                         (ezeka-timestamp their-modified 'full 'brackets))
          (when (or t (y-or-n-p "Update the text? "))
            ;; If current line is a comment, create a heading after it
            (when (org-at-comment-p)
              (org-insert-subheading nil))
            ;; Delete existing text
            (ezeka--org-move-after-properties)
            (let ((start (point))
                  (comments-removed 0)
                  (footnotes-removed 0)
                  (snip-buf (find-file-noselect snip-file))
                  (content '()))
              (delete-region start (point-max))
              ;; Get the Summary and Snippet subtrees from snippet snip-file
              (with-current-buffer snip-buf
                ;; Include Summary section if present
                (when (and (or summary ezeka-insert-snippet-summary)
                           (org-find-exact-headline-in-buffer "Summary"))
                  (goto-char (org-find-exact-headline-in-buffer "Summary"))
                  (forward-line)
                  (let ((summary-start (point)))
                    (org-end-of-subtree)
                    (push "#+begin_comment" content)
                    (push (buffer-substring-no-properties summary-start (point))
                          content)
                    (push "\n#+end_comment\n\n" content)))
                (or (ezeka--find-snippet-heading)
                    (signal 'ezeka-error (list "Can't find the Snippet or Content section")))
                (if (not org-id)
                    (warn "No org-id added to file %s" snip-file)
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
              t)))))))

(defun ezeka--update-inserted-snippet ()
  "Update the snippet in the current note wherever it is used."
  (let ((current (current-buffer)))
    (save-excursion
      (when-let ((label (string=
                         "ν" (ezeka-file-name-label (buffer-file-name current))))
                 (pos (or (org-find-exact-headline-in-buffer "Snippet")
                          (org-find-exact-headline-in-buffer "Content"))))
        (when (y-or-n-p "Did you modify the snippet text? ")
          (goto-char pos)
          (org-entry-put (point) "MODIFIED"
                         (ezeka-timestamp nil 'full 'brackets))
          (when-let* ((used-in (org-entry-get (point) "USED_IN+"))
                      (used-list
                       (split-string
                        (replace-regexp-in-string "\\(id:\\|\\[id:\\)" "" used-in)
                        nil t " \n"))
                      (not-tagged
                       (save-excursion
                         (cl-remove-if (lambda (org-id)
                                         (if (not (org-id-find org-id))
                                             (warn "Cannot find Org ID `%s' that uses snippet from `%s'"
                                                   org-id
                                                   (ezeka-file-name-id
                                                    (buffer-file-name current)))
                                           (org-id-goto org-id)
                                           (member "CHANGED" (org-get-tags))))
                                       used-list))))
            (when (y-or-n-p (format "%s\nAdd CHANGED tags in these files? "
                                    (mapconcat (lambda (id)
                                                 (ezeka-file-name-caption
                                                  (car (org-id-find id))))
                                               not-tagged
                                               "\n")))
              (dolist (org-id not-tagged)
                (org-id-goto org-id)
                (org-back-to-heading t)
                (org-set-tags (cl-union '("CHANGED") (org-get-tags) :test #'string=)))))))
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
         (format "%s [[%s]]" (alist-get 'title metadata) link))
        ;; Delete existing text
        (org-back-to-heading t)
        (delete-region (point-at-bol 2) (org-end-of-subtree t))
        ;; Insert the transclusion line
        (insert (format "\n#+transclude: [[%s::begin]] :lines 2- :end \"end\""
                        (file-relative-name file)))))))

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
          (when-let ((it (cadr
                          (split-string (ezeka-file-name-id buffer-file-name)
                                        "-"))))
            (kill-new it))
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
;;; Searching
;;;=============================================================================

(defcustom ezeka-find-regexp-buffer-format "Full-Text Search: %s"
  "Format string *xref* buffer name used by `ezeka-find-regexp'.
The control sequence %s is replaced with the xref search string."
  :group 'ezeka
  :type 'string)

(defun ezeka--find-regexp-open-buffer (regexp)
  "Open xref buffer showing results of searching for REGEXP."
  (let ((name (format (concat "*" ezeka-find-regexp-buffer-format "*") regexp)))
    (if (get-buffer name)
        (pop-to-buffer name)
      (user-error "No such buffer"))))

(org-add-link-type "efir" 'ezeka--find-regexp-open-buffer)

(defvar ezeka--read-regexp-history nil
  "History variable for `ezeka--read-regexp'.")

(defun ezeka--read-regexp (&optional prompt)
  "Interactively read a regexp with optional PROMPT."
  (let ((sym (thing-at-point 'symbol t)))
    (read-regexp (or prompt "Regexp ") (and sym (regexp-quote sym))
                 ezeka--read-regexp-history)))

(defun ezeka-find-regexp (regexp &optional kasten)
  "Find all matches of REGEXP in `ezeka-directory'.
If KASTEN is non-nil (or with \\[universal-argument]), limit to only to it."
  (interactive
   (list (ezeka--read-regexp "Regexp to find: ")
         (when current-prefix-arg
           (ezeka--read-kasten "Kasten to search: "))))
  (ezeka-breadcrumbs-drop nil
                          buffer-file-name
                          (format ezeka-find-regexp-buffer-format
                                  (regexp-quote regexp)))
  (require 'xref)
  (let ((xref-buffer-name (format ezeka-find-regexp-buffer-format regexp)))
    (xref--show-xrefs
     (xref-matches-in-directory regexp
                                (format "*.%s" ezeka-file-extension)
                                ezeka-directory nil)
     nil))
  (advice-add 'xref-goto-xref :before 'ezeka--breadcrumbs-xref-advice)
  (advice-add 'xref-show-location-at-point :before 'ezeka--breadcrumbs-xref-advice))

;;;=============================================================================
;;; System Log
;;;=============================================================================

(require 'json)

(defvar ezeka--system-log-file "auto/system.log"
  "Path, relative to `ezeka-directory', to log everything.")

;; Source: `cl--plist-to-alist'
(defun ezeka--plist-to-alist (plist)
  "Given PLIST, return an equivalent alist.
If PLIST is already an alist, leave it alone."
  (pcase (car plist)
    ((pred symbolp)
     (let ((res '()))
       (while plist
         (push (cons (pop plist) (pop plist)) res))
       (nreverse res)))
    ((pred consp) plist)))

(defun ezeka--system-log-record (time action &rest props)
  "Create a system log record with TIME, ACTION, and PROPS."
  (append `((time . ,(ezeka--iso8601-time-string time))
            (action . ,action))
          (ezeka--plist-to-alist props)))

(ert-deftest ezeka--system-log-record ()
  (should (ezeka--system-log-record nil 'update-modified 'note "a-1234"))
  (should (ezeka--system-log-record
           (parse-time-string "2024-01-01T00:00:00")
           'update-modified 'note "a-1234")))

(defun ezeka--system-log-repeat-record-p (object previous)
  "Return non-nil if OBJECT and PREVIOUS differ only in time."
  (let ((object (cl-remove 'time object :key #'car))
        (previous (cl-remove 'time previous :key #'car)))
    (cl-every #'equal object previous)))

(ert-deftest ezeka--system-log-repeat-record-p ()
  (should (ezeka--system-log-repeat-record-p
           (ezeka--system-log-record nil 'update-modified 'note "a-1234")
           (ezeka--system-log-record
            (parse-time-string "2024-01-01T00:00:00")
            'update-modified 'note "a-1234"))))

(defun ezeka--add-to-system-log (action time &rest props)
  "Add a log entry for ACTION at TIME (nil for now) with PROPS.
PROPS should be either a plist or an alist."
  (declare (indent 2))
  (let* ((time (or time (current-time)))
         (record (apply #'ezeka--system-log-record time action props))
         (json (json-encode record))
         (logfile (expand-file-name ezeka--system-log-file ezeka-directory))
         (logbuf (find-file-noselect logfile)))
    (with-current-buffer logbuf
      (goto-char (point-max))
      (if-let* ((_ (re-search-backward "^{" nil 'noerror))
                (previous (json-read))
                (_ (ezeka--system-log-repeat-record-p
                    (json-read-from-string json)
                    previous)))
          (forward-line)
        (insert json "\n"))
      (delete-trailing-whitespace)
      (save-buffer))))

(ert-deftest ezeka--add-to-system-log ()
  (should (ezeka--add-to-system-log 'move nil
            'from "k-7952"
            'to "20150603T2323"))
  (should (ezeka--add-to-system-log 'move nil
            'from "20150603T2323"
            'to "k-7952"))
  (should-not (ezeka--system-log-trail "k-7952")))

(defun ezeka--system-log-trail (note)
  "Return the move trail (if any) for NOTE.
If there are multiple records, they are returned in
reverse-chronological order (i.e. latest record first)."
  (let (trail)
    (with-temp-buffer
      (insert-file-contents (in-ezeka-dir ezeka--system-log-file))
      (goto-char (point-min))
      (while (re-search-forward (format "\"%s\"" note) nil t)
        (beginning-of-line)
        (push (json-read) trail)))
    trail))

;;;=============================================================================
;;; Maintenance
;;;=============================================================================

(defvar ezeka--move-log-file "auto/move.log"
  "Path, relative to `ezeka-directory', to the log file for recording moves.")

;; TODO Replace with JSON so it's more portable and comprehensible
;; TODO Distinguish between action (moved, deleted, archived) and target
(defun ezeka--parse-move-log-line (line)
  "Parse a move long line in LINE, returning an alist."
  (let ((result (read-from-string line)))
    (if (< (cdr result) (length line))
        (signal 'ezeka-error
                (list "Garbage at the end of move log line: %s"
                      (substring line (cdr result))))
      `((source . ,(nth 0 (car result)))
        (target . ,(nth 1 (car result)))
        (time   . ,(nth 2 (car result)))
        (caption . ,(nth 3 (car result)))
        (comment . ,(nth 4 (car result)))))))

(defun ezeka--add-to-move-log (source target &optional src-caption comment)
  "Log the move from SOURCE link to TARGET in `ezeka--move-log-file'.
SRC-CAPTION and COMMENT can be added to make the record
easier to later read."
  (interactive "sMoved from: \nsMoved to: \nsSource caption: \nsComment: ")
  ;; ("SOURCE" "TARGET" "TIME" "SOURCE-CAPTION" "COMMENT")
  (write-region (format "\n%S"
                        (list source
                              target
                              (format-time-string "%FT%T")
                              src-caption
                              comment))
                nil
                (expand-file-name ezeka--move-log-file ezeka-directory)
                'append))

(defun ezeka--note-move-trail (note)
  "Return the move trail (if any) for NOTE.
If there are multiple records, they are returned in
reverse-chronological order (i.e. latest record first)."
  (let (trail)
    (with-temp-buffer
      (insert-file-contents (in-ezeka-dir ezeka--move-log-file))
      (goto-char (point-min))
      (while (re-search-forward (format "\"%s\"" note) nil t)
        (push (ezeka--parse-move-log-line
               (buffer-substring (point-at-bol) (point-at-eol)))
              trail)))
    trail))

(defun ezeka-note-moved-p (note &optional confirm visit)
  "Check whether NOTE appears in the `ezeka--move-log-file'.
If CONFIRM is non-nil, confirm the note to check. If VISIT
is 'VISIT, visit the new note; if 'ASK or T, ask the user
whether to visit; if NIL, do not visit."
  (interactive
   (let ((file (ezeka--grab-dwim-file-target 'link-at-point)))
     (list (if (or current-prefix-arg (null file))
               (ezeka--read-id "Link of note to check: "
                               nil
                               (word-at-point)
                               'required)
             (ezeka-file-link file))
           nil
           'ask)))
  (let* ((_pprint_record
          (lambda (rec)
            (let-alist rec
              (let* ((t-file (ezeka-link-file .target))
                     (t-name (when t-file (file-name-base t-file))))
                (format "%s %s`%s' on %s (%s)"
                        .source
                        (if t-file "moved to " "")
                        (propertize (or t-name .target) 'face 'bold)
                        (format-time-string
                         "%F %a %R"
                         (encode-time (parse-time-string .time)))
                        (or .comment ""))))))
         (trail (ezeka--note-move-trail note)))
    (cond ((and (null trail) (ezeka-link-file note))
           (when (y-or-n-p (format "No record of moving %s, but it exists. Visit? " note))
             (ezeka-find-file (ezeka-link-file note))))
          ((null trail)
           (message "No record of moving %s" note)
           nil)
          ((and (ezeka-link-p (alist-get 'target (car trail)))
                (or (eq visit 'visit)
                    (y-or-n-p
                     (format "%s\nVisit %s? "
                             (mapconcat _pprint_record (nreverse trail) "\n")
                             (propertize (alist-get 'target (car trail)) 'face 'bold)))))
           (ezeka-find-link (alist-get 'target (car trail))))
          (t
           (message (mapconcat _pprint_record (nreverse trail) "\n"))))))

(defun ezeka--finish-moving-note (source target &optional metadata)
  "Finish moving SOURCE to TARGET (both file paths).
If METADATA is nil, read it from SOURCE."
  (let ((source-link (ezeka-file-link source))
        (source-rubric (file-name-base source))
        (mdata (or metadata (ezeka-file-metadata source)))
        (target-link (ezeka-file-link target)))
    (cond ((file-symlink-p target)
           (delete-file target))
          ((file-exists-p target)
           (user-error "`%s' is not a symlink, aborting!"
                       (file-relative-name target ezeka-directory)))
          (t
           ;; TARGET is new, proceed
           ))
    (ezeka--add-to-move-log source-link target-link source-rubric "Finish moving")
    (ezeka--add-to-system-log 'move nil
      'source source-rubric
      'target (ezeka-encode-rubric mdata))
    (unwind-protect
        (when-let ((_ (ezeka--rename-file source target))
                   (buf (or (get-file-buffer target)
                            (find-file-noselect target))))
          (with-current-buffer buf
            (ezeka--update-file-header
             target
             (ezeka--add-oldname mdata source-link))
            (ezeka-add-change-log-entry source
              (format "Finish moving +%s+ to %s." source-link target-link))
            (setf (alist-get 'keywords mdata)
                  (cl-set-difference (alist-get 'keywords mdata)
                                     (list ezeka-note-moving-keyword ezeka-rename-note-keyword)
                                     :test #'string=))
            (ezeka-harmonize-file-name target mdata t)
            (save-buffer)))
      (message "Replacing links: %s with %s" source-link target-link)
      (condition-case nil
          (let ((replaced (ezeka-octavo-replace-links source-link target-link)))
            (ezeka-add-change-log-entry
                source
              (format "Replace %d links in %d files."
                      (or (car replaced) 0) (or (cdr replaced) 0)))
            (message "Moved %s to %s, replacing %d links in %d files"
                     source-link target-link
                     (or (car replaced) 0) (or (cdr replaced) 0)))
        (error
         (kill-new (format "(ezeka-octavo-replace-links \"%s\" \"%s\")"
                           source-link target-link))
         (message "Replacing links failed; try manually"))))))

(defun ezeka--begin-moving-note (source target-link)
  "Begin moving Zettel note from SOURCE to TARGET-LINK.
To do that, make a symbolic link from source to target
first, which `ezeka--finish-moving-note' will deal with
afterwards. SOURCE can be a link or a file."
  (cl-assert (stringp source))
  (cl-assert (stringp target-link))
  (let* ((s-file (if (file-exists-p source)
                     source
                   (ezeka-link-file source)))
         (s-link (ezeka-file-link s-file))
         (t-link target-link)
         (s-mdata (ezeka-file-metadata s-file))
         (t-mdata (ezeka-file-metadata s-file))) ; TODO Need to deep-copy s-mdata
    (ezeka--add-to-move-log s-link target-link
                            (alist-get 'caption s-mdata)
                            "Begin moving")
    (setf (alist-get 'id t-mdata)
          (ezeka-link-id target-link)
          (alist-get 'label t-mdata)
          (ezeka--read-label (alist-get 'kasten t-mdata)
                             nil
                             nil
                             (alist-get 'label t-mdata))
          (alist-get 'title t-mdata)
          (ezeka--read-title "Title: "
                             (alist-get 'title t-mdata))
          (alist-get 'caption t-mdata)
          (ezeka--read-title "Caption: "
                             (ezeka--pasteurize-file-name (alist-get 'title t-mdata)))
          (alist-get 'citekey t-mdata)
          (ezeka--read-citekey (format "Title: %s\nCitekey: "
                                       (alist-get 'title t-mdata)))
          (alist-get 'keywords t-mdata)
          (cl-union (alist-get 'keywords t-mdata)
                    (list ezeka-note-moving-keyword ezeka-rename-note-keyword)
                    :test #'string=)
          (alist-get 'rubric t-mdata)
          (ezeka-encode-rubric t-mdata))
    (ezeka--add-to-system-log 'move nil
      'source (alist-get 'rubric s-mdata)
      'target (alist-get 'rubric t-mdata)
      'comment "Begin moving")
    (ezeka-add-change-log-entry s-file
      (format "Begin moving \"%s\" to \"%s.\""
              (alist-get 'rubric s-mdata)
              (alist-get 'rubric t-mdata)))
    (ezeka--update-file-header s-file t-mdata 'force)
    (let ((t-file (ezeka-link-path target-link t-mdata)))
      (unless (file-exists-p (file-name-directory t-file))
        (make-directory (file-name-directory t-file)))
      (ezeka--make-symbolic-link s-file t-file)
      (message "Began moving `%s' to `%s'; re-run `ezeka-move-to-another-kasten' \
after committing" s-link target-link))))

(defun ezeka--placeholders ()
  "Return a list of all placeholder symbolic links."
  (directory-files-recursively
   (ezeka-kasten-directory (ezeka-kasten "numerus"))
   ".*{ψ}.*.txt" nil nil nil))

(defun ezeka-replace-placeholder (placeholder &optional note metadata)
  "Replace PLACEHOLDER with NOTE (both file paths).
If METADATA is nil, read it from PLACEHOLDER's filename. If
NOTE is nil, create a new file."
  (interactive
   (list (ezeka-octavo-with-kasten "numerus"
           (ezeka--select-file (ezeka--placeholders)
                               "Placeholder to replace: "
                               'require-match))))
  (let* ((note-id (ezeka-file-link (or note placeholder)))
         (note-rubric (file-name-base (or note placeholder)))
         (ph-id (ezeka-file-link placeholder))
         (ph-rubric (file-name-base placeholder))
         (mdata (or metadata (ezeka-decode-rubric ph-rubric)))
         (parent (ezeka-file-link (file-symlink-p placeholder))))
    (cond ((file-symlink-p placeholder)
           (delete-file placeholder))
          ((file-exists-p placeholder)
           (user-error "`%s' is not a symlink, aborting!"
                       (file-relative-name placeholder ezeka-directory)))
          (t
           (error "Unknown error")))
    (ezeka--add-to-move-log note-id ph-id note-rubric "Replace placeholder")
    (ezeka--add-to-system-log 'replace-placeholder nil
      'placeholder (ezeka-encode-rubric mdata)
      'parent parent
      'note note-rubric)
    (if note
        (unwind-protect
            (when-let ((_ (ezeka--rename-file note placeholder))
                       (buf (or (get-file-buffer placeholder)
                                (find-file-noselect placeholder))))
              (with-current-buffer buf
                (ezeka--update-file-header
                 placeholder
                 (ezeka--add-oldname mdata note-id))
                (ezeka-add-change-log-entry note
                  (format "Repurpose +%s+ in place of \"%s\"."
                          note-rubric ph-rubric))
                (ezeka-harmonize-file-name placeholder mdata t)
                (save-buffer)))
          (message "Replacing links: %s with %s" note-id ph-id)
          (condition-case nil
              (let ((replaced (ezeka-octavo-replace-links note-id ph-id)))
                (ezeka-add-change-log-entry
                    note
                  (format "Replace %d links in %d files."
                          (or (car replaced) 0) (or (cdr replaced) 0)))
                (message "Moved %s to %s, replacing %d links in %d files"
                         note-id ph-id
                         (or (car replaced) 0) (or (cdr replaced) 0)))
            (error
             (kill-new (format "(ezeka-octavo-replace-links \"%s\" \"%s\")"
                               note-id ph-id))
             (message "Replacing links failed; try manually"))))
      (ezeka--set-new-child-metadata ph-id mdata
        'parent parent)
      (ezeka-find-file placeholder 'same-window))))

(defun ezeka--resurrectable-oldname (source-file id-type &optional metadata)
  "Check SOURCE-FILE's oldnames for an oldname of ID-TYPE.
ID-TYPE should be a keyword matching an ID type in
`ezeka-kaesten'. If METADATA is non-nil, use that rather
than parsing the file again. If successful, return the
appropriate oldname."
  (when-let* ((mdata (or metadata
                         (ignore-errors (ezeka-file-metadata source-file))))
              (cadaver (cl-find-if (lambda (link)
                                     (when (ezeka-link-p link)
                                       (eq (ezeka-id-type link) id-type)))
                                   (alist-get 'oldnames mdata))))
    (unless (ezeka-link-file cadaver)
      cadaver)))

(defun ezeka-move-to-another-kasten (source-file kasten &optional target-link noselect)
  "Move SOURCE-FILE Zettel to a generated link in KASTEN.
With \\[universal-argument], ask for an explicit TARGET-LINK instead.
Return the target link and open it (unless NOSELECT is non-nil)."
  (interactive
   (let* ((source (ezeka--grab-dwim-file-target))
          (src-header (ezeka-file-content source 'just-header))
          (target (cond ((equal current-prefix-arg '(4))
                         (ezeka--read-id "Target link: "))
                        ((string-match-p ezeka-note-moving-keyword src-header)
                         (alist-get 'id (ezeka--decode-header src-header source))))))
     (list (ezeka--grab-dwim-file-target)
           (if target
               (ezeka-link-kasten target)
             (completing-read "Which kasten to move to? "
                              (mapcar #'ezeka-kasten-name (ezeka-kaesten))))
           target)))
  (let* ((ezeka-header-update-modified 'never) ; HARDCODED
         (id-type (ezeka-kasten-id-type (ezeka-kasten kasten)))
         (source-link (ezeka-file-link source-file))
         (s-mdata (ezeka-file-metadata source-file))
         (target-link
          (or target-link
              (ezeka--resurrectable-oldname source-file id-type s-mdata)
              (if (eq id-type :tempus)  ; HARDCODED
                  (ezeka-tempus-currens-id-for source-file)
                (ezeka--generate-id kasten)))))
    (if (not target-link)
        (user-error "No target link specified")
      (save-some-buffers nil (lambda () (ezeka-file-p buffer-file-name t)))
      (if (and (member ezeka-note-moving-keyword (alist-get 'keywords s-mdata))
               (y-or-n-p (format "Finish moving %s to %s? "
                                 source-link target-link)))
          (ezeka--finish-moving-note
           source-file
           (ezeka-link-path target-link s-mdata))
        (ezeka--begin-moving-note source-file target-link))
      (unless noselect
        (ezeka-find-link target-link t)))))

(defun ezeka-stage-links-in-subtree (&optional start end)
  "Stage all links in the current `org-mode' subtree.
If region is active, only do so for links between START and
END."
  (interactive
   (when (use-region-p)
     (list (region-beginning) (region-end))))
  (save-restriction
    (if start
        (narrow-to-region start end)
      (org-narrow-to-subtree))
    (goto-char (point-min))
    (while (re-search-forward (concat "\\[\\[" (ezeka-link-regexp) "]]") nil t)
      (let ((file (ezeka-link-file (match-string 1))))
        (message "Staging %s %s..."
                 (ezeka-file-name-id file) (ezeka-file-name-caption file))
        (magit-stage-file file))))
  (magit-stage-file buffer-file-name))

(defun ezeka-generate-n-new-ids (how-many kasten)
  "Generate HOW-MANY new IDs for KASTEN, making sure there are no dulicates."
  (interactive
   (list (read-number "How many? " 10)
         (ezeka--read-kasten "Which kasten? ")))
  (goto-char (point-max))
  (let (ids)
    (dotimes (n how-many)
      (push (ezeka--generate-id kasten 'batch) ids))
    (mapc (lambda (s)
            (insert s "\n"))
          (delete-dups ids))
    (delete-duplicate-lines (point-min) (point-max))))

;;;=============================================================================
;;; Mode Line
;;;=============================================================================

;; Based on https://stackoverflow.com/a/30328255
;;
;; Add the following to emacs config file:
;;
;; (add-hook 'post-command-hook 'ezeka--magit-mode-line-show-file-type)
;; (add-hook 'magit-mode-hook
;;   (lambda ()
;;     (setq ezeka--original-mode-line mode-line-misc-info)
;;     (add-hook 'post-command-hook 'ezeka--magit-mode-line-show-file-type nil t)))

(defvar ezeka--original-mode-line nil
  "Value of `mode-line-misc-info' before we override it.")

(defun ezeka--magit-mode-line-show-file-type ()
  "Display in the mode line the type of the file under cursor."
  (while-no-input
    (redisplay)
    (when-let* ((file
                 (cl-case major-mode
                   (magit-status-mode (magit-file-at-point))
                   (dired-mode (dired-file-name-at-point))
                   (wdired-mode (dired-file-name-at-point))
                   (t (setq mode-line-misc-info ezeka--original-mode-line)
                      nil)))
                (line (magit-file-line file)))
      (setq mode-line-misc-info
        (format "%s"
                (propertize (if (file-symlink-p file)
                                (concat "symlink to " (file-name-base (file-symlink-p file)))
                              "regular file")
                            'face '(:weight bold)))))))

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
            ("C-c #" . ezeka-edit-keywords)
            ("C-c $" . ezeka-kill-ring-save-link-at-point)
            ("C-c %" . ezeka-kill-ring-save-link)
            ("C-c ^" . ezeka-find-ancestor)
            ;; ("C-c &" . ) ; yasnippet
            ;; ("C-c *" . ) ; `org-ctrl-c-star'
            ("C-c (" . ezeka-harmonize-file-name)
            ("C-c )" . ezeka-set-title-or-caption)
            ;; ("C-c -" . ) ; `org-ctrl-c-minus' that turns region into list
            ("C-c _" . ezeka-find-descendant)
            ("C-c =" . ezeka-kill-ring-save-metadata-field) ; `org-table-eval-formula'
            ("C-c +" . ezeka-insert-link-from-clipboard)
            ("C-c [" . ezeka-update-link-description) ; `org-agenda-file-to-front'
            ("C-c ]" . ezeka-add-reading)
            ("C-c |" . ezeka-toggle-update-header-modified) ; `org-table-create-or-convert-from-region'
            ("C-c '" . ezeka-set-label)                     ; `org-edit-special'
            ("C-c \"" . ezeka-insert-ancestor-link)
            ("C-c ," . ezeka-insert-new-child-with-title)
            ("C-c ." . ezeka-insert-or-convert-timestamp) ; `org-table-eval-formula'
            ("C-c /" . ezeka-set-author)                  ; `org-sparse-tree'
            ("C-c ?" . ezeka-links-to)  ; `org-table-field-info'

            ;; shadows `org-open-at-mouse', but allows opening in same window with C-u
            ([C-down-mouse-1] . ezeka-open-link-at-mouse)
            ([mouse-1] . ezeka-open-link-at-mouse)
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
         (when (or (ezeka-file-p (current-buffer))
                   (y-or-n-p "This doesn't look like an Ezeka note. Still enable `ezeka-mode'? "))
           (ezeka--make-header-read-only (current-buffer))

           (add-hook 'before-save-hook 'ezeka--update-file-header nil t)
           (add-hook 'before-save-hook 'ezeka-harmonize-file-name nil t)

           ;; Treat : (colon) as part of the word, allowing
           ;; forward/backward-word over full Zettel links.
           (modify-syntax-entry ?: "w")))
        (t
         (remove-hook 'before-save-hook 'ezeka--update-file-header t)
         (remove-hook 'before-save-hook 'ezeka-harmonize-file-name t)

         (modify-syntax-entry ?: "."))))

(provide 'ezeka)
;;; ezeka.el ends here
