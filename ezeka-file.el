;;; ezeka-file.el --- File handling for Ezeka -*- lexical-binding: t -*-

;; Copyright (C) 2025 Richard Boyechko

;; Author: Richard Boyechko <code@diachronic.net>
;; Created: 2025-04-12
;; Version: 0.1
;; Package-Requires: ((emacs 28.2))
;; Keywords: none
;; URL: https://github.com/boyechko/

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

;;

;;; Code:

(require 'ezeka-base)

;;;=============================================================================
;;; Directories
;;;=============================================================================
(defcustom ezeka-directory nil
  "The central Zettelkasten directory."
  :type 'string
  :group 'ezeka)

(defun in-ezeka-dir (&optional relative-path)
  "Return absolute path to RELATIVE-PATH in the Zettel directory."
  (unless ezeka-directory
    (signal 'ezeka-error "No `ezeka-directory' set"))
  (expand-file-name (or relative-path "") ezeka-directory))

;;;=============================================================================
;;; Files
;;;=============================================================================

(defcustom ezeka-file-extension "txt"
  "Default extension for Zettel files."
  :type 'string
  :group 'ezeka)

(defcustom ezeka-file-name-separator " "
  "String separating the ID from the rest of the file name.
For example, with `ezeka-file-name-format' set to \"%i-%c\",
`ezeka-file-name-separator' should be set to \"-\".")

(defcustom ezeka-find-file-functions '()
  "List of functions to call when finding Ezeka files.
Each function should accept two arguments: the file to find,
and optionally, the source from where the function was
called. TARGET should be a filename; SOURCE can be either a
filename or a symbol describing the source."
  :type 'list
  :group 'ezeka)

(defcustom ezeka-after-save-hook '()
  "Hook ran after an Ezeka file is saved."
  :type 'list
  :group 'ezeka)

;; See https://stackoverflow.com/a/65685019
(defun ezeka--save-buffer-read-only (file)
  "Save the FILE's buffer without running hooks."
  (with-current-buffer (get-file-buffer file)
    (if buffer-read-only
        (save-buffer)
      (read-only-mode 1)
      (save-buffer)
      (read-only-mode 0))))

(defun ezeka--git-stage-file (filename)
  "Stage FILENAME in git."
  (shell-command (format "git add -f \"%s\"" filename)))

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
                (rename-file filename tempname 'okay-if-exists)
                (rename-file tempname newname 'okay-if-exists)
                (file-exists-p newname)))
      (set-visited-file-name newname t t)
      (ezeka--add-to-system-log 'rename-file nil
        'old-name (file-relative-name filename ezeka-directory)
        'new-name (file-relative-name newname ezeka-directory))
      newname)))

(defun ezeka--copy-file (filename newname)
  "Create NEWNAME as a copy of FILENAME.
This is a wrapper around `copy-file' that also adds an entry
into the system log."
  (let ((newname (if (file-name-absolute-p newname)
                     newname
                   (expand-file-name
                    newname
                    (file-name-directory filename)))))
    (when (or (not (file-exists-p filename)) ; filename not saved yet
              (progn
                (copy-file filename newname nil 'keep-time)
                (file-exists-p newname)))
      (set-visited-file-name newname t t)
      (ezeka--add-to-system-log 'copy-file nil
        'old-name (file-relative-name filename ezeka-directory)
        'new-name (file-relative-name newname ezeka-directory))
      newname)))

(defun ezeka--make-symbolic-link (target linkname)
  "Make a symbolic link to TARGET from LINKNAME.
This is a wrapper around `make-symbolic-link' that also adds
an entry into the system log. Both TARGET and LINKNAME
should be files. On success, return LINKNAME."
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
       'note (file-name-base linkname))
     linkname)))

(defun ezeka-file-describe (file)
  "Describe everything known about Ezeka FILE."
  (interactive (list (ezeka--select-file (ezeka--directory-files))))
  (let* (attribs
         (slinkp (file-symlink-p file)))
    (when (file-exists-p file)
      (push "- exists" attribs))
    (push (if slinkp
              (format "- is a symbolic link to `%s'"
                      (file-relative-name slinkp ezeka-directory))
            (format "- is a regular file of %s bytes"
                    (file-attribute-size (file-attributes file))))
          attribs)
    (message (concat "File `" (file-name-base file) "'...\n"
                     (mapconcat #'identity (nreverse attribs) "\n")))))

(defun ezeka--directory-files (&optional kasten filter)
  "Return a list of all Ezeka files in KASTEN matching FILTER.
Unless KASTEN is specified, return a list of all Ezeka files.
FILTER can be a string, in which case it is a regexp that should
match the entire `base-file-name' of the desired file(s), or a
function accepting a file and returning non-nil if it should be
included."
  (let ((all-files (directory-files-recursively
                    (expand-file-name
                     (if kasten
                         (ezeka-kasten-directory (ezeka-kasten kasten))
                       "")
                     ezeka-directory)
                    (concat "^[^#.].*"
                            (when (stringp filter) filter)
                            ezeka-file-extension
                            "$"))))
    (if (functionp filter)
        (cl-remove-if-not filter all-files)
      all-files)))

;;;=============================================================================
;;; File Names
;;;=============================================================================
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

         (work-title-with-same-date
          ;; e.g. John Doe's "Something About Something" (2025) @Doe2025
          (list (rx-to-string `(seq ,given-name
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
         (work-title-with-different-dates
          (list (rx-to-string `(seq ,given-name
                                    " " word-start
                                    (group-n 1 ,family-name)
                                    "'s "
                                    (group-n 2 ,work-title)
                                    "(" (group-n 3 ,date) ")"
                                    (0+ " " (group-n 4 (any "&@") (1+ word) (1+ digit))))
                              'no-group)
                "\\2 (\\3) \\4"
                'regexp))
         (movie-title
          (list (rx-to-string `(seq (group-n 2 ,work-title)
                                    " "
                                    "(dir."
                                    ,given-name
                                    (group-n 1 ,family-name)
                                    ", "
                                    (group-n 3 ,date)
                                    ")"))
                "\\2"))
         (complex-replacements (list work-title-with-same-date
                                     work-title-with-different-dates
                                     movie-title))
         (simple-replacements
          '(("\"\\<\\([^\"]+\\)\\>\"" "'\\1'" regexp)
            ("\\</\\([^/]+\\)/\\>" "_\\1_" regexp)
            ("\\(\\w\\)/\\(\\w\\)" "\\1-\\2" regexp) ; slash between words
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

(defun ezeka-file-kasten (file)
  "Return the Kasten of the given Zettel FILE."
  (when-let ((id (or (ezeka-file-name-id file)
                     (file-name-base file))))
    (cl-find-if (lambda (re) (string-match-p (rx bos (regexp re) eos) id))
                (ezeka-kaesten)
                :key #'ezeka-kasten-id-regexp)))

(defun ezeka-directory-kasten (directory)
  "Return the kasten name of the given Zettel DIRECTORY."
  ;; FIXME: This is a hack that would not work if Kasten names don't match the
  ;; directory name.
  (file-name-base (directory-file-name directory)))

;;;=============================================================================
;;; User-Specific Kästen
;;;=============================================================================

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

;;------------------------------------------------------------------------------
;; Add contemporary Kästen
;;------------------------------------------------------------------------------

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

;;------------------------------------------------------------------------------
;; Add historic Kästen
;;------------------------------------------------------------------------------

(defun ezeka--kasten-hundreds-subdir-func (id)
  "Return the hundreds subdirectory for ID."
  (format "%c00-%c99" (elt id 0) (elt id 0)))

(ezeka-kasten-new "v1"
                  :id-regexp "[0-9]\\{3\\}\\(-[A-Z]\\(-[0-9][0-9]\\)*\\)*"
                  :minimal-id "123"
                  :subdir-func #'ezeka--kasten-hundreds-subdir-func)
(ezeka-kasten-new "v2"
                  :id-regexp "[0-9]\\{3\\}\\(-[a-z]+\\)*"
                  :minimal-id "123"
                  :subdir-func #'ezeka--kasten-hundreds-subdir-func)
(ezeka-kasten-new "v3"
                  :id-regexp "[0-9]\\{3\\}-[a-z]\\{3\\}"
                  :minimal-id "123-abc"
                  :subdir-func #'ezeka--kasten-hundreds-subdir-func)

;;;=============================================================================
;;; IDs
;;;=============================================================================

(define-error 'ezeka-unknown-id-type-error
  "Unknown type of ID: `%s'"
  'ezeka-error)

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

;; TODO Replace with `ezeka-file-name-id'?
;; TODO Replace "link" with "id," reserving "link" term for actual links
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

;; TODO Replace "link" with "id," reserving "link" term for actual links
;; TODO Replace with `ezeka-id-valid-p'?
(defun ezeka-link-p (string)
  "Return non-NIL if the STRING could be a link to a Zettel."
  (when (stringp string)
    (string-match-p (ezeka-link-regexp 'match-entire) string)))

;; TODO Replace "link" with "id," reserving "link" term for actual links
;; TODO Replace with `ezeka-id-kasten'?
(defun ezeka-link-kasten (link)
  "Return the name of the Kasten that matches LINK's ID type."
  (if (string-match (ezeka-link-regexp 'match-entire) link)
      (let* ((id (match-string 1 link))
             (kasten (match-string 2 link)))
        (or kasten
            (ezeka-kasten-name
             (cl-find (ezeka-id-type id)
                      (ezeka-kaesten)
                      :key #'ezeka-kasten-id-type))))
    (signal 'wrong-type-argument (list 'ezeka-link-p link))))

;; TODO Deprecate?
(defun ezeka-link-id (link)
  "Return the ID part of the given LINK."
  (save-match-data
    (when (string-match (ezeka-link-regexp) link)
      (match-string 1 link))))

;; TODO Deprecate?
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
     (file-name-concat (expand-file-name
                        (ezeka-kasten-directory eka)
                        ezeka-directory)
                       (or (funcall (ezeka-kasten-subdir-func eka) id)
                           "")))))

(defun ezeka--id-kaesten (id)
  "Return all kaesten for the ID's type."
  (let ((type (ezeka-id-type id)))
    (mapcar #'car
            (cl-remove-if-not (lambda (x)
                                (eq (cadr x) type))
                              (ezeka-kaesten)))))

;; TODO Replace "link" with "id," reserving "link" term for actual links
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
            ((eq t files)
             (warn "Found multiple symbolic links for `%s':\n    %s"
                   link
                   (mapcar #'file-name-base matches))
             (setq result (ezeka--select-file matches
                                              "Multiple matches found. Select one: "
                                              'require-match))
             (kill-new (mapconcat (lambda (m)
                                    (format "(delete-file \"%s\")" m))
                                  (cl-remove result matches :test #'string=)
                                  " "))
             (message "delete-file call saved to kill ring")
             result)
            ((not (cdr files)) (car files))
            (t
             (warn "Found multiple matches for `%s':\n- %s"
                   link
                   (mapconcat #'file-name-base matches "\n- "))
             (ezeka--select-file matches
                                 "Multiple matches found. Select one: "
                                 'require-match))))))

;; TODO Replace "link" with "id," reserving "link" term for actual links
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

;;------------------------------------------------------------------------------
;; Numerus Currens
;;------------------------------------------------------------------------------

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

;;------------------------------------------------------------------------------
;; Tempus Currens
;;------------------------------------------------------------------------------

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

;;------------------------------------------------------------------------------
;; Scriptum
;;------------------------------------------------------------------------------

(defun ezeka-scriptum-id (&optional project time)
  "Return a scriptum ID based on PROJECT and Emacs TIME object.
If TIME is nil, default to current time."
  (let ((_scriptum-id
         (lambda (project n)
           "Return scriptum ID as a string based on PROJECT and N."
           (format "%s~%02d" project n))))
    (while (not project)
      (setq project
        (or (ezeka--select-file
             (ezeka--directory-files (ezeka-kasten "numerus"))
             "Select project: ")
            (ezeka-link-file
             (ezeka--read-id
              "Scriptum project (numerus currens): "
              :numerus)))))
    (setq project (ezeka-file-link project))
    ;; TODO: If this is first entry in scriptum project, create a project
    ;; heading <numerus>~00 with caption for the project? Or a symbolic link
    ;; to numerus?
    (funcall _scriptum-id
             project
             (cl-find-if-not (lambda (n)
                               (ezeka-link-file
                                (funcall _scriptum-id project n)))
                             (number-sequence 1 99)))))

(provide 'ezeka-file)
;;; ezeka-file.el ends here
