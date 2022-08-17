;;; zettel.el --- Eclectic Zettelkasten -*- lexical-binding: t -*-

;; Copyright (C) 2015-2022 Richard Boyechko

;; Author: Richard Boyechko <code@diachronic.net>
;; Version: 0.1
;; Package-Requires: ((emacs "25.1") (org "9.5"))
;; Keywords: zettelkasten org
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

;; This package provides a very personalized implementation of Zettelkasten
;; relying on Org, Deft (now moved to zettel-deft.el) that began on 2015-06-31.

(require 'org)

;;;=============================================================================
;;; Internal Variables
;;;=============================================================================

(defun in-zettel-dir (&optional relative-path)
  "Returns absolute pathname of the given pathspec relative to
the Zettel directory."
  (expand-file-name (or relative-path "") zettel-directory))

;; FIXME: temporary
(defvar zettel-regexp-bolus-currens
  "\\([0-9]\\{3\\}\\)-\\([a-z]\\{3\\}\\)"
  "The regular expression that matches bolus currens like abc-123.")

(defvar zettel-regexp-numerus-currens
  "\\([a-z]\\)-\\([0-9]\\{4\\}\\)"
  "The regular expression that matches numerus currens like d-0503.")

(defvar zettel-regexp-tempus-currens
  "\\([0-9]\\{4\\}\\)\\([0-9][0-9]\\)\\([0-9][0-9]\\)T\\([0-9][0-9]\\)\\([0-9][0-9]\\)"
  "The regular expression that matches the basic (but not extended) ISO 8601
timestamp.
Groups 1-3 are year, month, day.
Groups 4-5 are hour, minute.")

;; FIXME: Is this or the individually-named variables redundant?
(defvar zettel-type-regexp-alist
  `((:bolus   . ,zettel-regexp-bolus-currens) ; FIXME: temporary
    (:numerus . ,zettel-regexp-numerus-currens)
    (:tempus  . ,zettel-regexp-tempus-currens))
  "An alist of type and its regular expressions for the various slug types.")

(defvar zettel-type-example-alist
  '((:bolus   . "123-abc") ; FIXME: temporary
    (:numerus . "a-1234")
    (:tempus  . "20210123T1234"))
  "An alist of type and an example of what it looks like for the various slug
types.")

(defvar zettel-regexp-slug
  ;; Strip the groups in the component regexps
  (concat "\\("
          (replace-regexp-in-string "\\\\[()]" "" zettel-regexp-numerus-currens)
          "\\|"
          (replace-regexp-in-string "\\\\[()]" "" zettel-regexp-tempus-currens)
          ;; FIXME: temporary
          "\\|"
          (replace-regexp-in-string "\\\\[()]" "" zettel-regexp-bolus-currens)
          "\\)")
  "A generalized regexp that matches any slug, whatever its slug type.")

(defvar zettel-regexp-link
  (concat "\\(\\([[:alpha:]]+\\):\\)*" zettel-regexp-slug)
  "The regular expression that matches Zettel links.
Group 2 is the kasten, if specified.
Group 3 is the slug.")

(defvar zettel-regexp-iso8601-date
  "\\<\\([0-9]\\{4\\}\\)-*\\([0-9]\\{2\\}\\)-*\\([0-9]\\{2\\}\\)"
  "The regular expression that matches ISO 8601-like date.
Groups 1-3 are year, month, day.")

(defvar zettel-regexp-iso8601-time
  "T*\\([0-9]\\{2\\}\\):*\\([0-9]\\{2\\}\\)\\>"
  "The regular expression that matches ISO 8601-like time.
Groups 1-2 are hour and minute.")

(defvar zettel-regexp-iso8601-datetime
  (concat zettel-regexp-iso8601-date zettel-regexp-iso8601-time)
  "The regular expression that matches ISO 8601 date and time separate with T.
Groups 1-3 are year, month, day.
Groups 4-5 are hour and minute.")

(defvar zettel-pregenerated-numeri "auto/unused-numeri.dat"
  "List of unused numri curentes to use for creating new numerus currens
Zettel in rumen when Emacs cannot check the list of existing files.")

;;;=============================================================================
;;; User Variables
;;;=============================================================================

(defcustom zettel-directory nil
  "The central Zettelkasten directory."
  :type 'string
  :group 'zettel)

(defcustom zettel-file-extension "txt"
  "Default extension for Zettel files."
  :type 'string
  :group 'zettel)

(defcustom zettel-kaesten
  ;; name | directory | slug type | list-order
  `(("os"         :tempus  1)
    ("rumen"      :numerus 2)
    ("esophagus"  :numerus 3)
    ("omasum"     :tempus  4)
    ("abomasum"   :tempus  5)
    ("rectum"     :tempus  6)
    ("fabula"     :tempus  7)
    ("machina"    :tempus  8))
  "An alist containing the names and slug types of kaesten."
  :type 'alist
  :group 'zettel)

(defcustom zettel-kaesten-aliases nil
  "An alist of any other aliases for the `zettel-kaesten'. This is an alist of
the actual name followed by the alias."
  :type 'alist
  :group 'zettel)

(defcustom zettel-default-kasten
  ;; slug type | kasten name
  `((:numerus . "rumen")
    (:bolus . "esophagus")              ; FIXME: temporary
    (:tempus . "omasum"))
  "An alist of default Kasten (i.e. not requiring fully qualified link) for
each slug type."
  :type 'alist
  :group 'zettel)

(defcustom zettel-categories nil
  "A list of categories used for Zettel."
  :type 'list
  :group 'zettel)

(defcustom zettel-number-of-frames nil
  "Try to use only this many frames. Nil means single frame."
  :type 'symbol
  :options '(one two many)
  :group 'zettel)

;;;=============================================================================
;;; General Functions
;;;=============================================================================

(defun space-or-punct-p (char)
  "Returns T if the character is a space or punctuation."
  (when char
    (string-match-p "[[:space:][:punct:]]" (char-to-string char))))

;; TODO: More extensible way to do this without invoking other modes?
(defun zettel--grab-dwim-file-target (&optional link-at-point)
  "Returns the do-what-I-mean Zettel file from a variety of modes. If
LINK-AT-POINT is non-nil, prioritize such a link if exists."
  (cond ((zettel-link-at-point-p)
         (zettel-link-file (zettel-link-at-point) t))
        ((and zettel-mode (zettel-p buffer-file-name t))
         buffer-file-name)
        ((eq major-mode 'magit-status-mode)
         (magit-file-at-point))
        ((eq major-mode 'deft-mode)     ; TODO Factor out
         (--if-let (button-at (point))
             (button-get it 'tag)
           (zettel-ivy-select-link)))
        (t
         (zettel-ivy-select-link))))

;;;=============================================================================
;;; Fundamental Functions
;;;=============================================================================

(defun zettel-p (file-or-buffer &optional strict)
  "Returns non-NIL if the file or buffer is a Zettel. It is a Zettel if all
of these conditions are met:
1) the file exists;
2) its extension is `zettel-file-extension';
3) its filename matches `zettel-regexp-slug'; and, if STRICT is non-NIL,
4) the file is inside `zettel-directory'."
  (interactive "f")
  (when file-or-buffer
    (let ((file (cl-typecase file-or-buffer
                  (buffer (buffer-file-name file-or-buffer))
                  (string (expand-file-name file-or-buffer))
                  (t
                   (signal 'type-error
                           '("FILE-OR-BUFFER can only be file or buffer"))))))
      (when file
        (and (string-equal (file-name-extension file) zettel-file-extension)
             (string-match zettel-regexp-slug (file-name-base file))
             (if strict
                 (string-prefix-p zettel-directory file)
               t))))))

(defun zettel-kasten-directory (kasten)
  "Returns the directory of the given KASTEN."
  (if (assoc kasten zettel-kaesten)
      (file-name-as-directory (in-zettel-dir (zettel-kasten-truename kasten)))
    (error "Unknown Kasten: %s" kasten)))

(defun zettel-directory-kasten (directory)
  "Returns the kasten name of the given Zettel directory."
  ;; FIXME: This is a hack that would not work if Kasten names don't match the
  ;; directory name.
  (file-name-base (directory-file-name directory)))

(defun zettel-kasten-truename (kasten)
  "Returns the true name of the given KASTEN."
  (or (cdr (assoc kasten zettel-kaesten-aliases))
      (car (assoc kasten zettel-kaesten))))

(defun zettel-file-slug (file)
  "Returns the slug part of the given Zettel file."
  (file-name-base file))

;; FIXME: Relies on the fact that the Kasten directory is 2nd from the last.
(defun zettel-file-kasten (file)
  "Returns the kasten of the given Zettel file."
  (let ((dirs (reverse (split-string (file-name-directory file) "/" t "/"))))
    (cond ((assoc (car dirs) zettel-kaesten)
           (zettel-kasten-truename (car dirs)))
          ((assoc (cadr dirs) zettel-kaesten)
           (zettel-kasten-truename (cadr dirs)))
          (t
           (error "Can't figure out kasten for %s" file)))))

(defun zettel-file-link (file)
  "Given the path to a Zettel FILE, returns a fully qualified link to it."
  (let ((kasten (zettel-file-kasten file)))
    (if (string= kasten
                 (alist-get (zettel-type file) zettel-default-kasten))
        (zettel-file-slug file)
      (concat kasten ":" (zettel-file-slug file)))))

(defun zettel-link-p (string)
  "Returns non-NIL if the string could be a link to a Zettel."
  (and (string-match (concat "^" zettel-regexp-link "$") string)
       ;; If kasten is specified, make sure it's a valid one
       (if (match-string-no-properties 2 string)
           (or (assoc (match-string-no-properties 2 string) zettel-kaesten)
               (assoc (match-string-no-properties 2 string) zettel-kaesten-aliases))
         t)))

(defun zettel-link-kasten (link)
  "Returns the kasten part of the given LINK. If no kasten is explicitly
specified, asks the user to resolve the ambiguity."
  (when (string-match zettel-regexp-link link)
    (let* ((kasten (match-string 2 link))
           (slug (match-string 3 link))
           (type (zettel-type slug)))
      (or kasten
          (if-let ((default (alist-get type zettel-default-kasten)))
              default
            (call-interactively #'zettel-set-default-kasten)
            (zettel-link-kasten link))))))

(defun zettel-set-default-kasten (type kasten)
  "Interactively set the default kasten for the given slug type. See
`zettel-default-kasten' for valid types."
  (interactive
   (list (intern (ivy-read "Set default for which type of Zettel? "
                           (mapcar #'first zettel-default-kasten)))
         (ivy-read "Set the default to what Kasten? "
                   (if (listp zettel-kaesten)
                       (mapcar #'first zettel-kaesten)
                     (error "No Zettelkästen defined")))))
  (setf (alist-get type zettel-default-kasten) kasten))

;; FIXME: Rename `zettel-type' to `zettel-slug-type'?
(defun zettel-kasten-slug-type (kasten)
  "Returns the Zettel slug naming type for the given kasten based on
`zettel-kaesten'."
  (cadr (assoc kasten zettel-kaesten #'string=)))

(defun zettel-link-slug (link)
  "Returns the slug part of the given LINK."
  (when (string-match zettel-regexp-link link)
    (match-string 3 link)))

(defun zettel-make-link (kasten slug)
  "Make a new proper link to SLUG in KASTEN."
  (let ((slug-type (zettel-kasten-slug-type kasten)))
    (cond ((not slug-type)
           (error "Unknown kasten: %s" kasten))
          ((not
            (string-match-p (alist-get slug-type zettel-type-regexp-alist) slug))
           (error "Slug doesn't match the slug type for %s kasten" kasten))
          ((rassoc kasten zettel-default-kasten)
           slug)
          (t
           (concat kasten ":" slug)))))

(defun zettel-numerus-subdirectory (slug)
  "Returns the right subdirectory for the given numerus currens slug."
  (when (string-match zettel-regexp-numerus-currens slug)
    (file-name-as-directory (cl-subseq slug 0 1))))

(defun zettel-tempus-subdirectory (slug)
  "Returns the right subdirectory for the given tempus currens slug."
  (when (string-match zettel-regexp-tempus-currens slug)
    (file-name-as-directory (match-string 1 slug))))

(defun zettel-bolus-subdirectory (slug)
  "Finds the right directory for the given bolus currens slug."
  (when (stringp slug)
    (let ((result
           (cl-case (elt slug 0)
             (?0 "000-099")
             (?1 "100-199")
             (?2 "200-299")
             (?3 "300-399")
             (?4 "400-499")
             (?5 "500-599")
             (?6 "600-699")
             (?7 "700-799")
             (?8 "800-899")
             (?9 "900-999"))))
      (when result
        (file-name-as-directory result)))))

(defun zettel-link-file (link &optional noerror)
  "Return a full file path to the Zettel LINK. If NOERROR is non-NIL,
don't signal an error if the link is invalid."
  (if (zettel-link-p link)
      (let ((kasten (zettel-link-kasten link))
            (slug (zettel-link-slug link)))
        (expand-file-name
         (concat slug "." zettel-file-extension)
         (expand-file-name
          (cl-case (zettel-type slug)
            (:numerus (zettel-numerus-subdirectory slug))
            (:tempus (zettel-tempus-subdirectory slug))
            (:bolus (zettel-bolus-subdirectory slug)) ; FIXME: temporary
            (t (unless noerror
                 (error "This is not a proper Zettel link: %s" link))))
          (zettel-kasten-directory kasten))))
    (unless noerror
      (error "This is not a proper Zettel link: %s" link))))

;; FIXME: Rename `zettel-type' to `zettel-slug-type'?
(defun zettel-type (slug-or-file)
  "Returns the type of the given slug or file: :NUMERUS or :TEMPUS."
  (let ((slug (file-name-base slug-or-file)))
    (cond ((string-match-p zettel-regexp-tempus-currens slug)
           :tempus)
          ((string-match-p zettel-regexp-numerus-currens slug)
           :numerus)
          ;; FIXME: Temporary
          ((string-match-p zettel-regexp-bolus-currens slug)
           :bolus)
          (t
           ;; Anything else is not a Zettel
           nil))))

(defun zettel-encode-iso8601-datetime (string)
  "Returns the internal encoded time given the ISO8601 date/time
expression, with or without time."
  (let ((cadr 0) (minute 0) (hour 0) day month year)
    (when (string-match (concat "^" zettel-regexp-iso8601-date) string)
      (setq year  (string-to-number (match-string 1 string))
            month (string-to-number (match-string 2 string))
            day   (string-to-number (match-string 3 string)))
      (when (string-match zettel-regexp-iso8601-time string
                          (match-end 0))
        (setq hour   (string-to-number (match-string 1 string))
              minute (string-to-number (match-string 2 string))))
      (encode-time second minute hour day month year))))

(defun zettel-file-content (file &optional metadata-only noerror)
  "Returns the content of the FILE, getting it either from an opened buffer
or the file itself. If NOERROR is non-NIL, don't signal an error if cannot
get the content. If METADATA-ONLY is non-nil, only get the metadata."
  (cl-flet ((retrieve-content ()
              "Get the content from `current-buffer'."
              (widen)
              (buffer-substring-no-properties
               (point-min)
               (if (not metadata-only)
                   (point-max)
                 (goto-char (point-min))
                 (if (re-search-forward "\n\n" nil t)
                     (match-beginning 0)
                   (unless noerror
                     (error "Cannot separate metadata")))))))
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
;;; Metadata
;;;=============================================================================

(defvar zettel-regexp-metadata-line
  "\\(\\w+\\):\\s-+\\(.*\\)"
  "The regular expression that matches a YAML metadata line.
Group 1 is the key.
Group 2 is the value.")

(defvar zettel-regexp-combined-title
  (concat "^§"
          zettel-regexp-link
          "\\. \\({\\([^}]+\\)}\\)*\\([^#@]+\\)*\\(@\\S-+\\)*\\(#.+\\)*")
  "Regular expression for a combined title string, used in `zettel-file-metadata'.
Group 2 is the kasten.
Group 3 is the slug.
Group 5 is the category.
Group 6 is the title itself.
Group 7 is the citation key.
Group 8 is the keyword block.")

(defun zettel-decode-combined-title (combined)
  "Returns an alist of metadata from a combined title. If cannot decode,
returns NIL."
  (when (and combined (string-match zettel-regexp-combined-title combined))
    (let ((slug     (match-string 3 combined))
          (category (match-string 5 combined))
          (title    (match-string 6 combined))
          (citekey  (match-string 7 combined))
          (keywords (match-string 8 combined)))
      (list (cons :slug slug)
            (cons :type (zettel-type slug))
            (cons :category category)
            (cons :title (if title (string-trim title) ""))
            (when citekey (cons :citekey (string-trim citekey)))
            (when keywords (cons :keywords (list (string-trim keywords))))))))

(defun zettel-encode-combined-title (metadata)
  "Returns a list of two elements: 1) string that encodes into the title line
the given METADATA, and 2) leftover metadata."
  (list (format "§%s. {%s} %s%s"
                (alist-get :link metadata)
                (or (alist-get :category metadata) "Unset")
                (alist-get :title metadata)
                (if (alist-get :citekey metadata)
                    (concat " " (alist-get :citekey metadata))
                  ""))
        (cl-set-difference metadata
                           '((:link) (:category) (:title) (:type) (:citekey))
                           :key #'car)))

(defun zettel-metadata-yaml-key (keyword)
  "Returns a YAML-formatted string that is the name of the KEY, a keyword
symbol."
  (cl-subseq (symbol-name keyword) 1))

(defun zettel-metadata-yaml-value (value)
  "Returns a YAML-formatted string for the given metadata VALUE."
  (cl-typecase value
    (string value)
    (list (concat "[ " (mapconcat #'identity value ", ") " ]"))
    (t
     (error "Not implemented for type %s" (type-of value)))))

(defun zettel-normalize-metadata (file &optional metadata)
  "Replaces the FILE's metadata section with either the given METADATA or
by parsing the FILE's metadata."
  (let ((metadata (or metadata (zettel-file-metadata file)))
        (old-point (point)))
    (save-mark-and-excursion
      (with-current-buffer (get-file-buffer file)
        (save-restriction
          (goto-char (point-min))
          (when (re-search-forward "^$" nil t 1)
            (narrow-to-region (point-min) (point)))
          (delete-region (point-min) (point-max))
          (cl-multiple-value-bind (title remaining-metadata)
              (zettel-encode-combined-title metadata)
            (insert "title: " title "\n")
            (mapc (lambda (cons)
                    (insert (format "%s: %s\n"
                                    (zettel-metadata-yaml-key (car cons))
                                    (zettel-metadata-yaml-value (cdr cons)))))
                  (let (ordered-metadata)
                    (dolist (key '(:subtitle :author
                                   :created :modified
                                   :parent :firstborn :oldnames
                                   :readings :keywords)
                                 (nreverse ordered-metadata))
                      (when (alist-get key remaining-metadata)
                        (push (cons key (alist-get key remaining-metadata))
                              ordered-metadata)))))))))
    ;; `Save-excursion' doesn't seem to restore the point, possibly because the
    ;; file is changed, so need to do it manually.
    (goto-char old-point)))

(defun zettel-decode-metadata-section (yaml-section file)
  "Returns an alist of metadata decoded from the given yaml metadata section
of FILE. They keys are converted to keywords."
  (let* ((metadata
          (mapcar
           (lambda (line)
             (when (> (length line) 0)
               (if (string-match zettel-regexp-metadata-line line)
                   (let ((key (intern (concat ":" (match-string 1 line))))
                         (value (string-trim (match-string 2 line) " " " ")))
                     (cons key
                           ;; Handle lists properly
                           (if (string-match "^\\[\\(.*\\)\\]$" value)
                               (split-string (match-string 1 value)
                                             "," t "[[:space:]]+")
                             value)))
                 (error "Malformed metadata line: '%s'" line))))
           (split-string yaml-section "\n")))
         (title (alist-get :title metadata))
         (decoded (zettel-decode-combined-title title)))
    ;; When successfully decoded combined title, replace the original title with
    ;; the decoded metadata.
    (when decoded
      (setq metadata
        (append decoded (cl-remove :title metadata :key #'car))))
    (push (cons :kasten (zettel-file-kasten file)) metadata)
    (push (cons :link (zettel-file-link file)) metadata)))

(defun zettel-file-metadata (file)
  "Returns an alist of metadata for the given FILE based on the most current
content of the FILE. They keys are converted to keywords."
  (zettel-decode-metadata-section (zettel-file-content file t t)))

(defcustom zettel-update-modification-date t
  "Determines whether `zettel-update-metadata-date' updates the modification
date. Possible choices are ALWAYS, SAMEDAY, NEVER, or CONFIRM (or T)."
  :type 'symbol)

(defun zettel-update-metadata-date ()
  "Updates the modification time in the metadata section of the Zettel in the
current buffer according to the value of `zettel-update-modifaction-date'."
  (interactive)
  (when (zettel-p buffer-file-name)
    (let* ((today (format-time-string "%Y-%m-%d"))
           (now (format-time-string "%Y-%m-%d %a %H:%M"))
           (metadata (zettel-file-metadata buffer-file-name))
           (last-modified (or (alist-get :modified metadata)
                              (alist-get :created metadata))))
      (unless (string-equal (or last-modified "") now)
        ;; FIXME: Probably better to convert modification times to Emacs's encoded
        ;; time rather than doing it with strings.
        (when (or (equal zettel-update-modification-date 'always)
                  (and (equal zettel-update-modification-date 'sameday)
                       (string= (cl-subseq last-modified 0 (length today)) today))
                  ;; Automatic updating conditions not met; need to confirm
                  (and (member zettel-update-modification-date '(sameday confirm t))
                       (y-or-n-p
                        (format "%s last modified at %s. Update to now? "
                                (file-name-base buffer-file-name)
                                last-modified))))
          (setf (alist-get :modified metadata) now)))
      (zettel-normalize-metadata buffer-file-name metadata))))

(defun zettel-update-title ()
  "Interactively asks for a different title and updates the Zettel's metadata."
  (interactive)
  (when (zettel-p buffer-file-name)
    (let ((metadata (zettel-file-metadata buffer-file-name)))
      (setf (alist-get :title metadata)
            (read-string "Change title to what? "
                         (alist-get :title metadata)))
      (zettel-normalize-metadata buffer-file-name metadata))))

;;;=============================================================================
;;; Numerus Currens
;;;=============================================================================

(defun zettel-make-numerus (number letters)
  "Returns a new numerus currens slug composed of the NUMBER and LETTERS,
both of which are strings."
  (concat number "-" letters))

(defun zettel-numerus-number (slug)
  "Returns the number part of the SLUG as a string."
  (when (string-match zettel-regexp-numerus-currens slug)
    (match-string 1 slug)))

(defun zettel-numerus-letters (slug)
  "Returns the letters part of the SLUG as a string."
  (when (string-match zettel-regexp-numerus-currens slug)
    (match-string 3 slug)))

(defun zettel-numerus-parts (slug)
  "Returns NIL if the slug is not a numerus currens slug, and otherwise
returns a list of two elements: the number and letters parts of the slug."
  (when (and (stringp slug)
             (string-match zettel-regexp-numerus-currens slug))
    (list (match-string 1 slug) (match-string 3 slug))))

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

(defun zettel-generate-new-slug (type)
  "Generates a random new slug of the given type."
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

(defun zettel-next-unused-slug (kasten)
  "Returns the next unused slug for the given KASTEN from `zettel-kaesten'."
  (let ((type (zettel-kasten-slug-type kasten))
        slug)
    (cl-flet ((exists? ()
                "Checks if SLUG is either NIL or exists."
                (or (null slug)
                    (file-exists-p
                     (zettel-link-file (zettel-make-link kasten slug))))))
     (cond ((eq type :tempus)
            (while (exists?)
              (setq slug (zettel-generate-new-slug type))))
           ((and (eq type :numerus)
                 (file-exists-p (in-zettel-dir zettel-pregenerated-numeri)))
            (let ((buffer (find-file-noselect
                           (in-zettel-dir zettel-pregenerated-numeri))))
              (with-current-buffer buffer
                (while (exists?)
                  (setq slug
                    (string-trim (delete-and-extract-region
                                  1 (search-forward-regexp "[[:space:]]" nil t)))))
                (let ((inhibit-message t)) ; don't show the "Wrote ..." messages
                  (basic-save-buffer))
                (message "%d pregenerated numerus/i left"
                         (count-lines (point-min) (point-max))))))
           (t
            (while (exists?)
              (setq slug (zettel-generate-new-slug type))))))
    slug))

;;;=============================================================================
;;; Tempus Currens
;;;=============================================================================

(defun zettel-decode-time-into-tempus-currens (time)
  "Returns a tempus currens slug based on the given Emacs time object."
  (format-time-string "%Y%m%dT%H%M" time))

(defun zettel-tempus-currens-slug-for (link)
  "Returns a suitable tempus currens slug for the given Zettel link."
  (if (eq (zettel-kasten-slug-type (zettel-link-kasten link)) :tempus)
      ;; If already tempus currens, just return that slug
      (zettel-link-slug link)
    ;; Otherwise come up with an appropriate slug based on the metadata
    (let ((metadata (zettel-file-metadata (zettel-link-file link)))
          oldname)
      (cond ((setq oldname
               (cl-find-if
                (lambda (l)
                  (eq (zettel-kasten-slug-type (zettel-link-kasten l))
                      :tempus))
                (alist-get :oldnames metadata)))
             ;; One of the old names was a tempus currens; just use that
             (zettel-link-slug oldname))
            ((alist-get :created metadata)
             ;; Use the created metadata and make up the time of creation
             ;; FIXME: Any more elegant way to do this?
             (zettel-decode-time-into-tempus-currens
              ;; TODO: This needs to handle org-mode timestamps in metadata
              (zettel-encode-iso8601-datetime
               (concat (alist-get :created metadata)
                       "T"
                       (format-time-string "%H:%M")))))
            (t
             ;; Can't figure out automatically; ask the user
             (read-string "No created metadata; make up your own name: "
                          (zettel-next-unused-slug (zettel-link-kasten link))))))))

;;;=============================================================================
;;; Zettel Links
;;;=============================================================================

(defun zettel-link-at-point-p (&optional freeform)
  "Returns non-nil if the thing at point is a wiki link (i.e. [[XXX]]). The
first group is the link target. If FREEFORM is non-nil, also consider Zettel
links that are not enclosed in square brackets."
  (thing-at-point-looking-at
   (if freeform
       (concat "\\(" zettel-regexp-link "\\)")
     (concat "\\[\\[\\(" zettel-regexp-link "\\)\\]\\(\\[[^]]+\\]\\)*\\]"))))

(defun zettel-link-at-point ()
  "Return the Zettel link at point. Needs to be called after
`zettel-link-at-point-p'."
  (match-string-no-properties 1))

(defun zettel-find-file (file &optional same-window)
  "Edit the given file based on the value of `zettel-number-of-frames'. If
SAME-WINDOW is non-NIL, opens the buffer visiting the file in the same
window."
  (if same-window
      (find-file file)
    (cl-case zettel-number-of-frames
      (two (if (< (length (frame-list)) 2)
               (find-file-other-frame file)
             (select-window (ace-select-window))
             (find-file file)))
      (one (let ((pop-up-windows t))
             (select-window (ace-select-window))
             (find-file file)))
      (nil (find-file file))
      (t (find-file-other-frame file)))))

(defun zettel-find-link (link &optional same-window)
  "Attempts to find the given Zettel link based on the value of
`zettel-number-of-frames'. If SAME-WINDOW is non-NIL, opens the link in the
same window. Returns T if the link is a Zettel link."
  (when (zettel-link-p link)
    (zettel-find-file (zettel-link-file link) same-window)
    (when (zerop (buffer-size))
      (call-interactively #'zettel-insert-metadata-template))
    ;; make sure to return T for `org-open-link-functions'
    t))

(defun zettel-kill-link-or-sexp-at-point (&optional arg)
  "If there is a Zettel link at point, kill it, including the square
brackets. Otherwise, call `kill-sex'."
  (interactive "p")
  (if (zettel-link-at-point-p)
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

(defun zettel-org-format-link (target &optional description)
  "Returns a formatted org-link to TARGET, which can be either a link or a filepath."
  (let* ((file (or (if (file-name-absolute-p target)
                       target
                     (zettel-link-file target))
                   (error "Link target doesn't exist; make sure it's saved")))
         (link (zettel-file-link file)))
    (format "[[%s]%s]"
            link
            (if description
                (format "[%s]" description) ""))))

(defun zettel-insert-link-with-metadata (link &optional field where confirm)
  "Inserts the Zettel link, optionally adding a metadata FIELD put
WHERE (:BEFORE, :AFTER, or in :DESCRIPTION). If CONFIRM is non-NIL, ask for
confirmation before inserting metadata."
  (let* ((file (zettel-link-file link))
         (metadata (zettel-file-metadata file))
         (field (or field
                    (when (called-interactively-p 'any)
                      (intern-soft
                       (ivy-read
                        "Which metadata field? "
                        '(":none" ":title" ":citekey" ":category"))))))
         (value (alist-get field metadata))
         (where (or where
                    (when field
                      (intern-soft
                       (ivy-read "Where? "
                                 '(":before" ":after" ":description")))))))
    (insert (if (or (bolp) (space-or-punct-p (char-before))) "" " ")
            (if (or (null value)
                    (not confirm)
                    (progn
                      ;; Pressing return just defaults to NO rather than quit
                      (define-key query-replace-map [return] 'act)
                      (y-or-n-p (format (if (eq where :description)
                                            "Insert %s in the link %s? "
                                          "Insert %s %s the link? ")
                                        field where))))
                (concat (if (eq where :before)
                            (concat value " ")
                          "")
                        (zettel-org-format-link
                         link
                         (when (eq where :description)
                           value))
                        (if (eq where :after)
                            (concat " " value)
                          ""))
              (zettel-org-format-link link))
            (if (or (eolp) (space-or-punct-p (char-after))) "" " "))))

(defun zettel-insert-link-from-clipboard (arg)
  "Link `zettel-insert-link' but attempts to get the link slug from OS
clipboard, inserting it with metadata. With prefix argument, insert just the
link itself."
  (interactive "P")
  (let ((link (gui-get-selection 'CLIPBOARD))
        (backlink (when buffer-file-name
                    (zettel-file-link buffer-file-name))))
    (when (zettel-link-p link)
      (if arg
          (zettel-insert-link-with-metadata link)
        (zettel-insert-link-with-metadata link :title :before t))
      (when backlink
        (gui-set-selection 'CLIPBOARD backlink)
        (message "Backlink to %s copied to clipboard" backlink)))))

(defun zettel-kill-ring-save-link-title (arg)
  "Save the title to the kill ring and system clipboard of either the Zettel
link at point or, if there is none, the current buffer. With prefix argument,
saves the 'combinted title'."
  (interactive "P")
  (let ((file (zettel--grab-dwim-file-target t)))
    (when file
      (let* ((metadata (zettel-file-metadata file))
             (title (if arg
                        (car (zettel-encode-combined-title metadata))
                      (alist-get :title metadata))))
        (kill-new title)
        (unless select-enable-clipboard
          (gui-set-selection 'CLIPBOARD title))
        (message "Saved [%s] in the kill ring" title)))))

(defun zettel-kill-ring-save-link (arg)
  "Save the Zettel link at point or the buffer base filename in the kill ring
to be used as a wiki link elsewhere. With prefix argument, save the file name
relative to `zettel-directory' instead. With two prefix arguments, open the
file in Finder with it selected."
  (interactive "p")
  (let ((file (zettel--grab-dwim-file-target t)))
    (when file
      (let ((link (if (= arg 4)
                      (file-relative-name file zettel-directory)
                    (zettel-file-link file))))
        (if select-enable-clipboard
            (kill-new link)
          (gui-set-selection 'CLIPBOARD link))
        (message "Saved [%s] in the kill ring" link)
        (when (= arg 16)
          (shell-command (format "open -R %s &" file)))))))

(defun zettel-kill-ring-save-next-link ()
  "Save the first link at or after point (but before EOL)."
  (interactive)
  (save-excursion
    (let ((link (if (zettel-link-at-point-p)
                    (zettel-link-at-point)
                  (let ((eol (save-excursion (end-of-visual-line) (point))))
                    (when (re-search-forward zettel-regexp-link eol t)
                      (match-string-no-properties 0))))))
      (when link
        (kill-new link)
        (message "Saved [%s] to kill ring" link)))))

(defun zettel-links-to (link)
  "Runs a recursive grep (`rgrep') to find references to the link at point or
to current Zettel. With prefix argument, explicitly select the link."
  (interactive (list (if current-prefix-arg
                         (zettel-ivy-select-link)
                       (zettel-file-link (zettel--grab-dwim-file-target t)))))
  (grep-compute-defaults)
  (rgrep link "*.txt" (f-slash zettel-directory) nil))

(defun zettel-rgrep (string)
  "Runs a recursive grep (`rgrep') for the given STRING across all Zettel."
  (interactive "sSearch for what? ")
  (grep-compute-defaults)
  (rgrep string "*.txt" (f-slash zettel-directory) nil))

;; Show the beginning of Zettel title in mode-line
(defun zettel-show-title-in-mode-line ()
  (interactive)
  (when (zettel-p buffer-file-name)
    (save-excursion
     (save-restriction
       (widen)
       (goto-char (point-min))
       (let ((metadata
              (zettel-decode-combined-title
               (buffer-substring-no-properties
                (or (re-search-forward "title: ") (point-min))
                (point-at-eol)))))
         (when metadata
           (let ((words (split-string (alist-get :title metadata))))
             (setq-local mode-line-misc-info
                         (replace-regexp-in-string
                          "/" "" (mapconcat #'identity
                                   (cl-subseq words 0 (min 5 (length words)))
                                   " "))))))))))
(add-hook 'zettel-mode-hook 'zettel-show-title-in-mode-line)

(defun zettel-update-link-prefix-title ()
  "Kills text from point to the next Zettel link, replacing it with that
Zettel's title."
  (interactive)
  (save-excursion
    (let ((start (point)))
      (org-next-link)
      (when (zettel-link-at-point-p)
        (let* ((file (zettel-link-file (zettel-link-at-point)))
               (title (alist-get :title (zettel-file-metadata file))))
          (delete-region start (1- (point)))
          (backward-char)
          (insert title))))))

;;;=============================================================================
;;; Genealogical
;;;=============================================================================

(defun zettel-trace-genealogy (file-or-link &optional degree)
  "Returns the FILE-OR-LINK's next genealogical link, or NIL if could not
figure out. With the optional DEGREE, try to find the Nth link (i.e.
grandparent if DEGREE is 2, child if DEGREE is -1, an so on), returning the
most remote link that could be found."
  (let ((degree (or degree 1)))
    (if (= (abs degree) 0)
        file-or-link
      (zettel-trace-genealogy (alist-get (if (> degree 0)
                                             :parent
                                           :firstborn)
                                         (zettel-file-metadata
                                          (if (zettel-link-p file-or-link)
                                              (zettel-link-file file-or-link)
                                            file-or-link)))
                              (if (> degree 0)
                                  (1- degree)
                                (1+ degree))))))

(defun zettel-find-ancestor (n)
  "Opens the current Zettel's immediate ancestor. With a prefix argument, try
to find the Nth ancestor."
  (interactive "p")
  (when (zettel-p buffer-file-name)
    (let ((ancestor (zettel-trace-genealogy buffer-file-name n)))
      (if ancestor
          (zettel-find-link ancestor)
        (message "No ancestor found")))))

(defun zettel-find-descendant (n)
  "Opens the current Zettel's immediate descendant. With a prefix argument,
try to find the Nth ancestor."
  (interactive "p")
  (when (zettel-p buffer-file-name)
    (let ((descendant (zettel-trace-genealogy buffer-file-name (- n))))
      (if descendant
          (zettel-find-link descendant)
        (message "No descendant found")))))

(defun zettel-insert-ancestor-link (arg)
  "Insert a link to the ancestor of the current Zettel, adding its title (if
available) before the link. With a numerical prefix argument, try to find Nth
ancestor. With a universal argument, ask for confirmation before inserting."
  (interactive "P")
  (let* ((degree (if (integerp arg) arg 1))
         (link (zettel-trace-genealogy buffer-file-name degree)))
    (if link
        (zettel-insert-link-with-metadata link :title :before (not arg))
      (message "Could not find such ancestor"))))

(defvar zettel-parent-of-new-child nil
  "An alist of new children and their respective parents.")

(defun zettel-generate-new-child (parent kasten)
  "Generates a new child of the given PARENT in the KASTEN."
  (let ((child-link (zettel-make-link
                     kasten
                     (zettel-next-unused-slug kasten))))
    (add-to-list 'zettel-parent-of-new-child (cons child-link parent))
    child-link))

(defun zettel-insert-new-child (&optional arg)
  "Inserts a link to a new Zettel in the same Kasten as the current Zettel,
saves the current Zettel as its parent, and sets the `zettel-link-backlink'
to current Zettel. With prefix argument, allows the user to select a
different Kasten. With double prefix argument, asks for the full link.
Returns link the new child."
  (interactive "P")
  (when (zettel-p buffer-file-name t)
    (let ((parent-link (zettel-file-link buffer-file-name))
          child-link)
      (if (equal arg '(16))
          (while (not child-link)
            (setq child-link (read-string "Enter link for new child: "))
            (when (file-exists-p (zettel-link-file child-link))
              (message "This Zettel already exists; try again")))
        (let ((kasten (if (equal arg '(4))
                          (completing-read
                           "Kasten: "
                           (if (listp zettel-kaesten)
                               (mapcar #'car zettel-kaesten)
                             (error "No `zettel-kaesten' defined")))
                        (zettel-link-kasten parent-link))))
          (setq child-link (zettel-generate-new-child parent-link kasten))))
      (insert (zettel-org-format-link child-link))
      child-link)))

(defun zettel-insert-new-child-from-org-item (&optional arg)
  "Wrapper around `zettel-insert-new-child' that, if the point is in a list
item, first saves the text of the item in the kill ring before inserting the
new child link. Passes the prefix argument to `zettel-insert-new-child'."
  (interactive "P")
  (let* ((start (or (org-in-item-p) (point)))
         (text (cond ((region-active-p)
                      (buffer-substring-no-properties
                       (region-beginning) (region-end)))
                     ((org-in-item-p)
                      (buffer-substring-no-properties (1+ start) (point)))
                     (t (save-excursion
                          (beginning-of-line)
                          ;; FIXME: Might be good to have some limit to prevent
                          ;; killing whole paragraphs worth of text with soft
                          ;; newlines.
                          (buffer-substring-no-properties
                           (point) (1- start))))))
         (citekey (or (alist-get :citekey
                        (zettel-file-metadata buffer-file-name))
                      "")))
    (when text
      (kill-new (concat (org-trim text)
                        (when citekey " @")
                        citekey)))
    (zettel-insert-new-child arg)))

;;;=============================================================================
;;; Buffers and Frames
;;;=============================================================================

(defun zettel-select-and-find-link (arg)
  "Interactively asks the user to select a link from the list of currently
cached Zettel titles. With universal prefix, finds the link in another
buffer. With double universal prefix, asks the user to type the link
instead."
  (interactive "P")
  (zettel-find-file (if (equal arg '(16))
                        (read-string "Zettel link to find: ")
                      (let ((choice (zettel-ivy-read-reverse-alist-action
                                     "Select title: "
                                     (zettel-ivy-titles-reverse-alist)
                                     #'identity)))
                        (or (cdr choice)
                            (zettel-link-file (car choice)))))
                    (not (equal arg '(4)))))

(defun zettel-visiting-buffer-list (&optional skip-current)
  "Returns a list of Zettel files that are currently being visited. If
SKIP-CURRENT is T, remove the current buffer."
  (mapcar #'buffer-file-name
          (cl-remove-if-not (lambda (buf)
                              (zettel-p (buffer-file-name buf)))
                            (remove (when skip-current
                                      (current-buffer))
                                    (buffer-list)))))

(defun zettel-kill-visiting-buffers ()
  "Kills all Zettel that are being currently visited."
  (interactive)
  (mapc (lambda (file)
          (kill-buffer (get-file-buffer file)))
        (zettel-visiting-buffer-list t)))

(defun zettel-formatted-frame-title ()
  "Returns a string suitable for `frame-title-format' as a way to
consistently format the frame title with useful information for
Zettelkasten work."
  (interactive)
  (concat (if (zettel-p buffer-file-name)
              (let ((metadata (zettel-file-metadata buffer-file-name)))
                (format "%s §%s@%s"
                        (alist-get :title metadata)
                        (alist-get :slug metadata)
                        (alist-get :kasten metadata)))
            "%b")))

;;;=============================================================================
;;; Categories
;;;=============================================================================

(defun zettel-ivy-read-category (&optional arg prompt sort-fn)
  "Uses `ivy-read' to select a category from `zettel-categories'. With prefix
argument, asks the user to type in the category directly. If SORT-FN is
given, use that to sort the list first."
  (let ((prompt (or prompt "Category: "))
        (categories (if (not (functionp sort-fn))
                        zettel-categories
                      (let ((cats-copy (cl-copy-list zettel-categories)))
                        (cl-sort cats-copy sort-fn :key #'cdr)))))
    (if current-prefix-arg
        (read-string prompt)
      (ivy-read prompt categories))))

(defun zettel-set-category (file category)
  "Sets the category to the Zettel title based on `zettel-categories'. With
prefix argument, allows the user to type in a custom category."
  (interactive (list (zettel--grab-dwim-file-target)
                     (zettel-ivy-read-category nil nil #'>)))
  (let ((orig-buffer (current-buffer)))
    (save-excursion
      (with-current-buffer (find-file-noselect file)
        (goto-char (point-min))
        (if (re-search-forward "^title: §?\\([^.]+\\)\\. \\({[^}]+} \\)*" nil t)
            (replace-match (format "title: §\\1. {%s} " category))
          (message "Not sure how to set the category here"))
        ;; only save buffer if was not initially visiting it
        (unless (eq orig-buffer (current-buffer))
          (save-buffer))))))

;;;=============================================================================
;;; Populating Files
;;;=============================================================================

(defun zettel-insert-metadata-template (category title &optional parent)
  "Inserts the metadata template into the current buffer."
  (interactive (list (zettel-ivy-read-category nil nil #'>)
                     (read-string "Zettel title: ")))
  (let* ((file (file-name-base buffer-file-name))
         (link (zettel-file-link buffer-file-name))
         (parent (or parent
                     (cdr (assoc link zettel-parent-of-new-child)))))
    (when (zerop (buffer-size))
      (insert (format "title: §%s. {%s} %s"
                      link
                      (or category "Unset")
                      (or title "Untitled")))
      (insert "\ncreated: "
              ;; Insert creation time, making it match a tempus currens filename
              (format-time-string
               "%Y-%m-%d %a %H:%M"
               (let ((today (format-time-string "%Y%m%d")))
                 (if (and (eq :tempus (zettel-type buffer-file-name))
                          (not (string-match-p (regexp-quote today) file))
                          (not
                           (when (called-interactively-p 'any)
                             (y-or-n-p "Past tempus currens; set created time to now? "))))
                     (zettel-encode-iso8601-datetime file)
                   nil)))
              "\n")                     ; i.e. current time
      (when parent
        (insert "parent: " parent "\n"))
      (insert "\n"))))

(defun zettel-incorporate-file (file kasten &optional arg)
  "Moves the file in the current buffer to the appropriate Zettelkasten. With
prefix argument, asks for a different name."
  (interactive (list (buffer-file-name)
                     (ivy-read "Zettel kasten: " zettel-kaesten)
                     current-prefix-arg))
  (rename-file-and-buffer
   (if (not arg)
       (zettel-link-file
        (zettel-make-link kasten (file-name-base file)))
     (call-interactively #'rename-file-and-buffer))))

;;;=============================================================================
;;; Org-Mode Intergration
;;;=============================================================================

(defun zettel-org-set-todo-properties ()
  "Set the FROM, CREATED, and ID properties for the current heading to
facilitate refiling."
  (interactive)
  (org-set-property "ID" (org-id-get-create))
  (org-set-property "FROM" (zettel-insert-link-with-metadata
                            buffer-file-name :title :after))
  (org-set-property "CREATED"
                    ;; FIXME: Surely there is a better function to do this, no?
                    (format-time-string
                     (format "[%s]"
                             (cl-subseq (cdr org-time-stamp-formats) 1 -1)))))

(defun zettel-org-interactive-tempus ()
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
    (insert (zettel-org-format-link datetime))))

(defun zettel-dwim-with-this-timestring (beg end)
  "Do What I Mean with the timestring in the region. If the timestring is
IS8601, make it into an org-time-stamp, and vice-versa. If it's something
else, try to make it into org-time-stamp."
  (interactive
   (cond ((org-at-timestamp-p t)
          (list (match-beginning 0) (match-end 0)))
         ((or (thing-at-point-looking-at zettel-regexp-iso8601-datetime)
              (thing-at-point-looking-at zettel-regexp-iso8601-date))
          (list (match-beginning 0) (match-end 0)))
         ((region-active-p)
          (list (region-beginning) (region-end)))
         ((user-error "Region not active"))))
  (let ((text (buffer-substring-no-properties beg end))
        timestamp)
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

(defun zettel-org-export-as-new-zettel ()
  "Creates a new Zettel file in the current Zettelkasten based on the current
org subtree."
  (interactive)
  (let ((parent-file buffer-file-name)
        (parent-link (zettel-file-link buffer-file-name))
        tempus-currens
        new-title
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
          (setq tempus-currens (org-timestamp-format timestamp "%Y%m%dT%H%M")
                new-file (zettel-link-file tempus-currens))
          (let* ((content (org-get-entry)))
            (if (file-exists-p new-file)
                (message "Aborting, file already exists: %s" new-file)
              (with-current-buffer (get-buffer-create new-file)
                (zettel-insert-metadata-template "Memo" new-title parent-link)
                (insert "\n" content)
                (save-buffer)))))))
    ;; Back in original buffer
    (with-current-buffer (get-file-buffer (file-truename parent-file))
      (org-cut-subtree)
      (insert (zettel-org-format-link (zettel-file-link new-file))
              " "
              (alist-get :title (zettel-file-metadata new-file))))))

(defun zettel-open-link-at-point (&optional arg)
  "Open a Zettel link at point even if it's not formatted as a link. With a
prefix argument, ignore `zettel-number-of-frames' and open the link in the
same window."
  (interactive "P")
  (when (zettel-link-at-point-p t)
    ;; FIXME: Is it okay to check like this for prefix arg "upstream"?
    (zettel-find-link (zettel-link-at-point) (or arg current-prefix-arg))
    ;; This function is later added to `org-open-at-point-functions', so "must
    ;; return t if they identify and follow a link at point. If they don’t find
    ;; anything interesting at point, they must return nil."
    t))

(defun zettel-open-link-at-mouse (ev)
  "Open a Zettel link at mouse point."
  (interactive "e")
  (mouse-set-point ev)
  (zettel-open-link-at-point t))

;; Org links
(eval-after-load "org"
  '(progn
     ;; Try to resolve "fuzzy" links (i.e. without explicit protocol). This is
     ;; all that is needed to handle links in the form [[ZETTEL-LINK]].
     (push #'zettel-find-link org-open-link-functions)

     ;; Do the same for Zettel links that lack even the link markup. This is
     ;; useful for following parents/children.
     (push #'zettel-open-link-at-point org-open-at-point-functions)

     ;; This allows following links as part of #+INCLUDE statements.
     ;; TODO: Add a function to follow #+INCLUDE links
     ))

;;;-----------------------------------------------------------------------------
;;; Org-Mode: Inserting Snippets
;;;-----------------------------------------------------------------------------

(defcustom zettel-insert-snippet-summary nil
  "Non-nil means insert the snippet summary."
  :type 'boolean)

(defcustom zettel-insert-snippet-footnotes nil
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
(defun zettel-insert-snippet-text (arg file)
  "Inserts the combination of Summary and Snippet sections from the given
snippet FILE into the current buffer. With prefix argument, forces update."
  (interactive
   ;; Assume the file is the last link on the current line
   (list current-prefix-arg
         (save-excursion
           (end-of-line)
           (org-previous-link)
           (when (zettel-link-at-point-p)
             (zettel-link-file (zettel-link-at-point))))))
  ;; Get the metadata and most recent modification
  (save-excursion
    (save-restriction
      (let* ((metadata (zettel-file-metadata file))
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
                (when (and zettel-insert-snippet-summary
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
              (unless zettel-insert-snippet-footnotes
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

(defun zettel-find-inserted-snippet ()
  "While the point is within the org entry, find the source of the snippet
inserted through `zettel-insert-snippet-text'."
  (interactive)
  (save-excursion
    (org-back-to-heading t)
    (org-next-link)
    (org-open-at-point)))

(defun zettel-transclude-snippet (link)
  "Inserts the transclusion statement from given snippet LINKE into the
current buffer."
  (interactive
   ;; Assume the file is the last link on the current line
   (list (save-excursion
           (org-back-to-heading)
           (end-of-line)
           (org-previous-link)
           (when (zettel-link-at-point-p)
             (zettel-link-at-point)))))
  ;; Get the metadata and most recent modification
  (save-excursion
    (save-restriction
      (let* ((file (zettel-link-file link))
             (metadata (zettel-file-metadata file)))
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

(defcustom zettel-snippet-heading "Snippet"
  "The text of the snippet heading."
  :type 'string)

(defun zettel-org-footnote-action-maybe-local (&optional arg)
  "This is a wrapper around `org-footnote-action' to be used in the
transcluded snippets, making sure that the footnotes are placed locally
rather in whatever `org-footnote-section' is set to."
  (interactive "P")
  (let (snippet?)
    (save-excursion
      (org-back-to-heading-or-point-min t)
      (setq snippet?
        (and (org-context)
             (string-equal (nth 4 (org-heading-components)) zettel-snippet-heading))))
    (if (not snippet?)
        (org-footnote-action arg)
      (let ((org-footnote-section nil)
            (context (org-context)))
        (org-element-cache-reset)
        ;; taken from `org-footnote-action'
        (if (not (and context (> (point)
	                             (save-excursion
		                           (goto-char (org-element-property :end context))
		                           (skip-chars-backward " \t")
		                           (point)))))
            (org-footnote-action arg)
          (kill-new (format-time-string "%H%M"))
          (org-footnote-new))))))

;;;=============================================================================
;;; Bookmark Integration
;;;=============================================================================

(defun bookmark-make-record-zettel ()
  "Bookmark record function for Zettel bookmarks, setting the
bookmark's filename property to the Zettel link."
  (list (cons 'filename (zettel-file-link buffer-file-name))
        (cons 'handler 'bookmark-zettel-handler)))

(defun bookmark-zettel-handler (bmk-record)
  "Bookmark record handler for Zettel bookmarks."
  (find-file (zettel-link-file (cdr (assoc 'filename bmk-record)))))

;; Use the special zettel bookmark handler in Zettel buffers
(add-hook 'zettel-mode-hook
  (lambda ()
    (setq-local bookmark-make-record-function 'bookmark-make-record-zettel)))

;;;=============================================================================
;;; Maintenance
;;;=============================================================================

(defun zettel-zmove-to-another-kasten (source-file &optional kasten target-link)
  "Generates a zmove shell command to move the current Zettel to another
kasten. With prefix argument, asks for a target link instead. Opens the
target link and returns it."
  (interactive (list (zettel--grab-dwim-file-target)
                     (ivy-read "Which kasten to move to? " zettel-kaesten)))
  (let ((source-link (zettel-file-link source-file)))
    (if (not target-link)
        (if (equal current-prefix-arg '(4))
            (read-string "Enter target link: ")
          (setq target-link
            (zettel-make-link
             kasten
             (cl-case (cadr (assoc kasten zettel-kaesten #'string=))
               (:numerus (zettel-next-unused-slug kasten))
               (:tempus (zettel-tempus-currens-slug-for source-link))
               (t
                (error "Don't know how to handle this"))))))
      (error "Don't know where to move %s" source-link))
    ;; Offer to save buffers, since zmove tries to update links
    (save-some-buffers nil (lambda () (zettel-p buffer-file-name t)))
    (shell-command (format "zmove %s %s" source-link target-link))
    (cond ((string= source-file buffer-file-name)
           (kill-this-buffer)
           (unless (eq (length (frame-list)) 1)
             (delete-frame)))
          ((eq major-mode 'magit-status-mode)
           (magit-refresh)))
    (zettel-find-link target-link t)))

(defun zettel-generate-n-new-slugs (how-many type)
  "Generates a bunch of new slugs, making sure there are no dulicates."
  (interactive
   (list (read-number "How many? " 10)
         (intern (ivy-read "Which type? "
                           (mapcar #'first zettel-default-kasten)))))
  (goto-char (point-max))
  (let (slugs)
    (dotimes (n how-many)
      (push (zettel-generate-new-slug type) slugs))
    (mapc (lambda (s)
            (insert s "\n"))
          (delete-dups slugs))
    (delete-duplicate-lines (point-min) (point-max))))

;; Based on https://stackoverflow.com/a/30328255
(defun magit-show-zettel-title-in-minibuffer ()
  "Displays Zettel title of the file under cursor in minibuffer."
  (while-no-input
    (redisplay)
    (let (file line)
      (when (and (eq major-mode 'magit-status-mode)
                 (setq file (magit-file-at-point))
                 (zettel-p file)
                 (setq line (magit-file-line file)))
        (let ((metadata (zettel-decode-combined-title
                         (car (split-string line "title: " t)))))
          (message "%s | %s"
                   (alist-get :slug metadata) (alist-get :title metadata)))))))
(add-hook 'post-command-hook 'magit-show-zettel-title-in-minibuffer)

;;;=============================================================================
;;; Mode
;;;=============================================================================

;; Define a minor mode for working with Zettel files
(define-minor-mode zettel-mode
  "Make the keymap zettel-mode-map active."
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
          ;;------------------------------------------------------------------
          '(("C-c ^" . zettel-find-ancestor)
            ("C-c _" . zettel-find-descendant)
            ("C-c @" . zettel-insert-ancestor-link)
            ("C-c ," . zettel-insert-new-child)
            ("C-c /" . zettel-kill-ring-save-link-title)
            ("C-c #" . zettel-kill-ring-save-link)
            ("C-c $" . zettel-kill-ring-save-link-at-point)
            ("C-c +" . zettel-dwim-with-this-timestring)
            ;; Shadows `org-agenda-file-to-front'
            ("C-c [" . zettel-update-title)
            ;; Shadows prefix map for `orgtbl-ascii-plot' and `org-plit/gnuplot'
            ("C-c \"" . zettel-avy-link-search)
            ;; Shadows `org-open-at-mouse', but allows opening in same window with C-u
            ([C-down-mouse-1] . zettel-open-link-at-mouse)
            ;;
            ;; Unsafe: reserved for major modes
            ;;
            ("C-c C-'" . zettel-set-category)
            ;; Shadows `org-schedule'
            ("C-c C-s" . zettel-select-and-find-link)
            ;; Shadows `org-deadline'
            ("C-c C-d" . zettel-kill-ring-save-link-title)
            ;; Shadows `kill-sexp' in global-map
            ("C-M-k" . zettel-kill-link-or-sexp-at-point)
            ;; Shadows `org-insert-link'
            ("C-c C-l" . zettel-insert-link-to-cached-or-visiting)
            ("C-c C-M-l" . zettel-insert-link-from-clipboard)
            ;; Shadows `org-ctrl-c-tab'
            ;;("C-c C-i" . 'zettel-org-include-cached-file)
            ;; Shadows `org-set-property-and-value'
            ("C-c C-x P" . zettel-ivy-set-parent)
            ("C-c C-x F" . zettel-org-set-todo-properties)
            ("C-c C-x z" . zettel-zmove-to-another-kasten)
            ;; Shadows org-mode's `org-toggle-ordered-property'
            ("C-c C-x l" . zettel-links-to)
            )))

;; Enable zettel-mode for files that match the pattern
(add-hook 'org-mode-hook
  (lambda ()
    (when (zettel-p buffer-file-name)
      (setq org-export-with-broken-links t)
      (zettel-mode 1))))

;; Treat : (colon) as part of the word, allowing forward/backward-word over full
;; Zettel links.
(add-hook 'zettel-mode-hook
  (lambda ()
    (modify-syntax-entry ?: "w")))

;; On save, update modificate date
(add-hook 'zettel-mode-hook
  (lambda ()
    (add-hook 'before-save-hook 'zettel-update-metadata-date nil t)))

(provide 'zettel)
