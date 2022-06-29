;;;; -*- mode: emacs-lisp -*-
;;;;-----------------------------------------------------------------------------
;;;;        Author: Richard Boyechko <rb-emacs@diachronic.net>
;;;;   Description: Zettelkasten implementation based on Deft
;;;;  Date Created: 2015-06-31
;;;;      Comments:
;;;;-----------------------------------------------------------------------------
;;;; TODO:
;;;;
;;;; - `zettel-kill-ring-save-link-title' should look ahead on the same line
;;;;   if there isn't one at point
;;;; - C-u `zettel-open-link-at-point' should allow choosing a function to use
;;;; - `zettel-deft-parse-title-function' should have citekey column
;;;; - `deft-filter-zettel-category' should not clobber existing input
;;;; - opening link from Deft should use `zettel-find-link-ace-window'
;;;; - intelligently include category when insert link with title (2022-06-14)
;;;; - remove various obsolete functions from this file (2022-06-14)
;;;; - implement some kind of checksum check for keeping draft up to date
;;;; - add function to set readings metadata from org LOGBOOK at point
;;;; - rename "undecoded" title to something like summary or first-line
;;;; - insert link to bookmark (C-x r) file
;;;; - save list of RUMEN Kasten titles for use in other Emacs instances
;;;; - good way to set keywords, ideally with completion of existing ones
;;;; - remove bibkey from title when inserting link with title
;;;; - add an easy way to insert zlinksto output

(require 'deft)

;;;=============================================================================
;;; Mode
;;;=============================================================================

;; Cretea a keymap for the mode
(defvar zettel-mode-map (make-sparse-keymap)
  "Keymap for Zettel mode.")

;; Define a minor mode for working with Zettelkasten in deft
(define-minor-mode zettel-mode
  "Make the keymap zettel-mode-map active."
  :lighter " Zettel"
  :keymap zettel-mode-map
  :require 'deft)

;; Enable zettel-mode for files that match the pattern
(eval-after-load "markdown"
  (add-hook 'markdown-mode-hook
    '(lambda ()
       (when (zettel-p buffer-file-name)
         (zettel-mode 1)))))
(add-hook 'org-mode-hook
  '(lambda ()
     (when (zettel-p buffer-file-name)
       (setq org-export-with-broken-links t)
       (zettel-mode 1))))

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

(defvar zettel-stored-links '()
  "A stack of links stored with `zettel-store-link'.")

(defvar zettel-stored-links-history '()
  "History of the links stored with `zettel-store-link'.")

(defvar zettel-link-backlink nil
  "Stores the file name of the document into which a link was inserted with
`zettel-insert-link-intrusive' or `zettel-insert-link', allowing for creation
of backlinks.")

(defvar zettel-deft-active-kasten nil
  "The name of the active Zettelkasten, if any. This variable is set by
`zettel-deft-choose-kasten'.")

(defvar zettel-pregenerated-numeri "auto/unused-numeri.dat"
  "List of unused numri curentes to use for creating new numerus currens
Zettel in rumen when Emacs cannot check the list of existing files.")

;;;=============================================================================
;;; User Variables
;;;=============================================================================

(defcustom zettel-directory nil
  "The central Zettelkasten directory."
  :type 'string)

(defcustom zettel-kaesten
  ;; name | directory | slug type | sort-order (| deft-sort-method)
  `(("os"         :tempus  1)
    ("rumen"      :numerus 2)
    ("esophagus"  :numerus 3)
    ("omasum"     :tempus  4)
    ("abomasum"   :tempus  5)
    ("rectum"     :tempus  6)
    ("fabula"     :tempus  7)
    ("machina"    :tempus  8))
  "An alist containing the names and slug types of kaesten."
  :type 'alist)

(defcustom zettel-kaesten-aliases nil
  "An alist of any other aliases for the `zettel-kaesten'. This is an alist of
the actual name followed by the alias."
  :type 'alist)

(defcustom zettel-default-kasten
  ;; slug type | kasten name
  `((:numerus . "rumen")
    (:bolus . "esophagus")              ; FIXME: temporary
    (:tempus . "omasum"))
  "An alist of default Kasten (i.e. not requiring fully qualified link) for
each slug type."
  :type 'alist)

(defcustom zettel-categories nil
  "A list of categories used for Zettel."
  :type 'list)

(defcustom zettel-proliferate-frames nil
  "When non-NIL, create new frames when opening links."
  :type 'boolean)

(defcustom zettel-sort-by-name-descending t
  "When non-NIL, `deft-sort-files-by-name' will sort in a descending order,
otherwise ascending."
  :type 'boolean)

(defcustom zettel-find-link-function #'zettel-find-link-simply
  "Which function of one argument --- the link --- to use in order to find
Zettel links. The function needs to return NIL if it cannot find the given
link."
  :type 'function)

;;;=============================================================================
;;; General Functions
;;;=============================================================================

(defun spacep (char)
  "Returns T if the character is some kind of a space."
  (when char
    (string-match-p "[[:space:][:punct:]]" (char-to-string char))))

(unless (fboundp 'rb-random-elt)
 (defun rb-random-elt (sequence)
   "Return a random element of SEQUENCE."
   (when (sequencep sequence)
     (elt sequence (random (length sequence))))))

(unless (fboundp 'rb-get-clipboard-data)
  (defun rb-get-clipboard-data ()
    "System-independent way to get current clipboard data. Returns
nil if there is nothing there."
    (case system-type
      (gnu/linux (x-get-selection 'CLIPBOARD))
      (windows-nt (w32-get-clipboard-data))
      (darwin (shell-command-to-string "/usr/bin/pbpaste"))
      (t nil))))

(unless (fboundp 'rb-set-clipboard-data)
  (defun rb-set-clipboard-data (string)
    "System-independent way to copy the given STRING to clipboard."
    (case system-type
      (gnu/linux (error "Not implemented"))
      (windows-nt (error "Not implemented"))
      (darwin
       (save-excursion
         (with-temp-buffer
           (insert string)
           (shell-command-on-region (point-min) (point-max)
                                    "/usr/bin/pbcopy"))))
      (t nil))))

;;;=============================================================================
;;; Fundamental Functions
;;;=============================================================================

(defun zettel-p (file-or-buffer)
  "Returns non-NIL if the file or buffer is a Zettel."
  (interactive "f")
  (when file-or-buffer
   (let ((file (typecase file-or-buffer
                 (buffer (buffer-file-name file-or-buffer))
                 (string file-or-buffer)
                 (t
                  (error "Don't know how to handle this type")))))
     (when file
       (and (string-equal (file-name-extension file) deft-extension)
            (string-match zettel-regexp-slug (file-name-base file)))))))

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
    (cond ((assoc (first dirs) zettel-kaesten)
           (zettel-kasten-truename (first dirs)))
          ((assoc (second dirs) zettel-kaesten)
           (zettel-kasten-truename (second dirs)))
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
  (second (assoc kasten zettel-kaesten #'string=)))

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
    (file-name-as-directory (subseq slug 0 1))))

(defun zettel-tempus-subdirectory (slug)
  "Returns the right subdirectory for the given tempus currens slug."
  (when (string-match zettel-regexp-tempus-currens slug)
    (file-name-as-directory (match-string 1 slug))))

(defun zettel-bolus-subdirectory (slug)
  "Finds the right directory for the given bolus currens slug."
  (when (stringp slug)
    (let ((result
           (case (elt slug 0)
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

(defun zettel-absolute-filename (link)
  "Return an absolute filename to the Zettel link.

This function replaces `deft-absolute-filename' for Zettel."
  (if (zettel-link-p link)
      (let ((kasten (zettel-link-kasten link))
            (slug (zettel-link-slug link)))
        (expand-file-name
         (concat slug "." deft-extension)
         (expand-file-name
          (case (zettel-type slug)
            (:numerus (zettel-numerus-subdirectory slug))
            (:tempus (zettel-tempus-subdirectory slug))
            (:bolus (zettel-bolus-subdirectory slug)) ; FIXME: temporary
            (t (error "This is not a proper Zettel link: %s" link)))
          (zettel-kasten-directory kasten))))
    (error "This is not a proper Zettel link: %s" link)))

(defun deft-absolute-filename--zettel (orig-fun &rest args)
  "Replaces the default `deft-absolute-filename' with
`zettel-absolute-filename'."
  (let ((kasten (zettel-directory-kasten deft-directory)))
    (zettel-absolute-filename
     (if kasten
         (concat kasten ":" (first args))
       (first args)))))
(advice-add 'deft-absolute-filename :around 'deft-absolute-filename--zettel)

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
  (let ((second 0) (minute 0) (hour 0) day month year)
    (when (string-match (concat "^" zettel-regexp-iso8601-date) string)
      (setq year  (string-to-number (match-string 1 string))
            month (string-to-number (match-string 2 string))
            day   (string-to-number (match-string 3 string)))
      (when (string-match zettel-regexp-iso8601-time string
                          (match-end 0))
        (setq hour   (string-to-number (match-string 1 string))
              minute (string-to-number (match-string 2 string))))
      (encode-time second minute hour day month year))))

(defun zettel-file-content (file)
  "Returns the content of the file, getting it either from an opened buffer,
Deft cache, or the file itself."
  (cond ((get-file-buffer file)
         (with-current-buffer (get-file-buffer file)
           (save-restriction
             (widen)
             (buffer-substring-no-properties (point-min) (point-max)))))
        ((and deft-hash-contents (deft-file-contents file))
         (deft-file-contents file))
        ((file-exists-p file)
         (with-temp-buffer
           (insert-file-contents file)
           (buffer-string)))
        (t
         (error "Cannot get content for %s" file))))

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
          "\\. \\({\\([^}]+\\)} \\)*\\([^#@]+\\)\\(#[^@]+\\)*\\(@.*\\)*")
  "Regular expression for a combined title string, used in `zettel-metadata'.
Group 2 is the kasten.
Group 3 is the slug.
Group 5 is the category.
Group 6 is the title itself.
Group 7 is the keyword block.
Group 8 is the citation key.")

(defun zettel-decode-combined-title (title)
  "Returns an alist of metadata from a combined title. If cannot decode,
return the original title."
  (if (and title (string-match zettel-regexp-combined-title title))
      (let ((slug (match-string 3 title)))
        (list (cons :slug slug)
              (cons :type (zettel-type slug))
              (cons :category (match-string 5 title))
              (cons :title (string-trim-right (match-string 6 title) " "))
              (when (match-string 7 title)
                (cons :keywords
                      (string-trim-right (match-string 7 title) " ")))
              (when (match-string 8 title)
                (cons :citekey
                      (string-trim-right (match-string 8 title) " ")))))
    (list (cons :title title))))

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
        (set-difference metadata '((:link) (:category) (:title) (:type) (:citekey))
                        :key #'car)))

(defun zettel-metadata-yaml-key (keyword)
  "Returns a YAML-formatted string that is the name of the KEY, a keyword
symbol."
  (subseq (symbol-name keyword) 1))

(defun zettel-metadata-yaml-value (value)
  "Returns a YAML-formatted string for the given metadata VALUE."
  (typecase value
    (string value)
    (list (concat "[ " (mapconcat #'identity value ", ") " ]"))
    (t
     (error "Not implemented for type %s" (type-of value)))))

(defun zettel-normalize-metadata (file &optional metadata)
  "Replaces the FILE's metadata section with either the given METADATA or
by parsing the FILE's metadata."
  (let ((metadata (or metadata (zettel-metadata file)))
        (old-point (point)))
    (save-excursion
      (with-current-buffer (get-file-buffer file)
        (save-restriction
          (goto-char (point-min))
          (when (re-search-forward "^$" nil t 1)
            (narrow-to-region (point-min) (point)))
          (delete-region (point-min) (point-max))
          (multiple-value-bind (title remaining-metadata)
              (zettel-encode-combined-title metadata)
            (insert "title: " title "\n")
            (mapc #'(lambda (cons)
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

;; TODO: Remove, deprecated
(defun zettel-update-metadata-in-vivo (key value)
  "Updates the Zettel metadata section in the current buffer, setting the KEY
to VALUE."
  (let ((key-name (zettel-metadata-yaml-key key))
        (value-string (zettel-metadata-yaml-value value))
        found)
    (save-excursion
      (save-restriction
        (goto-char (point-min))
        (when (re-search-forward "^$" nil t 1)
          (narrow-to-region (point-min) (point)))
        (goto-char (point-min))
        (while (re-search-forward zettel-regexp-metadata-line nil t)
          (when (equal (match-string-no-properties 1) key-name)
            (replace-match
             (format "%s: %s" (match-string-no-properties 1) value-string))
            (setq found t)))
        (unless found
          (open-line 1)
          (forward-line)
          (insert (format "%s: %s" key-name value-string)))))))

(defun zettel-metadata (file)
  "Returns an alist of metadata for the given FILE based on the most current
content of the FILE. They keys are converted to keywords."
  (let* ((metadata-section
          (split-string
           (first (split-string
                   ;; Do a sane thing when I opened a Zettel file directly
                   ;; rather than through Deft interface.
                   (zettel-file-content file)
                   "\n\n"))
           "\n"))
         (metadata
          (mapcar #'(lambda (line)
                      (when (> (length line) 0)
                        (if (string-match zettel-regexp-metadata-line line)
                            (let ((key (intern (concat ":" (match-string 1 line))))
                                  (value (match-string 2 line)))
                              (cons key
                                    ;; Handle lists properly
                                    (if (string-match "^\\[\\(.*\\)\\]$" value)
                                        (split-string (match-string 1 value)
                                                      "," t "[[:space:]]+")
                                      value)))
                          (error "Malformed metadata line: '%s'" line))))
                  metadata-section))
         (title (alist-get :title metadata))
         (decoded (zettel-decode-combined-title title)))
    ;; When successfully decoded combined title, replace the original title with
    ;; the decoded metadata.
    (when decoded
      (setq metadata
        (append decoded (cl-remove :title metadata :key #'car))))
    (push (cons :kasten (zettel-file-kasten file)) metadata)
    (push (cons :link (zettel-file-link file)) metadata)))

(defun zettel-update-metadata-date ()
  "Updates the date in the metadata section of the Zettel in the current
buffer."
  (interactive)
  (let* ((today (format-time-string "%Y-%m-%d"))
         (metadata (zettel-metadata buffer-file-name))
         (modified (alist-get :modified metadata))
         (created (alist-get :created metadata)))
    (if modified
        (when (and (not (string-equal modified today))
                   (save-match-data
                     (y-or-n-p
                      (format "Saving %s. Update the modified date? "
                              (file-name-base buffer-file-name)))))
          (message "Updating metadata modified date in %s from %s to %s."
                   buffer-file-name modified today)
          (zettel-update-metadata :modified today))
      (when (and (not (string-equal created today))
                 (y-or-n-p (format "Saving %s. Add modified date? "
                                   (file-name-base buffer-file-name))))
        (message "Adding metadata modified date in %s (created on %s)."
                 buffer-file-name created)
        (zettel-update-metadata :modified today)))
    (zettel-normalize-metadata buffer-file-name)))

(add-hook 'zettel-mode-hook
  '(lambda ()
     (add-hook 'before-save-hook 'zettel-update-metadata-date nil t)
     (add-hook 'after-save-hook
       '(lambda ()
          (deft-cache-file buffer-file-name))
       nil t)))

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
  (case type
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

(defun zettel-next-unused-slug (&optional type)
  "Returns the next unused slug, relying on `zettel-deft-active-kasten' and
`zettel-kaesten' to figure out the type if not given, and on `deft-all-files'
to avoid duplicates."
  (let* ((active-kasten-type (second (assoc zettel-deft-active-kasten
                                            zettel-kaesten
                                            #'string=)))
         (type (or type active-kasten-type))
         slug)
    (cond ((eq type :tempus)
           (setq slug (zettel-generate-new-slug type)))
          ((eq type active-kasten-type)
           (message "Generating next unused slug of type %s" type)
           (let ((used (mapcar #'zettel-file-slug deft-all-files)))
             (while (or (not slug) (find slug used))
               (setq slug (zettel-generate-new-slug type)))))
          ((and (eq type :numerus)
                (file-exists-p (in-zettel-dir zettel-pregenerated-numeri)))
           (message "Getting next numerus from `zettel-pregenerated-numeri'...")
           (let ((buffer (find-file-noselect
                          (in-zettel-dir zettel-pregenerated-numeri))))
             (with-current-buffer buffer
               (setq slug
                 (string-trim (delete-and-extract-region
                               1 (search-forward-regexp "[[:space:]]" nil t))))
               (basic-save-buffer))))
          (t
           (message "Generating unused slug without checking for duplicates...")
           (setq slug (zettel-generate-new-slug type))))
    slug))

(defun deft-new-unused-zettel ()
  "Create a new Zettel with unused numerus currens."
  (interactive)
  (deft-new-file-named (zettel-next-unused-slug)))

(defun zettel-rename-with-unused-slug ()
  "Rename the current file and buffer to an unused filename
slug (short name) in `deft-directory' with `deft-extension'.
Based on `rename-file-and-buffer'."
  (interactive)
  (rename-file-and-buffer (concat (zettel-next-unused-slug) "." deft-extension)))

(defun slug-pronounceable-p (letters next)
  "Returns NIL if NEXT is not pronounceable after LETTERS."
  (cl-flet* ((lastn (seq n)
                    "Returns last N members of SEQ, or nil if it's too
              short."
                    (when (<= n (length seq))
                      (subseq seq (- n))))
             (clusterp (str)
                       (member str '("ai" "au" "ea" "ia" "io" "oa" "oi" "ou" "ua"
                                     "ch" "ck" "ff" "gh" "gl" "mn" "ph"
                                     "qu" "rh" "rp" "rs" "rt" "rz" "sc" "sh" "sk"
                                     "st" "th" "zh")))
             (liquidp (ch) (member ch '("l" "r")))
             (sibilantp (ch) (member ch '("s" "z")))
             (vowelp (ch) (member ch '("a" "e" "i" "o" "u" "y")))
             (consonantp (ch) (not (vowelp ch))))
    (let* ((next (if (characterp next) (char-to-string next) next))
           (prev (lastn letters 1))
           (cluster (concat prev next)))
      (when (or (not letters)
                (clusterp cluster)
                (and (vowelp prev) (consonantp next))
                (and (consonantp prev) (vowelp next))
                (and (consonantp prev)
                     (or (and (not (liquidp prev)) (liquidp next))
                         (or (sibilantp prev) (sibilantp next)))))
        t))))

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
    (let ((metadata (zettel-metadata (zettel-absolute-filename link)))
          oldname)
      (cond ((setq oldname
               (find-if #'(lambda (l)
                            (eq (zettel-kasten-slug-type (zettel-link-kasten l))
                                :tempus))
                        (alist-get :oldnames metadata)))
             ;; One of the old names was a tempus currens; just use that
             (zettel-link-slug oldname))
            ((alist-get :created metadata)
             ;; Use the created metadata and make up the time of creation
             ;; FIXME: Any more elegant way to do this?
             (zettel-decode-time-into-tempus-currens
              (zettel-encode-iso8601-datetime
               (concat (alist-get :created metadata)
                       "T"
                       (format-time-string "%H:%M")))))
            (t
             ;; Can't figure out automatically; ask the user
             (read-string "No created metadata; make up your own name: "
                          (zettel-next-unused-slug :tempus)))))))

;;;=============================================================================
;;; Zettel Links
;;;=============================================================================

(defun zettel-link-at-point-p ()
  "Returns T if the thing at point is a wiki link (i.e. [[XXX]] or org-mode
link)."
  (thing-at-point-looking-at
   (concat "\\[\\[\\(" zettel-regexp-link "\\)\\]\\(\\[[^]]+\\]\\)*\\]")))

(defun zettel-link-at-point ()
  "Return the Zettel link at point. Needs to be called after
`zettel-link-at-point-p'."
  (match-string-no-properties 1))

;; TODO: Obsolete
(defun zettel-store-link (arg)
  "Add the link 1) to the Deft file at point if in *Deft* buffer, or 2) to
the file in current buffer into `zettel-stored-links'."
  (interactive "p")
  (let* ((file (cond ((equal major-mode 'deft-mode)
                      (button-get (button-at (point)) 'tag))
                     ((and (= arg 4) (zettel-link-at-point-p))
                      (zettel-absolute-filename (zettel-link-at-point)))
                     (buffer-file-name
                      buffer-file-name)
                     (t
                      (message "No file to store a link to."))))
         (link (zettel-file-link file))
         (already-stored (member link zettel-stored-links)))
    (when already-stored
      (setq zettel-stored-links (remove link zettel-stored-links)))
    (push link zettel-stored-links)
    (message (if already-stored
                 "Link moved to the front: %s"
               "Link stored: %s")
             (remove-if #'null zettel-stored-links))))
(advice-add 'zettel-store-link :before 'zettel-kill-ring-save-link)

;; TODO: Get rid of this function and change existing calls
(defun zettel-wiki-link (target &optional include-title where add-spaces)
  "Returns a wiki link to TARGET, which can be either a link or a filepath.
WHERE can be RIGHT or LEFT."
  (let* ((file (or (if (file-name-absolute-p target)
                       target
                     (zettel-absolute-filename target))
                   (error "Link target doesn't exist; make sure it's saved")))
         (link-text (zettel-file-link file)))
    ;; FIXME: More elegant way to do the following?
    (concat (if (or (not add-spaces) (spacep (char-before))) "" " ")
            (if include-title
                (let ((title (alist-get :title (zettel-metadata file))))
                  (if (or (eq where 'left) (null where))
                      (format "%s [[%s]]" title link-text)
                    (format "[[%s]] %s" link-text title)))
              (format "[[%s]]" link-text)) 
            (if (or (not add-spaces) (spacep (char-after))) "" " "))))

(defun zettel-org-format-link (target &optional description)
  "Returns a formatted org-link to TARGET, which can be either a link or a filepath."
  (let* ((file (or (if (file-name-absolute-p target)
                       target
                     (zettel-absolute-filename target))
                   (error "Link target doesn't exist; make sure it's saved")))
         (link (zettel-file-link file)))
    (format "[[%s]%s]"
            link
            (if description
                (format "[%s]" description) ""))))

;; TODO: Remove, deprecated
(defun zettel-insert-link (arg)
  "Insert the top link from `zettel-stored-links'. If called with
prefix argument, insert the link title to the left of the link.
If with double prefix argument, insert the title to the right."
  (interactive "P")
  (if zettel-stored-links
      (let ((link (pop zettel-stored-links)))
        ;; Save the link in link history
        (push link zettel-stored-links-history)
        ;; Insert the link
        (insert
         (zettel-wiki-link link (consp arg) (equal arg '(16)) t))
        ;; Save the backlink
        (setq zettel-link-backlink buffer-file-name))
    (message "No link to insert")))

;; TODO: Remove, deprecated?
(defun zettel-insert-link-intrusive (arg)
  "Like `zettel-insert-link', but also opens the Zettel of the
link inserted if it doesn't already have a backlink, and adds the
current Zettel to the `zettel-link-backlink'."
  (interactive "P")
  (when zettel-stored-links
    (let* ((link (first zettel-stored-links))
           (file (zettel-absolute-filename link)))
      (zettel-insert-link arg)
      ;; If the linked file doesn't already have a link to the current one,
      ;; opens the linked file in a new window, but does not switch to it.
      (cond ((string-match (regexp-quote (zettel-wiki-link buffer-file-name))
                           (zettel-file-content file))
             (message "The linked note %s has a backlink to %s already"
                      link
                      (zettel-file-link buffer-file-name)))
            (t
             (deft-open-file file t t))))))

(defun zettel-insert-link-with-extras (link extra)
  "Inserts the Zettel link, allowing the user to interactively select from a
list of extras to also add."
  (let ((file (zettel-absolute-filename link)))
    (unless extra
      (ivy-read "Also include: "
                '(("Title before" . :title-before)
                  ("Title after" . :title-after)
                  ("Citekey before" . :citekey-before)
;;                  ("Title in description" . :desc-title)
;;                  ("Category before" . :category-before)
;;                  ("Category in description" . :desc-category)
                  ("Just the slug" . :slug-only)
                  "Anything else as description")
                :action #'(lambda (cons)
                            (setq extra (cdr cons)))))
    ;; TODO: Simplify the code; very repetitive
    (cond ((eq extra :title-after)
           (insert (zettel-org-format-link link)
                   " "
                   (alist-get :title (zettel-metadata file))))
          ((eq extra :title-before)
           (insert (alist-get :title (zettel-metadata file))
                   " "
                   (zettel-org-format-link link)))
          ((eq extra :citekey-before)
           (insert (alist-get :citekey (zettel-metadata file))
                   " "
                   (zettel-org-format-link link)))
          ((eq extra :category-before)
           (insert (alist-get :category (zettel-metadata file))
                   " "
                   (zettel-org-format-link link)))
          ((eq extra :desc-title)
           (insert (zettel-org-format-link
                    link
                    (alist-get :title (zettel-metadata file)))))
          ((eq extra :desc-category)
           (insert (zettel-org-format-link
                    link
                    (alist-get :category (zettel-metadata file)))))
          ((eq extra :slug-only)
           (insert (zettel-org-format-link (zettel-link-slug link))))
          (t
           (insert (zettel-org-format-link link extra))))))

(defun zettel-insert-link-to-cached-or-visiting (arg)
  "Inserts a link to another Zettel being currently visited or to those in
the Deft cache. With prefix argument, offers a few options for including
Zettel descriptions. If the user selects a Zettel that does not exist in the
list of cached or visiting Zettel, just insert the link to what was
selected. If the cursor in already inside a link, replace it instead."
  (interactive "P")
  (let* ((choices
          (delete-dups (append
                        (mapcar (lambda (path)
                                  (cons (deft-file-title path) path))
                                (zettel-visiting-buffer-list t))
                        (zettel-ivy-titles-reverse-alist #'string>)))))
    (if choices
        (let* ((choice (zettel-ivy-read-reverse-alist-action
                        "Insert link to: " choices 'zettel-file-link nil))
               (link (or (cdr choice)
                         ;; Make a new link
                         (zettel-make-link
                          (zettel-directory-kasten deft-directory)
                          (car choice)))))
          (if (not (zettel-link-at-point-p))
              (zettel-insert-link-with-extras link (if arg nil :slug-only))
            ;; When replacing, don't including anything
            (delete-region (match-beginning 0) (match-end 0))
            (insert (zettel-org-format-link link))))
      (user-error "No Deft cache or visited Zettel"))))

;; TODO: Remove, deprecated
(defun zettel-insert-link-to-stored-or-visiting (arg)
  "Inserts a link to another Zettel being currently visited or to those in
`zettel-stored-links'."
  (interactive "P")
  (let* ((files (delete-dups (append (mapcar #'zettel-absolute-filename
                                             zettel-stored-links)
                                     (zettel-visiting-buffer-list t)))))
    (if files
        (progn
          (push (zettel-ivy-read-reverse-alist-action files 'zettel-file-link)
                zettel-stored-links)
          (zettel-insert-link arg)
          ;; FIXME: This is very brute force to remove the inserted link from
          ;; the list, so need to rewrite once I figure out what to do about
          ;; links.
          (setq zettel-stored-links '()))
      (user-error "No stored links or visited Zettel"))))

(defun zettel-insert-backlink (arg)
  "Like `zettel-insert-link', but instead of popping a link from
`zettel-stored-links', inserts the link in
`zettel-link-backlink', if set."
  (interactive "P")
  (cond (zettel-link-backlink
         (insert (zettel-wiki-link zettel-link-backlink
                                   (consp arg) (equal arg '(16)) t))
         (setq zettel-link-backlink nil))
        (t
         (message "No backlink to insert."))))

(defun zettel-list-links ()
  "Lists the currently stored links and backlink."
  (interactive)
  (message "Stored links: %s Backlink: %s"
           (mapcar #'file-name-base zettel-stored-links)
           (file-name-base zettel-link-backlink)))

(defun zettel-drop-link ()
  "Drops the most recent stored link."
  (interactive)
  (message "Dropping link to %s, remaining links: %s"
           (file-name-base (pop zettel-stored-links))
           (mapcar #'file-name-base zettel-stored-links)))

(defun zettel-clear-links ()
  "Clears `zettel-stored-links' and `zettel-link-backlink'."
  (interactive)
  (setq zettel-stored-links nil
        zettel-link-backlink nil)
  (message "Zettel links and backlink cleared"))

(defun zettel-insert-link-from-clipboard (arg)
  "Link `zettel-insert-link' but attempts to get the link slug
from OS clipboard."
  (interactive "P")
  (let ((link (rb-get-clipboard-data))
        (backlink (when buffer-file-name
                    (zettel-link-slug buffer-file-name))))
    (when (zettel-link-p link)
      (insert
       (zettel-wiki-link link (consp arg) (equal arg '(16)) t))
      (when backlink
        (rb-set-clipboard-data backlink)
        (message "Backlink to %s copied to clipboard" backlink)))))

(defun zettel-kill-ring-save-link-title (arg)
  "Save the title of the wiki link at point or the buffer to the kill ring
and system clipboard. With prefix argument, saves the combinted title from `'."
  (interactive "P")
  (let ((file (cond ((zettel-link-at-point-p)
                     (zettel-absolute-filename (zettel-link-at-point)))
                    ((zettel-p buffer-file-name)
                     buffer-file-name)
                    ((equal major-mode 'deft-mode)
                     (button-get (button-at (point)) 'tag))
                    (t
                     (message "Save title of what?")))))
    (when file
      ;; FIXME: Is this too low-level for here? Should `zettel-file-content'
      ;; handle this situation (cache too old) somehow?
      (when (and deft-hash-contents (deft-file-contents file))
        (deft-cache-update-file file))
      (let* ((metadata (zettel-metadata file))
             (title (if arg
                        (car (zettel-encode-combined-title metadata))
                      (alist-get :title metadata))))
        (kill-new title)
        (unless select-enable-clipboard
          (rb-set-clipboard-data title))
        (message "Saved [%s] in the kill ring" title)))))

(defun zettel-kill-ring-save-link (arg)
  "Save the deft note at point or the buffer base filename in the kill ring
to be used as a wiki link elsewhere. With prefix argument, save the file name
relative to `zettel-directory' instead. With two prefix arguments, open the
file in Finder with it selected."
  (interactive "p")
  (let ((file (cond ((equal major-mode 'deft-mode)
                     (button-get (button-at (point)) 'tag))
                    (buffer-file-name
                     buffer-file-name)
                    (t
                     (message "Save a link to what?")))))
    (when file
      (let ((link (if (= arg 4)
                      (file-relative-name file zettel-directory)
                    (zettel-file-link file))))
        (if select-enable-clipboard
            (kill-new link)
          (rb-set-clipboard-data link))
        (message "Saved [%s] in the kill ring" link)
        (when (= arg 16)
          (shell-command (format "open -R %s &" file)))))))

;; Modified from zetteldeft's `zetteldeft-avy-link-search'.
(defun zettel-avy-link-search ()
  "Use `avy' to follow a Zettel wiki link."
  (interactive)
  (save-excursion
    (when (consp (avy-jump zettel-regexp-link))
      (zettel-open-link-at-point))))

(defun zettel-links-to (arg)
  "List links to the current Zettel from anywhere else in the Zettelkasten."
  (interactive "p")
  (funcall (if arg
               #'async-shell-command
             #'shell-command)
           (concat "zlinksto" " " (zettel-file-link buffer-file-name)))
  (when arg
    (switch-to-buffer-other-window "*Async Shell Command*")))

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
      (zettel-trace-genealogy (alist-get (if (plusp degree)
                                             :parent
                                           :firstborn)
                                         (zettel-metadata
                                          (if (zettel-link-p file-or-link)
                                              (zettel-absolute-filename file-or-link)
                                            file-or-link)))
                              (if (plusp degree)
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
  "Insert a link to the ancestor of the current zettel. With a
numerical prefix argument, try to find Nth ancestor. With
universal argument, behave like `zettel-insert-link'."
  (interactive "P")
  (let* ((degree (if (integerp arg) arg 1))
         (link (zettel-trace-genealogy buffer-file-name degree))
         ;; If user specified degree, include title by default.
         (arg (if (integerp arg) '(4) arg)))
    (if link
        (insert (zettel-wiki-link link (consp arg) (equal arg '(16))))
      (message "Could not find such ancestor"))))

(defvar zettel-parent-of-new-child nil
  "An alist of new children and their respective parents.")

(defun zettel-generate-new-child (parent kasten)
  "Generates a new child of the given PARENT in the KASTEN."
  (let ((child-link (zettel-make-link
                     kasten
                     (zettel-next-unused-slug (zettel-kasten-slug-type kasten)))))
    (add-to-list 'zettel-parent-of-new-child (cons child-link parent))
    child-link))

(defun zettel-insert-new-child (arg)
  "Creates a new Zettel in the current `deft-directory', inserting a link to
it at point, saves the current Zettel as its parent, and sets the
`zettel-link-backlink' to current Zettel. With prefix argument, allows the
user to select the Zettelkasten. With double prefix argument, asks for the
full link."
  (interactive "p")
  (let ((parent-link
         (zettel-file-link (cond ((zettel-p buffer-file-name)
                                  buffer-file-name)
                                 ((equal major-mode 'deft-mode)
                                  (button-get (button-at (point)) 'tag))
                                 (t
                                  (user-error "Child of what?")))))
        child-link)
    (if (= arg 16)
        (while (not child-link)
          (setq child-link (read-string "Enter link for new child: "))
          (when (file-exists-p (zettel-absolute-filename child-link))
            (message "This Zettel already exists; try again")))
      (let ((kasten (cond ((= arg 4)
                           (ivy-read "Zettelkasten: "
                                     (if (listp zettel-kaesten)
                                         (mapcar #'first zettel-kaesten)
                                       (error "No Zettelkasten defined"))))
                          (t
                           (unless (assoc :numerus zettel-default-kasten)
                             (call-interactively #'zettel-set-default-kasten))
                           (zettel-directory-kasten deft-directory)))))
        (setq child-link (zettel-generate-new-child parent-link kasten))))
    (if (equal major-mode 'deft-mode)
        (deft-new-file-named (zettel-link-slug child-link))
      (insert (zettel-org-format-link child-link)))))

(defun zettel-ivy-set-parent ()
  "Sets the parent metadata of the current Zettel to the Zettel chosen by the
user from cached and visiting Zettel."
  (interactive)
  (zettel-ivy-read-reverse-alist-action
   "Set parent: "
   (delete-dups
    (append (mapcar (lambda (path)
                      (cons (deft-file-title path) path))
                    (zettel-visiting-buffer-list t))
            (zettel-ivy-titles-reverse-alist)))
   (lambda (path)
     (zettel-update-metadata-in-vivo :parent (zettel-file-link path)))))

;; TODO: Remove or update
(defun zettel-numerus-children (slug)
  "Returns a list of the slugs of Zettel's children. If the Zettel doesn't
have any children, returns NIL."
  (multiple-value-bind (number letters)
      (zettel-numerus-parts slug)
    (let ((children-regex (format "%s-%s[a-z]$" number (or letters ""))))
      (sort
       (mapcar #'file-name-base
               (remove-if-not #'(lambda (x) (string-match children-regex x))
                              deft-all-files
                              :key #'file-name-base))
       #'string-lessp))))

;; TODO: Remove or update
(defun zettel-insert-list-of-children (slug arg)
  "Insert a list of links to children and their titles for the
given slug (defaults to current one). With prefix argument,
the links are on the right of titles; otherwise, to the left."
  (interactive (list
                (read-string
                 (format "slug (%s): " (file-name-base buffer-file-name))
                 nil nil (file-name-base buffer-file-name))
                current-prefix-arg))
  (when (zettel-p buffer-file-name)
    (dolist (child (zettel-numerus-children slug))
      (beginning-of-line)
      (insert "* " (zettel-wiki-link (zettel-absolute-filename child) t arg) "\n"))))

;;;=============================================================================
;;; Buffers, Files, Categories
;;;=============================================================================

(defun zettel-find-link (link)
  "Attempts to find the given Zettel link with the function specified in
`zettel-find-link-function'. Returns T."
  (funcall zettel-find-link-function link)
  t)

(defun zettel-find-link-simply (link)
  "Finds the provided Zettel link, returning T if it's a Zettel link. If the
file is empty, inserts the metadata template."
  (when (zettel-link-p link)
    (let ((file (zettel-absolute-filename link)))
      (funcall (if zettel-proliferate-frames
                   #'find-file-other-frame
                 #'find-file)
               file)
      (when (zerop (buffer-size))
       (call-interactively #'zettel-insert-metadata-template)))))

(defun zettel-find-link-ace-window (link)
  "Finds the provided Zettel link and opens it in the selected window. This
function ignores the value of `zettel-proliferate-frames'. With prefix
argument, don't try creating a new frame."
  (when (zettel-link-p link)
    (let* ((file (zettel-absolute-filename link))
           (new-buffer
            (if (or (> (max (length (window-list)) (length (frame-list))) 1)
                    (equal prefix-arg '(4)))
                (with-selected-window (ace-select-window)
                  (find-file file))
              (find-file-other-frame file))))
      (with-current-buffer new-buffer
        (when (zerop (buffer-size))
          (call-interactively #'zettel-insert-metadata-template))))))

(defun zettel-select-link (arg)
  "Interactively asks the user to select a link from the list of currently
cached Zettel titles. With universal prefix, open the link using
`zettel-find-link-simply'. With double universal prefix, asks the user to
type the link instead."
  (interactive "P")
  (funcall (if (eql arg '(4))
               #'zettel-find-link-simply
             #'zettel-find-link)
           (if (eql arg '(16))
               (read-string "Zettel link to find: ")
             (zettel-file-link (cdr (zettel-ivy-read-reverse-alist-action
                                     "Select title: "
                                     (zettel-ivy-titles-reverse-alist)
                                     #'identity))))))

(defun zettel-ivy-read-reverse-alist-action (prompt choices func &optional require-match)
  "Uses `ivy-read' to select from list of CHOICES alist composed of value/key
pairs. Upon selection, call the given FUNC, a function accepting one
argument, on the key. Returns a cons cell consisting of the match from
`ivy-read' and the result of FUNC."
  (let (result)
    (ivy-read prompt
              choices
              :action (lambda (choice)
                        (setq result
                          (if (consp choice)
                              (cons (car choice) (funcall func (cdr choice)))
                            (cons choice nil))))
              :re-builder 'ivy--regex-ignore-order
              :require-match require-match)
    result))

(defun zettel-ivy-titles-reverse-alist (&optional sort)
  "Returns a reverse alist of choices consisting of cached Zettel titles and
their paths. For use with `zettel-ivy-read-reverse-alist-action'."
  (let (titles-alist)
    (cond (deft-hash-titles
            (maphash (lambda (key val)
                       (push (cons (or val key) key) titles-alist))
                     deft-hash-titles)
            (if (functionp sort)
                (cl-sort titles-alist sort :key #'car)
              titles-alist))
          (t
           (error "No Deft titles cached")))))

(defun zettel-ivy-metadata-reverse-alist (files)
  "Given a list of Zettel files, returns a nicely formatted list of choices
suitable for passing to `zettel-ivy-read-reverse-alist-action' as collection.
Relies on Zettel metadata, so slower than `zettel-ivy-titles-reverse-alist'."
  (let ((fmt (concat "%s%-12s %-10s %-53s %s")))
    (mapcar #'(lambda (file)
                (let ((metadata (zettel-metadata file))
                      (buf (get-file-buffer file)))
                  (cons (format fmt
                                (if (and buf (buffer-modified-p buf)) "*" " ")
                                (alist-get :slug metadata)
                                (alist-get :category metadata)
                                (subseq (alist-get :title metadata) 0
                                        (min (length (alist-get :title metadata))
                                             53))
                                (or (alist-get :keywords metadata) ""))
                        file)))
            files)))

(defun zettel-visiting-buffer-list (&optional skip-current)
  "Returns a list of Zettel files that are currently being visited. If
SKIP-CURRENT is T, remove the current buffer."
  (mapcar #'buffer-file-name
          (remove-if-not #'(lambda (buf)
                             (zettel-p (buffer-file-name buf)))
                         (remove (when skip-current
                                   (current-buffer))
                                 (buffer-list)))))

(defun zettel-kill-visiting-buffers ()
  "Kills all Zettel that are being currently visited."
  (interactive)
  (mapc #'(lambda (file)
            (kill-buffer (get-file-buffer file)))
        (zettel-visiting-buffer-list t)))

(defun zettel-switch-to-buffer (arg)
  "Quickly switch to other open Zettel buffers. With prefix argument, do so
in another window."
  (interactive "P")
  (zettel-ivy-read-reverse-alist-action
   "Switch to Zettel: "
   (mapcar (lambda (path)
             (when (null (deft-file-title path))
               (deft-cache-file path))
             (cons (format "%s%s"
                           (if (buffer-modified-p (get-file-buffer path))
                               "✒︎"
                             "")
                           (deft-file-title path))
                   path))
           (zettel-visiting-buffer-list t))
   (if (not arg) 'find-file 'find-file-other-window)))

(defun zettel-ivy-read-category (&optional arg prompt)
  "Uses `ivy-read' to select a category from `zettel-categories'. With prefix
argument, asks the user to type in the category directly."
  (let ((prompt (or prompt "Category: ")))
    (if current-prefix-arg
        (read-string prompt)
      (ivy-read prompt zettel-categories))))

(defun zettel-set-category (file category)
  "Sets the category to the Zettel title based on `zettel-categories'. With
prefix argument, allows the user to type in a custom category."
  (interactive (list (cond ((zettel-p buffer-file-name)
                            buffer-file-name)
                           ((equal major-mode 'deft-mode)
                            (button-get (button-at (point)) 'tag))
                           (t
                            (user-error "Set category of what?")))
                     (zettel-ivy-read-category)))
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

(defun zettel-populate-categories ()
  "Populate `zettel-categories' based on the titles in Deft cache."
  (interactive)
  (setq zettel-categories '())
  (dolist (file deft-all-files)
    (add-to-list 'zettel-categories
      (alist-get :category (zettel-metadata file))))
  (message "Populated with %d categories" (length zettel-categories)))

(defun zettel-add-bibliographic-category ()
  "Add a category to the Zettel title based on the bibliographic title."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    ;; 1: slug
    ;; 2: given name(s)
    ;; 3: family name
    ;; 4: title
    ;; 5: year
    (when (re-search-forward "^title: §?\\([0-9a-z:-]+\\)\\. \
\\(\\w+ \\)+\\(\\w+\\), \\([^(]+\\) (\\([0-9]+\\))")
      (replace-match (subseq (match-string 2) 0 1) nil nil nil 2)
      (replace-match "title: §\\1. {\\3\\5} \\2. \\3's \\4 (\\5)"))))

;;;=============================================================================
;;; Deft-Mode Integration
;;;=============================================================================

;; Adjust how Deft lists Zettel
(setq deft-strip-title-regexp "^\\(title: +\\)"
      ;; Default: "\\(?:^%+\\|^[#* ]+\\|-\\*-[[:alpha:]]+-\\*-\\|#+$\\)"
      deft-strip-summary-regexp ".*"
      ;; Default: "\\([\n	]\\|^#\\+[[:upper:]_]+:.*$\\)"
      ;; Modified: "\\(^\\w+: .*\\)"
      deft-time-format nil
      deft-use-filename-as-title nil
      deft-current-sort-method 'mtime)

(add-hook 'deft-mode-hook
  (lambda ()
    (setq show-trailing-whitespace nil)))

(defun zettel-deft-choose-kasten (arg new-kasten)
  "If there is an existing `deft-buffer', switches to it, otherwise
interactively selects the deft directory from among `zettel-kaesten'. With
a prefix argument, selects new deft directory regardless of `deft-buffer';
with double prefix argument calls `zettel-deft-choose-directory' instead."
  (interactive
   (list
    current-prefix-arg
    (when (or (null (get-buffer deft-buffer))
              (equal current-prefix-arg '(4)))
      (ivy-read "Zettel kasten: " zettel-kaesten))))
  (cond ((equal arg '(16))
         (call-interactively #'zettel-deft-choose-directory))
        ((not new-kasten)
         (if zettel-proliferate-frames
             (switch-to-buffer-other-frame deft-buffer)
           (switch-to-buffer deft-buffer)))
        (t
         (setq zettel-deft-active-kasten new-kasten)
         (zettel-deft-choose-directory (zettel-kasten-directory new-kasten)))))

(defun zettel-deft-choose-directory (directory)
  "Interactively selects the directory, starting in `zettel-directory'."
  (interactive
   (list
    (read-directory-name "Deft directory: "
                         zettel-directory
                         "" t)))
  (when (and (get-buffer deft-buffer))
    (kill-buffer deft-buffer))
  (setq deft-directory directory
        deft-buffer (format "*Deft: %s*"
                            (capitalize
                             (file-name-base (directory-file-name directory)))))
  (deft)
  (deft-filter nil)
  (deft-refresh)
  (zettel-populate-categories))

(defun zettel-deft-parse-title-function (line)
  "Function for post-processing titles for display in Deft buffer, intended
as the value for `deft-parse-title-function'."
  (let ((metadata (zettel-decode-combined-title
                   (replace-regexp-in-string "^\\(title: +\\)" "" line))))
    (when metadata
      ;; FIXME: Any more elegant way to do this?
      ;; FIXME: I should make an alist of slug-len and cat-len for each zettel type
      (if (eq (zettel-type (alist-get :slug metadata)) :tempus)
          (setq slug-len 13             ; 19700101T0000
                cat-len 15)
        (setq slug-len 6                ; a-0000
              cat-len 15))
      ;; SLUG CATEGORY/CITEKEY TITLE
      (format (format "%%-%ds%%-%ds%%s" (+ slug-len 2) cat-len)
              (alist-get :slug metadata)
              (let ((cat (if (alist-get :citekey metadata)
                             ;; Skip @
                             (subseq (alist-get :citekey metadata) 1)
                           (alist-get :category metadata))))
                (if (> (length cat) cat-len)
                    (concat (subseq cat 0 (- cat-len 2)) "..")
                  cat))
              (alist-get :title metadata)))))
(setq deft-parse-title-function 'zettel-deft-parse-title-function)

(defun deft-new-file-maybe-named (arg)
  "Extends `deft-new-file' to call `deft-new-file-named' if called with
prefix argument."
  (interactive "p")
  (if (= arg 4)
      (call-interactively #'deft-new-file-named)
    (deft-new-file)))

(advice-add 'deft-auto-populate-title-maybe :around #'list)

(defun zettel-add-section-sign-to-deft-filter ()
  "Inserts the Unicode section sign (§) to Deft filter string."
  (interactive)
  (setq last-command-event 167)
  (deft-filter-increment))

(defun deft-filter-zettel-category (category arg)
  "Inserts a category into deft-filter if there is no category there or
changes the existing one. With prefix argument, replaces the current
`deft-filter-regexp'."
  (interactive (list (zettel-ivy-read-category) current-prefix-arg))
  (deft-filter (format "{%s}" category)
    (or arg (null deft-filter-regexp))))

;;
;; Insert my zettel title string into new zettel rather than contents of deft's
;; filter string.
;;
(defun zettel-insert-metadata-template (&optional category title)
  "Inserts the metadata template into the current buffer."
  (interactive (list (zettel-ivy-read-category)))
  (let ((base (file-name-base buffer-file-name))
        (link (zettel-file-link buffer-file-name))
        insert-point)
    (if (not (= (point-min) (point-max))) ; file is not empty
        (error "The Zettel is not empty")
      (insert "title: §" link ". ")
      (setq insert-point (point))
      (insert "\ncreated: "
              ;; Insert creation date, making it match a tempus currens filename
              (format-time-string
               "%Y-%m-%d"
               (let ((today (format-time-string "%Y%m%d")))
                 (if (and (eq :tempus (zettel-type buffer-file-name))
                          (not (string-match-p (regexp-quote today) base))
                          (not
                           (when (called-interactively-p 'any)
                             (y-or-n-p "Past tempus currens; set created date to today? "))))
                     (zettel-encode-iso8601-datetime base)
                   nil)))
              "\n")                     ; i.e. current time
      (when (assoc link zettel-parent-of-new-child)
        (insert "parent: " (cdr (assoc link zettel-parent-of-new-child)) "\n"))
      (goto-char insert-point)
      (insert (format "{%s} %s" (or category "Unset") (or title "")))
      (end-of-line))))

(defun zettel-incorporate-file (file kasten &optional arg)
  "Moves the file in the current buffer to the appropriate Zettelkasten. With
prefix argument, asks for a different name."
  (interactive (list (buffer-file-name)
                     (ivy-read "Zettel kasten: " zettel-kaesten)
                     current-prefix-arg))
  (rename-file-and-buffer
   (if (not arg)
       (zettel-absolute-filename
        (zettel-make-link kasten (file-name-base file)))
     (call-interactively #'rename-file-and-buffer))))

(defun deft-new-file--add-zettel-title (orig-fun slug)
  "Replaces deft's default behavior of putting the filter string
on the first line with the Zettel title string."
  ;; `DEFT-NEW-FILE-NAMED' returns either a string (from MESSAGE) about an
  ;; error, or the result of (GOTO-CHAR (POINT-MAX)), which means an integer
  ;; buffer location.
  (when (integerp (funcall orig-fun slug))
    (let ((file (deft-absolute-filename slug)))
      (with-current-buffer (get-file-buffer file)
        (erase-buffer)
        (zettel-insert-metadata-template)))))
(advice-add 'deft-new-file-named :around #'deft-new-file--add-zettel-title)

;;
;; Sorting deft-buffer by filename
;;
(defun deft-sort-files-by-name (files)
  "Sort FILES by name, in reverse, ignoring case."
  (sort files (lambda (f1 f2)
                (funcall (if zettel-sort-by-name-descending
                             #'string-lessp
                           #'string-greaterp)
                         (downcase (file-name-base f2))
                         (downcase (file-name-base f1))))))

(defun zettel-title-lessp (file1 file2)
  "Return non-nil if the Zettel title of FILE1 is lexicographically less than
that of FILE2. Case is ignored."
  ;; FIXME: Hack to get the title from the result of
  ;; `zettel-deft-parse-title-function'
  (let ((title1 (third (split-string (or (deft-file-title file1) "") "  +")))
        (title2 (third (split-string (or (deft-file-title file2) "") "  +"))))
    (string-lessp (downcase (or title1 ""))
                  (downcase (or title2 "")))))

(defun deft-sort-files-by-zettel-title (files)
  "Sort FILES by the Zettel title."
  (sort files 'zettel-title-lessp))

(eval-after-load "deft"
  '(defalias 'deft-sort-files-by-title 'deft-sort-files-by-name))

;; Having a visual indicator of the sort method is helpful
(defun deft-set-mode-name ()
  "Set the mode line text based on search mode."
  (setq mode-name
    (format "Deft[%s]%s"
            deft-current-sort-method
            (if deft-incremental-search "" "/R"))))
(advice-add 'deft-toggle-sort-method :after 'deft-set-mode-name)

;;;=============================================================================
;;; Org-Mode Intergration
;;;=============================================================================

(defun zettel-org-set-todo-properties ()
  "Set the FROM, CREATED, and ID properties for the current heading to
facilitate refiling."
  (interactive)
  (org-set-property "ID" (org-id-get-create))
  (org-set-property "FROM" (zettel-wiki-link buffer-file-name t 'right))
  (org-set-property "CREATED"
                    ;; FIXME: Surely there is a better function to do this, no?
                    (format-time-string
                     (format "[%s]"
                             (subseq (cdr org-time-stamp-formats) 1 -1)))))

(defun zettel-org-interactive-tempus ()
  "Inserts a tempus currens link after having the user select the date using
org-mode's interactive `org-time-stamp' command."
  (interactive)
  (let ((start (point)))
    (org-time-stamp '(4) t)
    (insert
     "[["
     (org-timestamp-format (org-timestamp-from-string
                            (delete-and-extract-region start (point)))
                           "%Y%m%dT%H%M")
     "]]")))

(defun zettel-org-include-cached-file ()
  "Add an org-mode #+INCLUDE to a cached Zettel."
  (interactive)
  (let* ((choices
          (delete-dups (append
                        (mapcar (lambda (path)
                                  (cons (deft-file-title path) path))
                                (zettel-visiting-buffer-list t))
                        (zettel-ivy-titles-reverse-alist)))))
    (if choices
        (zettel-ivy-read-reverse-alist-action
         "Include: "
         choices
         (lambda (file)
           (let ((summary-section "Summary")
                 (snippet-section (replace-regexp-in-string
                                   "ß" "Snippet "
                                   (first (split-string
                                           (alist-get :title (zettel-metadata file))
                                           ":")))))
             (insert (format "#+INCLUDE: \"%s::%s\" :only-contents t\n"
                             (file-relative-name file)
                             summary-section))
             (insert (format "#+INCLUDE: \"%s::%s\" :only-contents t"
                             (file-relative-name file)
                             snippet-section)))))
      (user-error "No Deft cache or visited Zettel"))))

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
                new-file (zettel-absolute-filename tempus-currens))
          (let* ((content (org-get-entry)))
            ;; Code adapted from `deft-new-file', since calling that function in
            ;; turn calls my `deft-new-file--add-zettel-title' that interferes
            ;; with populating the file properly.
            (if (file-exists-p new-file)
                (message "Aborting, file already exists: %s" new-file)
              (deft-open-file new-file)
              (with-current-buffer (get-file-buffer (file-truename new-file))
                (insert (format "title: §%s. {Memo} %s\n" tempus-currens new-title))
                (insert "created: " (format-time-string "%Y-%m-%d") "\n")
                (insert "parent: " parent-link "\n")
                (insert content)
                (save-buffer))
              (deft-cache-update-file new-file)
              (deft-refresh-filter))))))
    ;; Back in original buffer
    (with-current-buffer (get-file-buffer (file-truename parent-file))
      (org-cut-subtree)
      (insert (zettel-org-format-link (zettel-file-link new-file))
              " "
              (alist-get :title (zettel-metadata new-file))))))

;; Org links
(eval-after-load "org"
  '(progn
     ;; Try to resolve "fuzzy" links (i.e. without explicit protocol). This is
     ;; all that is needed to handle links in the form [[ZETTEL-LINK]].
     (push #'zettel-find-link org-open-link-functions)

     ;; Do the same for Zettel links that lack even the link markup. This is
     ;; useful for following parents/children.
     (push 'zettel-open-link-at-point org-open-at-point-functions)

     ;; This allows following links as part of #+INCLUDE statements.
     ;; TODO: Add a function to follow #+INCLUDE links
     ))

;; Treat : (colon) as part of the word, allowing forward/backward-word over full
;; Zettel links.
(add-hook 'zettel-mode-hook
  '(lambda ()
     (modify-syntax-entry ?: "w")))

(defun zettel-open-link-at-point (&optional arg)
  "Open a Zettel link at point even if it's not formatted as a link. With a
prefix argument, calls `ZETTEL-FIND-LINK-SIMPLY' rather than whatever is in
`ZETTEL-FIND-LINK-FUNCTION'."
  (interactive "p")
  (save-excursion
    (forward-word)
    (backward-word)
    (when (thing-at-point-looking-at (concat "\\(" zettel-regexp-link "\\)"))
      ;; FIXME: Is it okay to check like this for prefix arg "upstream"?
      (funcall (if (or arg current-prefix-arg)
                   #'zettel-find-link-simply
                 #'zettel-find-link)
               (match-string-no-properties 1))
      ;; This function is later added to `org-open-at-point-functions', so "must
      ;; return t if they identify and follow a link at point. If they don’t find
      ;; anything interesting at point, they must return nil."
      t)))

(defun org-zettel-link-context (file)
  "Returns a string of Zettel context."
  (if (zettel-p file)
      (format "[[%s]] %s"
              (file-name-base file)
              (or (alist-get :title (zettel-metadata file)) ""))
    ;; (format "[[%s][%s]]" file (file-name-base file))
    (error "Not a Zettel")))

;;;=============================================================================
;;; Markdown-Mode Integration
;;;=============================================================================

(eval-after-load 'markdown-mode
  '(progn
     ;; This must be enabled to have wiki links
     (setq markdown-enable-wiki-links t)

     ;;
     ;; By default, `markdown-convert-wiki-link-to-filename' concatenates the
     ;; file extension of the current buffer's file to the link name when you
     ;; press C-c C-o over something like [[bib/Key2015.bib]], so it ends up
     ;; opening Key2015.bib.txt. The markdown-cwltf-fix-link removes the extra
     ;; extension, among other things.
     ;;
     ;; Unfortunately, `markdown-follow-wiki-link' also "ensure[s] that the new
     ;; buffer remains in `markdown-mode'", so I need yet another work-around to
     ;; fix that: `markdown-fwl-set-auto-mode'.
     ;;

     (defun markdown-fwl--set-auto-mode (&rest args)
       "After advice for `markdown-follow-wiki-link'. Reverses the default
behavir of ensuring that the buffer is in markdown mode, and instead sets it
back to the mode it 'wants to be'."
       (set-auto-mode t))
     (advice-add 'markdown-follow-wiki-link :after #'markdown-fwl--set-auto-mode)

     (defun markdown-cwltf--fix-link (orig-fun name)
       "Advice for `markdown-convert-wiki-link-to-filename',
completely overriding the originall functionality. It combines the not
clobbering of extension, finding the right directory directory for the Zettel
so that the links can be found across multiple directories within the main
Zettelkasten, and also handling 'subkasten:' notation."
       (save-match-data
         (let ((result (or (zettel-absolute-filename name)
                           (funcall orig-fun name))))
           (if (string-match "\\.\\w+$" name)
               (let ((orig-ext (match-string 0 name)))
                 (if (string-match (concat orig-ext "\\(\\.\\w+\\)$") result)
                     (replace-match orig-ext nil nil result)
                   result))
             result))))

     (advice-add 'markdown-convert-wiki-link-to-filename
                 :around #'markdown-cwltf--fix-link)))

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
  (find-file (zettel-absolute-filename (cdr (assoc 'filename bmk-record)))))

;; Use the special zettel bookmark handler in Zettel buffers
(add-hook 'zettel-mode-hook
  (lambda ()
    (setq-local bookmark-make-record-function 'bookmark-make-record-zettel)))

;;;=============================================================================
;;; Frames
;;;=============================================================================

(defun zettel-toggle-proliferate-frames ()
  "Toggle the value of `zettel-proliferate-frames' variable."
  (interactive)
  (message "Proliferating frames set to %s"
   (setq zettel-proliferate-frames (not zettel-proliferate-frames))))

(defun zettel-formatted-frame-title ()
  "Returns a string suitable for `frame-title-format' as a way to
consistently format the frame title with useful information for
Zettelkasten work."
  (interactive)
  (concat (if zettel-deft-active-kasten
              (format "〔%s〕"
                      (upcase zettel-deft-active-kasten))
            "")
          (if (and (boundp 'zettel-mode) zettel-mode)
              (let ((metadata (zettel-metadata buffer-file-name)))
                (format "%s §%s@%s"     ; {%s} (alist-get :category metadata)
                        (alist-get :title metadata)
                        (alist-get :slug metadata)
                        (alist-get :kasten metadata)))
            "%b")))

(defun zettel-update-frame-title ()
  "Sets the frame title for the current Zettel buffer to be more useful."
  (when (and (boundp 'zettel-mode) zettel-mode)
    (let ((metadata (zettel-metadata buffer-file-name)))
      (setq-local frame-title-format
                  (list "@" (upcase zettel-deft-active-kasten)
                        " "
                        "{" (alist-get :category metadata) "}"
                        " "
                        "«" (alist-get :title metadata) "»"
                        " "
                        "§" (alist-get :slug metadata))))))

;;;=============================================================================
;;; Maintenance
;;;=============================================================================

(defun zettel-zmove-to-another-kasten (source-file &optional target-link)
  "Generates a zmove shell command to move the current Zettel to another
kasten. With prefix argument, asks for a target link instead."
  (interactive (list (cond (zettel-mode
                            buffer-file-name)
                           ((eq major-mode 'magit-status-mode)
                            (magit-file-at-point))
                           ((eq major-mode 'deft-mode)
                            (button-get (button-at (point)) 'tag))
                           (t
                            (read-file-name "Move which Zettel? ")))))
  (let ((source-link (zettel-file-link source-file)))
    (if (and (not target-link) (called-interactively-p 'any))
        (if (equal current-prefix-arg '(4))
            (read-string "Enter target link: ")
          (let ((kasten (ivy-read "Which kasten to move to? "
                                  zettel-kaesten)))
            (setq target-link
              (zettel-make-link
               kasten
               (case (second (assoc kasten zettel-kaesten #'string=))
                 (:numerus (zettel-next-unused-slug :numerus))
                 (:tempus (zettel-tempus-currens-slug-for source-link))
                 (t
                  (error "Don't know how to handle this")))))))
      (error "Don't know where to move %s" source-link))
    (shell-command (format "zmove %s %s" source-link target-link))
    (cond ((string= source-file buffer-file-name)
           (kill-this-buffer))
          ((eq major-mode 'magit-status-mode)
           (magit-refresh))
          ((eq major-mode 'deft-mode)
           (deft-cache-update-file source-file)))))

;; FIXME: Update? Rename? Remove?
(defun zettel-rename-and-update-title ()
  "Using most of the code from deft.el's `DEFT-RENAME-FILE'."
  (interactive)
  (let (old-filename new-filename old-name new-name)
    (setq old-filename (button-get (button-at (point)) 'tag))
    (when old-filename
      (setq old-name (file-name-base old-filename))
      (setq new-name (read-string
                      (concat "Rename " old-name " to (without extension): ")
                      old-name))
      (setq new-filename
        (concat (file-name-as-directory deft-directory)
                new-name "." deft-extension))
      ;; Use appropriate command depending on if tracked or not
      (if (vc-backend old-name)
          (vc-rename-file old-filename new-filename)
        (rename-file old-filename new-filename))
      ;; Update the title
      (zettel-match-title-to-filename)
      ;; Update Deft
      (deft-update-visiting-buffers old-filename new-filename)
      (deft-refresh))))

(defun zettel-match-title-to-filename ()
  "Updates the title metadata tag to match the file's filename.
Adds an 'oldname' tag with the previous name."
  (interactive)
  (error "FIXME: Old way of handling oldname")
  (let (oldname)
    (save-excursion
      (save-restriction
        (goto-char (point-min))
        (forward-paragraph)
        (narrow-to-region (point-min) (point))
        (goto-char (point-min))
        (when (re-search-forward "title: §\\([a-z0-9:-]+\\)\\.")
          (setq oldname (match-string 1))
          (replace-match (file-name-base buffer-file-name) t nil nil 1)
          (forward-paragraph)
          (forward-line 1)
          (insert (concat "oldname: " oldname))
          (open-line 1))))))

(defun zettel-batch-update-titles ()
  "Runs `zettel-match-title-to-filename' on all the
`deft-current-files'."
  (interactive)
  (dolist (zettel deft-current-files)
    (find-file zettel)
    (zettel-match-title-to-filename)))

(defun zettel-filter-for-link-at-point ()
  "Modifies the Deft filter to look for the Zettel linked with
the link at point. If there is only one match, opens the note in
another window."
  (interactive)
  (error "FIXME: Old way of handling oldname")
  (push (buffer-file-name) zettel-stored-links)
  (when (zettel-link-at-point-p)
    (let ((link (zettel-link-at-point))
          (deft-incremental-search nil))
      (deft-filter (concat "oldname: " link "$") t)
      (unless deft-current-files
        (deft-filter (concat "§" link ".") t))
      (cond ((= (length deft-current-files) 1)
             (deft-open-file (first deft-current-files) t t))
            ((null deft-current-files)
             (message "No notes with current or old name matching `%s'" link))
            (t
             (switch-to-buffer-other-window deft-buffer))))))

(defun zettel-replace-link-at-point (arg)
  "Replaces the link at point with the stored link. With a prefix
argument, or if there are no stored links, replaces with the
backlink."
  (interactive "P")
  (when (zettel-link-at-point-p)
    (let ((link  (zettel-link-at-point)))
      (save-excursion
        ;; Make sure we are at the start of the link
        (unless (string-match "\\[\\[[^]]+\\]\\]" (thing-at-point 'sexp))
          (re-search-backward "\\[\\["))
        (kill-sexp)
        (cond ((or (equal arg '(4))
                   (not zettel-stored-links))
               (zettel-insert-backlink nil))
              ((integerp arg)
               (zettel-insert-link-intrusive arg))
              (t
               (zettel-insert-link-intrusive nil)))))))

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
    (mapc #'(lambda (s)
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
                         (first (split-string line "title: " t)))))
          (message "%s | %s"
                   (alist-get :slug metadata) (alist-get :title metadata)))))))
(add-hook 'post-command-hook 'magit-show-zettel-title-in-minibuffer)

;;;-----------------------------------------------------------------------------
;;; Zettel-Mode Key Bindings
;;;
;; According to key binding conventions, the only bindings reserved for minor
;; modes are "Sequences consisting of C-c followed by any other punctuation
;; character" than {, }, <, >, : or ;, which are reserved for major modes.
;;;-----------------------------------------------------------------------------

(define-key zettel-mode-map (kbd "C-c ^") 'zettel-find-ancestor)
(define-key zettel-mode-map (kbd "C-c _") 'zettel-find-descendant)
(define-key zettel-mode-map (kbd "C-c @") 'zettel-insert-ancestor-link)
(define-key zettel-mode-map (kbd "C-c ,") 'zettel-insert-new-child)
(define-key zettel-mode-map (kbd "C-c C-'") 'zettel-set-category)
(define-key zettel-mode-map (kbd "C-c ~") 'zettel-kill-ring-save-link-title)
(define-key zettel-mode-map (kbd "C-c #") 'zettel-kill-ring-save-link)
(define-key zettel-mode-map (kbd "C-c C-S-f") 'zettel-select-link)
(define-key zettel-mode-map (kbd "C-c C-g") 'zettel-avy-link-search)

;; These keybindings shadow Org-mode's global "C-c l" and local "C-c C-l"
(define-key deft-mode-map (kbd "C-c l") 'zettel-store-link)
(define-key zettel-mode-map (kbd "C-c C-l") 'zettel-insert-link-to-cached-or-visiting)
(define-key zettel-mode-map (kbd "C-c C-M-l") 'zettel-insert-link-from-clipboard)

;; Was: `org-ctrl-c-tab'
;;(define-key zettel-mode-map (kbd "C-c C-i") 'zettel-org-include-cached-file)

;; Was: org-set-property-and-value
(define-key zettel-mode-map (kbd "C-c C-x P") 'zettel-ivy-set-parent)
(define-key zettel-mode-map (kbd "C-c C-x F") 'zettel-org-set-todo-properties)

;; Ztools interaction
(define-key zettel-mode-map (kbd "C-c C-x z") 'zettel-zmove-to-another-kasten)
;; Was: org-toggle-ordered-property
(define-key zettel-mode-map (kbd "C-c C-x l") 'zettel-links-to)

;;;-----------------------------------------------------------------------------
;;; Deft-Mode Keybindings
;;;-----------------------------------------------------------------------------

;; "Shadow" the built-in slug generator to generate timestamps by default,
;; i.e. when DEFT-NEW-FILE is called (C-c C-n)
(eval-after-load "deft"
  '(defalias 'deft-unused-slug 'zettel-next-unused-slug))
(define-key deft-mode-map (kbd "C-c C-S-n") 'deft-new-unused-zettel)

(define-key deft-mode-map (kbd "C-c s") 'zettel-add-section-sign-to-deft-filter)
(define-key deft-mode-map (kbd "C-c C-n") 'deft-new-file-maybe-named)
(define-key deft-mode-map (kbd "C-c #") 'zettel-kill-ring-save-link)
(define-key deft-mode-map (kbd "C-c C-f") 'zettel-select-link) ; Was: deft-find-file
(define-key deft-mode-map (kbd "C-c C-'") 'deft-filter-zettel-category)
(define-key deft-mode-map (kbd "C-c C-p") 'zettel-populate-categories)
;; Was: deft-filter-clear
(define-key deft-mode-map (kbd "C-c C-c") 'zettel-set-category)

(provide 'zettel)
