;;;; -*- mode: emacs-lisp -*-
;;;;-----------------------------------------------------------------------------
;;;;        Author: Richard Boyechko <rb-emacs@diachronic.net>
;;;;   Description: Zettelkasten implementation based on Deft
;;;;  Date Created: 2015-06-31
;;;;      Comments:
;;;;-----------------------------------------------------------------------------

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

(defvar zettel-filename-format '("%03d" "%c" "%c" "%c" "%c" "%c" "%c")
  "A list of elements of the Zettel filename as FORMAT control
  sequences.")

(defvar zettel-base-format (first zettel-filename-format)
  "The format of the base numerical component of Zettel's name.")

(defvar zettel-regexp-numerus-currens
  "\\([0-9]\\{3\\}\\)-\\([a-z]\\{3\\}\\)"
  "The regular expression that matches numerus currens like 261-cab.")

(defvar zettel-regexp-tempus-currens
  "\\([0-9]\\{4\\}\\)\\([0-9][0-9]\\)\\([0-9][0-9]\\)T\\([0-9][0-9]\\)\\([0-9][0-9]\\)"
  "The regular expression that matches the basic (but not extended) ISO 8601
date and time.
Groups 1-3 are year, month, day.
Groups 4-5 are hour, minute.")

(defvar zettel-regexp-slug
  ;; Strip the groups in the component regexps
  (concat "\\("
          (replace-regexp-in-string "\\\\[()]" "" zettel-regexp-numerus-currens)
          "\\|"
          (replace-regexp-in-string "\\\\[()]" "" zettel-regexp-tempus-currens)
          "\\)")
  "A generalized regexp that matches any slug, whether numerus or tempus
currens.")

(defvar zettel-regexp-link
  (concat "\\(\\([[:alpha:]]+\\):\\)*" zettel-regexp-slug)
  "The regular expression that matches Zettel links.
Group 2 is the kasten, if specified.
Group 3 is numerus or tempus currens.")

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

;;;=============================================================================
;;; User Variables
;;;=============================================================================

(defcustom zettel-directory nil
  "The central Zettelkasten directory."
  :type 'string)

(defcustom zettel-kasten nil
  "An alist containing the names and directories of the Kasten."
  :type 'alist)

(defcustom zettel-kasten-aliases nil
  "An alist of any other aliases for the `zettel-kasten'. This is an alist of
the actual name followed by the alias."
  :type 'alist)

(defcustom zettel-default-numerus-kasten "reticulum"
  "Name of the main numerus currens kasten."
  :type 'string)

(defcustom zettel-default-tempus-kasten "rumen"
  "Name of the main tempus currens kasten."
  :type 'string)

(defcustom zettel-categories nil
  "A list of categories used for Zettel."
  :type 'list)

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

(defun zettel-p (file)
  "Returns non-NIL if the file is a Zettel."
  (interactive "f")
  (when file
    (and (string-equal (file-name-extension file) deft-extension)
         (string-match zettel-regexp-slug (file-name-base file)))))

(defun zettel-kasten-directory (kasten)
  "Returns the directory of the given KASTEN."
  (second (assoc (zettel-kasten-truename kasten) zettel-kasten)))

(defun zettel-directory-kasten (directory)
  "Returns the kasten name of the given Zettel directory."
  (car (cl-rassoc directory zettel-kasten :key #'first :test #'string=)))

(defun zettel-kasten-truename (kasten)
  "Returns the true name of the given KASTEN."
  (or (cdr (assoc kasten zettel-kasten-aliases))
      (car (assoc kasten zettel-kasten))))

(defun zettel-file-slug (file)
  "Returns the slug part of the given Zettel file."
  (file-name-base file))

(defun zettel-file-kasten (file)
  "Returns the kasten of the given Zettel file.

The function relies on the fact that the Kasten directory is 2nd from the
last for any numerus or tempus Zettel."
  (zettel-kasten-truename
   (second (reverse (split-string (file-name-directory file) "/" t "/")))))

(defun zettel-file-link (file)
  "Given the path to a Zettel FILE, returns a fully qualified link to it."
  (let ((kasten (zettel-file-kasten file)))
    (if (or (equal kasten zettel-default-numerus-kasten)
            (equal kasten zettel-default-tempus-kasten))
        (zettel-file-slug file)
      (concat kasten ":" (zettel-file-slug file)))))

(defun zettel-link-p (string)
  "Returns non-NIL if the string could be a link to a Zettel."
  (and (string-match (concat "^" zettel-regexp-link "$") string)
       ;; If kasten is specified, make sure it's a valid one
       (if (match-string-no-properties 2 string)
           (or (assoc (match-string-no-properties 2 string) zettel-kasten)
               (assoc (match-string-no-properties 2 string) zettel-kasten-aliases))
         t)))

(defun zettel-link-kasten (link)
  "Returns the kasten part of the given LINK. If no kasten is explicitly
specified, asks the user to resolve the ambiguity."
  (when (string-match zettel-regexp-link link)
    (let* ((kasten (match-string 2 link))
           (slug (match-string 3 link))
           (type (zettel-type slug)))
      (or kasten
          (let ((default-kasten (if (eq type :numerus)
                                    zettel-default-numerus-kasten
                                  zettel-default-tempus-kasten)))
            (cond ((equal current-prefix-arg '(4))
                   (ivy-read (format "Ambiguous link [[%s]], select Kasten: " link)
                             (if (listp zettel-kasten)
                                 (mapcar #'first zettel-kasten)
                               (error "No Zettelkästen defined"))
                             :preselect default-kasten))
                  (default-kasten
                    default-kasten)
                  ((not default-kasten)
                   (call-interactively #'zettel-set-default-kasten)
                   (zettel-link-kasten link))
                  (t
                   (error "Umm, this shouldn't happen"))))))))

(defun zettel-set-default-kasten (type kasten)
  "Interactively set the default kasten for the given type (:NUMERUS or :TEMPUS)."
  (interactive
   (list (intern (concat ":" (ivy-read "Set default for which type of Zettel? "
                                       '(NUMERUS TEMPUS))))
         (ivy-read "Set the default to what Kasten? "
                   (if (listp zettel-kasten)
                       (mapcar #'first zettel-kasten)
                     (error "No Zettelkästen defined")))))
  (case type
    (:NUMERUS (setq zettel-default-numerus-kasten kasten))
    (:TEMPUS (setq zettel-default-tempus-kasten kasten))
    (t (error "Zettel type not selected"))))

(defun zettel-link-slug (link)
  "Returns the slug part of the given LINK."
  (when (string-match zettel-regexp-link link)
    (match-string 3 link)))

(defun zettel-make-link (kasten slug)
  "Make a new proper link to SLUG in KASTEN."
  (if (or (equal kasten zettel-default-numerus-kasten)
          (equal kasten zettel-default-tempus-kasten))
      slug
    (concat kasten ":" slug)))

(defun zettel-tempus-directory (slug)
  "Returns the right subdirectory for the given tempus currens slug."
  (when (string-match zettel-regexp-tempus-currens slug)
    (file-name-as-directory (match-string 1 slug))))

(defun zettel-numerus-directory (slug)
  "Finds the right directory for the given numerus currens slug."
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
            (:numerus (zettel-numerus-directory slug))
            (:tempus (zettel-tempus-directory slug))
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

(defun zettel-type (slug-or-file)
  "Returns the type of the given slug or file: :NUMERUS or :TEMPUS."
  (let ((slug (file-name-base slug-or-file)))
    (cond ((string-match-p zettel-regexp-tempus-currens slug)
           :tempus)
          ((string-match-p zettel-regexp-numerus-currens slug)
           :numerus)
          (t
           (error "The slug is neither numerus nor tempus: %s" slug)))))

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
          zettel-regexp-slug
          "\\. \\({\\([^}]+\\)} \\)*\\([^#]+\\)\\( \\(#.*\\)\\)*$")
  "Regular expression for a combined title string, used in `zettel-metadata'.
Group 1 is the slug.
Group 3 is the category.
Group 4 is the title itself.
Group 6 is the keyword block.")

(defun zettel-combined-title-metadata (title)
  "Returns an alist of metadata from a combined title."
  (when (string-match zettel-regexp-combined-title title)
    (let ((slug (match-string 1 title)))
      (list (cons :slug slug)
            (cons :type (zettel-type slug))
            (cons :category (match-string 3 title))
            (cons :title (match-string 4 title))
            (cons :keywords
                  (when (match-string 6 title)
                    (split-string (match-string 6 title))))))))

(defun zettel-combined-title (metadata)
  "Returns a list of two elements: 1) a string that encodes into the title
line the given METADATA, and 2) leftover metadata."
  (list (format "title: §%s. %s%s"
                (alist-get :slug metadata)
                (if (alist-get :category metadata)
                    (concat "{" (alist-get :category metadata) "} ")
                  "")
                (alist-get :title metadata))
        (set-difference metadata '((:slug) (:category) (:title) (:type))
                        :key #'car)))

(defun zettel-metadata-key-name (key)
  "Returns a string that is the name of the KEY, a keyword symbol."
  (subseq (symbol-name key) 1))

(defun zettel-replace-metadata (file metadata)
  "Replaces the metadata of the given FILE with the one in the METADATA
alist."
  (save-excursion
    (with-current-buffer (get-file-buffer file)
      (save-restriction
        (goto-char (point-min))
        (when (re-search-forward "^$" nil t 1)
          (narrow-to-region (point-min) (point)))
        (kill-region (point-min) (point-max))
        (multiple-value-bind (title rest)
            (zettel-combined-title metadata)
          (insert title)
          (newline)
          (mapc #'(lambda (cons)
                    (insert (format "%s: %s"
                                    (zettel-metadata-key-name (car cons))
                                    (cdr cons)))
                    (newline))
                (cl-sort rest #'string-lessp :key #'car)))))))

(defun zettel-update-metadata (key value)
  "Updates the Zettel metadata section in the current buffer, setting the KEY
to VALUE."
  (let ((key-name (zettel-metadata-key-name key))
        (value-string
         (typecase value
           (string value)
           (list (concat "[" (mapconcat #'identity value ", ") "]"))
           (t
            (error "Not implemented for type %s" (type-of value)))))
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
  "Returns an alist of metadata, with the keys as keywords."
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
                            (cons (intern (concat ":" (match-string 1 line)))
                                  (match-string 2 line))
                          (error "Malformed metadata line: '%s'" line))))
                  metadata-section))
         (title (alist-get :title metadata)))
    (when title
      (setq metadata
        (append (zettel-combined-title-metadata title)
                (cl-remove :title metadata :key #'car))))
    (push (list :kasten (zettel-file-kasten file)) metadata)))

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
        (zettel-update-metadata :modified today)))))

(add-hook 'zettel-mode-hook
  '(lambda ()
     (add-hook 'before-save-hook 'zettel-update-metadata-date nil t)))

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

(defun zettel-next-unused-slug ()
  "Returns the next unused slug, relying on `deft-all-files'."
  (let ((used (mapcar #'zettel-file-slug deft-all-files))
        slug)
    (while (or (not slug) (find slug used))
      (setq slug (format "%03d-%s"
                         (random 1000)
                         (abase26-encode (random (expt 26 3)) 3))))
    slug))

(defun zettel-timestamp-slug ()
  "Returns a timestamp in the form YYYYMMDDTHHmm to use as the slug."
  (format-time-string "%Y%m%dT%H%M"))

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
;;; Zettel Links
;;;=============================================================================

(defun zettel-link-at-point-p ()
  "Returns T if the thing at point is a wiki link (i.e. [[XXX]])."
  (thing-at-point-looking-at
   (concat "\\[\\[\\(" zettel-regexp-link "\\)\\]\\(\\[[^]]+\\]\\)\\]")))

(defun zettel-link-at-point ()
  "Return the Zettel link at point. Needs to be called after
`zettel-link-at-point-p'."
  (match-string-no-properties 1))

(defun zettel-store-link (arg)
  "Add the link 1) to the Deft file at point if in *Deft* buffer, or 2) to
the file in current buffer into `zettel-stored-links'."
  (interactive "p")
  (let* ((file (cond ((equal major-mode 'deft-mode)
                      (widget-get (widget-at (point)) :tag))
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

(defun zettel-insert-link-to-stored-or-visiting (arg)
  "Inserts a link to another Zettel being currently visited or to those in
`zettel-stored-links'."
  (interactive "P")
  (let* ((files (delete-dups (append (mapcar #'zettel-absolute-filename
                                             zettel-stored-links)
                                     (zettel-visiting-buffer-list t)))))
    (if files
        (progn
          (push (zettel-ivy-read-file files 'zettel-file-link)
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

(defun zettel-kill-ring-save-link-title ()
  "Save the title of the wiki link at point or the buffer to kill
ring."
  (interactive)
  (let ((file (cond ((zettel-link-at-point-p)
                     (zettel-absolute-filename (zettel-link-at-point)))
                    ((zettel-p buffer-file-name)
                     buffer-file-name))))
    (let ((title (alist-get :title (zettel-metadata file))))
      (cond (title
             (kill-new title)
             (message "Link title to %s saved in kill ring."
                      (file-name-base file)))
            (t
             (message "Could not get the title of %s."
                      (file-name-base file)))))))

(defun zettel-kill-ring-save-link (arg)
  "Save the deft note at point or the buffer base filename in the kill ring
to be used as a wiki link elsewhere. With prefix argument, save the file name
relative to `zettel-directory' instead. With two prefix arguments, open the
file in Finder with it selected."
  (interactive "p")
  (let ((file (cond ((equal major-mode 'deft-mode)
                     (widget-get (widget-at (point)) :tag))
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

;;;=============================================================================
;;; Genealogical
;;;=============================================================================

(defun zettel-ancestor (file-or-link &optional degree)
  "Returns the FILE-OR-LINK's ancestor, or NIL if could not figure out. With
the optional DEGREE, try to find the Nth ancestor (i.e. grandparent if DEGREE
is 2, an so on), returning the most remote ancestor that could find."
  (if (= (abs degree) 0)
      file-or-link
    (zettel-ancestor (alist-get :parent
                                (zettel-metadata
                                 (if (zettel-link-p file-or-link)
                                     (zettel-absolute-filename file-or-link)
                                   file-or-link)))
                     (1- degree))))

(defun zettel-find-ancestor (n)
  "Opens the current Zettel's ancestor. With a prefix argument, try
to find the Nth ancestor."
  (interactive "p")
  (when (zettel-p buffer-file-name)
    (let ((ancestor (zettel-ancestor buffer-file-name n)))
      (if ancestor
          (find-file (zettel-absolute-filename ancestor))
        (message "No ancestor found")))))

(defun zettel-insert-ancestor-link (arg)
  "Insert a link to the ancestor of the current zettel. With a
numerical prefix argument, try to find Nth ancestor. With
universal argument, behave like `zettel-insert-link'."
  (interactive "P")
  (let ((link (zettel-ancestor buffer-file-name (if (integerp arg) arg 1))))
    (if link
        (insert (zettel-wiki-link link (consp arg) (equal arg '(16))))
      (message "Could not find such ancestor"))))

(defvar zettel-parent-of-new-child nil
  "An alist of new children and their respective parents.")

(defun zettel-insert-new-child (arg)
  "Creates a new Zettel in the current `deft-directory', inserting a link to
it at point, saves the current Zettel as its parent, and sets the
`zettel-link-backlink' to current Zettel. With prefix argument, allows the
user to select the Zettelkasten."
  (interactive "p")
  (let* ((parent (cond ((zettel-p buffer-file-name)
                        buffer-file-name)
                       ((equal major-mode 'deft-mode)
                        (widget-get (widget-at (point)) :tag))
                       (t
                        ;; Something weird happened
                        nil)))
         (kasten (if (> arg 1)
                     (ivy-read "Zettelkasten: "
                               (if (listp zettel-kasten)
                                   (mapcar #'first zettel-kasten)
                                 (error "No Zettelkasten defined")))
                   (zettel-directory-kasten deft-directory)))
         (slug (progn
                 (unless zettel-default-numerus-kasten
                   (call-interactively #'zettel-set-default-kasten))
                 (if (equal kasten zettel-default-numerus-kasten)
                     (zettel-next-unused-slug)
                   (zettel-timestamp-slug))))
         (child-link (zettel-make-link kasten slug))
         (parent-link (when parent (zettel-file-link parent))))
    (when parent
      (add-to-list 'zettel-parent-of-new-child (cons child-link parent-link))
      (setq zettel-link-backlink parent-link))
    (if (equal major-mode 'deft-mode)
        (deft-new-file-named slug)
      (insert (zettel-wiki-link child-link nil nil t)))))

(defun zettel-set-parent ()
  "Sets the parent metadata of the current Zettel to the open Zettel chosen
by the user."
  (interactive)
  (ivy-read "Set parent: "
            (zettel-ivy-collection-alist (zettel-visiting-buffer-list t))
            :require-match nil
            :action (lambda (choice)
                      (zettel-update-metadata
                       :parent (zettel-file-link (cdr choice))))))

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
      (insert "* ")
      (insert (zettel-wiki-link (zettel-absolute-filename child) t arg))
      (newline))))

;;;=============================================================================
;;; Buffers, Files, Categories
;;;=============================================================================

(defun zettel-find-file (link)
  "Finds the Zettel with the given link specified interactively by the user,
returning T if it's a Zettel link. If the file is empty, inserts the metadata
template."
  (interactive "sZettel link to find: ")
  (when (zettel-link-p link)
    (find-file (zettel-absolute-filename link))
    (when (= (point-min) (point-max)) ; file is empty
      (message "Empty Zettel, inserting metadata template")
      (zettel-insert-metadata-template))
    t))

(defun zettel-ivy-collection-alist (files)
  "Given a list of Zettel files, returns a nicely formatted list of choices
suitable for passing to `ivy-read' as collection."
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

(defun zettel-ivy-read-file (files func)
  "Uses `ivy-read' to select from list of Zettel FILES. Upon selection, call
the given FUNC, a function accepting one argument that is a pathname. Returns
the result of FUNC."
  (let* ((choices (zettel-ivy-collection-alist files)))
    (let (result)
      (ivy-read "Zettel: "
                choices
                :action (lambda (choice)
                          (setq result
                            (unless (string-empty-p choice)
                              (funcall func (cdr choice))))))
      result)))

(defun zettel-switch-to-buffer ()
  "Quickly switch to other open Zettel buffers."
  (interactive)
  (zettel-ivy-read-file (zettel-visiting-buffer-list t) 'find-file))

(defun zettel-ivy-read-category ()
  "Returns a list, suitable to be passed to `interactive', asking the user to
choose a category from `zettel-categories'."
  (if current-prefix-arg
      (read-string "Category: ")
    (ivy-read "Category: " zettel-categories)))

(defun zettel-set-category (file category)
  "Sets the category to the Zettel title based on `zettel-categories'. With
prefix argument, allows the user to type in a custom category."
  (interactive (list (if (eq major-mode 'deft-mode)
                         (widget-get (widget-at (point)) :tag)
                       buffer-file-name)
                     (zettel-ivy-read-category)))
  (let ((orig-buffer (current-buffer)))
    (save-excursion
      (with-current-buffer (find-file-noselect file)
        (goto-char (point-min))
        (when (re-search-forward "^title: §?\\([^.]+\\)\\. \\({[^}]+} \\)*")
          (replace-match (format "title: §\\1. {%s} " category)))
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
    (when (re-search-forward "^title: §?\\([0-9a-z-]+\\)\\. \
\\(\\w+ \\)+\\(\\w+\\), \\([^(]+\\) (\\([0-9]+\\))")
      (replace-match (subseq (match-string 2) 0 1) nil nil nil 2)
      (replace-match "title: §\\1. {\\3\\5} \\2. \\3's \\4 (\\5)"))))

;;;=============================================================================
;;; Deft-Mode Integration
;;;=============================================================================

;; Adjust how Deft lists Zettel
(setq deft-strip-title-regexp "^\\(title: +\\)"
      ;; Default: "\\(?:^%+\\|^[#* ]+\\|-\\*-[[:alpha:]]+-\\*-\\|#+$\\)"
      deft-strip-summary-regexp "\\(^\\w+: .*\\)"
      ;; Default: "\\([\n	]\\|^#\\+[[:upper:]_]+:.*$\\)"
      deft-time-format nil
      deft-use-filename-as-title nil
      deft-current-sort-method 'mtime)

(add-hook 'deft-mode-hook
  (lambda ()
    (setq show-trailing-whitespace nil)))

(defun zettel-deft-parse-title-function (line)
  "Function for post-processing titles for display in Deft buffer, intended
as the value for `deft-parse-title-function'."
  (let ((metadata (zettel-combined-title-metadata
                   (replace-regexp-in-string "^\\(title: +\\)" "" line))))
    (when metadata
     ;; FIXME: Any more elegant way to do this?
     (if (eq (zettel-type (alist-get :slug metadata)) :numerus)
         (setq slug-len 7               ; 000-aaa
               cat-len 20)
       (setq slug-len 13                ; 19700101T0000
             cat-len 15))
     ;; SLUG CATEGORY TITLE KEYWORDS
     (format (format "%%-%ds%%-%ds%%s %%s" (+ slug-len 2) cat-len)
             (alist-get :slug metadata)
             (let ((cat (alist-get :category metadata)))
               (if (> (length cat) cat-len)
                   (concat (subseq cat 0 (- cat-len 2)) "..")
                 cat))
             (alist-get :title metadata)
             (or (alist-get :keywords metadata) "")))))
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

(defun deft-filter-zettel-category (category)
  "Inserts a category into deft-filter if there is no category there or
changes the existing one."
  (interactive (list (zettel-ivy-read-category)))
  (when deft-incremental-search
    ;; Duplicate car of the `deft-filter-regexp', since `deft-filter' replaces it.
    (push (car deft-filter-regexp) deft-filter-regexp)
    (deft-filter (format "{%s}" category) t)))

;;
;; Insert my zettel title string into new zettel rather than contents of deft's
;; filter string.
;;
(defun zettel-insert-metadata-template ()
  "Inserts the metadata template into the current buffer."
  (let ((base (file-name-base buffer-file-name))
        (link (zettel-file-link buffer-file-name))
        insert-point)
    (insert "title: §" base ". ")
    (setq insert-point (point))
    (newline)
    (insert "created: "
            ;; Insert creation date, making it match a tempus currens filename
            (format-time-string
             "%Y-%m-%d"
             (let ((today (format-time-string "%Y%m%d")))
               (if (and (eq :tempus (zettel-type buffer-file-name))
                        (not (string-match-p (regexp-quote today) base))
                        (y-or-n-p "Match creation date to filename? "))
                   (zettel-encode-iso8601-datetime base)
                 nil))))                  ; i.e. current time
    (newline)
    (when (assoc link zettel-parent-of-new-child)
      (insert "parent: " (cdr (assoc link zettel-parent-of-new-child)))
      (newline))
    (goto-char insert-point)
    (call-interactively 'zettel-set-category)
    (end-of-line)))

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
                (string-lessp (downcase (file-name-base f2))
                              (downcase (file-name-base f1))))))
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

(defun zettel-org-include-visiting-file ()
  "Add an org-mode #+INCLUDE to a visiting Zettel."
  (interactive)
  (let* ((files (zettel-visiting-buffer-list t)))
    (cond (files
           (zettel-ivy-read-file
            files
            #'(lambda (file)
                (insert (format "#+INCLUDE: \"%s::\""
                                (file-relative-name file))))))
          (t
           (user-error "You are not visiting any Zettel")))))

;; Org links
(eval-after-load "org"
  '(progn
     ;; Try to resolve "fuzzy" links (i.e. without explicit protocol). This is
     ;; all that is needed to handle links in the form [[ZETTEL-LINK]].
     (push #'zettel-find-file org-open-link-functions)

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

(defun zettel-open-link-at-point ()
  "Open a Zettel link at point even if it's not formatted as a link."
  (interactive)
  (save-excursion
    (forward-word)
    (backward-word)
    (when (thing-at-point-looking-at (concat "\\(" zettel-regexp-link "\\)"))
      (zettel-find-file (match-string-no-properties 1)))))

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
;;; Maintenance
;;;=============================================================================

(defun zettel-rename-and-update-title ()
  "Using most of the code from deft.el's DEFT-RENAME-FILE."
  (interactive)
  (let (old-filename new-filename old-name new-name)
    (setq old-filename (widget-get (widget-at) :tag))
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
  (let (oldname)
    (save-excursion
      (save-restriction
        (goto-char (point-min))
        (forward-paragraph)
        (narrow-to-region (point-min) (point))
        (goto-char (point-min))
        (when (re-search-forward "title: §*\\([a-z0-9-]+\\)\\.")
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

(defun zettel-generate-new-slugs (how-many)
  (interactive "nHow many: ")
  (goto-char (point-max))
  (let (slugs)
    (message "Refreshing Deft cache...")
    (deft-refresh)
    (dotimes (n how-many)
      (push (zettel-next-unused-slug) slugs)
      (message "%d generated" n))
    (mapc #'(lambda (s)
              (insert s)
              (newline))
          (delete-dups slugs))
    (delete-duplicate-lines (point-min) (point-max))))

;;;-----------------------------------------------------------------------------
;;; Zettel-Mode Key Bindings
;;;
;; According to key binding conventions, the only bindings reserved for minor
;; modes are "Sequences consisting of C-c followed by any other punctuation
;; character" than {, }, <, >, : or ;, which are reserved for major modes.
;;;-----------------------------------------------------------------------------

(define-key zettel-mode-map (kbd "C-c ^") 'zettel-find-ancestor)
(define-key zettel-mode-map (kbd "C-c @") 'zettel-insert-ancestor-link)
(define-key zettel-mode-map (kbd "C-c ,") 'zettel-insert-new-child)
(define-key zettel-mode-map (kbd "C-c C-'") 'zettel-set-category)
(define-key zettel-mode-map (kbd "C-c ~") 'zettel-kill-ring-save-link-title)
(define-key zettel-mode-map (kbd "C-c #") 'zettel-kill-ring-save-link)
(define-key zettel-mode-map (kbd "C-c C-S-f") 'zettel-find-file)

;; These keybindings shadow Org-mode's global "C-c l" and local "C-c C-l"
(define-key deft-mode-map (kbd "C-c l") 'zettel-store-link)
(define-key zettel-mode-map (kbd "C-c C-S-l")
            'zettel-insert-link-to-stored-or-visiting)

(define-key zettel-mode-map (kbd "C-c C-M-l")
            'zettel-insert-link-from-clipboard)
(define-key zettel-mode-map (kbd "C-c C-M-S-l") 'zettel-list-links)
(define-key zettel-mode-map (kbd "C-c C-S-b") 'zettel-insert-backlink)

;; Was: org-set-property-and-value
(define-key zettel-mode-map (kbd "C-c C-x P") 'zettel-set-parent)
(define-key zettel-mode-map (kbd "C-c C-x F") 'zettel-org-set-todo-properties)

;;;-----------------------------------------------------------------------------
;;; Deft-Mode Keybindings
;;;-----------------------------------------------------------------------------

;; "Shadow" the built-in slug generator to generate timestamps by default,
;; i.e. when DEFT-NEW-FILE is called (C-c C-n)
(eval-after-load "deft"
  '(defalias 'deft-unused-slug 'zettel-timestamp-slug))
(define-key deft-mode-map (kbd "C-c C-S-n") 'deft-new-unused-zettel)

(define-key deft-mode-map (kbd "C-c s") 'zettel-add-section-sign-to-deft-filter)
(define-key deft-mode-map (kbd "C-c C-n") 'deft-new-file-maybe-named)
(define-key deft-mode-map (kbd "C-c #") 'zettel-kill-ring-save-link)
(define-key deft-mode-map (kbd "C-c C-f") 'zettel-find-file) ; Was: deft-find-file
(define-key deft-mode-map (kbd "C-c C-'") 'deft-filter-zettel-category)
(define-key deft-mode-map (kbd "C-c C-p") 'zettel-populate-categories)

(provide 'zettel)
