;;;; -*- mode: emacs-lisp -*-
;;;;-----------------------------------------------------------------------------
;;;;        Author: Roderick Hoybach <hoybach-code@diachronic.net>
;;;;   Description: Zettelkasten implementation based on Deft and Markdown
;;;;  Date Created: 2015-06-31
;;;;      Comments:
;;;;-----------------------------------------------------------------------------
;;;;
;;;; TODO:
;;;; * Write `zettel-link-p'
;;;; * Write `zettel-slug-new-sibling'
;;;;

(require 'deft)

;; Cretea a keymap for the mode
(defvar zettel-mode-map (make-sparse-keymap)
  "Keymap for Zettel mode.")

;; Define a minor mode for working with Zettelkasten in deft
(define-minor-mode zettel-mode
  "Make the keymap zettel-mode-map active."
  :lighter " Zettel"
  :keymap zettel-mode-map
  :require 'deft)

(defvar zettel-directory nil            ; should be set on each computer
  "The central Zettelkasten directory housing all the sub-Zettelkasten.")

(defun in-zettel-dir (&optional relative-path)
  "Returns absolute pathname of the given pathspec relative to
the Zettel directory."
  (expand-file-name (or relative-path "") zettel-directory))

(defvar zettel-sub-kasten
  nil
  "An alist of translating various sub-Zettelkasten into pathnames.")

(defvar zettel-filename-format '("%03d" "%c" "%c" "%c" "%c" "%c" "%c")
  "A list of elements of the Zettel filename as FORMAT control
  sequences.")
(defvar zettel-base-format (first zettel-filename-format)
  "The format of the base numerical component of Zettel's name.")

(defvar zettel-regexp-numerus-currens
  "§*\\([0-9]\\{3\\}\\)\\(-\\([a-z]+\\)\\)*\\>"
  "The regular expression that matches numerus currens like 261-cabf.")

(defvar zettel-regexp-date
  "§*\\([0-9-]\\{8,10\\}\\(T*[0-9:]\\{4,5\\}\\)*\\)"
  "The regular expression that matches ISO 8601-like date/time expression.")

(defun zettel-p (file)
  "Returns non-NIL if the file is a Zettel."
  (interactive "f")
  (when file
    (and (string-equal (file-name-extension file) deft-extension)
     (or (string-match zettel-regexp-numerus-currens (file-name-base file))
         (string-match zettel-regexp-date (file-name-base file))))))

(defun zettel-link-p (string)
  "Returns non-NIL if the string could be a link to a zettel."
  ;; TODO: Deal with subkasten in the link
  (or (string-match zettel-regexp-numerus-currens string)
      (string-match zettel-regexp-date string)))

(defun backup-file-name-p (file)
  "Return non-nil if FILE is a backup file name (numeric or not)
OR if FILE is a hidden dot-file.

Redefining the standard function so that temporary dot-files are
not listed by Deft."
  (or (string-match "\\`\\." (file-name-nondirectory file))
      (string-match "~\\'" file)))

(defun zettel-absolute-filename (slug &optional extension)
  "Return an absolute filename to file named SLUG with optional EXTENSION.
If the slug is a numerus currens, makes sure to locate it in the
right split-up directory. Otherwise, relies on `deft-directory'.
If EXTENSION is not given, `deft-extension' is assumed.

This function replaces `deft-absolute-filename' for zettels."
  (expand-file-name
   (concat slug "." (or extension deft-extension))
   (if (string-match (concat "^" zettel-regexp-numerus-currens) slug)
       (expand-file-name (zettel-right-directory slug)
                         (cdr (assoc "main" zettel-sub-kasten)))
     deft-directory)))

(defun deft-absolute-filename--around (orig-fun &rest args)
  "Replaces the default `deft-absolute-filename' with
`zettel-absolute-filename'."
  (apply #'zettel-absolute-filename args))
(advice-add 'deft-absolute-filename :around 'deft-absolute-filename--around)

;; Enable zettel-mode for files that match the pattern
(eval-after-load "markdown"
  (add-hook 'markdown-mode-hook
    '(lambda ()
      (when (zettel-p buffer-file-name)
        (zettel-mode 1)))))
(add-hook 'org-mode-hook
  '(lambda ()
     (when (zettel-p buffer-file-name)
       (zettel-mode 1)
       (setq org-descriptive-links nil))))

;;;=============================================================================
;;; Deft buffer
;;;=============================================================================

;; Default: "\\(?:^%+\\|^[#* ]+\\|-\\*-[[:alpha:]]+-\\*-\\|#+$\\)"
(setq deft-strip-title-regexp "^\\(title: +\\)")
;; Default: "\\([\n	]\\|^#\\+[[:upper:]_]+:.*$\\)"
(setq deft-strip-summary-regexp "\\(^\\w+: .*\\)"
      deft-time-format nil)
(setq deft-use-filename-as-title nil)

(add-hook 'deft-mode-hook
  (lambda ()
    (setq show-trailing-whitespace nil)))

(defvar zettel-indent-title-column 15
  "The column number where the Zettel title (without the numerus
currens) will be indented to.")

(defun deft-file-title--separate (orig-fun file-name)
  "Replace the first slash of with enough spaces to justify the actual title."
  (let ((title (funcall orig-fun file-name)))
    (when title
      (if (zettel-p file-name)
          (let ((numerus-length
                 (if (or (string-match zettel-regexp-numerus-currens title)
                         (string-match zettel-regexp-date title))
                     ;; Strip the § before the numerus currens, if exists
                     (let ((match-end (match-end 0)))
                       (cond ((string-match "§" title)
                              (setq title (replace-regexp-in-string "§" "" title))
                              (- match-end 1))
                             (t
                              match-end)))
                     0)))
            ;; Replace the ". " in the first title (following § + numerus currens)
            ;; with indentation.
            (replace-regexp-in-string
             "\\(\\. \\).*\\'"
             (let ((diff (- zettel-indent-title-column numerus-length)))
               (make-string (max diff 0) ?\s))
             title nil nil 1))
          title))))
(advice-add 'deft-file-title :around #'deft-file-title--separate)

(defun zettel-add-subtree-to-deft-filter (slug)
  "Replaces the filter with the subtree where the point rests. If
called with universal argument, ask the user to enter the zettel
number."
  (interactive
   (list (if current-prefix-arg
             (read-string "Numerus currens to find: ")
             (when (string-equal (buffer-name) "*Deft*")
               (file-name-sans-extension
                (file-name-nondirectory
                 (widget-get (widget-at (point)) :tag)))))))
  (let ((subtree (concat "§" slug)))
    (setq deft-filter-regexp (list subtree))
    (deft-refresh-filter)))

(defun zettel-add-section-sign-to-deft-filter ()
  "Inserts the Unicode section sign (§) to Deft filter string."
  (interactive)
  (setq last-command-event 167)
  (deft-filter-increment))

(defun zettel-add-index-to-deft-filter ()
  "Replaces the current filter string with [Index]."
  (interactive)
  (if deft-incremental-search
      (deft-filter "[Index]" t)
      (deft-filter "\\[Index\\]")))

(define-key deft-mode-map (kbd "C-c s") 'zettel-add-section-sign-to-deft-filter)
(define-key deft-mode-map (kbd "C-c C-i") 'zettel-add-index-to-deft-filter)
(define-key deft-mode-map (kbd "C-c C-h") 'zettel-add-subtree-to-deft-filter)

;;;=============================================================================
;;; Unused Slugs
;;;=============================================================================

(defun zettel-local-unused-slug ()
  "Return an unused filename slug (short name) in the current `deft-directory'."
  (let* ((fmt zettel-base-format)
         (counter 0)
         (slug (format fmt counter))
         (file (deft-absolute-filename slug)))
    (while (or (file-exists-p file) (get-file-buffer file))
      (setq counter (1+ counter))
      (setq slug (format fmt counter))
      (setq file (deft-absolute-filename slug)))
    slug))

(defun zettel-used-slugs ()
  "Returns a list -- sorted and with no duplicates -- of used slugs."
  (delete-dups
   (delete nil
           (mapcar #'(lambda (file)
                       (when (string-match
                              (format "/\\([0-9]\\{3\\}\\)\\(-[a-z]+\\)*\.%s$"
                                      deft-extension)
                              file)
                         (string-to-number (match-string 1 file))))
                   deft-all-files))))

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

(defcustom zettel-new-numerus-currens-method 'random
  "How new numeri currenses are created: RANDOM or NEXT are
acceptable."
  :type 'symbol)

(defcustom zettel-new-child-method 'random
  "How new children are created: RANDOM or NEXT are acceptable."
  :type 'symbol)

(defcustom zettel-new-numerus-currens-random-limit 200
  "Upper limit for generating random numeri currenses; exclusive."
  :type 'integer)

(defun zettel-next-unused-slug ()
  "Returns the next unused slug, relying on `deft-all-files'.
Respects the metnod in `zettel-new-numerus-currens-method'."
  (let ((used (zettel-used-slugs))
        (j 0))
    (case zettel-new-numerus-currens-method
      (random
       ;; Generate a random number that doesn't occur in the list.
       (while (find j used)
         (setq j (random zettel-new-numerus-currens-random-limit))))
      (next
       (let ((used (apply #'vector used)))
         ;; Since the vector is sorted and only has unique elements, we can find
         ;; any holes simply by making sure the element is the same as the index.
         (while (and (< j (length numbers))
                     (= j (aref numbers j)))
           (setq j (1+ j))))))
    (format zettel-base-format j)))

(defun zettel-timestamp-slug ()
  "Returns a timestamp in the form YYYYMMDDTHHmm to use as the slug."
  (format-time-string "%Y%m%dT%H%M"))

;; "Shadow" the built-in slug generator to generate timestamps by default,
;; i.e. when DEFT-NEW-FILE is called (C-c C-n)
(eval-after-load "deft"
  '(defalias 'deft-unused-slug 'zettel-timestamp-slug))

;; C-c C-S-n, on the other hand, will create a new Zettel with unused numerus
;; currens
(defun deft-new-unused-zettel ()
  "Create a new Zettel with unused numerus currens."
  (interactive)
  (deft-new-file-named (zettel-next-unused-slug)))
(define-key deft-mode-map (kbd "C-c C-S-n") 'deft-new-unused-zettel)

(defun zettel-rename-with-unused-slug ()
  "Rename the current file and buffer to an unused filename
slug (short name) in `deft-directory' with `deft-extension'.
Based on `rename-file-and-buffer'."
  (interactive)
  (rename-file-and-buffer (concat (zettel-next-unused-slug) "." deft-extension)))

;;;=============================================================================
;;; Automatically update the date in the title line
;;;=============================================================================

(defun zettel-update-metadata-date ()
  "Updates the date in the metadata section of the Zettel."
  (interactive)
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (search-forward-regexp "^$" nil)
      (narrow-to-region (point-min) (point))
      (goto-char (point-min))
      (let ((new-date (format-time-string "%Y-%m-%d"))
            old-date)
        (cond ((save-excursion
                 (re-search-forward (concat "^modified: +" zettel-regexp-date)
                                    nil t))
               (setq old-date (match-string 1))
               (when (and (not (string-equal old-date new-date))
                          (save-match-data
                            (y-or-n-p
                             (format "Saving %s. Update the modified date? "
                                     (file-name-base buffer-file-name)))))
                 (message "Updating metadata modified date in %s from %s to %s."
                          buffer-file-name old-date new-date)
                 (replace-match new-date nil t nil 1)))
              ((re-search-forward (concat "^created: +" zettel-regexp-date)
                                  nil t)
               (setq old-date (match-string 1))
               (when (and (not (string-equal old-date new-date))
                          (y-or-n-p (format "Saving %s. Add modified date? "
                                            (file-name-base buffer-file-name))))
                 (message "Adding metadata modified date in %s (created on %s)."
                          buffer-file-name old-date)
                 (insert (format "\nmodified: %s" new-date)))))))))

(add-hook 'zettel-mode-hook
  '(lambda ()
     (add-hook 'before-save-hook 'zettel-update-metadata-date nil t)))

(eval-after-load "markdown"
  (add-hook 'markdown-mode-hook
    '(lambda ()
       (add-hook 'before-save-hook 'zettel-update-metadata-date nil t))))

;;
;; Insert my zettel title string into new zettel rather than contents of deft's
;; filter string.
;;
;; Warning: Relies on yasnippet expansion of "title"
;;

(defun deft-new-file--add-zettel-title (orig-fun slug)
  "Replaces deft's default behavior of putting the filter string
on the first line with the Zettel title string."
  ;; DEFT-NEW-FILE-NAMED returns either a string (from MESSAGE) about an
  ;; error, or the result of (GOTO-CHAR (POINT-MAX)), which means an integer
  ;; buffer location.
  (when (integerp (funcall orig-fun slug))
    (let ((file (deft-absolute-filename slug)))
      (with-current-buffer (get-file-buffer file)
        (erase-buffer)
        ;; FIXME: Relying on an external yasnippet
        (insert "title")
        (yas-expand)))))

(advice-add 'deft-new-file-named :around #'deft-new-file--add-zettel-title)

;;;=============================================================================
;;; Zettel Links
;;;=============================================================================

(defvar zettel-stored-links '()
  "A stack of links stored with `zettel-store-link'.")
(defvar zettel-stored-links-history '()
  "History of the links stored with `zettel-store-link'.")
(defvar zettel-link-backlink nil
  "Stores the file name of the document into which a link was inserted
with `zettel-insert-link-intrusive' or `zettel-insert-link', allowing
for creation of backlinks.")

(defun zettel-link-slug (target &optional context)
  "Returns the slug of the given TARGET, which might be a
pathname or just the slug itself. If CONTEXT is given and is a
file, returns a slug relative to it, otherwise returns a 'fully
qualified' link."
  (let ((base-slug (file-name-base target))
        (target-dir (file-name-directory target)))
    (cond ((and (stringp context)
                (file-exists-p context)
                (equal target-dir (file-name-directory context)))
           ;; The target and context of the link are in the same location,
           ;; return relative link.
           base-slug)
          (target-dir
           ;; The target is an actual file, return "fully qualified" link with
           ;; the explicit subkasten.
           (let* ((dir (car (last (split-string target-dir "/" t))))
                  (keyval (assoc dir zettel-sub-kasten)))
             (if keyval
                 (concat (car keyval) ":" base-slug)
               base-slug)))
          (t
           base-slug))))

(defun zettel-store-link (arg)
  "Store the link 1) to the Deft file at point if in *Deft*
buffer, or 2) to the file in current buffer. With prefix
argument, stores the link to the markdown wiki link if at a
markdown wiki link."
  (interactive "p")
  (let ((link (cond ((equal major-mode 'deft-mode)
                     (widget-get (widget-at (point)) :tag))
                    ((and (= arg 4) (markdown-wiki-link-p))
                     (zettel-absolute-filename (markdown-wiki-link-link)))
                    (buffer-file-name
                     buffer-file-name)
                    (t
                     (message "No file to store a link to.")))))
    (cond ((string-equal link (first zettel-stored-links))
           (message "Link to %s is already in the stored links: %s"
                    (file-name-base link)
                    (mapcar #'file-name-base zettel-stored-links)))
          (t
           (push link zettel-stored-links)
           (message "Link to %s stored: %s"
                    (file-name-base link)
                    (mapcar #'file-name-base zettel-stored-links))))))

(defun zettel-link (target &optional include-title title-location context)
  "Returns a markdown-formatted link to TARGET, including the
link title when INCLUDE-TITLE is true. If TITLE-LOCATION is RIGHT
or T, puts the title on the right; otherwise, on the left. If
CONTEXT is given, attemps to include the relative form of the
link."
  (let* ((file (or (if (file-exists-p target)
                       target
                     (zettel-convert-link-to-filename target))
                   (error "Link target doesn't exist; make sure it's saved.")))
         (link-text (zettel-link-slug file context)))
    (cond (include-title
           ;; Make sure cache is updated for the linked file
           (deft-cache-file file)
           ;; NOTE: This works on the assumption that there will be at least two
           ;; spaces separating the zettel number from its title, which might be
           ;; affected by `deft-strip-title-regexp' and/or
           ;; `zettel-separate-deft-file-title'.
           (let ((title
                  ;; Strip any {...} type keyword, if any
                  (replace-regexp-in-string
                   "^{.*} " ""
                   (second
                    (split-string (deft-file-title file)
                                  "[[:space:]]\\{2,\\}")))))
             (if (or (null title-location) (eq title-location 'left))
                 (format "%s [[%s]]" title link-text)
               (format "[[%s]] %s" link-text title))))
          (t
           (format "[[%s]]" link-text)))))

(defun zettel-link-with-spaces (file &optional
                                     include-title title-location context)
  "A wrapper around `zettel-link' that returns the link with
appropriate spaces around."
  ;; FIX: Is it okay that assumes it's called from a buffer?
  (cl-flet ((spacep (char)
              "Returns T if the character is some kind of a space."
              (when char
                (string-match-p "[[:space:][:punct:]]" (char-to-string char)))))
    (concat (if (spacep (char-before)) "" " ")
            (zettel-link file include-title title-location context)
            (if (spacep (char-after)) "" " "))))

(defun zettel-insert-link (arg)
  "Insert the top link from `zettel-stored-links'. If called with
prefix argument, insert the link title to the left of the link.
If with double prefix argument, insert the title to the right."
  (interactive "P")
  (cond (zettel-stored-links
         (let ((link (pop zettel-stored-links)))
           ;; Save the link in link history
           (push link zettel-stored-links-history)
           ;; Insert the link
           (insert
            (zettel-link-with-spaces link
                                     (consp arg)
                                     (equal arg '(16))
                                     buffer-file-name))
           ;; Save the backlink
           (setq zettel-link-backlink buffer-file-name)))
        (t
         (message "No link to insert.")
         )))

(defun zettel-insert-link-intrusive (arg)
  "Like `zettel-insert-link', but also opens the Zettel of the
link inserted if it doesn't already have a backlink, and adds the
current Zettel to the `zettel-link-backlink'."
  (interactive "P")
  (when zettel-stored-links
    ;; Make sure the current buffer is saved and cached properly
    (save-buffer)
    (deft-cache-update-file buffer-file-name)
    (let ((link-to-insert (first zettel-stored-links)))
      (zettel-insert-link arg)
      ;; If the linked file doesn't already have a link to the current one,
      ;; opens the linked file in a new window, but does not switch to it.
      (cond ((string-match (regexp-quote (zettel-link buffer-file-name))
                           ;; Try to get the `deft-file-contents', updating the
                           ;; cache if neccessary.
                           (or (deft-file-contents link-to-insert)
                               (progn (deft-cache-update-file link-to-insert)
                                      (deft-file-contents link-to-insert))))
             (message "The linked note %s has a backlink to %s already."
                      (file-name-base link-to-insert)
                      (file-name-base buffer-file-name)))
            (t
             (deft-open-file link-to-insert t t))))))

(defun zettel-insert-backlink (arg)
  "Like `zettel-insert-link', but instead of popping a link from
`zettel-stored-links', inserts the link in
`zettel-link-backlink', if set."
  (interactive "P")
  (cond (zettel-link-backlink
         (insert (zettel-link-with-spaces zettel-link-backlink
                                          (consp arg)
                                          (equal arg '(16))
                                          buffer-file-name))
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

(defun zettel-get-clipboard-data ()
  "Get the text from the OS clipboard no matter the system."
  (string-trim
   (or (x-get-selection 'CLIPBOARD)
       (w32-get-clipboard-data)
       "")))

(defun zettel-insert-link-from-clipboard (arg)
  "Link `zettel-insert-link' but attempts to get the link slug
from OS clipboard."
  (interactive "P")
  (let ((link (zettel-get-clipboard-data)))
    (if (zettel-link-p link)
        (insert
         (zettel-link-with-spaces link
                                  (consp arg)
                                  (equal arg '(16))
                                  buffer-file-name)))))

;; These keybindings shadow Org-mode's global "C-c l" and local "C-c C-l"
(define-key deft-mode-map (kbd "C-c l") 'zettel-store-link)
(define-key zettel-mode-map (kbd "C-c l") 'zettel-store-link)
(define-key zettel-mode-map (kbd "C-c C-l") 'zettel-insert-link-intrusive)
(define-key zettel-mode-map (kbd "C-c C-S-l") 'zettel-insert-link)
(define-key zettel-mode-map (kbd "C-c C-M-S-l") 'zettel-insert-link-from-clipboard)
(define-key zettel-mode-map (kbd "C-c C-M-l") 'zettel-list-links)
(define-key zettel-mode-map (kbd "C-c C-b") 'zettel-insert-backlink)
(define-key zettel-mode-map (kbd "C-c C-r") 'zettel-replace-link-at-point)

;; Allow handling markdown wiki links in org-mode
(defun org-open-at-point--zettel-links (orig-fun &rest args)
  "Around advice for `org-open-at-point' that adds support for
following internal Zettel links."
  (if (markdown-wiki-link-p)
      (markdown-follow-wiki-link-at-point)
    (apply orig-fun args)))
(advice-add 'org-open-at-point :around #'org-open-at-point--zettel-links)

;;;=============================================================================
;;; Renaming
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

(defun zettel-goto-next-missing-link ()
  "Moves the point to the first instance of a missing link based
on its font lock properties."
  (interactive)
  (let* ((from
          (if (and (markdown-wiki-link-p)
                   (not (file-exists-p
                         (markdown-convert-wiki-link-to-filename
                          (markdown-wiki-link-link)))))
              (progn (forward-sexp) (point))
              (point-min)))
         (loc (text-property-any
               from (point-max)
               'font-lock-face 'markdown-missing-link-face)))
    (if loc
        (goto-char loc)
        (message "No missing links"))))

(defun zettel-filter-for-link-at-point ()
  "Modifies the Deft filter to look for the Zettel linked with
the link at point. If there is only one match, opens the note in
another window."
  (interactive)
  (push (buffer-file-name) zettel-stored-links)
  (when (markdown-wiki-link-p)
    (let ((link (markdown-wiki-link-link))
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
backlink. With C-u C-u, simply fixes the [[alias|link]] to put
the alias outside of the link."
  (interactive "P")
  (when (markdown-wiki-link-p)
    (let ((alias (markdown-wiki-link-alias))
          (link  (markdown-wiki-link-link)))
      (save-excursion
        ;; Make sure we are at the start of the link
        (unless (string-match "\\[\\[[^]]+\\]\\]" (thing-at-point 'sexp))
          (re-search-backward "\\[\\["))
        (kill-sexp)
        (cond ((equal arg '(16))
               (unless (string-equal link alias) (insert alias " "))
               (insert (zettel-link-with-spaces link)))
              ((or (equal arg '(4))
                   (not zettel-stored-links))
               (unless (string-equal link alias) (insert alias " "))
               (zettel-insert-backlink nil))
              ((integerp arg)
               (unless (string-equal link alias) (insert alias " "))
               (zettel-insert-link-intrusive arg))
              (t
               (unless (string-equal link alias) (insert alias " "))
               (zettel-insert-link-intrusive nil)))))))

;;;=============================================================================
;;; Bibliography
;;;=============================================================================

(defconst rb-markdown-regex-citation-key
  "\\(\\[\\)\\(#\\)\\([^]]+\\)\\(\\]\\)\\(:.*\\)*"
  "Regular expression for a citation key [#CiteKey].
Group 1 matches the opening square bracket.
Group 2 matches the hash sign.
Group 3 matches the text inside the square brackets.
Group 4 matches the closing square bracket.
Group 5 matches the citation definition (if present).")

(defun rb-markdown-citation-key-p ()
  "Returns non-nil when `point' is at a citation key."
  (let ((case-fold-search nil))
    (and (not (markdown-wiki-link-p))
         (thing-at-point-looking-at rb-markdown-regex-citation-key))))

(defun rb-markdown-citation-key-key ()
  "Returns the citation key of the key at point (relies on match
data from `rb-markdown-regex-citation-key')."
  (when (thing-at-point-looking-at rb-markdown-regex-citation-key)
    (match-string-no-properties 3)))

(defun rb-markdown-follow-citation-key-at-point (arg)
  "Opens the citation key at point in the default bibliography.
With prefix argument ARG, open the file in other window."
  (interactive "P")
  (let ((key (rb-markdown-citation-key-key)))
    (funcall (if (equal arg '(4))
                 #'find-file-other-window
                 #'find-file)
             (first reftex-default-bibliography))
    (bibtex-search-entry key)))

(defun rb-markdown-follow-thing-at-point (orig-fun arg)
  "Around advice for `markdown-follow-thing-at-point' that adds
support for following citations."
  (cond ((markdown-link-p)
         (markdown-follow-link-at-point))
        ((markdown-wiki-link-p)
         (markdown-follow-wiki-link-at-point arg))
        ((rb-markdown-citation-key-p)
         (rb-markdown-follow-citation-key-at-point arg))
        (t
         (error "Nothing to follow at point"))))
(advice-add 'markdown-follow-thing-at-point :around #'rb-markdown-follow-thing-at-point)

(defun zettel-kill-ring-save-link-title ()
  "Save the title of the wiki link at point or the buffer to kill
ring."
  (interactive)
  (let ((file (cond ((markdown-wiki-link-p)
                     (zettel-convert-link-to-filename (markdown-wiki-link-link)))
                    ((zettel-p buffer-file-name)
                     buffer-file-name))))
    (deft-cache-file file)
    (let ((title (deft-file-title file)))
      (cond (title
             (kill-new
              ;; FIXME: Remove the now obsolete unguillemet
              (replace-regexp-in-string
               "[«»]" "_" (second (split-string title "[[:space:]]\\{2,\\}"))))
             (message "Link title to %s saved in kill ring."
                      (file-name-base file)))
            (t
             (message "Could not get the title of %s."
                      (file-name-base file)))))))

(defun zettel-kill-ring-save-link (arg)
  "Save the current link, the deft note at point, or the buffer
base filename in the kill ring to be used as a wiki link
elsewhere. With prefix argument, save the file name relative to
`zettel-directory' instead."
  (interactive "p")
  (let ((link (cond ((equal major-mode 'deft-mode)
                     (widget-get (widget-at (point)) :tag))
                    ((markdown-wiki-link-p)
                     (markdown-wiki-link-link))
                    (buffer-file-name
                     buffer-file-name)
                    (t
                     (message "No file to save a link to.")))))
    (when link
      (let ((link (if (= arg 4)
                      (file-relative-name link zettel-directory)
                    (zettel-link-slug link))))
       (kill-new link)
       (message "Saved [%s] in the kill ring." link)))))

;;;=============================================================================
;;; Wiki Links
;;;=============================================================================

;; This must be enabled to have wiki links
(setq markdown-enable-wiki-links t)

;;
;; By default, `markdown-convert-wiki-link-to-filename' concatenates the file
;; extension of the current buffer's file to the link name when you press C-c
;; C-o over something like [[bib/Key2015.bib]], so it ends up opening
;; Key2015.bib.txt. The markdown-cwltf-fix-link removes the extra extension,
;; among other things.
;;
;; Unfortunately, `markdown-follow-wiki-link' also "ensure[s] that the new
;; buffer remains in `markdown-mode'", so I need yet another work-around to fix
;; that: `markdown-fwl-set-auto-mode'.
;;

(defun markdown-fwl--set-auto-mode (&rest args)
  "After advice for `markdown-follow-wiki-link'. Reverses the
default behavir of ensuring that the buffer is in markdown mode,
and instead sets it back to the mode it `wants to be'."
  (set-auto-mode t))

(advice-add 'markdown-follow-wiki-link :after #'markdown-fwl--set-auto-mode)

(defun zettel-right-directory (numerus-currens)
  "Finds the right directory for the numerus currens provided."
  (when (stringp numerus-currens)
    (let ((result
           (case (elt numerus-currens 0)
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

(defun zettel-convert-link-to-filename (name)
  "Like `markdown-convert-wiki-link-to-filename', but local to
Zettelkasten."
  (cond ((string-match (concat "^" zettel-regexp-numerus-currens) name)
         ;; name is a numerus currens
         (zettel-absolute-filename name))
        ((string-match "[[:alpha:]]+:[[:alnum:]]+" name)
         ;; name is a 'subkasten:zettel' link
         (let* ((split (split-string name ":"))
                (subkasten (first split))
                (name-only (second split))
                (sk-dir (cdr (assoc subkasten zettel-sub-kasten))))
           (when sk-dir
             (expand-file-name (concat name-only "." deft-extension) sk-dir))))
        ((string-match (concat "^" zettel-regexp-date) name)
         ;; name is a datename by itself, assume it's local to `deft-directory'
         (expand-file-name (concat name "." deft-extension) deft-directory))
        (t
         ;; name is something else, return nil
         nil)))

(defun markdown-cwltf--fix-link (orig-fun name)
  "Advice for `markdown-convert-wiki-link-to-filename',
completely overriding the originall functionality. It combines
the not clobbering of extension, finding the right directory
directory for the Zettel so that the links can be found across
multiple directories within the main Zettelkasten, and also
handling 'subkasten:' notation."
  (save-match-data
    (let ((result (or (zettel-convert-link-to-filename name)
                      (funcall orig-fun name))))
      (if (string-match "\\.\\w+$" name)
          (let ((orig-ext (match-string 0 name)))
            (if (string-match (concat orig-ext "\\(\\.\\w+\\)$") result)
                (replace-match orig-ext nil nil result)
                result))
          result))))

(advice-add 'markdown-convert-wiki-link-to-filename
            :around #'markdown-cwltf--fix-link)

(defun zettel-make-word-wiki-link ()
  "Make the current word (including dashes) into a wiki link by
enclosing it in [[]]."
  (interactive)
  (save-excursion
    (let ((table (make-syntax-table)))
      (modify-syntax-entry ?- "w" table)
      (with-syntax-table table
        (multiple-value-bind (start end)
         (let* ((bounds (bounds-of-thing-at-point 'word))
                (start (car bounds))
                (end (cdr bounds))
                (word (buffer-substring-no-properties start end)))
           (delete-region start end)
           (insert (zettel-link-with-spaces word))))))))

;;;=============================================================================
;;; Children, siblings, and ancestors
;;;=============================================================================

(defun zettel-slug (number letters)
  "Returns a Zettel slug composed of the NUMBER and LETTERS, both
of which are strings."
  (concat number "-" letters))

(defun zettel-slug-p (slug)
  "Returns NIL if the slug is not a Zettel slug, and otherwise
returns a list of two elements: the number and letters part of
the slug."
  (when (and (stringp slug)
             (string-match zettel-regexp-numerus-currens slug))
    (list (match-string 1 slug) (match-string 3 slug))))

(defun zettel-slug-number (slug)
  "Returns the number part of the slug."
  (when (string-match zettel-regexp-numerus-currens slug)
    (match-string 1 slug)))

(defun zettel-slug-letters (slug)
  "Returns the letters part of the slug as a string."
  (when (string-match zettel-regexp-numerus-currens slug)
    (match-string 3 slug)))

(defun zettel-split-slug (slug)
  "Returns the slug in its split form as a vector: 234-abc ->
#('234' 'a' 'b''c')."
  (when (zettel-slug-p slug))
  (when (string-match zettel-regexp-numerus-currens slug)
    (let ((number (match-string 1 slug))
          (letters (split-string (match-string 3 slug) "" t)))
      (apply #'vector number letters))))

;; TODO: Tired, needs a rewrite.
(defun zettel-slug-ancestor (slug &optional n)
  "Returns the slug's ancestor, or NIL if could not figure out.
With the optional N, try to find the Nth ancestor (i.e.
grandparent if N is 2, an so on), returning the most remote
ancestor that could find."
  (let ((n (if (integerp n) (abs n) 1)))
   (multiple-value-bind (number letters)
       (zettel-slug-p slug)
     (when number
       (cond ((zerop (length letters)) nil)
             ((<= (length letters) n) number)
             (t
              (concat number "-" (substring letters 0 (- n)))))))))

(defun zettel-slug-children (slug)
  "Returns a list of the slugs of Zettel's children. If the Zettel doesn't
have any children, returns NIL."
  (multiple-value-bind (number letters)
      (zettel-slug-p slug)
    (let ((children-regex (format "%s-%s[a-z]$" number (or letters ""))))
      (sort
       (mapcar #'file-name-base
               (remove-if-not #'(lambda (x) (string-match children-regex x))
                              deft-all-files
                              :key #'file-name-base))
       #'string-lessp))))

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
    (dolist (child (zettel-slug-children slug))
      (beginning-of-line)
      (insert "* ")
      (insert (zettel-link-with-spaces (zettel-absolute-filename child)
                                       t
                                       arg
                                       buffer-file-name))
      (newline))))

(defun zettel-slug-siblings (slug)
  "Returns a list of the Zettel's siblings, inclusive. If the
Zettel is a top-level one, or it doesn't have any siblings,
returns NIL."
  (let ((parent (zettel-slug-ancestor slug)))
    (when parent
      (zettel-slug-children parent))))

(defun zettel-find-ancestor (n)
  "Opens the current Zettel's ancestor. With a prefix argument, try
to find the Nth ancestor."
  (interactive "p")
  (when (zettel-p buffer-file-name)
    (let ((ancestor (zettel-slug-ancestor (file-name-base buffer-file-name) n)))
      (when ancestor
        (find-file
         (zettel-absolute-filename ancestor))))))

(defun zettel-insert-ancestor-link (arg)
  "Insert a link to the ancestor of the current zettel. With a
numerical prefix argument, try to find Nth ancestor. With
universal argument, behave like `zettel-insert-link'."
  (interactive "P")
  (when (zettel-p buffer-file-name)
    (let ((link (zettel-slug-ancestor (file-name-base buffer-file-name)
                                      (if (integerp arg) arg 1))))
      (when link
        (insert
         (zettel-link-with-spaces link
                                  (consp arg)
                                  (equal arg '(16))
                                  buffer-file-name))))))

(defcustom zettel-loop-siblings t
  "When T, commands `zettel-next-sibling' and
`zettel-prev-sibling' will loop over the list of siblings.")

(defun zettel-next-sibling (&optional n)
  "Opens the current Zettel's next Nth sibling (if any).

If `zettel-loop-siblings' is set to T, treats the list of
siblings as a loop."
  (interactive "p")
  (when (zettel-p buffer-file-name)
    (let* ((slug (file-name-base buffer-file-name))
           (siblings (zettel-slug-siblings slug))
           (length (length siblings)))
      (if (> length 1)
          (let* ((pos (position slug siblings :test #'string-equal))
                 (n (or n 1))
                 (next-sibling-pos
                  (cond ((< -1 (+ n pos) length)
                         (+ n pos))
                        (zettel-loop-siblings
                         (mod (+ n pos) length))
                        (t
                         nil)))
                 (next-sibling (when next-sibling-pos
                                 (elt siblings next-sibling-pos))))
            (if next-sibling
                (find-file (zettel-absolute-filename next-sibling))
                (message "No other siblings.")))
          (message "This Zettel has no siblings.")))))

(defun zettel-prev-sibling (&optional n)
  "Opens the current Zettel's previous Nth sibling (if any).

If `zettel-loop-siblings' is set to T, treats the list of
siblings as a loop."
  (interactive "p")
  (zettel-next-sibling (- (abs (or n 1)))))

(defun zettel-choose-sibling ()
  "Interactively choose which sibling to visit."
  (interactive)
  (when (zettel-p buffer-file-name)
    (find-file
     (deft-absolute-filename
         (completing-read "Choose sibling: "
                          (zettel-slug-siblings
                           (file-name-base buffer-file-name)))))))

(defun zettel-slug-first-child (slug)
  "Returns a new slug that would be a first child of the given one."
  (multiple-value-bind (number letters)
      (zettel-slug-p slug)
    (cond (letters (zettel-slug number (concat letters "a")))
          (number (zettel-slug number "a"))
          (t nil))))

(defun zettel-slug-numeric (slug)
  "Converts the given alphanumer slug into its numeric version (a list)."
  (multiple-value-bind (number letters)
      (zettel-slug-p slug)
    (when number
      (append (list (string-to-number number))
              (mapcar #'(lambda (c) (+ 1 (- c ?a)))
                      (coerce letters 'list))))))

(defun zettel-slug-alphanumeric (numeric-slug)
  "Converts the given numeric slug into its alphanumeric version (a string)."
  (zettel-slug (number-to-string (first numeric-slug))
               (coerce (mapcar #'(lambda (num)
                                   (+ ?a (- num 1)))
                               (rest numeric-slug))
                       'string)))

(defun zettel-slug-new-sibling (slug)
  "Returns the next unused sibling of the given slug, or NIL if
there are none. Respects `zettel-new-child-method'."
  )

(defun zettel-slug-new-child (slug)
  "Generate a new available child of the given SLUG, respecting
`zettel-new-child-method'."
  (multiple-value-bind (number letters)
      (zettel-slug-p slug)
    (when number
      (let* ((alphabet (coerce "abcdefghijklmnoprqstuvxy" 'list))
             ;; 'z' is left out as reserved letter
             (used-letters (mapcar #'(lambda (s)
                                       (string-to-char
                                        (substring
                                         (zettel-slug-letters s) -1)))
                                   (zettel-slug-children slug)))
             (unused-letters
              (sort (set-difference alphabet used-letters) #'<)))
        (if unused-letters
            (case zettel-new-child-method
              (random
               (zettel-snc--pronounceable number letters unused-letters))
              (next
               (setf (elt letters (- (length letters) 1))
                     (first unused-letters))))
          ;; If no more unused letters, try going into the "z" extension.
          (zettel-slug-new-child
           (zettel-slug (zettel-slug-number slug)
                        (concat (zettel-slug-letters slug) "z"))))))))

(defun zettel-snc--pronounceable (number letters unused-letters)
  "Helper function for `zettel-slug-new-child', which tries to
generate a pronounceable new random child of the slug (NUMBERS +
LETTERS) among UNUSED-LETTERS."
  (cl-flet ((random-elt (sequence)
              "Return a random element of SEQUENCE."
              (when (sequencep sequence)
                (elt sequence (random (length sequence))))))
    (let ((random-letter (random-elt unused-letters))
          (attempts '()))
      (while (and (not (slug-pronounceable-p letters random-letter))
                  (<= (length attempts) 10))
        (push (setq random-letter (random-elt unused-letters)) attempts))
      (message "Attempted letters: %s" (mapcar #'char-to-string
                                               (nreverse attempts)))
      (zettel-slug number
                   (concat letters (char-to-string random-letter))))))

(defun zettel-insert-new-child (arg)
  "Creates a new Zettel at the next branching level of the
current one, inserting a link to it at point. If called with a
prefix argument, ask for the slug. With double prefix argument,
show the child instead on inserting."
  (interactive "p")
  (cond ((string-match (concat "^" zettel-regexp-date)
                       (file-name-base buffer-file-name))
         (insert (zettel-link-with-spaces (zettel-timestamp-slug))))
        ((string-match (concat "^" zettel-regexp-numerus-currens)
                       (file-name-base buffer-file-name))
         (message "Updating cache...")
         (deft-cache-update-all)
         (message "Generating a new child...")
         (let* ((slug (cond ((= arg 4)
                             (read-string "Slug: " nil))
                            ((zettel-p buffer-file-name)
                             (file-name-base buffer-file-name))
                            ((equal major-mode 'deft-mode)
                             (file-name-base
                              (widget-get (widget-at (point)) :tag)))
                            (t
                             (read-string "Slug: " nil))))
                (new-child-slug (when slug (zettel-slug-new-child slug))))
           (cond ((not slug)
                  (message "Could not figure out which zettel to find a child for."))
                 ((not new-child-slug)
                  (message "There are no unused children for %s." slug))
                 ((or (= arg 16) (equal major-mode 'deft-mode))
                  ;; If invoked from deft buffer or with C-u C-u, just
                  ;; show child
                  (message "New child: %s" new-child-slug)
                  (kill-new new-child-slug))
                 (t
                  (insert (zettel-link-with-spaces new-child-slug))))))))

(defun zettel-new-sibling ()
  "Creates a new Zettel at the same level as current one."
  )

(defun zettel-find-numerus-currens (slug)
  "Finds the Zettel with the numerus currens specified
interactively by the user."
  (interactive "sNumerus currens to find: ")
  (find-file (zettel-absolute-filename slug)))

;;;=============================================================================
;;; Bookmarks
;;;=============================================================================

(defun bookmark-make-record-zettel ()
  "Bookmark record function for Zettel bookmarks, setting the
bookmark's filename property to the Zettel link."
  (list (cons 'filename (concat "zettel:" (zettel-link-slug buffer-file-name)))
        (cons 'handler 'bookmark-zettel-handler)))

(defun bookmark-zettel-handler (bmk-record)
  "Bookmark record handler for Zettel bookmarks."
  (find-file
   (zettel-convert-link-to-filename
    (replace-regexp-in-string "^zettel:" ""
                              (cdr (assoc 'filename bmk-record))))))

;; Use the special zettel bookmark handler in Zettel buffers
(add-hook 'zettel-mode-hook
  (lambda ()
    (setq-local bookmark-make-record-function 'bookmark-make-record-zettel)))

;;;=============================================================================
;;; Switch to Buffer
;;;=============================================================================

(defun zettel-switch-to-buffer ()
  "Quickly switch to other open Zettel buffer via `helm'."
  (interactive)
  (let ((buffers
         (mapcar #'(lambda (buf)
                     (cons (deft-file-title (buffer-file-name buf)) buf))
                 (remove-if-not #'(lambda (buf)
                                    (zettel-p (buffer-file-name buf)))
                                (buffer-list)))))
    (if buffers
        (ivy-read "Switch to Zettel buffer: "
                  buffers
                  :action (lambda (choice)
                            (switch-to-buffer (cdr choice))))
      (user-error "No open Zettel buffers to switch to"))))

;;;-----------------------------------------------------------------------------
;;; Key Bindings
;;;
 ;; According to key binding conventions, the only bindings reserved for minor
 ;; modes are "Sequences consisting of C-c followed by any other punctuation
 ;; character" than {, }, <, >, : or ;, which are reserved for major modes.
;;;-----------------------------------------------------------------------------

(define-key zettel-mode-map (kbd "C-c #") 'zettel-match-title-to-filename)

(define-key zettel-mode-map (kbd "C-c ^") 'zettel-find-ancestor)
(define-key zettel-mode-map (kbd "C-c @") 'zettel-insert-ancestor-link)
(define-key zettel-mode-map (kbd "C-c .") 'zettel-insert-new-child)

(define-key zettel-mode-map (kbd "C-c %") 'zettel-goto-next-missing-link)
(define-key zettel-mode-map (kbd "C-c `") 'zettel-filter-for-link-at-point)
;;(define-key zettel-mode-map (kbd "C-c *") 'zettel-make-word-wiki-link)
(define-key zettel-mode-map (kbd "C-c ~") 'zettel-kill-ring-save-link-title)

(define-key zettel-mode-map (kbd "C-c !") 'zettel-kill-ring-save-link)
(define-key deft-mode-map (kbd "C-c !") 'zettel-kill-ring-save-link)

(define-key deft-mode-map (kbd "C-c '") 'zettel-filter-add-subtree)
;; Was: deft-find-file
(define-key deft-mode-map (kbd "C-c C-f") 'zettel-find-numerus-currens)
(define-key zettel-mode-map (kbd "C-c \"") 'zettel-find-numerus-currens)

;; Set the citation key in `rb-reftex-last-citation'.
(eval-after-load "markdown"
  '(define-key markdown-mode-map (kbd "C-c |")
    (cmd
      (when (rb-markdown-citation-key-p)
        (push (rb-markdown-citation-key-key) rb-reftex-last-citation)))))

(provide 'zettel)
