;;;; -*- mode: lisp -*-
;;;; -----------------------------------------------------------------------------
;;;;        Author: Roderick Hoybach <hoybach-code@diachronic.net>
;;;;   Description: Zettelkasten implementation based on Deft and Markdown
;;;;  Date Created: 31 August 2015, 21:01:43
;;;;      Comments: 
;;;; -----------------------------------------------------------------------------

;; Cretea a keymap for the mode
(defvar zettel-mode-map (make-sparse-keymap)
  "Keymap for Zettel mode.")

;; Define a minor mode for working with Zettelkasten in deft
(define-minor-mode zettel-mode
  "Make the keymap zettel-mode-map active."
  :lighter " Zettel"
  :keymap zettel-mode-map
  :require 'deft)

(defvar zettel-directory (in-home-dir "doc/zettel/")
  "My central Zettelkasten directory.")
(defvar zettel-filename-format '("%03d" "%c" "%c" "%c" "%c" "%c" "%c")
  "A list of elements of the Zettel filename as FORMAT control
  sequences.")
(defvar zettel-base-format (first zettel-filename-format)
  "The format of the base numerical component of Zettel's name.")

(defun zettel-p (file)
  "Returns non-NIL if the file is a Zettel."
  (interactive "f")
  (or (string-match zettel-filename-regexp (file-name-base file))
      (string-match zettel-date-regexp (file-name-base file))))

;; Enable zettel-mode for files that match the pattern
(add-hook 'markdown-mode-hook
  '(lambda ()
     (when (zettel-p buffer-file-name)
       (zettel-mode 1))))
(add-hook 'org-mode-hook
  '(lambda ()
     (when (zettel-p buffer-file-name)
       (zettel-mode 1))))

;; Enable wc-mode (shows word count in the mode line) in Zettel files
(require 'wc-mode)
(add-hook 'zettel-mode-hook
  (lambda ()
    (wc-mode 1)))

;;-----------------------------------------------------------------------------
;; How zettel are displayed in the Deft buffer
;;-----------------------------------------------------------------------------

;; Default: "\\(?:^%+\\|^[#* ]+\\|-\\*-[[:alpha:]]+-\\*-\\|#+$\\)"
(setq deft-strip-title-regexp "\\(\\+.*$\\|^\\(title: +\\)\\)")
(setq deft-use-filename-as-title nil)

(defvar zettel-filename-regexp
  "\\([0-9]\\{3\\}\\)\\(-\\([a-z]+\\)\\)*"
  "The regular expression that matches filenames like 261-cabf.")

(defvar zettel-date-regexp
  "\\([0-9-]\\{8,10\\}\\(T*[0-9:]\\{4,5\\}\\)*\\)"
  "The regular expression that matches ISO 8601-like date/time expression.")

(defvar zettel-indent-title-column 12
  "The column number where the Zettel title (without the numerus
currens) will be indented to.")

(defun zettel-separate-deft-file-title (orig-fun &rest args)
  "Replace the first slash of with enough spaces to justify the actual title."
  (let* ((title (apply orig-fun args))
         (numerus-length (if (string-match zettel-filename-regexp title)
                             (match-end 0)
                             zettel-indent-title-column)))
    ;; Strip the § before the numerus currens
    (setq title (replace-regexp-in-string "§" "" title))
    ;; Replace the ". " in the first title (following § + numerus currens) with indentation
    (replace-regexp-in-string
     "\\(\\. \\).*\\'"
     (make-string (max 0 (- zettel-indent-title-column numerus-length)) ?\s)
     title nil nil 1)))

(advice-add 'deft-file-title :around #'zettel-separate-deft-file-title)

(defun zettel-filter-add-subtree (&optional arg)
  "Adds the subtree where the point rests to the filter string,
updating the filter. If called with universal argument, replace
the filter string with the subtree."
  (interactive "p")
  (when (string-equal (buffer-name) "*Deft*")
    (let ((subtree (file-name-sans-extension
                    (file-name-nondirectory
                     (widget-get (widget-at (point)) :tag)))))
      (if (= arg 4)
          (setq deft-filter-regexp (list subtree))
          (push subtree deft-filter-regexp))
      (deft-refresh-filter))))

(define-key deft-mode-map (kbd "C-c C-f") 'zettel-filter-add-subtree) ; was: deft-find-file

(defun deft-add-section-sign-to-filter-increment ()
  "Inserts the Unicode section sign: §."
  (interactive)
  (setq last-command-event 167)
  (deft-filter-increment))

(define-key deft-mode-map (kbd "C-c s") 'deft-add-section-sign-to-filter-increment)

;;-----------------------------------------------------------------------------
;; Find the next unused slug in the entire zettel tree
;;-----------------------------------------------------------------------------

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

(defun zettel-unused-slug ()
  "Returns the next unused slug for the Zettelkasten in ZETTEL-DIRECTORY.

Limitation: Only regards saved files. An unsaved buffers are not
taken into account."
  (save-excursion
    (let ((buffer (get-buffer-create "*Zettel Unused Slug*")))
      (with-current-buffer buffer
        ;; Populate the buffer with results of the find
        (cd zettel-directory)
        (call-process "/usr/bin/find" nil t nil "-L" "-name" "[0-9][0-9][0-9]*.txt")
        ;; Replace './001.txt' or './dir/001-acc.txt' with '001'
        (goto-char (point-min))
        (while (re-search-forward "^.*/\\([0-9]\\{3\\}\\).*\.txt" nil t)
          (replace-match "\\1"))
        ;; Remove any non-numeric text files, since would screw things up
        (goto-char (point-min))
        (while (re-search-forward "^[^0-9]+$" nil t)
          (delete-region (line-beginning-position)
                         (save-excursion
                           (forward-line 1)
                           (point))))
        ;; Sort the buffer
        (sort-lines nil (point-min) (point-max))
        ;; Find the next slug number by processing the list of slugs we created
        ;; into a list of numbers, and then finding either a "hole" or the next
        ;; unused number
        (let ((numbers
               (apply #'vector
                      (mapcar #'string-to-number
                              (delete-dups (split-string (buffer-string))))))
              (j 0))
          ;; Don't need the buffer any more
          (kill-buffer buffer)
          ;; Since the vector is sorted and only has unique elements, we can find
          ;; any holes simply by making sure the element is the same as the index.
          (while (and (< j (length numbers))
                  (= j (aref numbers j)))
            (setq j (1+ j)))
          (format zettel-base-format j))))))

(defun zettel-timestamp-slug ()
  "Returns a timestamp in the form YYYYMMDDTHHmm to use as the slug."
  (format-time-string "%Y%m%dT%H%M"))

;; "Shadow" the built-in slug generator to generate timestamps by default,
;; i.e. when DEFT-NEW-FILE is called (C-c C-n)
(eval-after-load "deft"
  '(defalias 'deft-unused-slug 'zettel-timestamp-slug))

;; C-c C-S-n, on the other hand, will create a new Zettel with unused numerus currens
(define-key deft-mode-map (kbd "C-c C-S-n")
  (commandify
   (deft-new-file-named (zettel-unused-slug))))

(defun zettel-rename-with-unused-slug ()
  "Rename the current file and buffer to an unused filename
slug (short name) in DEFT-DIRECTORY with DEFT-EXTENSION. Based on
RENAME-FILE-AND-BUFFER."
  (interactive)
  (rename-file-and-buffer (concat (zettel-unused-slug) "." deft-extension)))

;;
;; Automatically update the date in the title line
;;
;; (eval-after-load "markdown"
;;   (add-hook 'markdown-mode-hook
;;     '(lambda ()
;;        (add-hook 'before-save-hook 'zettel-update-metadata-date))))

(defun zettel-update-metadata-date ()
  "Updates the date in the metadata section of the Zettel."
  (interactive)
  (when zettel-mode
    (save-excursion
      (save-restriction
        (widen)
        (goto-char (point-min))
        (forward-line 4)
        (narrow-to-region (point-min) (point))
        (goto-char (point-min))
        (let ((new-date (format-time-string "%Y-%m-%d"))
              old-date)
          (cond ((save-excursion (re-search-forward (concat "^modified: +" zettel-date-regexp) nil t))
                 (setq old-date (match-string 1))
                 (when (and (not (string-equal old-date new-date))
                            (y-or-n-p "Update the modified date? "))
                   (message "Updating metadata modified date in %s from %s to %s."
                            buffer-file-name old-date new-date)
                   (replace-match (format-time-string "%Y-%m-%d") nil t nil 1)))
                ((re-search-forward (concat "^created: +" zettel-date-regexp) nil t)
                 (setq old-date (match-string 1))
                 (when (and (not (string-equal old-date new-date))
                            (y-or-n-p "Add modified date? "))
                   (message "Adding metadata modified date in %s (created on %s)."
                            buffer-file-name old-date)
                   (insert (format "\nmodified: %s" new-date))))
                (t
                 (message "Tried updating metadata in %s, but no created or modified date found."
                          buffer-file-name))))))))

(add-hook 'zettel-mode-hook
  '(lambda ()
     (add-hook 'before-save-hook 'zettel-update-metadata-date nil t)))

;;
;; Insert my zettel title string into new zettel rather than contents of deft's
;; filter string.
;;
;; Warning: Relies on yasnippet expansion of "title"
;;

(defun zettel-title-in-deft-new-file (orig-fun slug)
  "Replaces deft's default behavior of putting the filter string
on the first line with the Zettel title string."
  ;; DEFT-NEW-FILE-NAMED returns either a string (from MESSAGE) about an
  ;; error, or the result of (GOTO-CHAR (POINT-MAX)), which means an integer
  ;; buffer location.
  (when (integerp (funcall orig-fun slug))
    (let ((file (deft-absolute-filename slug)))
      (with-current-buffer (get-file-buffer file)
        (erase-buffer)
        (insert "title")
        (yas-expand)))))

(advice-add 'deft-new-file-named :around #'zettel-title-in-deft-new-file)

;;-----------------------------------------------------------------------------
;; Storing zettel links
;;-----------------------------------------------------------------------------

(defvar zettel-stored-links '()
  "A stack of links stored with ZETTEL-STORE-LINK.")
(defvar zettel-stored-links-history '()
  "History of the links stored with ZETTEL-STORE-LINK.")
(defvar zettel-link-inserted-into nil
  "Stores the slug of the document into which a link was inserted
with ZETTEL-INSERT-LINK-INTRUSIVE or ZETTEL-INSERT-LINK, allowing
for creation of backlinks.")

(defun zettel-store-link ()
  "Store the link to the given Zettel, also putting it into the
window system's clipboard."
  (interactive)
  (let ((link (deft-base-filename
                  (if (string-equal (buffer-name) "*Deft*")
                      (widget-get (widget-at (point)) :tag)
                      (if buffer-file-name
                          buffer-file-name
                          (message "No file to store a link to."))))))
    (x-set-selection 'clipboard link)
    (push link zettel-stored-links)))

;; Could ask for description with C-u
(defun zettel-insert-link (arg)
  "Insert the top link from ZETTEL-STORED-LINKS. If called with
prefix argument, inserts the last link."
  (interactive "P")
  ;; Convert C-u prefix arguments to list element numbers
  (when (and arg (listp arg))
    (case (first arg)
      (4  (setq arg 1))
      (16 (setq arg 2))))
  (cond ((and (integerp arg) (< 0 arg (length zettel-stored-links-history)))
         (insert (concat "[[" (nth (1- arg) zettel-stored-links-history) "]]")))
        (zettel-stored-links
         ;; Save the link in link history
         (push (first zettel-stored-links) zettel-stored-links-history)
         ;; Save the current file's slug for possible backlinking
         (setq zettel-link-inserted-into (deft-base-filename buffer-file-name))
         (insert
          (concat "[[" (pop zettel-stored-links) "]]")))))

(defun zettel-insert-link-intrusive (arg)
  "Like ZETTEL-INSERT-LINK, but also opens the Zettel of the link
inserted, and adds the current Zettel to the ZETTEL-STORED-LINKS
for easy back-linking."
  (interactive "P")
  (when zettel-stored-links
    (let ((link-to-insert (first zettel-stored-links)))
      (zettel-insert-link arg)
      ;; (zettel-store-link)
      (deft-open-file (deft-absolute-filename link-to-insert) t t))))

(defun zettel-insert-backlink (arg)
  "Like ZETTEL-INSERT-LINK, but instead of popping a link from
ZETTEL-STORED-LINKS, inserts the link in ZETTEL-INSERTED-LINK-INTO,
if set."
  (interactive "P")
  (cond (zettel-link-inserted-into
         (insert (concat "[[" zettel-link-inserted-into "]]"))
         (setq zettel-link-inserted-into nil))
        (t
         (message "No backlink to insert."))))

;; These keybindings shadow Org-mode's global "C-c l" and local "C-c C-l"
(define-key deft-mode-map (kbd "C-c l") 'zettel-store-link)
(define-key zettel-mode-map (kbd "C-c l") 'zettel-store-link)
(define-key zettel-mode-map (kbd "C-c C-l") 'zettel-insert-link-intrusive)
(define-key zettel-mode-map (kbd "C-c C-b") 'zettel-insert-backlink)

;;-----------------------------------------------------------------------------
;; Renaming
;;-----------------------------------------------------------------------------

(defun zettel-rename-and-update-title ()
  "Using most of the code from deft.el's DEFT-RENAME-FILE."
  (interactive)
  (let (old-filename new-filename old-name new-name)
    (setq old-filename (widget-get (widget-at) :tag))
    (when old-filename
      (setq old-name (deft-base-filename old-filename))
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
        (when (re-search-forward
               "title: +§\\([0-9]\\{3\\}[a-z-]*\\|[0-9]\\{8\\}T[0-9]\\{4\\}\\)\\.")
          (setq oldname (match-string 1))
          (replace-match (deft-base-filename buffer-file-name) t nil nil 1)
          (forward-paragraph)
          (forward-line 1)
          (insert (concat "oldname: " oldname))
          (newline))))))

;;-----------------------------------------------------------------------------
;; Bibliography
;;-----------------------------------------------------------------------------

(defvar zettel-bibliography-directory
  (concat deft-base-directory "bib/")
  "Directory where individual bibliographic entries are stored.")

(defvar zettel-bibliography-extension
  ".bib"
  "The extension appended to the key when creating a new entry.")

(defun zettel-edit-bibliographic-entry (bib-key)
  "Visists or creates a file in ZETTEL-BIBLIOGRAPHY-DIRECTORY
named after the given bibliographic key."
  (interactive
   (list (read-string "Create bibliography file for key: ")))
  (let ((expanded
         (expand-file-name (concat bib-key zettel-bibliography-extension) zettel-bibliography-directory)))
    (find-file-other-window expanded)))

;; Keybindings
(define-key deft-mode-map (kbd "C-c C-S-b") 'zettel-edit-bibliographic-entry)
(define-key zettel-mode-map (kbd "C-c C-S-b") 'zettel-edit-bibliographic-entry)

;;
;; By default, MARKDOWN-CONVERT-WIKI-LINK-TO-FILENAME concatenates the file
;; extension of the current buffer's file to the link name when you press C-c
;; C-o over something like [[bib/Key2015.bib]], so it ends up opening
;; Key2015.bib.txt. This work-around removes the extra extension.
;;
;; Unfortunately, MARKDOWN-FOLLOW-WIKI-LINK also "ensure[s] that the new buffer
;; remains in `markdown-mode'", so I need yet another work-around to fix that.
;;

(defun markdown-cwltf-dont-clobber-extension (orig-fun name)
  "If NAME already has an extension, don't clobber it by adding
the current buffer file's extension."
  (let ((result (funcall orig-fun name)))
    (save-match-data
      (if (string-match "\\.\\w+$" name)
          (let ((orig-ext (match-string 0 name)))
            (if (string-match (concat orig-ext "\\(\\.\\w+\\)$") result)
                (replace-match orig-ext nil nil result)
                result))
          result))))

(advice-add 'markdown-convert-wiki-link-to-filename
            :around #'markdown-cwltf-dont-clobber-extension)

(defun markdown-fwl-set-auto-mode (&rest args)
  "Reverses MARKDOWN-FOLLOW-WIKI-LINK's ensuring that the buffer
is in markdown mode, and instead sets it back to the mode it
`wants to be'."
  (set-auto-mode t))

(advice-add 'markdown-follow-wiki-link :after #'markdown-fwl-set-auto-mode)

;;-----------------------------------------------------------------------------
;; Moving between Zettel
;;-----------------------------------------------------------------------------

(defun zettel-slug (number letters)
  "Returns a Zettel slug composed of the NUMBER and LETTERS, both
of which are strings."
  (concat number "-" letters))

(defun zettel-slug-p (slug)
  "Returns NIL if the slug is not a Zettel slug, and otherwise
returns a list of two elements: the number and letters part of
the slug."
  (when (string-match zettel-filename-regexp slug)
    (list (match-string 1 slug) (match-string 3 slug))))

(defun zettel-slug-number (slug)
  "Returns the number part of the slug."
  (when (string-match zettel-filename-regexp slug)
    (match-string 1 slug)))

(defun zettel-slug-letters (slug)
  "Returns the letters part of the slug as a string."
  (when (string-match zettel-filename-regexp slug)
    (match-string 3 slug)))

(defun zettel-split-slug (slug)
  "Returns the slug in its split form as a vector: 234-abc ->
#('234' 'a' 'b''c')."
  (when (zettel-slug-p slug))
  (when (string-match zettel-filename-regexp slug)
    (let ((number (match-string 1 slug))
          (letters (split-string (match-string 3 slug) "" t)))
      (apply #'vector number letters))))

;; TODO: Tired, needs a rewrite.
(defun zettel-slug-parent (slug)
  "Returns the slug's parent, or NIL if could not figure out."
  (multiple-value-bind (number letters)
      (zettel-slug-p slug)
    (when number
      (case (length letters)
        (0 nil)
        (1 number)
        (t (concat number "-" (substring letters 0 -1)))))))

(defun zettel-slug-siblings (slug &optional exclusive)
  "Returns a list of the Zettel's siblings, inclusive. If the
Zettel is a top-level one, returns NIL.

If EXCLUSIVE is T, don't include the Zettel itself."
  (multiple-value-bind (number letters)
      (zettel-slug-p slug)
    (when letters
      (let ((sibling-regex
             (if exclusive
                 (format "%s-%s[^%s]$" number (substring letters 0 -1) (substring letters -1))
                 (format "%s-%s[a-z]$" number (substring letters 0 -1)))))
        (cond (letters
               (sort (mapcar #'deft-base-filename
                             (remove-if-not #'(lambda (x) (string-match sibling-regex x))
                                            deft-all-files
                                            :key #'deft-base-filename))
                     #'string-lessp)))))))

(defun zettel-find-parent ()
  "Opens the current Zettel's parent."
  (interactive)
  (when (zettel-p buffer-file-name)
    (let ((parent (zettel-slug-parent (deft-base-filename buffer-file-name))))
      (when parent
        (find-file (deft-absolute-filename parent))))))

(defcustom zettel-loop-siblings t
  "When T, commands 'zettel-next-sibling' and
'zettel-prev-sibling' will loop over the list of siblings.")

(defun zettel-next-sibling (&optional n)
  "Opens the current Zettel's next Nth sibling (if any).

If 'zettel-loop-siblings' is set to T, treats the list of
siblings as a loop."
  (interactive "p")
  (when (zettel-p buffer-file-name)
    (let* ((slug (deft-base-filename buffer-file-name))
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
            ;; (message "POS: %d, N: %d, NEXT: %d, NS: %s" pos n next-sibling-pos next-sibling)
            (if next-sibling
                (find-file (deft-absolute-filename next-sibling))
                (message "No other siblings.")))
          (message "This Zettel has no siblings.")))))

(defun zettel-prev-sibling (&optional n)
  "Opens the current Zettel's previous Nth sibling (if any).

If 'zettel-loop-siblings' is set to T, treats the list of
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
                           (deft-base-filename buffer-file-name)))))))

(defun zettel-slug-first-child (slug)
  "Returns a new slug that would be a first child of the given one."
  (multiple-value-bind (number letters)
      (zettel-slug-p slug)
    (cond (letters (zettel-slug number (concat letters "a")))
          (number (zettel-slug number "a"))
          (t nil))))

(defun zettel-slug-next-unused-sibling (slug)
  "Returns the first unused sibling of the given slug."
  (multiple-value-bind (number letters)
      (zettel-slug-p slug)
    (when letters
      (let* ((alphabet (coerce "abcdefghijklmnoprqstuvxyz" 'list))
             (next-letter
              (first (sort
                      (set-difference alphabet
                                      (mapcar #'(lambda (s)
                                                  (let ((sl (zettel-slug-letters s)))
                                                    (elt sl (- (length sl) 1))))
                                              (zettel-slug-siblings slug)))
                      #'<))))
        (setf (elt letters (- (length letters) 1)) next-letter)
        (zettel-slug number letters)))))

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

(defun zettel-new-child (arg)
  "Creates a new Zettel at the next branching level of the
current one, inserting a link to it at point unless universal
argument is given."
  (interactive "p")
  (when (zettel-p buffer-file-name)
    (let* ((slug (deft-base-filename buffer-file-name))
           (new-child-slug (zettel-slug-next-unused-sibling
                            (zettel-slug-first-child slug))))
      (zettel-store-link)
      (find-file (deft-absolute-filename new-child-slug)))))

(defun zettel-new-sibling ()
  "Creates a new Zettel at the same level as current one."
  )

(define-key zettel-mode-map (kbd "C-c ^") 'zettel-find-parent)
(define-key zettel-mode-map (kbd "C-c C-n") 'zettel-next-sibling)
(define-key zettel-mode-map (kbd "C-c C-p") 'zettel-prev-sibling)

(provide 'zettel)
