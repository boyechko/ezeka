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

(require 'ezeka-base)
(require 'ezeka-file)
(require 'ezeka-syslog)
(require 'ezeka-meta)
(require 'ezeka-compose)

;;;=============================================================================
;;; User Interface
;;;=============================================================================

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

(defcustom ezeka-save-after-metadata-updates 'confirm
  "Whether to automatically save the file after modification.
Functions affected are `ezeka-set-label', `ezeka-set-citekey', and
`ezeka-set-title-or-caption'."
  :type 'symbol
  :options '(nil t confirm)
  :group 'ezeka)

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

(defun ezeka--select-file (files &optional prompt require-match initial-input)
  "Select from among Zettel FILES, presenting optional PROMPT.
If REQUIRE-MATCH is non-nil, require match, otherwise treat
entered text as a Zettel link. INITIAL-INPUT is passed to
`completing-read'."
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
                                (and require-match) ; `completing-read' needs `t'
                                initial-input)
               table))))

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
    (cond ((or (eq t ezeka-number-of-frames)
               (and (eq 'two ezeka-number-of-frames)
                    (< (length (frame-list)) 2)))
           (find-file-other-frame truename))
          ((and (eq 'one ezeka-number-of-frames)
                (not same-window))
           (let ((pop-up-windows t))
             (if (featurep 'ace-window)
                 (ace-select-window)
               (other-window 1))
             (find-file truename)))
          (t (find-file truename)))))

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
        (interactive
         (ezeka--select-file (ezeka--directory-files (ezeka--read-kasten))))))

(defun ezeka--update-symbolic-link (linkname &optional kasten new-name)
  "Update the symbolic link LINKNAME, selecting a new target for it.
Unless KASTEN is specified, default to the same Kasten as the
original target; if not a string (or called with \\[universal-argument]), select
interactively. If NEW-NAME is a string, update the name of the link;
if not a string (or with \\[universal-argument] \\[universal-argument]), ask for a new name interactively."
  (interactive
   (list (ezeka--grab-dwim-file-target 'grab-from-links 'interactive)
         (equal current-prefix-arg '(4))
         (equal current-prefix-arg '(16))))
  (if-let* ((linkname linkname)
            (target (file-symlink-p linkname))
            (kasten (cond ((and (not kasten) (ezeka-file-kasten target))
                           (ezeka-file-kasten target))
                          ((stringp kasten)
                           (ezeka-kasten kasten))
                          (t
                           (ezeka-kasten
                            (ezeka--read-kasten
                             (format "`%s' %s.\nSelect new target in which Kasten? "
                                     (ezeka-file-name-id linkname)
                                     (if (file-exists-p target)
                                         (format "points to `%s'" (file-name-base target))
                                       " doesn't exist")))))))
            (new-target (ezeka--select-file
                         (ezeka--directory-files kasten)
                         "Select symbolic link target: "
                         'require-match
                         (file-name-base target)))
            (new-name (cond ((stringp new-name) new-name)
                            (t (read-string "New link name: " linkname)))))
      (progn
        (ezeka--add-to-system-log 'delete-symlink nil
          'note linkname
          'target target)
        (delete-file linkname)
        (when (ezeka--make-symbolic-link new-target (or new-name linkname))
          (message "Symbolic link updated")))
    (user-error "This is not a symbolic link")))

(defun ezeka-handle-symlink (file &optional same-window)
  "Prompt how to handle FILE if it is a symlink.
If SAME-WINDOW is non-NIL, open the file interactive he same
window. Return T if an action was taken, nil otherwise."
  (when-let* ((target (file-symlink-p file))
              (action (intern-soft
                       (completing-read "What to do with this symbolic link? "
                                        '(follow create update delete)
                                        nil t nil nil "follow"))))
    (cond ((eq action 'follow) (ezeka-find-file file same-window))
          ((eq action 'update) (ezeka--update-symbolic-link file))
          ((eq action 'delete)
           (when (y-or-n-p "Really delete this symbolic link? ")
             (ezeka--add-to-system-log 'delete-symlink nil
               'note (file-name-base file)
               'target (file-name-base target))
             (delete-file file)))
          ((eq action 'create)
           (when (y-or-n-p "Delete symlink and create a new note? ")
             (ezeka-replace-placeholder file)))
          (t (message "No action taken.")))
    t))

;;;=============================================================================
;;; Zettel Links
;;;=============================================================================

(defun ezeka-link-at-point-p (&optional freeform)
  "Return non-nil if the thing at point is a wiki link (i.e. [[xxx#YYY]]).
The first group is the Zettel ID for the target. The second
group contains the `org-mode' link target, if any. If
FREEFORM is non-nil, also consider Zettel links that are not
enclosed in square brackets."
  (thing-at-point-looking-at
   (let ((link (ezeka--regexp-strip-named-groups (ezeka-link-regexp))))
     (if freeform
         (concat "\\(?1:" link "\\)")
       (rx (seq "[[" (group-n 1 (regexp link)) "]"
                (0+ (group "[" (1+ (not "]")) "]"))
                "]"))))))

(defun ezeka-link-at-point ()
  "Return the Zettel link at point.
Needs to be called after `ezeka-link-at-point-p'."
  (match-string-no-properties 1))

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
      (cond (file (unless (ezeka-handle-symlink file same-window)
                    (ezeka-find-file file same-window)))
            (buf (if same-window
                     (pop-to-buffer-same-window buf)
                   (pop-to-buffer buf)))
            ((ezeka-note-moved-p link nil 'ask 'nosearch))
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

(defun ezeka-insert-with-spaces (&rest strings)
  "Insert STRINGS at point surrounded by spaces as appropriate.
If the point is between punctuation symbols, no spaces are
added. STRINGS are themselves concatenated with spaces in
between."
  (insert
   (concat (unless (or (bolp) (ezeka--space-or-punct-p (char-before))) " ")
           (string-trim (mapconcat #'identity strings " "))
           (unless (or (eolp) (ezeka--space-or-punct-p (char-after))) " "))))

(defvar ezeka-insert-link-hook nil
  "Normal hook that is run after `ezeka-insert-link' and friends.")

(defun ezeka--insert-link-with-spaces (link &rest strings)
  "Insert LINK at point, confirming first if it exists already.
If STRINGS is non-nil, do not insert LINK itself, but rather
just STRINGS, which are assumed to include the link in
question."
  (let* ((breadcrumbs
          (save-excursion
            (re-search-forward
             ezeka-breadcrumbs-trail-headline nil 'noerror)))
         (existing
          (cond ((save-excursion (re-search-backward link nil 'noerror))
                 'above)
                ((save-excursion (re-search-forward link breadcrumbs 'noerror))
                 'below)))
         (strings (or strings (list (ezeka--format-link link))))
         (insert-how
          (when existing
            (completing-read (format "Link already exists %s. Proceed how? " existing)
                             '("insert anyway" "don't include the link" "never mind")
                             nil
                             t))))
    (cond ((or (not existing) (string= insert-how "insert anyway"))
           (apply #'ezeka-insert-with-spaces strings)
           (run-hooks 'ezeka-insert-link-hook))
          ((string= insert-how "don't include the link")
           ;; FIXME: The link might not be the last element
           (apply #'ezeka-insert-with-spaces (butlast strings)))
          (t
           (message "Okay, have it your way")))))

(defun ezeka-insert-link-with-metadata (zettel &optional fields where noedit)
  "Insert link to ZETTEL, optionally adding metadata FIELD(S).
ZETTEL can be a buffer, a file name, or a link.
WHERE (:before, :after, or in :description) determines where
the fields are added. FIELDS can be a list. If NOEDIT is
non-nil, insert the link without allowing the user to
interactively edit the text."
  (interactive
   (list (ezeka--read-id "Link to insert: ")
         (list (ezeka--read-metadata-field))
         ;; FIXME Where argument is completely ignored
         (intern-soft (completing-read "Where? " '(":before" ":after") nil t))))
  (let* ((file (pcase zettel
                 ((pred ezeka-link-p) (ezeka-link-file zettel))
                 ((pred bufferp) (buffer-file-name zettel))
                 ((pred ezeka-file-p) zettel)
                 (_ (signal 'wrong-type-argument (list 'link-file-or-buffer zettel)))))
         (link (ezeka-file-link file)))
    (if-let* ((_ (car fields))
              (desc-fmt (mapconcat
                         (lambda (f)
                           (format "%%%c"
                                   (plist-get (alist-get f ezeka-metadata-fields)
                                              :format)))
                         fields
                         " "))
              (mdata (if (file-symlink-p file)
                         (ezeka-decode-rubric (file-name-base file))
                       (ezeka-file-metadata file)))
              (desc-string (ezeka-format-metadata
                            desc-fmt
                            mdata
                            (format "[%s]" ezeka-long-timestamp-format))))
        (ezeka--insert-link-with-spaces link
                                        (if noedit
                                            desc-string
                                          (read-string "Insert: " desc-string))
                                        (ezeka--format-link link))
      (ezeka--insert-link-with-spaces link))))

(defun ezeka-insert-link-to-visiting (arg)
  "Insert a link to another Zettel being currently visited.
With \\[universal-argument] ARG offers a few options for including
Zettel metadata. If the user selects a Zettel that does not exist in
the list, just insert the link to what was selected. If the cursor in
already inside a link, replace it instead."
  (interactive "P")
  (if-let ((link (ezeka-file-link
                  (ezeka--select-file (ezeka-visiting-files-list)
                                      "Insert link to: " t))))
      (cond ((ezeka-link-at-point-p)
             (delete-region (match-beginning 0) (match-end 0))
             (ezeka--insert-link-with-spaces link))
            (t
             (ezeka-insert-link-with-metadata
              link
              (list (ezeka--read-metadata-field nil))
              :before)))
    (message "No visiting Zettel")))

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
By default, include the title. With LINK-ONLY (or \\[universal-argument]),
insert just the link; with RUBRIC (or \\[universal-argument] \\[universal-argument]),
insert the rubric instead."
  (interactive
   (list (equal current-prefix-arg '(4))
         (equal current-prefix-arg '(16))))
  (let* ((other-file (ezeka--note-in-other-window))
         (other-link (alist-get 'id (ezeka-file-metadata other-file))))
    (cond (link-only
           (ezeka--insert-link-with-spaces other-link))
          (rubric
           (ezeka-insert-link-with-metadata other-link '(rubric) :before))
          (t
           (ezeka-insert-link-with-metadata
            other-link
            (list (ezeka--read-metadata-field)))))))

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

(defun ezeka-kill-ring-save-metadata-field (file field &optional time-format insert)
  "Save the value of FILE's metadata FIELD to kill ring and system clipboard.
FIELD should be one of `ezeka-metadata-fields'. If called
interactively and the point is at Zettel link, use that as
the target FILE; otherwise, the current buffer. With non-nil
TIME-FORMAT, format time accordingly. If INSERT is non-nil (or \\[universal-argument])
also insert the value at point."
  (interactive
   (let ((file (ezeka--grab-dwim-file-target))
         (field (ezeka--read-metadata-field)))
     (list file
           field
           (when (eq 'ezeka--timep (ezeka--metadata-field-predicate field))
             (read-string "How to format time values? "
                          (concat "[" ezeka-long-timestamp-format "]")
                          'ezeka--krsmf-time-format-history))
           current-prefix-arg)))
  (if-let* ((mdata (ezeka-file-metadata file))
            (text (ezeka-format-metadata (ezeka--metadata-field-format field)
                                         mdata
                                         time-format)))
      (prog1 (ezeka--kill-ring-clipboard-save text)
        (when insert (insert text)))
    (user-error "Could not retrieve metadata for %s" (file-name-base file))))

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
          (shell-command (format "open -R \"%s\" &" file)))))))

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
                  (text (ezeka-format-metadata
                         (ezeka--metadata-field-format (or field 'title))
                         (ezeka-file-metadata file))))
        (delete-region start (point))
        (unless delete
          (ezeka-insert-with-spaces text " "))))))

;;;=============================================================================
;;; Link hints
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
        (setq-local mode-line-misc-info
                    (propertize
                     (format "%s%s%s" (alist-get 'title metadata)
                             (if (alist-get 'citekey metadata) " " "")
                             (or (alist-get 'citekey metadata) ""))
                     'face '(:slant italic :height 0.9)))
      (setq-local mode-line-misc-info (symbol-value mode-line-misc-info)))))

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
If KASTEN name is given, use that Kasten instead. Return a
fully qualified link to the new child. If ID is non-nil, use
that instead of generating one."
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
    (let ((context
           (if (org-at-heading-p)
               (nth 4 (org-heading-components))
             (buffer-substring-no-properties
              (point)
              (max (point-min)
                   (save-excursion
                     (if (cl-member :item (org-context) :key #'car)
                         (org-beginning-of-item)
                       (org-beginning-of-line -1))
                     (point)))))))
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
         (child-file (ezeka-link-file child-link))
         (metadata `((id . ,child-link)
                     (kasten . ,kasten)
                     (title . ,title)
                     (caption . ,(ezeka--pasteurize-file-name title))
                     (parent . ,parent)
                     (citekey . ,citekey))))
    (ezeka--insert-link-with-spaces child-link)
    (ezeka--set-new-child-metadata child-link metadata)
    (cond ((string= "placeholder"
                    (completing-read "Create a new note or just a placeholder? "
                                     '(new-note placeholder) nil t))
           (ezeka--create-placeholder child-link metadata)
           (ezeka-breadcrumbs-drop child-link parent "(Placeholder)"))
          ((and child-file
                (file-exists-p child-file)
                (file-symlink-p child-file)
                (y-or-n-p (format "Remove placeholder %s first? "
                                  (file-name-base child-file))))
           (ezeka-replace-placeholder child-file
                                      (ezeka-link-path child-link metadata)))
          ((and child-file
                (file-exists-p child-file)
                (file-symlink-p child-file))
           (ezeka-find-link child-link))
          (t
           (ezeka--add-to-system-log 'new-note nil
             'note (ezeka-encode-rubric metadata)
             'parent parent)
           (ezeka-find-link child-link)))))

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
If called with \\[universal-argument], select Kasten first.
With \\[universal-argument] \\[universal-argument], read the parent ID manually."
  (interactive
   (list (ezeka--grab-dwim-file-target)
         (if (equal current-prefix-arg '(16))
             (ezeka--read-id "New parent: "
                             nil
                             (ezeka-file-link (ezeka--note-in-other-window)))
           (ezeka-file-link (ezeka--select-file
                             (ezeka--directory-files
                              (if current-prefix-arg
                                  (ezeka--read-kasten)
                                "numerus"))
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
user edit it before setting. With DEGREE (or numerical
prefix arg), trace genealogy further than parent."
  (interactive (list (buffer-file-name)
                     nil
                     (if (integerp current-prefix-arg)
                         current-prefix-arg
                       1)))
  (unless (ezeka-file-p filename)
    (user-error "Not a Zettel note"))
  (let* ((ancestor (ezeka-trace-genealogy filename degree))
         (mdata (ezeka-file-metadata filename))
         (old-citekey (or (ezeka-file-name-citekey filename)
                          (alist-get 'citekey mdata)))
         (title (alist-get 'title mdata))
         (citekey (ezeka--validate-citekey
                   (ezeka--minibuffer-edit-string
                    old-citekey
                    (or old-citekey
                        (and ancestor
                             (ezeka-file-name-citekey (ezeka-link-file ancestor)))
                        (ezeka--citekey-from-note-title title)
                        (concat title
                                (format-time-string " %F"
                                                    (alist-get 'created mdata))))
                    nil
                    'ezeka--read-citekey-history))))
    (if (equal old-citekey citekey)
        (message "No changes")
      (ezeka--update-metadata-values filename nil 'citekey citekey)
      (when (y-or-n-p "Record the change in the change log? ")
        (ezeka-add-change-log-entry
            filename
          (cond ((and old-citekey (not citekey))
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
  (interactive
   (list (ezeka--grab-dwim-file-target)
         (let ((org-read-date-popup-calendar t))
           (org-read-date nil nil nil "When did you read this? " nil nil 'inactive))))
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
  (completing-read (or prompt "Kasten: ")
                   (if (listp (ezeka-kaesten))
                       (mapcar #'ezeka-kasten-name (ezeka-kaesten))
                     (signal 'ezeka-error (list "No `ezeka-kaesten' defined")))
                   nil
                   t))

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
;;; Change Log
;;;=============================================================================

(defun ezeka--skip-line-and-insert (&rest args)
  "Insert a blank line and insert ARGS."
  (let ((pos (point)))
    (when (re-search-backward "^[^\n]" nil 'noerror)
      (end-of-line)
      (delete-region (point) pos))
    (apply #'insert "\n\n" args)))

(defun ezeka--demote-quotes (string)
  "Demote double quotes in STRING to single quotes.
Only ASCII double and single quotes are touched."
  (string-replace "\"" "'" string))

(defvar ezeka--read-change-log-entry-history nil
  "History variable for `ezeka--read-change-log-entry'.")

(defun ezeka--read-change-log-entry (&optional initial-input prompt)
  "Read a change log entry from the minibuffer.
PROMPT (or reasonable default) and INITIAL-INPUT are passed to
`read-string'. Return NIL if the user entered an empty string."
  (let* ((prompt (or prompt "Change log entry (leave empty to skip): "))
         (entry (read-string prompt
                             initial-input
                             'ezeka--read-change-log-entry-history)))
    (unless (string-empty-p entry)
      entry)))

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

;;;=============================================================================
;;; Org-Mode Intergration
;;;=============================================================================

;;;=============================================================================
;;; Converting Timestamps
;;;=============================================================================

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
     ;; something -> org timestamp =============================================
     ((integerp (nth 4 (setq parsed (parse-time-string text))))
      (setq parsed (mapcar (lambda (x) (or x 0)) parsed))
      (let ((timestamp (ezeka-timestamp (encode-time parsed) 'full 'brackets)))
        (when (setq timestamp (read-string "Best attempt: " timestamp))
          (delete-region beg end)
          (insert timestamp)
          timestamp)))
     ;; something else =========================================================
     (t
      (display-warning 'ezeka-dwim-with-this-timestring
                       "`%s' doesn't look like a timestring" text)))))

(defun ezeka-convert-timestamp (&optional beg end)
  "Convert timestamp at point or between BEG and END.
If it's IS8601, make it into an org-time-stamp, and vice-versa.
If it's something else, try to make it into org-time-stamp.
Return the result of the conversion."
  (interactive (if (region-active-p)
                   (list (region-beginning) (region-end))
                 (list)))
  (save-excursion
    (let ((ts-char "[0-9-T:/]"))
      (if (and beg end)
          (ezeka-dwim-with-this-timestring beg end)
        (cond ((looking-at ts-char)
               (while (string-match-p ts-char (string (char-before)))
                 (backward-char)))
              ((re-search-forward "[0-9]" (pos-eol) 'noerror)
               (backward-char))
              (t
               (user-error "No timestamp to convert here")))
        (when (or (thing-at-point-looking-at ezeka--org-timestamp-regexp)
                  (thing-at-point-looking-at ezeka-iso8601-datetime-regexp)
                  (thing-at-point-looking-at ezeka-iso8601-date-regexp))
          (save-restriction
            (narrow-to-region (match-beginning 0) (match-end 0))
            (ezeka-dwim-with-this-timestring)))))))

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

(defun ezeka-extract-subtree-as-new-note (&optional id kasten)
  "Create new Zettel from the current org subtree.
With \\[universal-argument], query for ID. With \\[universal-argument] \\[universal-argument],
use the current KASTEN without asking."
  (interactive
   (let ((id (when current-prefix-arg
               (ezeka--read-id "ID for new note: "))))
     (list id
           (or (and id (ezeka-link-kasten id))
               (if (equal current-prefix-arg '(16))
                   (ezeka-file-kasten buffer-file-name)
                 (ezeka--read-kasten "New note in which Kasten? "))))))
  (let* ((parent-file buffer-file-name)
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
               match-data
               timestamp)
          (cond ((string-match "\\(.*\\) \\([[<].*[]>]\\)" head-title)
                 (setq match-data (match-data))
                 (setf (alist-get 'title mdata)
                       (ezeka--minibuffer-edit-string
                        (match-string-no-properties 1 head-title)
                        nil
                        "Title for new note: "))
                 (setf (alist-get 'created mdata)
                       (org-timestamp-to-time
                        (org-timestamp-from-string
                         (progn
                           (set-match-data match-data)
                           (match-string 2 head-title)))))
                 (when (string-match "^\\([[:alpha:]]+\\): .*" head-title)
                   (setf (alist-get 'label mdata)
                         (ezeka--minibuffer-edit-string
                          (match-string 1 head-title)
                          nil
                          "Label: "))))
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
          (setf (alist-get 'label mdata)
                (ezeka--read-label kasten))
          (setf (alist-get 'id mdata)
                (or id
                    (if (eq (ezeka-kasten-id-type kstruct) :tempus)
                        (ezeka-tempus-currens
                         (ezeka--complete-time (alist-get 'created mdata)))
                      (ezeka--generate-id kasten))))
          (setf (alist-get 'link mdata)
                (ezeka-make-link kasten (alist-get 'id mdata)))
          (setf (alist-get 'path mdata)
                (ezeka-link-path (alist-get 'link mdata) mdata))
          (setf (alist-get 'rubric mdata)
                (ezeka-encode-rubric mdata))
          (if (file-exists-p (alist-get 'path mdata))
              (user-error "Aborting, file already exists: %s" (alist-get 'path mdata))
            (let ((entry-pt (point))
                  (content (org-copy-subtree)))
              (with-current-buffer (get-buffer-create (alist-get 'path mdata))
                ;; New file buffer
                (ezeka-insert-header-template nil nil nil nil nil mdata)
                (insert "\n" org-subtree-clip)
                (set-visited-file-name (alist-get 'path mdata) t)
                (basic-save-buffer)
                (ezeka-add-change-log-entry (alist-get 'path mdata)
                  (ezeka-format-metadata "Extract from [[%p]]." mdata)))
              (with-current-buffer (get-file-buffer (file-truename parent-file))
                ;; Back in original buffer
                (goto-char entry-pt)
                (org-cut-subtree)
                (insert (make-string head-level ?*)
                        " "
                        head-title
                        " "
                        (ezeka--format-link (alist-get 'link mdata)))
                (when (y-or-n-p "Add change log entry in this file? ")
                  (ezeka-add-change-log-entry (file-truename parent-file)
                    (ezeka-format-metadata "Extract \"%R\" [[%i]]." mdata)))))))))))

(defun ezeka-open-link-at-point (&optional same-window freeform)
  "Open a Zettel link at point even if it's not formatted as a link.
If SAME-WINDOW is non-nil (or \\[universal-argument]), open the link in the same window.
With FREEFORM non-nil (also \\[universal-argument]), also open freeform links."
  (interactive "p\np")
  (if-let ((_ (ezeka-link-at-point-p freeform))
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

(defun ezeka-open-link-at-mouse-same-window (ev)
  "Open in the same window the Zettel link at mouse point EV."
  (interactive "e")
  (mouse-set-point ev)
  (ezeka-open-link-at-point 'same-window 'freeform))

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
  "Find all matches of REGEXP in KASTEN or `ezeka-directory'.
Called interactively, ask for KASTEN unless called with \\[universal-argument]."
  (interactive
   (list (ezeka--read-regexp "Regexp to find: ")
         (unless current-prefix-arg
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
                                (expand-file-name
                                 (if kasten
                                     (ezeka-kasten-directory (ezeka-kasten kasten))
                                   "")
                                 ezeka-directory)
                                nil)
     nil))
  (advice-add 'xref-goto-xref :before 'ezeka--breadcrumbs-xref-advice)
  (advice-add 'xref-show-location-at-point :before 'ezeka--breadcrumbs-xref-advice))

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

(defun ezeka-note-moved-p (note &optional confirm visit nosearch)
  "Check whether NOTE appears in the `ezeka--move-log-file'.
If CONFIRM is non-nil, confirm the note to check. If VISIT
is 'VISIT, visit the new note; if 'ASK or T, ask the user
whether to visit; if NIL, do not visit. If NOSEARCH is
non-nil, do not offer to do a text search."
  (interactive
   (let ((file (ezeka--grab-dwim-file-target 'link-at-point)))
     (list (if (or current-prefix-arg (null file))
               (ezeka--read-id "Link of note to check: "
                               nil
                               (word-at-point 'no-properties)
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
          ((and (null trail) nosearch)
           nil)
          ((null trail)
           (when (y-or-n-p (format "No record of moving %s. Do text search? " note))
             (ezeka-find-regexp (format "oldnames:.*%s" note)))
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

(defun ezeka--replace-links (before after &optional confirm)
  "Replace links to BEFORE to AFTER.
With CONFIRM non-nil (or \\[universal-argument]), ask for confirmations."
  (condition-case nil
      (let* ((replaced (ezeka-octavo-replace-links before after nil confirm))
             (basemsg (format "Replace %d links" (or (car replaced) 0))))
        (when (car replaced)
          (ezeka-add-change-log-entry (ezeka-link-file after) basemsg))
        (message basemsg)
        (kill-new basemsg)
        (ezeka-force-save-buffer)
        (ezeka--git-stage-file buffer-file-name))
    (error
     (kill-new (format "(ezeka-octavo-replace-links \"%s\" \"%s\")"
                       before after))
     (message "Replacing links failed; try manually"))))

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
                            (find-file-noselect target)))
                   (mdata (ezeka--add-oldname mdata source-link)))
          (with-current-buffer buf
            (ezeka--update-file-header target mdata)
            (let ((entry (ezeka--read-change-log-entry
                          (format "Move +%s+ to \"%s\" %s."
                                  source-rubric
                                  (alist-get 'rubric mdata)
                                  (ezeka--format-link target-link)))))
              (when entry
                (ezeka-add-change-log-entry source entry)))
            (setf (alist-get 'keywords mdata)
                  (cl-set-difference (alist-get 'keywords mdata)
                                     (list ezeka-note-moving-keyword ezeka-rename-note-keyword)
                                     :test #'string=))
            (ezeka--update-metadata-values target mdata)
            (when (and (called-interactively-p 'any)
                       (equal '(4) current-prefix-arg)
                       (y-or-n-p (format "Edit caption \"%s\"? "
                                         (alist-get 'caption mdata))))
              (ezeka-harmonize-file-name target mdata 'force))
            (save-buffer)))
      (message "Replacing links: %s with %s" source-link target-link)
      (ezeka--replace-links source-link target-link)
      (ezeka--git-stage-file target))))

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
         (t-path (ezeka-link-path t-link))
         (s-mdata (ezeka-file-metadata s-file))
         (t-mdata (ezeka-file-metadata s-file))) ; FIXME deep-copy s-mdata
    (ezeka--add-to-move-log s-link target-link
                            (alist-get 'caption s-mdata)
                            "Begin moving")
    (setf (alist-get 'id t-mdata)
          (ezeka-link-id target-link)
          (alist-get 'label t-mdata)
          (ezeka--read-label (ezeka-link-kasten t-link)
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
    (when-let* ((initial (format "Begin moving \"%s\" to \"%s.\""
                                 (alist-get 'rubric s-mdata)
                                 (alist-get 'rubric t-mdata)))
                (entry (ezeka--read-change-log-entry initial)))
      (ezeka-add-change-log-entry s-file entry))
    (ezeka--update-file-header s-file t-mdata 'force)
    (let ((t-file (ezeka-link-path target-link t-mdata)))
      (unless (file-exists-p (file-name-directory t-file))
        (make-directory (file-name-directory t-file)))
      (ezeka--make-symbolic-link s-file t-file)
      (message "Began moving `%s' to `%s'; re-run `ezeka-move-to-another-kasten' \
after committing" s-link target-link))))

(defun ezeka--copy-note (source target-link)
  "Copy Zettel note from SOURCE to TARGET-LINK.
SOURCE can be a link or a file."
  (cl-assert (stringp source))
  (cl-assert (stringp target-link))
  (let* ((s-file (if (file-exists-p source)
                     source
                   (ezeka-link-file source)))
         (s-link (ezeka-file-link s-file))
         (t-link target-link)
         (s-mdata (ezeka-file-metadata s-file))
         (t-mdata (copy-sequence (ezeka-file-metadata s-file)))
         change-log-entry)
    (ezeka--add-to-move-log s-link target-link
                            (alist-get 'caption s-mdata)
                            "Copy note")
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
          (alist-get 'rubric t-mdata)
          (ezeka-encode-rubric t-mdata)
          (alist-get 'oldnames t-mdata)
          (cl-remove (ezeka-link-id target-link)
                     (alist-get 'oldnames s-mdata)
                     :test #'string=))
    (ezeka--add-to-system-log 'copy nil
      'source (alist-get 'rubric s-mdata)
      'target (alist-get 'rubric t-mdata))
    (ezeka-add-change-log-entry s-file
      (setq change-log-entry
        (format "Copy \"%s\" [[%s]] to \"%s.\" [[%s]]"
                (alist-get 'rubric s-mdata)
                (alist-get 'id s-mdata)
                (alist-get 'rubric t-mdata)
                (alist-get 'id t-mdata))))
    (ezeka--update-file-header s-file t-mdata 'force)
    (let ((t-file (ezeka-link-path target-link t-mdata)))
      (unless (file-exists-p (file-name-directory t-file))
        (make-directory (file-name-directory t-file)))
      (ezeka--copy-file s-file t-file)
      (ezeka--update-file-header t-file
                                 (ezeka--add-oldname t-mdata
                                                     (alist-get 'id s-mdata)))
      (message "Copied `%s' to `%s'" s-link target-link))
    (ezeka-find-file s-file 'same-window)
    (ezeka-add-change-log-entry s-file change-log-entry)))

(defun ezeka--create-placeholder (id metadata &optional quietly)
  "Create a placeholder for ID with METADATA alist.
If QUIETLY is non-nil, don't add anything to the logs."
  (unless (alist-get 'label metadata)
    (push (cons 'label
                (ezeka--read-genus "Genus" 'verbose
                                   ezeka-placeholder-genus
                                   'require-match))
          metadata))
  (let-alist metadata
    (let* ((path (ezeka-link-path
                  id
                  (cons (cons 'label (string ezeka-placeholder-genus))
                        metadata)))
           (link-to (ezeka--select-file
                     (ezeka--directory-files
                      (ezeka-kasten
                       (ezeka--read-kasten "Kasten for symbolic link target: ")))
                     nil 'require-match \.parent))
           (link-target (file-relative-name
                         (ezeka-link-file link-to)
                         (file-name-directory path))))
      (setf (alist-get 'path metadata) path
            (alist-get 'filename metadata) (file-name-base path))
      (unless (alist-get 'rubric metadata)
        (setf (alist-get 'rubric metadata) (ezeka-encode-rubric metadata)))
      (unless quietly
        (ezeka--add-to-system-log 'placeholder nil
          'note (ezeka-encode-rubric metadata)
          'target (file-name-base link-target))
        (ezeka--add-to-move-log id link-to .caption "Placeholder"))
      (when (ezeka--make-symbolic-link link-target path)
        (if-let ((_ (y-or-n-p (format "Add something to `%s'? "
                                      (file-name-base link-to))))
                 (buf (find-file-noselect link-to)))
            (with-current-buffer buf
              (goto-char (point-max))
              (org-insert-heading nil nil 'top)
              (insert (ezeka-format-metadata
                       ezeka-placeholder-marker-format metadata)
                      " ")
              (org-insert-time-stamp nil 'with-hm 'inactive)
              (org-set-tags '("placeholder"))
              (switch-to-buffer buf))
          t)))))

(ert-deftest ezeka--create-placeholder ()
  (let* ((mdata (ezeka-metadata "a-1234"
                  'label "ψ"
                  'caption "ezeka--create-placeholder test"))
         (path (ezeka-link-path "a-1234" mdata)))
    (should (and (ezeka--create-placeholder "a-1234"
                                            mdata
                                            'quietly)
                 (file-symlink-p path)
                 (if (y-or-n-p (format "Delete placeholder `%s'?" path))
                     (delete-file path)
                   (message "Placeholder not deleted: %s" path))))))

(defun ezeka-replace-placeholder (placeholder &optional note metadata)
  "Replace PLACEHOLDER with NOTE (both file paths).
If METADATA is nil, read it from PLACEHOLDER's filename. If
NOTE is nil, create a new file."
  (interactive
   (list (ezeka--select-file (ezeka--directory-files
                              (ezeka-kasten "numerus")
                              (format ".*{%c}.*" ezeka-placeholder-genus))
                             "Placeholder to replace: "
                             'require-match)
         (when (ezeka-file-p buffer-file-name)
           buffer-file-name)))
  (let* ((ph-id (ezeka-file-link placeholder))
         (ph-rubric (file-name-base placeholder))
         (ph-mdata (or metadata (ezeka-decode-rubric ph-rubric)))
         (ph-linkto (ezeka-file-link (file-symlink-p placeholder)))
         (ph-date (file-attribute-modification-time
                   (file-attributes placeholder))))
    (unwind-protect
        (progn
          (cond ((file-symlink-p placeholder)
                 (rename-file placeholder (concat placeholder ".bak")))
                ((file-exists-p placeholder)
                 (user-error "`%s' is not a symlink, aborting!"
                             (file-relative-name placeholder ezeka-directory)))
                (t
                 (error "Unknown error")))
          (let* ((existing-p note)
                 (note-id (if existing-p (ezeka-file-link note) ph-id))
                 (note-rubric (if existing-p (file-name-base note) ph-rubric)))
            (with-current-buffer (find-file-noselect (or note placeholder))
              (unless existing-p
                (ezeka-insert-header-template note-id nil nil nil nil ph-mdata))
              (ezeka-add-change-log-entry note
                (concat (if existing-p
                            (format "Repurpose +%s+" note-rubric)
                          "Create a new note")
                        (format " in place of \"%s\" (created on %s, linked to [[%s]])"
                                ph-rubric
                                (format-time-string "%F" ph-date)
                                ph-linkto)))
              (save-buffer)
              (ezeka--add-to-move-log note-id ph-id note-rubric "Replace placeholder")
              (ezeka--add-to-system-log 'replace-placeholder nil
                'placeholder ph-rubric
                'parent ph-linkto
                'note note-rubric)
              (when existing-p
                (ezeka-move-to-another-kasten note "numerus" ph-id))
              (when (file-exists-p (concat placeholder ".bak"))
                (delete-file (concat placeholder ".bak")))
              (message "Visiting placeholder target [[%s]]..." ph-linkto)
              (ezeka-find-link ph-linkto))))
      (when (file-exists-p (concat placeholder ".bak"))
        (rename-file (buf (find-file-noselect note)) placeholder)
        (error "Replacing placeholder %s aborted" ph-rubric)))))

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

(defun ezeka--git-dissimilarity-index (filename)
  "Return numerical value of the dissimilarity index for FILENAME."
  (interactive (list (or (magit-file-at-point) (buffer-file-name))))
  (let ((di (with-temp-buffer
              (shell-command (format "git diff -B1%%/1%% \"%s\"" filename)
                             'current-buffer)
              (goto-char (point-min))
              (when (re-search-forward "^dissimilarity index \\([0-9]+\\)%"
                                       nil
                                       'noerror)
                (match-string 1)))))
    (when (called-interactively-p 'any)
      (message "Dissimilarity index %s"
               (if di
                   (concat di "%")
                 "unknown")))
    (when di
      (string-to-number di))))

(defun ezeka-move-to-another-kasten (source-file kasten &optional target-link noselect)
  "Move SOURCE-FILE Zettel to a generated link in KASTEN.
With \\[universal-argument], ask for an explicit TARGET-LINK instead.
Return the target link and open it (unless NOSELECT is non-nil)."
  (interactive
   (let* ((source (or (ezeka--grab-dwim-file-target)
                      buffer-file-name))
          (src-header (ezeka-file-content source 'just-header))
          (target (cond ((equal current-prefix-arg '(4))
                         (ezeka--read-id "Target link: "))
                        ((string-match-p ezeka-note-moving-keyword src-header)
                         (alist-get 'id (ezeka--decode-header src-header source))))))
     (list source
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
                (ezeka--generate-id kasten))))
         (target-path (when target-link
                        (ezeka-link-path target-link s-mdata))))
    (save-some-buffers nil (lambda () (ezeka-file-p buffer-file-name t)))
    (cond ((not target-link)
           (user-error "No target link specified"))
          ((ezeka-same-file-p source-link target-link)
           (user-error "Source and target links are the same"))
          ((and (member ezeka-note-moving-keyword
                        (alist-get 'keywords s-mdata))
                (y-or-n-p (format "Dissimilarity index %s. Finish moving %s to %s? "
                                  (if-let ((di (ezeka--git-dissimilarity-index source-file)))
                                      (format "%d%%" di)
                                    "unknown")
                                  source-link target-link)))
           (ezeka--finish-moving-note source-file target-path)
           (unless noselect
             (ezeka-find-link target-link t)))
          ((and (not (equal (ezeka-link-kasten source-link)
                            (ezeka-link-kasten target-link)))
                (y-or-n-p "Moving between Kaesten. Copy instead? "))
           (ezeka--copy-note source-link target-link))
          (t
           (ezeka--begin-moving-note source-file target-link)))))

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
        (ezeka--git-stage-file file))))
  (ezeka--git-stage-file buffer-file-name))

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
;;     (setq ezeka--original-mode-line nil)
;;     (add-hook 'post-command-hook 'ezeka--magit-mode-line-show-file-type nil t)))

(defvar ezeka--original-mode-line nil
  "Value of `mode-line-misc-info' before we override it.")

(defun ezeka-describe-file-at-point (&optional file)
  "Return a description of the FILE at point."
  (interactive)
  (let* ((file (cl-case major-mode
                 (magit-status-mode (magit-file-at-point))
                 (dired-mode (dired-file-name-at-point))
                 (wdired-mode (dired-file-name-at-point))))
         (symlink-p (and file
                         (file-symlink-p file)
                         (expand-file-name (file-symlink-p file)
                                           (ezeka-id-directory
                                            (ezeka-file-name-id file)))))
         (desc (cond ((null file))
                     (symlink-p
                      (concat (unless (file-exists-p symlink-p)
                                "BROKEN ")
                              (when (ezeka--marked-for-rename-p symlink-p)
                                "[renaming] ")
                              "symlink to "
                              (propertize
                               (file-name-base symlink-p)
                               'face :bold)))
                     ((file-directory-p file) "directory")
                     (t "regular file"))))
    (when (called-interactively-p 'any)
      (message desc))
    desc))

(defun ezeka--magit-mode-line-show-file-type ()
  "Display in the mode line the type of the file under cursor."
  (while-no-input
    (redisplay)
    (when (eq major-mode 'magit-status-mode)
      (setq-local mode-line-misc-info
                  (or (ezeka-describe-file-at-point)
                      ezeka--original-mode-line)))))

(defun ezeka--magit-file-creation-date (file)
  "Save the file creation date of FILE to kill ring."
  (interactive
   (list (or (magit-file-at-point 'expand)
             (and (ezeka-link-at-point-p)
                  (ezeka-link-file (ezeka-link-at-point))))))
  (let ((time-string
         (format-time-string
          "[%F %a %R]"
          (file-attribute-modification-time (file-attributes file)))))
    (kill-new time-string)
    (message "Saved %s to kill ring" time-string)))

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
            ("C-c ~" . ezeka-set-title-or-caption) ; `org-table-create-with-table.el'
            ;; ("C-c !" . ) ; `org-time-stamp-inactive'
            ("C-c @" . ezeka-set-citekey)
            ("C-c #" . ezeka-edit-keywords)
            ;; ("C-c $" . ) ; `flyspell-correct-word-before-point'
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
            ("C-c ;" . ezeka-set-label)
            ("C-c :" . ezeka-set-author)
            ;; ("C-c '" . ) ; `org-edit-special'
            ("C-c \"" . ezeka-insert-ancestor-link)
            ("C-c ," . ezeka-insert-new-child-with-title)
            ("C-c ." . ezeka-octavo-insert-contextual-link) ; `org-table-eval-formula'
            ("C-c /" . ezeka-convert-timestamp) ; `org-sparse-tree'
            ("C-c ?" . ezeka-links-to) ; `org-table-field-info'

            ;; shadows `org-open-at-mouse', but allows opening in same window with C-u
            ([S-mouse-1] . ezeka-open-link-at-mouse-same-window))) ; end of :keymap
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
