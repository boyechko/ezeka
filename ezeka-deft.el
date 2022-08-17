;;; zettel-deft.el --- Eclectic Zettelkasten Deft Integration -*- lexical-binding: t -*-

;; Copyright (C) 2015-2022 Richard Boyechko

;; Author: Richard Boyechko <code@diachronic.net>
;; Version: 0.1
;; Package-Requires: ((emacs "25.1") (zettel "0.8") (deft "0.8") (org "9.5"))
;; Keywords: deft zettelkasten org
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

;; Deft integration for zettel.el

(require 'zettel)
(require 'deft)

;;;=============================================================================
;;; User Variables
;;;=============================================================================

(defcustom zettel-sort-by-name-descending t
  "When non-NIL, `deft-sort-files-by-name' will sort in a descending order,
otherwise ascending."
  :type 'boolean
  :group 'zettel)

;;;=============================================================================
;;; Internal Variables
;;;=============================================================================

(defvar zettel-deft-active-kasten nil
  "The name of the active Zettelkasten, if any. This variable is set by
`zettel-deft-choose-kasten'.")

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

(defun zettel-deft-choose-kasten (arg new-kasten &optional number-of-frames)
  "If there is an existing `deft-buffer', switches to it, otherwise
interactively selects the deft directory from among `zettel-kaesten'. With a
prefix argument, selects new deft directory regardless of `deft-buffer'; with
double prefix argument calls `zettel-deft-choose-directory' instead. With
optinal NUMBER-OF-FRAMES, set the `zettel-number-of-frames' to that value."
  (interactive
   (cond ((null (get-buffer deft-buffer))
          (list current-prefix-arg
                (ivy-read "Zettel kasten: " zettel-kaesten)
                (or zettel-number-of-frames
                    (intern (ivy-read "Number of frames: " '(one two many))))))
         ((equal current-prefix-arg '(4))
          (list current-prefix-arg
                (ivy-read "Zettel kasten: " zettel-kaesten)
                (intern (ivy-read "Number of frames: " '(one two many)))))
         (t (list current-prefix-arg nil zettel-number-of-frames))))
  (setq zettel-number-of-frames number-of-frames)
  (cond ((equal arg '(16))
         (call-interactively #'zettel-deft-choose-directory))
        ((not new-kasten)
         (cl-case zettel-number-of-frames
           (nil (switch-to-buffer deft-buffer))
           (one (switch-to-buffer deft-buffer))
           (two (with-selected-window (ace-select-window)
                  (switch-to-buffer deft-buffer)))
           (many
            (switch-to-buffer-other-frame deft-buffer))))
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

(defun deft-new-unused-zettel ()
  "Create a new Zettel with unused numerus currens."
  (interactive)
  (deft-new-file-named (zettel-next-unused-slug)))

(defun zettel-deft-parse-title-function (line &optional show-missing)
  "Function for post-processing titles for display in Deft buffer, intended
as the value for `deft-parse-title-function'. If SHOW-MISSING is non-NIL,
the missing metadata is explicitly displayed."
  (when line
    (let* ((metadata (zettel-decode-combined-title
                      (replace-regexp-in-string "^\\(title: +\\)" "" line)))
           (slug (or (alist-get :slug metadata)
                     (if show-missing "<slug>" "")))
           (cat (or (alist-get :category metadata)
                    (if show-missing "<category>" "")))
           (key (or (alist-get :citekey metadata)
                    (if show-missing "<citekey>" "")))
           (key (if (string-match "^@" key)
                    (replace-match "" nil nil key)
                  key))
           (title (or (alist-get :title metadata)
                      (if (not (zerop (length line)))
                          line
                        "<title>")))
           (SLUG-LEN
            (length
             (alist-get (or (zettel-type (alist-get :slug metadata))
                            (zettel-kasten-slug-type zettel-deft-active-kasten)
                            :tempus)    ; assume longest
                        zettel-type-example-alist)))
           (CAT-LEN 12)
           (KEY-LEN 10)
           ;; SLUG---CATEGORY---CITEKEY---TITLE [where --- is tab]
           (fmt (format "%%-%ds\t%%-%ds\t%%-%ds\t%%s" SLUG-LEN CAT-LEN KEY-LEN)))
      (format fmt                       ; requires 4 arguments
              slug
              (if (> (length cat) CAT-LEN)
                  (concat (cl-subseq cat 0 (- CAT-LEN 1)) "…")
                cat)
              (if (> (length key) KEY-LEN)
                  (concat (cl-subseq key 0 (- KEY-LEN 1)) "…")
                key)
              title))))
(setq deft-parse-title-function 'zettel-deft-parse-title-function)

(defalias 'zettel-deft-file-title-slug 'zettel-file-slug
  "Returns the slug part of `deft-file-title' of the given Zettel file.")

(defun zettel-deft-file-title-category (file)
  "Returns the category part of `deft-file-title' of the given Zettel file."
  (--if-let (deft-file-title file)
      (cadr (split-string it "[ \t]+" nil))))

(defun zettel-deft-file-title-citekey (file)
  "Returns the citekey part of `deft-file-title' of the given Zettel file."
  (--if-let (deft-file-title file)
      (caddr (split-string it "[ \t]+" nil " "))))

(defun zettel-deft-file-title-title (file)
  "Returns the title part of `deft-file-title' of the given Zettel file."
  (--if-let (deft-file-title file)
      (cadddr (split-string it "[ \t]+" nil " "))))

(defun zettel-deft-parsed-title (file)
  "Returns the result of `deft-file-title' if available or the result of
`zettel-deft-parse-title-function' on the first line of the given FILE."
  (or (deft-file-title file)
      (zettel-deft-parse-title-function
       (zettel-file-content file t t))))

(defun zettel-adv--deft-new-file-maybe-named (arg)
  "Extends `deft-new-file' to call `deft-new-file-named' if called with
prefix argument."
  (interactive "p")
  (if (= arg 4)
      (call-interactively #'deft-new-file-named)
    (deft-new-file)))

;; Don't ever auto populate the title
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
  (interactive (list (zettel-ivy-read-category nil nil #'>)
                     current-prefix-arg))
  (deft-filter (format "{%s}" category)
    (or arg (null deft-filter-regexp))))

;;;=============================================================================
;;; Convenience
;;;=============================================================================

(defun zettel-show-link-title-in-minibuffer ()
  "Displays Zettel title of the link under cursor, less category and citekey,
in the minibuffer."
  (while-no-input
    (redisplay))
  (when (and (eq major-mode 'org-mode)
             (zettel-link-at-point-p))
    (let* ((file (zettel-link-file (match-string 1) t))
           (title (zettel-deft-parsed-title file))
           (title
            (if (string-match "^\\([[:alnum:]-]+\\).*	.*	\\(.*\\)$" title)
                (format "%s / %s" (match-string 1 title) (match-string 2 title))
              title)))
      (message (s-center (window-width) title)))))
(add-hook 'zettel-mode-hook
  (lambda ()
    (add-hook 'post-command-hook
      'zettel-show-link-title-in-minibuffer)))
(add-hook 'zettel-mode-hook
  (lambda ()
    (add-hook 'after-save-hook
      'zettel-show-link-title-in-minibuffer)))

;;;=============================================================================
;;; Utility Commands
;;;=============================================================================

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
                                   (car (split-string
                                         (alist-get :title (zettel-file-metadata file))
                                         ":")))))
             (insert (format "#+INCLUDE: \"%s::%s\" :only-contents t\n"
                             (file-relative-name file)
                             summary-section))
             (insert (format "#+INCLUDE: \"%s::%s\" :only-contents t"
                             (file-relative-name file)
                             snippet-section)))))
      (user-error "No Deft cache or visited Zettel"))))

(defun zettel-zmove-all-in-browser-region (start end kasten arg)
  "Move all files listed in the active region of deft-browser to KASTEN. With
prefix argument, confirm each move and ask about destination kasten."
  (interactive (list (region-beginning)
                     (region-end)
                     (ivy-read "Which kasten to move to? " zettel-kaesten)
                     current-prefix-arg))
  (save-excursion
    (save-restriction
      (let* ((lines (split-string
                     (buffer-substring-no-properties start end) "\n" t))
             (alist (mapcar (lambda (line)
                              (let ((split (split-string line "\t")))
                                (cons (car split) (fourth split))))
                            lines))
             (j 1))
        (dolist (tup alist)
          (let* ((slug (car tup))
                 (title (cdr tup))
                 (file (zettel-link-file slug)))
            (when (or (not arg)
                      (setq kasten
                        (ivy-read "Which kasten to move to? " zettel-kaesten))
                      (y-or-n-p
                       (format "[%d/%d] Move %s [%s] to %s? "
                               j (length alist) slug title kasten)))
              (message "[%d/%d] Moved %s to %s in %s" j (length alist) slug
                       (zettel-zmove-to-another-kasten file kasten)
                       kasten))
            (incf j)))
        (deft-refresh)))))

(defun zettel-filter-for-link-at-point ()
  "Modifies the Deft filter to look for the Zettel linked with
the link at point. If there is only one match, opens the note in
another window."
  (interactive)
  (when (zettel-link-at-point-p)
    (let ((link (zettel-link-at-point))
          (deft-incremental-search nil))
      (deft-filter (concat "^oldnames: \\[.*" link ".*\\]$") t)
      (unless deft-current-files
        (deft-filter (concat "§" link ".") t))
      (cond ((= (length deft-current-files) 1)
             (deft-open-file (car deft-current-files) t t))
            ((null deft-current-files)
             (message "No notes with current or old name matching `%s'" link))
            (t
             (switch-to-buffer-other-window deft-buffer))))))

(defun zettel-populate-categories ()
  "Populate `zettel-categories' based on the titles in Deft cache."
  (interactive)
  (setq zettel-categories '())
  (dolist (file deft-all-files)
    (let* ((category (alist-get :category (zettel-file-metadata file)))
           (frequency (alist-get category zettel-categories 0 nil #'string-equal)))
      (setf (alist-get category zettel-categories nil nil #'string-equal)
            (1+ frequency))))
  (message "%d categories in %d files"
           (length zettel-categories) (length deft-all-files)))

;;;=============================================================================
;;; TODO: Work in Progress
;;;=============================================================================

(defun zettel--file-content-from-deft-cache ()
  ((and deft-hash-contents (deft-file-contents file))
         (deft-file-contents file)))

(defun zettel--next-unused-slug-from-deft-cache ()
  ((eq type active-kasten-type)
   (message "Generating next unused slug of type %s" type)
   (let ((used (mapcar #'zettel-file-slug deft-all-files)))
     (while (or (not slug) (member slug used))
       (setq slug (zettel-generate-new-slug type))))))

(defun zettel--kill-ring-save-link-title ()
  (when (and deft-hash-contents (deft-file-contents file))
    (deft-cache-update-file file)))

(defun zetel--formatted-frame-title ()
  ;; Added in the beginning
  (if zettel-deft-active-kasten
      (format "〔%s〕"
              (upcase zettel-deft-active-kasten))
    ""))

;;;=============================================================================
;;; Internal
;;;=============================================================================

(defun zettel-adv--deft-new-file-insert-metadata (orig-fun slug)
  "Replaces deft's default behavior of putting the filter string
on the first line with the Zettel title string."
  ;; `DEFT-NEW-FILE-NAMED' returns either a string (from MESSAGE) about an
  ;; error, or the result of (GOTO-CHAR (POINT-MAX)), which means an integer
  ;; buffer location.
  (when (integerp (funcall orig-fun slug))
    (let ((file (deft-absolute-filename slug)))
      (with-current-buffer (get-file-buffer file)
        (erase-buffer)
        (call-interactively #'zettel-insert-metadata-template)))))
(advice-add 'deft-new-file-named :around #'zettel-adv--deft-new-file-insert-metadata)

(defun zettel-adv--deft-absolute-filename (orig-fun &rest args)
  "Replaces the default `deft-absolute-filename' with
`zettel-link-file'."
  (let ((kasten (zettel-directory-kasten deft-directory)))
    (zettel-link-file
     (if kasten
         (concat kasten ":" (car args))
       (car args)))))
(advice-add 'deft-absolute-filename :around 'zettel-adv--deft-absolute-filename)

;;
;; Sorting deft-buffer by filename
;;
(defun deft-sort-files-by-name (files)
  "Sort FILES by name in reverse order, ignoring case."
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
  (let ((title1 (caddr (split-string (or (deft-file-title file1) "") "  +")))
        (title2 (caddr (split-string (or (deft-file-title file2) "") "  +"))))
    (string-lessp (downcase (or title1 ""))
                  (downcase (or title2 "")))))

(defun deft-sort-files-by-zettel-title (files)
  "Sort FILES by the Zettel title."
  (sort files 'zettel-title-lessp))

(eval-after-load "deft"
  '(progn
     ;; Use our own `deft-sort-files-by-name' rather that respects
     ;; `zettel-sort-by-name-descending'.
     (defalias 'deft-sort-files-by-title 'deft-sort-files-by-name)
     ;; "Shadow" the built-in slug generator to generate timestamps by default,
     ;; i.e. when DEFT-NEW-FILE is called (C-c C-n)
     (defalias 'deft-unused-slug 'zettel-next-unused-slug)))

;; Having a visual indicator of the sort method is helpful
(defun deft-set-mode-name ()
  "Set the mode line text based on search mode."
  (setq mode-name
    (format "Deft[%s]%s"
            deft-current-sort-method
            (if deft-incremental-search "" "/R"))))
(advice-add 'deft-toggle-sort-method :after 'deft-set-mode-name)

(defun zettel-adv--deft-open-button (orig-fun &rest args)
  "Advice :around `deft-open-button' to call `zettel-find-link' instead of
`deft-open-file'."
  (zettel-find-file (button-get (car args) 'tag) current-prefix-arg))
(advice-add 'deft-open-button :around #'zettel-adv--deft-open-button)

;; On save, update deft-cache
(add-hook 'zettel-mode-hook
  (lambda ()
    (add-hook 'after-save-hook
      (lambda ()
        (deft-cache-file buffer-file-name))
      nil t)))

;;-----------------------------------------------------------------------------
;; Deft-Mode Keybindings
;;-----------------------------------------------------------------------------

(define-key deft-mode-map (kbd "C-c C-S-n") 'deft-new-unused-zettel)
(define-key deft-mode-map (kbd "C-c s") 'zettel-add-section-sign-to-deft-filter)
(define-key deft-mode-map (kbd "C-c C-n") 'deft-new-file-named)
(define-key deft-mode-map (kbd "C-c C-o") 'push-button)
(define-key deft-mode-map (kbd "C-c #") 'zettel-kill-ring-save-link)
(define-key deft-mode-map (kbd "C-c C-s") 'zettel-select-and-find-link) ; was: `deft-toggle-sort-method'
(define-key deft-mode-map (kbd "C-c C-f") 'zettel-select-and-find-link) ; was: `deft-find-file'
(define-key deft-mode-map (kbd "C-c C-'") 'deft-filter-zettel-category)
(define-key deft-mode-map (kbd "C-c C-p") 'zettel-populate-categories)
(define-key deft-mode-map (kbd "C-c C-x l") 'zettel-links-to)

(provide 'zettel-deft)
