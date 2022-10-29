;;; ezeka-deft.el --- Eclectic Zettelkasten Deft Integration -*- lexical-binding: t -*-

;; Copyright (C) 2015-2022 Richard Boyechko

;; Author: Richard Boyechko <code@diachronic.net>
;; Version: 0.1
;; Package-Requires: ((emacs "25.1") (ezeka "0.8") (deft "0.8") (org "9.5"))
;; Keywords: deft zettelkasten org
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

;; Deft integration for ezeka.el

(require 'ezeka)
(require 'deft)

;;;=============================================================================
;;; User Variables
;;;=============================================================================

(defcustom ezeka-deft-sort-by-name-descending t
  "When non-NIL, `deft-sort-files-by-name' will sort in a descending order,
otherwise ascending."
  :type 'boolean
  :group 'ezeka)

;;;=============================================================================
;;; Internal Variables
;;;=============================================================================

(defvar ezeka-deft-active-kasten nil
  "The name of the active Zettelkasten, if any. This variable is set by
`ezeka-deft-choose-kasten'.")

;;;=============================================================================
;;; Deft-Mode Integration
;;;=============================================================================

;; Adjust how Deft lists Zettel
(setq deft-strip-title-regexp "^\\(rubric: +\\)"
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

(defun ezeka-deft-choose-kasten (arg new-kasten &optional number-of-frames)
  "If there is an existing `deft-buffer', switches to it, otherwise
interactively selects the deft directory from among `ezeka-kaesten'. With a
prefix argument, selects new deft directory regardless of `deft-buffer'; with
double prefix argument calls `ezeka-deft-choose-directory' instead. With
optinal NUMBER-OF-FRAMES, set the `ezeka-number-of-frames' to that value."
  (interactive
   (cond ((null (get-buffer deft-buffer))
          (list current-prefix-arg
                (completing-read "Zettel kasten: " ezeka-kaesten)
                (or ezeka-number-of-frames
                    (intern (completing-read "Number of frames: " '(one two many))))))
         ((equal current-prefix-arg '(4))
          (list current-prefix-arg
                (completing-read "Zettel kasten: " ezeka-kaesten)
                (intern (completing-read "Number of frames: " '(one two many)))))
         (t (list current-prefix-arg nil ezeka-number-of-frames))))
  (setq ezeka-number-of-frames number-of-frames)
  (cond ((equal arg '(16))
         (call-interactively #'ezeka-deft-choose-directory))
        ((not new-kasten)
         (cl-case ezeka-number-of-frames
           (nil (switch-to-buffer deft-buffer))
           (one (switch-to-buffer deft-buffer))
           (two (with-selected-window (ace-select-window)
                  (switch-to-buffer deft-buffer)))
           (many
            (switch-to-buffer-other-frame deft-buffer))))
        (t
         (setq ezeka-deft-active-kasten new-kasten)
         (ezeka-deft-choose-directory (ezeka-kasten-directory new-kasten)))))

(defun ezeka-deft-choose-directory (directory)
  "Interactively selects the directory, starting in `ezeka-directory'."
  (interactive
   (list
    (read-directory-name "Deft directory: "
                         ezeka-directory
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
  (ezeka-deft-populate-categories))

(defun ezeka-deft-new-unused-note ()
  "Create a new Zettel with unused numerus currens."
  (interactive)
  (deft-new-file-named (ezeka--generate-id)))

(defun ezeka-deft-parse-title-function (line &optional show-missing)
  "Function for post-processing titles for display in Deft buffer, intended
as the value for `deft-parse-title-function'. If SHOW-MISSING is non-NIL,
the missing metadata is explicitly displayed."
  (when line
    (let* ((metadata (ezeka-decode-rubric
                      (replace-regexp-in-string "^\\(rubric: +\\)" "" line)))
           (id (or (alist-get :id metadata)
                     (if show-missing "<ID>" "")))
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
           (ID-LEN
            (length
             (alist-get (or (ezeka-id-type (alist-get :id metadata))
                            (ezeka-kasten-id-type ezeka-deft-active-kasten)
                            :tempus)    ; assume longest
                        '((:numerus . "a-1234")
                          (:tempus  . "20210123T1234")))))
           (CAT-LEN 12)
           (KEY-LEN 10)
           ;; ID---CATEGORY---CITEKEY---TITLE [where --- is tab]
           (fmt (format "%%-%ds\t%%-%ds\t%%-%ds\t%%s" ID-LEN CAT-LEN KEY-LEN)))
      (format fmt                       ; requires 4 arguments
              id
              (if (> (length cat) CAT-LEN)
                  (concat (cl-subseq cat 0 (- CAT-LEN 1)) "…")
                cat)
              (if (> (length key) KEY-LEN)
                  (concat (cl-subseq key 0 (- KEY-LEN 1)) "…")
                key)
              title))))
(setq deft-parse-title-function 'ezeka-deft-parse-title-function)

(defalias 'ezeka-deft-file-title-id 'ezeka-file-name-id
  "Returns the ID part of `deft-file-title' of the given Zettel file.")

(defun ezeka-deft-file-title-category (file)
  "Returns the category part of `deft-file-title' of the given Zettel file."
  (--if-let (deft-file-title file)
      (cadr (split-string it "[ \t]+" nil))))

(defun ezeka-deft-file-title-citekey (file)
  "Returns the citekey part of `deft-file-title' of the given Zettel file."
  (--if-let (deft-file-title file)
      (caddr (split-string it "[ \t]+" nil " "))))

(defun ezeka-deft-file-title-title (file)
  "Returns the title part of `deft-file-title' of the given Zettel file."
  (--if-let (deft-file-title file)
      (cadddr (split-string it "[ \t]+" nil " "))))

(defun ezeka-deft-parsed-title (file)
  "Returns the result of `deft-file-title' if available or the result of
`ezeka-deft-parse-title-function' on the first line of the given FILE."
  (or (deft-file-title file)
      (ezeka-deft-parse-title-function
       (ezeka-file-content file t t))))

(defun ezeka-deft--adv-deft-new-file-maybe-named (arg)
  "Extends `deft-new-file' to call `deft-new-file-named' if called with
prefix argument."
  (interactive "p")
  (if (= arg 4)
      (call-interactively #'deft-new-file-named)
    (deft-new-file)))

;; Don't ever auto populate the title
(advice-add 'deft-auto-populate-title-maybe :around #'list)

(defun ezeka-deft-add-section-sign-to-filter ()
  "Inserts the Unicode section sign (§) to Deft filter string."
  (interactive)
  (setq last-command-event 167)
  (deft-filter-increment))

(defun ezeka-deft-filter-category (category arg)
  "Inserts a category into deft-filter if there is no category there or
changes the existing one. With prefix argument, replaces the current
`deft-filter-regexp'."
  (interactive (list (ezeka-read-category nil nil #'>)
                     current-prefix-arg))
  (deft-filter (format "{%s}" category)
    (or arg (null deft-filter-regexp))))

;;;=============================================================================
;;; Convenience
;;;=============================================================================

(defun ezeka-deft-show-link-title-in-minibuffer ()
  "Displays Zettel title of the link under cursor, less category and citekey,
in the minibuffer."
  (while-no-input
    (redisplay))
  (when (and (eq major-mode 'org-mode)
             (ezeka-link-at-point-p))
    (let* ((file (ezeka-link-file (match-string 1) t))
           (title (ezeka-deft-parsed-title file))
           (title
            (if (string-match "^\\([[:alnum:]-]+\\).*	.*	\\(.*\\)$" title)
                (format "%s / %s" (match-string 1 title) (match-string 2 title))
              title)))
      (message (s-center (window-width) title)))))
(add-hook 'ezeka-mode-hook
  (lambda ()
    (add-hook 'post-command-hook
      'ezeka-deft-show-link-title-in-minibuffer)))
(add-hook 'ezeka-mode-hook
  (lambda ()
    (add-hook 'after-save-hook
      'ezeka-deft-show-link-title-in-minibuffer)))

;;;=============================================================================
;;; Utility Commands
;;;=============================================================================

(defun ezeka-deft-org-include-cached-file ()
  "Add an org-mode #+INCLUDE to a cached Zettel."
  (interactive)
  (let* ((choices
          (delete-dups (append
                        (mapcar (lambda (path)
                                  (cons (deft-file-title path) path))
                                (ezeka-visiting-buffer-list t))
                        (ezeka-ivy-titles-reverse-alist)))))
    (if choices
        (ezeka-ivy-read-reverse-alist-action
         "Include: "
         choices
         (lambda (file)
           (let ((summary-section "Summary")
                 (snippet-section (replace-regexp-in-string
                                   "ß" "Snippet "
                                   (car (split-string
                                         (alist-get :title (ezeka-file-metadata file))
                                         ":")))))
             (insert (format "#+INCLUDE: \"%s::%s\" :only-contents t\n"
                             (file-relative-name file)
                             summary-section))
             (insert (format "#+INCLUDE: \"%s::%s\" :only-contents t"
                             (file-relative-name file)
                             snippet-section)))))
      (user-error "No Deft cache or visited Zettel"))))

(defun ezeka-deft-zmove-all-in-region (start end kasten arg)
  "Move all files listed in the active region of deft-browser to KASTEN. With
prefix argument, confirm each move and ask about destination kasten."
  (interactive (list (region-beginning)
                     (region-end)
                     (completing-read "Which kasten to move to? " ezeka-kaesten)
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
          (let* ((id (car tup))
                 (title (cdr tup))
                 (file (ezeka-link-file id)))
            (when (or (not arg)
                      (setq kasten
                        (completing-read "Which kasten to move to? " ezeka-kaesten))
                      (y-or-n-p
                       (format "[%d/%d] Move %s [%s] to %s? "
                               j (length alist) id title kasten)))
              (message "[%d/%d] Moved %s to %s in %s" j (length alist) id
                       (ezeka-zmove-to-another-kasten file kasten)
                       kasten))
            (incf j)))
        (deft-refresh)))))

(defun ezeka-deft-filter-for-link-at-point ()
  "Modifies the Deft filter to look for the Zettel linked with
the link at point. If there is only one match, opens the note in
another window."
  (interactive)
  (when (ezeka-link-at-point-p)
    (let ((link (ezeka-link-at-point))
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

(defun ezeka-deft-populate-categories ()
  "Populate `ezeka-categories' based on the titles in Deft cache."
  (interactive)
  (setq ezeka-categories '())
  (dolist (file deft-all-files)
    (let* ((category (alist-get :category (ezeka-file-metadata file)))
           (frequency (alist-get category ezeka-categories 0 nil #'string-equal)))
      (setf (alist-get category ezeka-categories nil nil #'string-equal)
            (1+ frequency))))
  (message "%d categories in %d files"
           (length ezeka-categories) (length deft-all-files)))

;;;=============================================================================
;;; TODO: Work in Progress
;;;=============================================================================

(defun ezeka-deft--file-content ()
  ((and deft-hash-contents (deft-file-contents file))
   (deft-file-contents file)))

(defun ezeka-deft--next-unused-id ()
  ((eq type active-kasten-type)
   (message "Generating next unused ID of type %s" type)
   (let ((used (mapcar #'ezeka-file-name-id deft-all-files)))
     (while (or (not id) (member id used))
       (setq id (ezeka--random-id type))))))

(defun ezeka-deft--kill-ring-save-link-title ()
  (when (and deft-hash-contents (deft-file-contents file))
    (deft-cache-update-file file)))

(defun ezeka-deft--formatted-frame-title ()
  ;; Added in the beginning
  (if ezeka-deft-active-kasten
      (format "〔%s〕"
              (upcase ezeka-deft-active-kasten))
    ""))

;;;=============================================================================
;;; Internal
;;;=============================================================================

(defun ezeka-deft--adv-deft-new-file-insert-metadata (orig-fun id)
  "Replaces deft's default behavior of putting the filter string
on the first line with the Zettel title string."
  ;; `DEFT-NEW-FILE-NAMED' returns either a string (from MESSAGE) about an
  ;; error, or the result of (GOTO-CHAR (POINT-MAX)), which means an integer
  ;; buffer location.
  (when (integerp (funcall orig-fun id))
    (let ((file (deft-absolute-filename id)))
      (with-current-buffer (get-file-buffer file)
        (erase-buffer)
        (call-interactively #'ezeka-insert-header-template)))))
(advice-add 'deft-new-file-named :around #'ezeka-deft--adv-deft-new-file-insert-metadata)

(defun ezeka-deft--adv-deft-absolute-filename (orig-fun &rest args)
  "Replaces the default `deft-absolute-filename' with
`ezeka-link-file'."
  (let ((kasten (ezeka-directory-kasten deft-directory)))
    (ezeka-link-file
     (if kasten
         (concat kasten ":" (car args))
       (car args)))))
(advice-add 'deft-absolute-filename :around 'ezeka-deft--adv-deft-absolute-filename)

;;
;; Sorting deft-buffer by filename
;;
(defun ezeka-deft-sort-files-by-name (files)
  "Sort FILES by name in reverse order, ignoring case."
  (sort files (lambda (f1 f2)
                (funcall (if ezeka-deft-sort-by-name-descending
                             #'string-lessp
                           #'string-greaterp)
                         (downcase (file-name-base f2))
                         (downcase (file-name-base f1))))))

(defun ezeka-deft-title-lessp (file1 file2)
  "Return non-nil if the Zettel title of FILE1 is lexicographically less than
that of FILE2. Case is ignored."
  ;; FIXME: Hack to get the title from the result of
  ;; `ezeka-deft-parse-title-function'
  (let ((title1 (caddr (split-string (or (deft-file-title file1) "") "  +")))
        (title2 (caddr (split-string (or (deft-file-title file2) "") "  +"))))
    (string-lessp (downcase (or title1 ""))
                  (downcase (or title2 "")))))

(defun ezeka-deft-sort-files-by-title (files)
  "Sort FILES by the Zettel title."
  (sort files 'ezeka-deft-title-lessp))

(eval-after-load "deft"
  '(progn
     ;; Use our own `ezeka-deft-sort-files-by-name' rather that respects
     ;; `ezeka-deft-sort-by-name-descending'.
     (defalias 'deft-sort-files-by-title 'ezeka-deft-sort-files-by-name)
     ;; "Shadow" the built-in id generator to generate timestamps by default,
     ;; i.e. when DEFT-NEW-FILE is called (C-c C-n)
     (defalias 'deft-unused-slug 'ezeka--generate-id)))

;; Having a visual indicator of the sort method is helpful
(defun ezeka-deft-set-mode-name ()
  "Set the mode line text based on search mode."
  (setq mode-name
    (format "Deft[%s]%s"
            deft-current-sort-method
            (if deft-incremental-search "" "/R"))))
(advice-add 'deft-toggle-sort-method :after 'ezeka-deft-set-mode-name)

(defun ezeka-deft--adv-deft-open-button (orig-fun &rest args)
  "Advice :around `deft-open-button' to call `ezeka-find-link' instead of
`deft-open-file'."
  (ezeka-find-file (button-get (car args) 'tag) current-prefix-arg))
(advice-add 'deft-open-button :around #'ezeka-deft--adv-deft-open-button)

;; On save, update deft-cache
(add-hook 'ezeka-mode-hook
  (lambda ()
    (add-hook 'after-save-hook
      (lambda ()
        (deft-cache-file buffer-file-name))
      nil t)))

;;-----------------------------------------------------------------------------
;; Deft-Mode Keybindings
;;-----------------------------------------------------------------------------

(define-key deft-mode-map (kbd "C-c C-S-n") 'ezeka-deft-new-unused-note)
(define-key deft-mode-map (kbd "C-c s") 'ezeka-deft-add-section-sign-to-filter)
(define-key deft-mode-map (kbd "C-c C-n") 'deft-new-file-named)
(define-key deft-mode-map (kbd "C-c C-o") 'push-button)
(define-key deft-mode-map (kbd "C-c #") 'ezeka-kill-ring-save-link)
(define-key deft-mode-map (kbd "C-c C-s") 'ezeka-select-and-find-link) ; was: `deft-toggle-sort-method'
(define-key deft-mode-map (kbd "C-c C-f") 'ezeka-select-and-find-link) ; was: `deft-find-file'
(define-key deft-mode-map (kbd "C-c C-'") 'ezeka-deft-filter-category)
(define-key deft-mode-map (kbd "C-c C-p") 'ezeka-deft-populate-categories)
(define-key deft-mode-map (kbd "C-c C-x l") 'ezeka-links-to)

(provide 'ezeka-deft)
