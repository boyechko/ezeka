;;; ezeka-breadcrumbs.el --- Create breadcrumb trails -*- lexical-binding: t -*-

;; Copyright (C) 2023 Richard Boyechko

;; Author: Richard Boyechko <code@diachronic.net>
;; Created: 2023-07-24
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

;; Automatically create breadcrumb trails when visiting Zettel notes.

;;; Code:

(require 'ezeka)

(defcustom ezeka-zk-drop-breadcrumbs t
  "When non-nil, record visited notes into current Zk-Desktop.")

(defvar ezeka-zk--breadcrumbs-stack nil
  "Stack of breadcrumbs dropped.
The variable is reset whenever `ezeka-zk-initialize-desktop'
is executed.")

(defun ezeka-zk-breakcrumbs-reset-stack ()
  "Reset the breadcrumb stack."
  (setq ezeka-zk--breadcrumbs-stack nil))

;;; FIXME: Why does this stop working after midnight?!
;;;###autoload
(defun ezeka-zk-drop-breadcrumbs (&optional target source)
  "Add the Zettel TARGET to the current `zk-desktop'.
SOURCE, if set, should be either a Zettel link for the
source, or a symbol describing where the function is being
called from. If the command is executed interactively,
the SOURCE is set to 'interactive."
  (interactive (list buffer-file-name 'interactive))
  (let ((target (cond ((ezeka-note-p target) target)
                      ((ezeka-link-p target) (ezeka-link-file target))
                      (t (buffer-file-name (current-buffer)))))
        (timestamp (format-time-string (cdr org-time-stamp-formats))))
    (when (and ezeka-zk-drop-breadcrumbs
               (or (eq source 'interactive)
                   (not (cl-member (ezeka-file-link target)
                                   ezeka-zk--breadcrumbs-stack
                                   :key #'car
                                   :test #'string=))
                   (not (and (boundp 'zk-desktop-current)
                             (string= (buffer-file-name zk-desktop-current)
                                      target))))
               (file-exists-p target)
               (ezeka-note-p target))
      (when (and (not (and (boundp 'zk-desktop-current) ; FIXME: Add our own variable?
                           (buffer-live-p zk-desktop-current)))
                 (y-or-n-p "No Zk-Desktop. Create one? "))
        (ezeka-zk-initialize-desktop))
      (if-let ((_ (and (boundp 'zk-desktop-current)
                       (buffer-live-p zk-desktop-current)))
               (org-blank-before-new-entry '((heading . nil))))
          (save-excursion
            (with-current-buffer zk-desktop-current
              (goto-char (org-find-exact-headline-in-buffer "Breadcrumbs"))
              (let ((headline (when (stringp source)
                                (search-forward (ezeka--format-link source) nil t))))
                (cond (headline
                       (end-of-line)
                       (org-insert-heading-after-current)
                       (org-demote-subtree))
                      (t
                       (if (org-forward-heading-same-level 1)
                           (org-previous-visible-heading 1)
                         (goto-char (point-max)))
                       (org-insert-subheading nil)))
                (insert (format "%s [[%s]] %s%s"
                                (if-let ((mdata (ezeka-file-metadata target)))
                                    (alist-get :title mdata)
                                  (ezeka-file-name-caption target))
                                (ezeka-file-name-id target)
                                timestamp
                                (if (not headline)
                                    (format " (from %s)"
                                            (cond ((symbolp source)
                                                   source)
                                                  ((ezeka-note-p source)
                                                   (ezeka--format-link source))
                                                  ((stringp source)
                                                   (file-name-base source))
                                                  (t
                                                   source)))
                                  "")))
                (push (list (ezeka-file-link target) timestamp source)
                      ezeka-zk--breadcrumbs-stack)
                (message "Dropped breadcrumbs for `%s'" (file-name-base target)))))
        (message "Did not drop breadcrumbs for `%s'" (file-name-base target))))))

;;; TODO: Since this is needed to actually drop breadcrumbs, the breadcrumb
;;; dropping should perhaps be a minor mode?
(add-hook 'ezeka-find-file-functions #'ezeka-zk-drop-breadcrumbs)
(add-hook 'ezeka-mode-hook #'ezeka-zk-drop-breadcrumbs)

(defun ezeka-zk-stage-links-in-subtree (&optional start end)
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

;;;###autoload
(defun ezeka-zk-initialize-desktop (&optional title)
  "Set `zk-desktop-current' to today's desktop with TITLE.
If the current buffer is a Zettel file, ask about using
that; otherwise, create a new one."
  (interactive)
  (let ((new-id (ezeka-format-tempus-currens))
        (ezeka-create-nonexistent-links t)
        desktop-file)
    (if (and (ezeka-note-p buffer-file-name)
             (y-or-n-p "Set this as the desktop file? "))
        (setq zk-desktop-current (current-buffer))
      (when (and (ezeka-note-p buffer-file-name)
                 (y-or-n-p "Treat current file as parent? "))
        (ezeka--replace-file-header buffer-file-name
                                    (ezeka-set-metadata-value
                                     (ezeka-file-metadata buffer-file-name)
                                     :firstborn new-id))
        (ezeka--set-new-child-metadata
         new-id :parent (ezeka-file-link buffer-file-name)))
      (ezeka--set-new-child-metadata
       new-id
       :label "Journal"
       :title (read-string "Title for new desktop file: "
                           (format "%s for %s%s"
                                   zk-desktop-basename
                                   (format-time-string "%A, %B %-d")
                                   (ezeka--ordinal-suffix
                                    (decoded-time-day (decode-time))))))
      (ezeka-find-link new-id)
      (setq zk-desktop-current (current-buffer)))
    (setq ezeka-zk--breadcrumbs-stack nil)
    (message "Zk-Desktop initialized to %s" (current-buffer))))

(provide 'ezeka-breadcrumbs)
;;; ezeka-breadcrumbs.el ends here
