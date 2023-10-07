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
(require 'org)

(defvar ezeka-breadcrumb-trail-buffer nil
  "Buffer where to record the breadcrumb trail.")

(defcustom ezeka-leave-breadcrumb-trail t
  "When non-nil, leave a trail of visited Zettel notes.")

(defvar ezeka--breadcrumb-trail nil
  "Stack of breadcrumbs dropped.
The variable is reset whenever new trail is started with
`ezeka-start-breadcrumb-trail'.")

(defun ezeka-reset-breakcrumb-trail ()
  "Reset the breadcrumb stack."
  (setq ezeka--breadcrumb-trail nil))

;;; FIXME: Why does this stop working after midnight?!
;;;###autoload
(defun ezeka-breadcrumbs-drop (&optional target source)
  "Add the Zettel TARGET to `ezeka-breadcrumb-trail-buffer'.
SOURCE, if set, should be either a Zettel link for the
source, or a symbol describing where the function is being
called from. If the command is executed interactively,
the SOURCE is set to 'interactive."
  (interactive (list buffer-file-name 'interactive))
  (let ((target (cond ((ezeka-note-p target) target)
                      ((ezeka-link-p target) (ezeka-link-file target))
                      (t (buffer-file-name (current-buffer)))))
        (timestamp (format-time-string (cdr org-time-stamp-formats))))
    (when (and ezeka-leave-breadcrumb-trail
               (or (eq source 'interactive)
                   (not (cl-member (ezeka-file-link target)
                                   ezeka--breadcrumb-trail
                                   :key #'car
                                   :test #'string=))
                   (not (and (boundp 'ezeka-breadcrumb-trail-buffer)
                             (string= (buffer-file-name ezeka-breadcrumb-trail-buffer)
                                      target))))
               (file-exists-p target)
               (ezeka-note-p target))
      (when (and (not (buffer-live-p ezeka-breadcrumb-trail-buffer))
                 (y-or-n-p "There is no breadcrumb trail. Start one? "))
        (call-interactively #'ezeka-start-breadcrumb-trail))
      (if-let ((_ (and (boundp 'ezeka-breadcrumb-trail-buffer)
                       (buffer-live-p ezeka-breadcrumb-trail-buffer)))
               (org-blank-before-new-entry '((heading . nil))))
          (save-excursion
            (with-current-buffer ezeka-breadcrumb-trail-buffer
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
                      ezeka--breadcrumb-trail)
                (message "Dropped breadcrumbs for `%s'" (file-name-base target)))))
        (message "Did not drop breadcrumbs for `%s'" (file-name-base target))))))

;;; TODO: Since this is needed to actually drop breadcrumbs, the breadcrumb
;;; dropping should perhaps be a minor mode?
(add-hook 'ezeka-find-file-functions #'ezeka-breadcrumbs-drop)
(add-hook 'ezeka-mode-hook #'ezeka-breadcrumbs-drop)

;;;###autoload
(defun ezeka-start-breadcrumb-trail (file)
  "Start a new breadcrumb trail in Zettel FILE.
If called interactively, use the current file."
  (interactive
   (list (let ((ezeka-leave-breadcrumb-trail nil))
           (ezeka-zk-find-note-in-tempus 'other-window)
           buffer-file-name)))
  (setq ezeka--breadcrumb-trail nil
        ezeka-breadcrumb-trail-buffer (find-file-noselect file))
  (with-current-buffer ezeka-breadcrumb-trail-buffer
   (if-let ((head (org-find-exact-headline-in-buffer "Breadcrumbs")))
       (goto-char head)
     (goto-char (point-max))
     (org-insert-heading nil nil 'top)
     (insert "Breadcrumbs"))))

(provide 'ezeka-breadcrumbs)
;;; ezeka-breadcrumbs.el ends here
