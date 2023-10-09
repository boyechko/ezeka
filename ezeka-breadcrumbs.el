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
  "When non-nil, leave a trail of visited Zettel notes."
  :type 'boolean
  :group 'ezeka)

(defcustom ezeka-breadcrumb-trail-headline "Breadcrumbs"
  "Exact name of the breadcrumb trail headline."
  :type 'string
  :group 'ezeka)

(defun ezeka--find-breadcrumb-trail (source)
  "Find the place in the current buffer where to drop breadcrumbs.
SOURCE should be a filename. Return T if an existing heading
exists for SOURCE, and nil otherwise."
  (save-restriction
    (let ((org-blank-before-new-entry '((heading . nil)))
          (headline (org-find-exact-headline-in-buffer
                     ezeka-breadcrumb-trail-headline)))
      ;; 1) Get positioned in the ezeka-breadcrumb-trail-headline subtree
      (cond (headline
             (goto-char headline)
             (org-narrow-to-subtree)
             (org-back-to-heading))
            (t
             (goto-char (point-max))
             (org-insert-heading-after-current)
             (insert ezeka-breadcrumb-trail-headline)))
      ;; 2) Try to find an existing SOURCE headline
      (cond ((and source
                  (search-forward (ezeka--format-link source) nil t)
                  (eq 'headline (car (org-element-at-point))))
             (end-of-line)
             (org-insert-heading-after-current)
             (org-demote-subtree)
             t)
            (t
             (org-end-of-subtree)
             (org-insert-heading nil nil 'top)
             (org-demote-subtree)
             nil)))))

(defun ezeka--breadcrumb-string (target source)
  "Return a breadcrumb string for TARGET from SOURCE."
  (let ((mdata (ezeka-file-metadata target 'noerror))
        (timestamp (format-time-string (cdr org-time-stamp-formats))))
    (concat (or (alist-get :title mdata)
                (ezeka-file-name-caption target))
            " "
            (ezeka--format-link (ezeka-file-name-id target))
            (when source (format " (%s)"
                                 (if (stringp source)
                                     (ezeka-file-name-id source)
                                   source))))))

;;;###autoload
(defun ezeka-breadcrumbs-drop (&optional target source)
  "Add the Zettel TARGET to `ezeka-breadcrumb-trail-buffer'.
TARGET should be a Zettel filename; SOURCE can be one too,
or a symbol describing where the function is being called
from."
  (interactive (list buffer-file-name 'interactive))
  (when ezeka-leave-breadcrumb-trail
    (let* ((problem nil)
           (t-file (cond ((and (null target) (ezeka-file-p buffer-file-name))
                          buffer-file-name)
                         ((ezeka-link-p target) (ezeka-link-file target))
                         ((ezeka-file-p target) target)
                         (t
                          (setq problem "target is not a Zettel file"))))
           (s-file (cond ((ezeka-link-p source) (ezeka-link-file source))
                         ((ezeka-file-p source) source)
                         (t                     nil))))
      (when (and (not (buffer-live-p ezeka-breadcrumb-trail-buffer))
                 (y-or-n-p "There is no breadcrumb trail. Start one? "))
        (call-interactively #'ezeka-start-breadcrumb-trail))
      (unless (buffer-live-p ezeka-breadcrumb-trail-buffer)
        (setq problem "No active breadcrumb trail"))
      (if problem
          (message "Could not drop breadcrumbs for `%s' (from %s):"
                   (file-name-base target)
                   source
                   problem)
        (with-current-buffer ezeka-breadcrumb-trail-buffer
          (save-excursion
            (let ((s-head (unless
                              (ezeka-same-file-p s-file
                                                 (buffer-file-name
                                                  ezeka-breadcrumb-trail-buffer))
                            (ezeka--find-breadcrumb-trail s-file))))
              (insert (ezeka--breadcrumb-string t-file (unless s-head source)))
              (message "Dropped breadcrumbs for `%s'"
                       (ezeka-file-name-id t-file)))))))))

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
  (setq ezeka-breadcrumb-trail-buffer (find-file-noselect file))
  (with-current-buffer ezeka-breadcrumb-trail-buffer
   (if-let ((head (org-find-exact-headline-in-buffer ezeka-breadcrumb-trail-headline)))
       (goto-char head)
     (goto-char (point-max))
     (org-insert-heading nil nil 'top)
     (insert ezeka-breadcrumb-trail-headline))))

(provide 'ezeka-breadcrumbs)
;;; ezeka-breadcrumbs.el ends here
