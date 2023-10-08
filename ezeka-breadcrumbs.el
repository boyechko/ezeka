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

(defun ezeka--breadcrumb-trail-end (source)
  "Find the place in the current buffer where to drop breadcrumbs.
SOURCE is passed from `ezeka-breadcrumbs-drop'. Return non-
nil if an existing SOURCE headline was found, and nil
otherwise."
  (save-restriction
    (when-let ((org-blank-before-new-entry '((heading . nil)))
               (heading (org-find-exact-headline-in-buffer "Breadcrumbs")))
      (goto-char heading)
      (org-narrow-to-subtree)
      (org-back-to-heading)
      (if (and (stringp source)
               (search-forward (ezeka--format-link source) nil t))
          (progn
            (end-of-line)
            (org-insert-heading-after-current)
            (org-demote-subtree)
            t)
        (org-end-of-subtree)
        (org-insert-heading nil nil 'top)
        (org-demote-subtree)
        nil))))

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
SOURCE, if set, should be either a Zettel link for the
source, or a symbol describing where the function is being
called from. If the command is executed interactively,
the SOURCE is set to 'interactive."
  (interactive (list buffer-file-name 'interactive))
  (let ((t-file (pcase target
                  ('nil                buffer-file-name)
                  ((pred ezeka-file-p) target)
                  ((pred ezeka-link-p) (ezeka-link-file target))
                  (_                    nil)))
        (s-file (pcase source
                  ((pred ezeka-file-p) source)
                  ((pred ezeka-link-p) (ezeka-link-file source))
                  (_                    nil))))
    (when ezeka-leave-breadcrumb-trail
      (when (and (not (buffer-live-p ezeka-breadcrumb-trail-buffer))
                 (y-or-n-p "There is no breadcrumb trail. Start one? "))
        (call-interactively #'ezeka-start-breadcrumb-trail))
      (if (or (null t-file)
              (not (buffer-live-p ezeka-breadcrumb-trail-buffer)))
          (message "Could not drop breadcrumbs for `%s'" (file-name-base target))
        (with-current-buffer ezeka-breadcrumb-trail-buffer
          (save-excursion
            (let ((trail-end (ezeka--breadcrumb-trail-end source)))
              ;; FIXME: What happens when trail-end can't be found?
              (insert (ezeka--breadcrumb-string t-file (unless trail-end source)))
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
   (if-let ((head (org-find-exact-headline-in-buffer "Breadcrumbs")))
       (goto-char head)
     (goto-char (point-max))
     (org-insert-heading nil nil 'top)
     (insert "Breadcrumbs"))))

(provide 'ezeka-breadcrumbs)
;;; ezeka-breadcrumbs.el ends here
