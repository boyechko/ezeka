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

(defvar ezeka-breadcrumb-trail nil
  "Overlay object pointing to the current breadcrumb trail.")

(defcustom ezeka-leave-breadcrumb-trail t
  "When non-nil, leave a trail of visited Zettel notes."
  :type 'boolean
  :group 'ezeka)

(defcustom ezeka-breadcrumb-trail-headline "Breadcrumbs"
  "Exact name of the breadcrumb trail headline."
  :type 'string
  :group 'ezeka)

(defcustom ezeka-breadcrumb-record-source nil
  "If non-nil, include the breadcrumb source in parentheses."
  :type 'boolean
  :group 'ezeka)

(defcustom ezeka-breadcrumb-find-trail-function #'ezeka-breadcrumb-find-linear-trail
  "Function called with TARGET and SROUCE to find the trail.
TARGET and SOURCE should be strings or symbols. Return
'primary or 'secondary if the trail was found (i.e. drop
breadcrumbs here), or nil if can't locate trail (i.e. don't
drop breadcrumbs).")

(defun ezeka-breadcrumb-find-trail (target source)
  "Call `ezeka-breadcrumb-find-trail-function' on TARGET and SOURCE."
  (funcall ezeka-breadcrumb-find-trail-function target source))

(defun ezeka--goto-breadcrumb-head ()
  "Go to the head of the current breadcrumb trail.
Return the position on success, otherwise NIL."
  (let ((pos (overlay-end ezeka-breadcrumb-trail)))
    (when pos
      (goto-char pos))))

(defun ezeka--insert-heading-after-current (level)
  "Insert `org-mode' heading of LEVEL after the current one."
  (end-of-line)
  (newline)
  (insert (make-string level ?*) " "))

(defun ezeka--update-breadcrumb-heading (target source)
  "Update the breadcrumb heading for TARGET (from SOURCE)."
  (save-excursion
    (beginning-of-line)
    (when (re-search-forward "\\(?1:\\*+\\) \\(.*\\<nil\\>.*\\)$" (point-at-eol) 'noerror)
      (replace-match
       (format "%s %s"
               (match-string-no-properties 1)
               (ezeka--breadcrumb-string target source))))))

(defun ezeka-breadcrumb-find-arboreal-trail (target source)
  "Find where to drop breadcrumbs in an arboreal trail.
See `ezeka-breadcrumb-find-trail-function' for details about
TARGET and SOURCE."
  (let ((target (cond ((null target) (error "Target is null"))
                      ((ezeka-file-p target) (ezeka-file-link target))
                      ((ezeka-link-p target) target)
                      ((symbolp target) (symbol-name target))
                      (t target)))
        (source (cond ((ezeka-file-p source) (ezeka-file-link source))
                      ((ezeka-link-p source) source)
                      ((symbolp source) (symbol-name source))
                      (t source))))
    (with-current-buffer (overlay-buffer ezeka-breadcrumb-trail)
      (save-restriction
        (goto-char (overlay-end ezeka-breadcrumb-trail))
        (org-narrow-to-subtree)
        (let ((org-blank-before-new-entry '((heading . nil)))
              (head-level (org-current-level)))
          (cond ((and (stringp source)
                      (search-forward source nil t)
                      (org-at-heading-p))
                 ;; Breadcrumb for SOURCE found, so add one for TARGET
                 (let ((src-level (car (org-heading-components)))
                       (src-head (point)))
                   (cond ((and (search-forward target nil t)
                               (org-at-heading-p))
                          (ezeka--update-breadcrumb-heading target source)
                          (message "Breadcrumb for %s already exists under %s"
                                   target
                                   source)
                          nil)
                         (t
                          (goto-char src-head)
                          (ezeka--insert-heading-after-current (1+ src-level))
                          'secondary))))
                ((and (goto-char (point-min))
                      (search-forward target nil 'noerror)
                      (org-at-heading-p))
                 ;; Breadcrumbs already dropped for TARGET
                 (ezeka--update-breadcrumb-heading target source)
                 (when (and nil
                            (y-or-n-p (format "Breadcrumbs already exist for %s. Visit it?" target)))
                   (point-marker)))
                (t
                 ;; No breadcrumbs dropped for TARGET
                 (org-end-of-subtree)
                 (ezeka--insert-heading-after-current (1+ head-level))
                 'primary)))))))

(defun ezeka-breadcrumb-find-linear-trail (target source)
  "Find where to drop breadcrumbs on a linear trail.
See `ezeka-breadcrumb-find-trail-function' for details about
TARGET and SOURCE."
  (let ((target (cond ((null target) (error "Target is null"))
                      ((ezeka-file-p target) (ezeka-file-link target))
                      ((ezeka-link-p target) target)
                      ((symbolp target) (symbol-name target))
                      (t target)))
        (source (cond ((ezeka-file-p source) (ezeka-file-link source))
                      ((ezeka-link-p source) source)
                      ((symbolp source) (symbol-name source))
                      (t source))))
    (save-restriction
      (ezeka--goto-breadcrumb-head)
      (org-narrow-to-subtree)
      (let ((org-blank-before-new-entry '((heading . nil)))
            (head-level (org-current-level)))
        (cond ((search-forward target nil t)
               ;; Breadcrumbs already dropped for TARGET
               (message "Breadcrumbs already exist for %s" target)
               nil)
              (t
               ;; No breadcrumbs dropped for TARGET
               (org-end-of-subtree)
               (ezeka--insert-heading-after-current (1+ head-level))
               'primary))))))

(defun ezeka--breadcrumb-string (target source &optional comment)
  "Return a breadcrumb string for TARGET from SOURCE.
Optionally, add COMMENT after TARGET."
  (save-match-data
    (let* ((t-file (cond ((ezeka-file-p target) target)
                         ((ezeka-link-p target) (ezeka-link-file target))))
           (s-file (cond ((ezeka-file-p source) source)
                         ((ezeka-link-p source) (ezeka-link-file source))))
           (timestamp (format-time-string (cdr org-time-stamp-formats))))
      (ezeka--concat-strings " "
        (if t-file
            (or (ezeka-format-file-name "{%l} %c [[%i]]" t-file)
                (format "%s" (file-name-nondirectory target)))
          target)
        comment
        ;; TODO Implement `format-spec' for breadcrumb strings?
        ;; Perhaps having different entries for ezeka and non-ezeka targets?
        (when (and source ezeka-breadcrumb-record-source)
          (format "(%s)"
                  (cond (s-file (ezeka-file-name-id s-file))
                        ((stringp source) (file-name-nondirectory source))
                        (t source))))))))

;;;###autoload
(defun ezeka-breadcrumbs-drop (&optional target source)
  "Add the Zettel TARGET to `ezeka-breadcrumb-trail'.
TARGET should be a Zettel filename; SOURCE can be one too,
or a symbol describing where the function is being called
from."
  (interactive (list buffer-file-name 'interactive))
  (when ezeka-leave-breadcrumb-trail
    (if-let ((trail ezeka-breadcrumb-trail)
             (trail-buf (overlay-buffer trail)))
        (let* ((inhibit-read-only t)
               (problem nil)
               (t-file (cond ((ezeka-file-p target) target)
                             ((ezeka-link-p target) (ezeka-link-file target))
                             ((and (null target) (ezeka-file-p buffer-file-name))
                              buffer-file-name)
                             (t nil)))
               (s-file (cond ((ezeka-file-p source) source)
                             ((ezeka-link-p source) (ezeka-link-file source))
                             (t nil))))
          (cond ((ezeka-same-file-p t-file (buffer-file-name trail-buf))
                 (setq problem "same Zettel"))
                ((ezeka-same-file-p s-file (buffer-file-name trail-buf))
                 ;; FIXME: There has to be a better way to do this
                 (setq s-file nil)))
          (if problem
              (message "Could not drop breadcrumbs for `%s' (from %s): %s"
                       (if t-file (ezeka-file-link t-file) target)
                       (if s-file (ezeka-file-link s-file) source)
                       problem)
            (let ((status (ezeka-breadcrumb-find-trail target source)))
              (pcase status
                ((pred null)
                 nil)
                ((pred symbolp)
                 (with-current-buffer (overlay-buffer ezeka-breadcrumb-trail)
                   (insert (ezeka--breadcrumb-string target source))
                   (message "Dropped breadcrumbs for `%s' as %s"
                            (ezeka-file-name-id t-file)
                            status)))
                ((pred markerp)
                 (pop-to-buffer (overlay-buffer ezeka-breadcrumb-trail))
                 (goto-char (marker-position status)))))))
      (setq ezeka-breadcrumb-trail nil)
      (unless (y-or-n-p "There is no active breadcrumb trail. Continue anyway? ")
        (user-error "There is no active breadcrumb trail")))))

;;; TODO: Since this is needed to actually drop breadcrumbs, the breadcrumb
;;; dropping should perhaps be a minor mode?
(add-hook 'ezeka-find-file-functions #'ezeka-breadcrumbs-drop)

(defvar ezeka--breadcrumbs-xref-format "Find Regexp: %s"
  "Format-like string for dropping bookmarks from *xref* buffer.
The control sequence %s is replaced with the xref search string.")

(defun ezeka--breadcrumbs-xref-advice (&rest _)
  "Before advice for `xref-goto-xref' to drop breadcrumbs from xref buffer."
  (when-let* ((xref (xref--item-at-point))
              (file (xref-file-location-file (xref-item-location xref)))
              (_ (ezeka-file-p file)))
    (ezeka-breadcrumbs-drop file
                            (format ezeka--breadcrumbs-xref-format
                                    (ezeka--breadcrumbs-xref-search-string)))))

(defun ezeka--breadcrumbs-xref-search-string ()
  "Return xref search string."
  (save-excursion
    (goto-char (point-min))
    (when-let ((match (text-property-search-forward 'face 'xref-match t)))
      (downcase (buffer-substring-no-properties
                 (prop-match-beginning match) (prop-match-end match))))))

(defun ezeka--breadcrumbs-elisp-target ()
  "Return a breadcrumbs target for the current Emacs Lisp function."
  (when-let ((defname (rb-kill-ring-save-def-name)))
    (format "[[elisp:(find-function '%s)][%s]]" defname defname)))

(defun ezeka--breadcrumbs-buffer-target ()
  "Return a breadcrumbs target for the current buffer."
  (if buffer-file-name
      (format "[[file:%s::%s][%s]]"
              buffer-file-name
              (string-trim
               (buffer-substring-no-properties (point-at-bol) (point-at-eol)))
              (file-name-nondirectory buffer-file-name))
    (format "\"%s\"" (buffer-name))))

;;;###autoload
(defun ezeka-breadcrumbs-drop-external (source &optional comment)
  "Drop breadcrumbs for the current external location.
SOURCE should be a string or symbol; COMMENT can be a short string."
  (interactive
   (list (buffer-file-name
          (get-buffer (read-buffer "How did you get here? " nil t)))))
  (unless (or (null ezeka-breadcrumb-trail)
              (not (overlay-buffer ezeka-breadcrumb-trail)))
    (let ((inhibit-read-only t)
          (target (or (ezeka--breadcrumbs-elisp-target)
                      (ezeka--breadcrumbs-buffer-target))))
      (with-current-buffer (overlay-buffer ezeka-breadcrumb-trail)
        (save-excursion
          (when-let ((status (ezeka-breadcrumb-find-trail target source)))
            (insert (ezeka--breadcrumb-string target source comment))
            (message "Dropped breadcrumbs from `%s' as %s"
                     (file-name-nondirectory source)
                     status)))))))

(defun ezeka--find-function-drop-breadcrumbs (&rest _)
  "After advice for `find-function' to drop breadcrumbs."
  (ezeka-breadcrumbs-drop-external (buffer-name (current-buffer))))
;; (advice-add 'find-function :after 'ezeka--find-function-drop-breadcrumbs)
;; (advice-remove 'find-function 'ezeka--find-function-drop-breadcrumbs)

;;;###autoload
(defun ezeka-start-breadcrumb-trail (file)
  "Start a new breadcrumb trail in FILE at the current heading."
  (interactive
   (list (let ((ezeka-leave-breadcrumb-trail nil))
           (if ezeka-mode
               buffer-file-name
             (user-error "Point must be on an org-mode heading")))))
  (save-excursion
    (with-current-buffer (find-file-noselect file)
      (unless (org-at-heading-p)
        (if (not (y-or-n-p (format "Insert `%s' heading here? "
                                   ezeka-breadcrumb-trail-headline)))
            (user-error "Move to desired heading first")
          (ezeka--insert-heading-after-current (1+ (or (org-current-level) 0)))
          (insert ezeka-breadcrumb-trail-headline)))
      (ezeka-reset-breadcrumb-trail)
      (setq ezeka-breadcrumb-trail (make-overlay (point-at-eol) (point-at-eol)))
      (overlay-put ezeka-breadcrumb-trail 'type 'ezeka-breadcrumb-trail)
      (overlay-put ezeka-breadcrumb-trail 'after-string " (🍞)")
      (add-hook 'kill-buffer-hook #'ezeka-reset-breadcrumb-trail nil t)
      (message "Breadcrumbs will be dropped in `%s'" (file-name-base file)))))

;; TODO: Any way to mark the breadcrumb heading and buffer?
(defun ezeka-reset-breadcrumb-trail ()
  "Stop dropping breadcrumbs on this trail."
  (interactive)
  (when (overlayp ezeka-breadcrumb-trail)
    (message "Breadcrumbs will no longer be dropped in `%s'"
             (overlay-buffer ezeka-breadcrumb-trail))
    (delete-overlay ezeka-breadcrumb-trail))
  (setq ezeka-breadcrumb-trail nil))

(defun ezeka-switch-to-breadcrumb-trail ()
  "Switch to the buffer of the current breadcrumb trail."
  (interactive)
  (when ezeka-breadcrumb-trail
    (pop-to-buffer (overlay-buffer ezeka-breadcrumb-trail))
    (ezeka--goto-breadcrumb-head)))

(defun ezeka-breadcrumb-trail-dispatch (arg)
  "Start, switch to, or reset breadcrumb trail based on ARG."
  (interactive "p")
  (pcase arg
    (1 (if (and (org-at-heading-p)
                (string= (nth 4 (org-heading-components))
                         ezeka-breadcrumb-trail-headline))
           (call-interactively 'ezeka-start-breadcrumb-trail)
         (or (ezeka-switch-to-breadcrumb-trail)
             (call-interactively 'ezeka-start-breadcrumb-trail))))
    (4 (ezeka-reset-breadcrumb-trail))
    (_ (user-error "Not a valid option"))))

(provide 'ezeka-breadcrumbs)
;;; ezeka-breadcrumbs.el ends here
