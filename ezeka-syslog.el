;;; ezeka-syslog.el --- System log for EZeKa -*- lexical-binding: t -*-

;; Copyright (C) 2025 Richard Boyechko

;; Author: Richard Boyechko <code@diachronic.net>
;; Created: 2025-04-20
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

;;

;;; Code:

(require 'ezeka-base)
(require 'json)

(defvar ezeka--system-log-file "auto/system.log"
  "Path, relative to `ezeka-directory', to log everything.")

;; Source: `cl--plist-to-alist'
(defun ezeka--plist-to-alist (plist)
  "Given PLIST, return an equivalent alist.
If PLIST is already an alist, leave it alone."
  (pcase (car plist)
    ((pred symbolp)
     (let ((res '()))
       (while plist
         (push (cons (pop plist) (pop plist)) res))
       (nreverse res)))
    ((pred consp) plist)))

(defun ezeka--system-log-record (time action &rest props)
  "Create a system log record with TIME, ACTION, and PROPS."
  (append `((time . ,(ezeka--iso8601-time-string time))
            (action . ,action))
          (ezeka--plist-to-alist props)))

(ert-deftest ezeka--system-log-record ()
  (should (ezeka--system-log-record nil 'update-modified 'note "a-1234"))
  (should (ezeka--system-log-record
           (parse-time-string "2024-01-01T00:00:00")
           'update-modified 'note "a-1234")))

(defun ezeka--system-log-repeat-record-p (object previous)
  "Return non-nil if OBJECT and PREVIOUS differ only in time."
  (let ((object (cl-remove 'time object :key #'car))
        (previous (cl-remove 'time previous :key #'car)))
    (cl-every #'equal object previous)))

(ert-deftest ezeka--system-log-repeat-record-p ()
  (should (ezeka--system-log-repeat-record-p
           (ezeka--system-log-record nil 'update-modified 'note "a-1234")
           (ezeka--system-log-record
            (parse-time-string "2024-01-01T00:00:00")
            'update-modified 'note "a-1234"))))

(defun ezeka--add-to-system-log (action time &rest props)
  "Add a log entry for ACTION at TIME (nil for now) with PROPS.
PROPS should be either a plist or an alist."
  (declare (indent 2))
  (let* ((time (or time (current-time)))
         (record (apply #'ezeka--system-log-record time action props))
         (json (json-encode record))
         (logfile (expand-file-name ezeka--system-log-file ezeka-directory))
         (logbuf (find-file-noselect logfile)))
    (with-current-buffer logbuf
      (goto-char (point-max))
      (if-let* ((_ (re-search-backward "^{" nil 'noerror))
                (previous (json-read))
                (_ (ezeka--system-log-repeat-record-p
                    (json-read-from-string json)
                    previous)))
          (goto-char (point-max))
        (insert "\n" json "\n"))
      (delete-trailing-whitespace)
      (save-buffer))))

(ert-deftest ezeka--add-to-system-log ()
  (should (ezeka--add-to-system-log 'move nil
            'from "k-7952"
            'to "20150603T2323"))
  (should (ezeka--add-to-system-log 'move nil
            'from "20150603T2323"
            'to "k-7952"))
  (should-not (ezeka--system-log-trail "k-7952")))

(defun ezeka--system-log-trail (note)
  "Return the system log trail for NOTE, including source line numbers.
Returns a list of plists: each contains :line and :entry keys."
  (let (trail)
    (with-temp-buffer
      (insert-file-contents (in-ezeka-dir ezeka--system-log-file))
      (goto-char (point-min))
      (while (not (eobp))
        (when (re-search-forward (regexp-quote note) (line-end-position) t)
          (beginning-of-line)
          (let ((entry (ignore-errors (json-read))))
            (when entry
              (push (list :line (line-number-at-pos) :entry entry) trail))))
        (forward-line 1)))
    trail))

(defun ezeka-view-system-log-entries (file-or-id)
  "Show system log entries related to FILE-OR-ID in a temporary buffer."
  (interactive
   (list (if (eq major-mode 'magit-status-mode)
             (magit-file-at-point)
           (ezeka--read-id "Show system log entries for ID: "))))
  (let* ((file (if (ezeka-id-valid-p file-or-id)
                   (ezeka-link-file file-or-id)
                 file-or-id))
         (id (ezeka-file-name-id file))
         (buffer (get-buffer-create "*EZeKa System Log*"))
         (trail (ezeka--system-log-trail id)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "System log entries for note ID: %s\n\n" id))
        (dolist (item (nreverse trail))
          (insert (format "%s" (plist-get item :entry)) "\n")))
      (goto-char (point-min))
      (view-mode))
    (display-buffer buffer)))

(defun ezeka--jump-to-log-entry ()
  "Jump to the corresponding entry in `ezeka--system-log-file`."
  (interactive)
  (let ((line (get-text-property (point) 'ezeka-log-line)))
    (when line
      (find-file ezeka--system-log-file)
      (goto-char (point-min))
      (forward-line (1- line)))))

(defun ezeka--file (arg)
  "Return Ezeka file path from the fuzzy ARG.
ARG can be file name, ID, or rubric."
  (let ((arg (when (stringp arg)
               (substring-no-properties arg))))
    (cond ((or (null arg) (string-empty-p arg))
           nil)
          ((ezeka-id-valid-p arg)
           (ezeka-link-file arg))
          ((ezeka-file-p arg)
           arg)
          ((setq mdata (ezeka-decode-rubric arg))
           (let-alist mdata
             (or (ezeka-link-file .id)
                 (ezeka-link-path .id mdata))))
          (t
           (error "`%s' is not a valid Ezeka file, ID, or rubric" arg)))))

(defun ezeka-system-log-trail (file-or-id)
  "Show system log trail for FILE-OR-ID in a human-readable Org buffer.
Press RET on a line to jump to its entry in the system log."
  (interactive
   (list (if (eq major-mode 'magit-status-mode)
             (magit-file-at-point)
           (ezeka--read-id "System log trail for ID: "))))
  (let* ((file (ezeka--file file-or-id))
         (id (ezeka-file-name-id file))
         (buf (get-buffer-create "*System Log Trail*"))
         (fields-to-check '(note target source old-name new-name original parent placeholder))
         (trail (ezeka--system-log-trail id)))
    (with-current-buffer buf
      (let ((inhibit-read-only t)
            (fmt "%-23s %-18s %-13s %-13s %-13s\n")
            (map (let ((km (make-sparse-keymap)))
                   (define-key km (kbd "RET") #'ezeka--jump-to-log-entry)
                   km)))
        (erase-buffer)
        (insert (format "System log trail for: %s\n\n" (ezeka--format-link id)))
        (insert (format fmt "TIME" "ACTION" "NOTE" "SOURCE" "TARGET"))
        (dolist (entry (nreverse trail))
          (let-alist (plist-get entry :entry)
            (let* ((note (if .note (ezeka-file-name-id (ezeka--file \.note)) "-"))
                   (source (ezeka--file (or \.source \.old-name \.parent \.original)))
                   (source (if source (ezeka-file-name-id source) "-"))
                   (target (ezeka--file (or \.target \.new-name \.placeholder \.new)))
                   (target (if target (ezeka-file-name-id target) "-"))
                   (time-str (format-time-string
                              (org-time-stamp-format 'long 'inactive)
                              (encode-time (parse-time-string \.time)))))
              (insert (format fmt
                              time-str
                              (propertize \.action
                                          'ezeka-log-line (plist-get entry :line)
                                          'mouse-face 'highlight
                                          'keymap map)
                              note
                              (truncate-string-to-width source 13 nil nil 'ellipsis)
                              (truncate-string-to-width target 13 nil nil 'ellipsis)))))))
      (org-mode)
      (view-mode)
      (setq-local show-trailing-whitespace nil)
      (pop-to-buffer buf))))

(provide 'ezeka-syslog)
;;; ezeka-syslog.el ends here
