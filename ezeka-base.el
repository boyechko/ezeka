;;; ezeka-base.el --- Base functions, errors, etc. for Ezeka -*- lexical-binding: t -*-

;; Copyright (C) 2025 Richard Boyechko

;; Author: Richard Boyechko <code@diachronic.net>
;; Created: 2025-04-12
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

(require 'org)
(require 'format-spec)
(require 'cl-lib)
(require 'cl-generic)
(require 'seq)
(require 'subr-x)                       ; for if-let, when-let*, etc.

;;;=============================================================================
;;; Error-Handling
;;;=============================================================================

(define-error 'ezeka-error
  "Something went wrong: %s"
  'error)

;;;=============================================================================
;;; General Functions
;;;=============================================================================

(defun ezeka--space-or-punct-p (character)
  "Return T if the CHARACTER is a space or punctuation."
  (when character
    ;; [:punct:] is too permissive, since includes (), [], {}, etc.
    (string-match-p "[[:space:].,;:+=~!?'\"-]" (char-to-string character))))

(defmacro ezeka--with-space (value &optional where)
  "If VALUE is non-nil, return a string with specified space.
If VALUE is nil, return an empty string. VALUE is evaluated
only once. WHERE can be 'before, 'after, or 'both."
  (let ((sym (gensym)))
    `(let ((,sym ,value))
       (if ,sym
           (concat (if (member ,where '(before both)) " " "")
                   ,sym
                   (if (member ,where '(after both)) " " ""))
         ""))))

;; FIXME No references as of [2025-07-18 Fri]
(defun ezeka--ordinal-suffix (n)
  "Ordinal suffix for N, a number or string.
\(That is, `st', `nd', `rd', or `th', as appropriate.)
This function is based on `diary-ordinal-suffix'."
  (let ((n (round (if (numberp n) n (string-to-number n)))))
    (if (or (memq (% n 100) '(11 12 13))
            (< 3 (% n 10)))
        "th"
      (aref ["th" "st" "nd" "rd"] (% n 10)))))

(defun ezeka--replace-in-string (string &rest replacements)
  "Replace in STRING all the regexp/replacement pairs in REPLACEMENTS.
Each item in REPLACEMENTS is in the form (FROM TO) for simple
string replacements or (FROM TO 'regexp [&rest ARGS]) for regexp,
where ARGS is the argument list to `replace-regexp-in-string'
 after STRING. Each replacement pair is processed in turn."
  (declare (indent 1))
  (save-match-data
    (dolist (recipe replacements string)
      (setq string
        (if (memq 'regexp recipe)
            (apply #'replace-regexp-in-string
                   (car recipe)
                   (cadr recipe)
                   string
                   (cdr (memq 'regexp recipe)))
          (string-replace (car recipe) (cadr recipe) string))))))

(defun ezeka--concat-strings (separator &rest elements)
  "Concatenate ELEMENTS, separating them with SEPARATOR.
Any NULLs are stripped from ELEMENTS, and everything else is fed to
FORMAT."
  (declare (indent 1))
  (mapconcat #'(lambda (elt)
                 (if (stringp elt)
                     elt
                   (format "%s" elt)))
    (cl-remove-if #'null elements)
    separator))

(defun ezeka--regexp-strip-named-groups (regexp)
  "Strip the named groups in the given REGEXP."
  (replace-regexp-in-string "(\\?[0-9]+:" "(" regexp))

;;;=============================================================================
;;; Time Stamps
;;;=============================================================================

;; For our purposes, a "timestamp" is a string representation of "time," the
;; default Emacs time value.
(defcustom ezeka-long-timestamp-format "%F %a %R"
  "Format string used for created and modified timestamps.
See `format-time-string' for details about details."
  :type 'string
  :group 'ezeka)

(defcustom ezeka-short-timestamp-format "%F %a"
  "Format string used for reading dates and change log entries.
See `format-time-string' for details about details."
  :type 'string
  :group 'ezeka)

(defvar ezeka-iso8601-date-regexp
  (rx word-boundary
      (group (repeat 4 digit))
      (* "-")
      (group (repeat 2 digit))
      (* "-")
      (group (repeat 2 digit)))
  "The regular expression that matches ISO 8601-like date.
Groups 1-3 are year, month, day.")

(defvar ezeka-iso8601-time-regexp
  "T*\\([0-9]\\{2\\}\\):*\\([0-9]\\{2\\}\\)\\>"
  "The regular expression that matches ISO 8601-like time.
Groups 1-2 are hour and minute.")

(defvar ezeka-iso8601-datetime-regexp
  (concat ezeka-iso8601-date-regexp ezeka-iso8601-time-regexp)
  "The regular expression that matches ISO 8601 date and time separate with T.
Groups 1-3 are year, month, day.
Groups 4-5 are hour and minute.")

(defun ezeka--parse-time-string (string)
  "Return the internal encoded time corresponding to STRING.
It should be an ISO8601 date/time expression or an
`org-mode' timestamp, with or without time. Cannot use
`parse-time-string' because it always expects time."
  (let ((second 0) (minute 0) (hour 0) day month year)
    (when (string-match (concat "^" ezeka-iso8601-date-regexp) string)
      (setq year  (string-to-number (match-string 1 string))
            month (string-to-number (match-string 2 string))
            day   (string-to-number (match-string 3 string)))
      (when (string-match ezeka-iso8601-time-regexp string
                          (match-end 0))
        (setq hour   (string-to-number (match-string 1 string))
              minute (string-to-number (match-string 2 string))))
      (encode-time second minute hour day month year))))

(defun ezeka--iso8601-time-string (&optional time)
  "Return a string with full ISO8601 representation of TIME (or now)."
  (format-time-string "%FT%T%z" time))

(defun ezeka--complete-time (time1 &optional time2)
  "Complete TIME1 from TIME2, returning time value.
If TIME2 is not given, use current time."
  (let* ((dt1 (decode-time time1))
         (dt2 (decode-time time2))
         (dt-now (decode-time)))
    (when (and (zerop (decoded-time-hour dt1))
               (zerop (decoded-time-minute dt1)))
      (setf (decoded-time-hour dt1)
            (if (zerop (decoded-time-hour dt2))
                (decoded-time-hour dt-now)
              (decoded-time-hour dt2))
            (decoded-time-minute dt1)
            (if (zerop (decoded-time-minute dt2))
                (decoded-time-minute dt-now)
              (decoded-time-minute dt2))))
    (encode-time dt1)))

;; FIXME: The NOWEEKDAY argument is a hack to avoid having yet another custom
;; variable for short timestamp without the weekday.
(defun ezeka-timestamp (&optional time full brackets noweekday)
  "Return properly formatted Emacs TIME.
If FULL is non-nil, use `ezeka-long-timestamp-format';
otherwise use `ezeka-short-timestamp-format'. If BRACKETS is
non-nil, surround the timestamp with square brackets. If
NOWEEKDAY is non-nil, do not include the abbreviated weekday
in date-only timestamp."
  (let ((fmt (if full ezeka-long-timestamp-format ezeka-short-timestamp-format)))
    (format (if brackets "[%s]" "%s")
            (format-time-string (if (and (not full) noweekday)
                                    (string-replace " %a" "" fmt)
                                  fmt)
                                time))))

(defun ezeka--timep (time)
  "If TIME is a Lisp time value then return TIME, else return nil.
This is a copy of `timep' from `type-break'."
  (condition-case nil
      (and (float-time time) time)
    (error nil)))

(defun ezeka--full-time-p (time)
  "Return non-NIL if TIME contains non-zero hour, minute, or second."
  (cl-notevery (lambda (x) (or (null x) (zerop x)))
               (cl-subseq (decode-time time) 0 3)))

(provide 'ezeka-base)
;;; ezeka-base.el ends here
