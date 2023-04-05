;;; ezeka-ivy.el --- Eclectic Zettelkasten Ivy Integration -*- lexical-binding: t -*-

;; Copyright (C) 2015-2022 Richard Boyechko

;; Author: Richard Boyechko <code@diachronic.net>
;; Version: 0.1
;; Package-Requires: ((emacs "25.1") (ivy "0.13.4") (ezeka "0.8"))
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

;; Ivy integration for ezeka.el

(require 'ezeka)
(require 'ivy)

(defun ezeka-ivy-titles-reverse-alist (&optional sort-by)
  "Returns a reverse alist of choices consisting of cached Zettel titles and
their paths. For use with `ezeka-ivy-read-reverse-alist-action'. SORT-BY is
either 'MTIME [default] or 'TITLE."
  (let (titles-alist)
    (cond (deft-hash-titles
            (maphash (lambda (key val)
                       (push (cons val key) titles-alist))
                     deft-hash-titles)
            (cl-sort titles-alist
                     (if (equal sort-by 'title)
                         #'deft-file-title-lessp
                       #'deft-file-newer-p)
                     :key #'cdr))
          (t
           (error "No Deft titles cached")))))

(defun ezeka-ivy-read-reverse-alist-action (prompt choices func &optional require-match)
  "Uses `ivy-read' to select from list of CHOICES alist composed of value/key
pairs. Upon selection, call the given FUNC, a function accepting one
argument, on the key. Returns a cons cell consisting of the match from
`ivy-read' and the result of FUNC."
  (let (result)
    (ivy-read prompt
              choices
              :action (lambda (choice)
                        (setq result
                          (if (consp choice)
                              (cons (car choice) (funcall func (cdr choice)))
                            (cons choice nil))))
              :re-builder 'ivy--regex-ignore-order
              :require-match require-match)
    result))

(defun ezeka-ivy-select-link (&optional prompt require-match)
  "Interactively asks the user to select a link from the list of currently
cached Zettel titles. PROMPT is the prompt to pass to `ivy-read'; if
REQUIRE-MATCH is non-nil, do not allow entering link manually."
  (let ((choice
         (ezeka-ivy-read-reverse-alist-action (or prompt "Select link to: ")
                                              (ezeka-ivy-titles-reverse-alist)
                                              #'identity
                                              require-match)))
    (cond ((cdr choice)                 ; link selected from candidates
           (ezeka-file-link (cdr choice)))
          ((ezeka-link-p (car choice)) ; valid link typed in
           (car choice))
          (t
           (signal 'wrong-type-argument '("That is not a valid link"))))))

;; TODO: Rewrite without ivy
(defun ezeka-select-and-find-link (arg)
  "Interactively asks the user to select a link from the list of currently
cached Zettel titles. With universal prefix, finds the link in another
buffer. With double universal prefix, asks the user to type the link
instead."
  (interactive "P")
  (ezeka-find-file (if (equal arg '(16))
                       (read-string "Zettel link to find: ")
                     (let ((choice (ezeka-ivy-read-reverse-alist-action
                                    "Select title: "
                                    (ezeka-ivy-titles-reverse-alist)
                                    #'identity)))
                       (or (cdr choice)
                           (ezeka-link-file (car choice)))))
                   (not (equal arg '(4)))))

(defun ezeka-ivy-insert-link (arg)
  "Inserts a link to another Zettel being currently visited or to those in
the Deft cache. With prefix argument, offers a few options for including
Zettel metadata. If the user selects a Zettel that does not exist in the list
of cached or visiting Zettel, just insert the link to what was selected. If
the cursor in already inside a link, replace it instead."
  (interactive "P")
  (let* ((choices
          (delete-dups (append
                        (mapcar (lambda (path)
                                  (cons (ezeka-deft-parsed-title path) path))
                                (ezeka-visiting-buffer-list t))
                        (ezeka-ivy-titles-reverse-alist 'mtime)))))
    (if choices
        (let* ((choice (ezeka-ivy-read-reverse-alist-action
                        "Insert link to: " choices 'ezeka-file-link nil))
               (link (or (cdr choice)
                         ;; Create a new child if there is no match
                         (let ((new-child (ezeka-create-new-child nil)))
                           (kill-new (car choice)) ; save the entered text
                           (save-excursion
                             (with-current-buffer
                                 (ezeka-link-file
                                  (ezeka-find-link new-child))
                               (ezeka-insert-header-template
                                nil (car choice))))
                           new-child))))
          (if (not (ezeka-link-at-point-p))
              (if arg
                  (funcall-interactively #'ezeka-insert-link-with-metadata link)
                (ezeka-insert-link-with-metadata link '(:title) :before t))
            ;; When replacing, don't including anything
            (delete-region (match-beginning 0) (match-end 0))
            (insert (ezeka-org-format-link link))))
      (user-error "No Deft cache or visited Zettel"))))

(provide 'ezeka-ivy)
