;;; tests-ezeka-file.el --- Unit tests for ezeka-file.el -*- lexical-binding: t -*-

;; Copyright (C) 2025 Richard Boyechko

;; Author: Richard Boyechko <code@diachronic.net>
;; Created: 2025-07-17
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

(require 'ert)
(require 'bytecomp)
(require 'ezeka-file)

;; Make `M-x eval-buffer' in this file enough to find `ezeka-test'.
(add-to-list 'load-path (file-name-directory (or load-file-name buffer-file-name)))
(require 'ezeka-test)

(ert-deftest ezeka-id-valid-p ()
  (should-not (ezeka-id-valid-p "goggly-gook"))
  (should (ezeka-id-valid-p "a-1234"))
  (should (ezeka-id-valid-p "327-C-02-A")))

(ert-deftest ezeka-id-type ()
  (should-error (ezeka-id-type "1234"))
  (should (ezeka-id-type "a-1234"))
  (should (ezeka-id-type "20230403T1000"))
  (should-error (ezeka-id-type "MS-123"))
  (should (eq (ezeka-id-type "a-1234") :numerus))
  (should (eq (ezeka-id-type "20221029T1534") :tempus))
  (should-error (ezeka-id-type "abc-1234")))

(ert-deftest ezeka-file-kasten ()
  (ezeka-test-with-zettelkasten
    ;; `ezeka-file-kasten' returns the Kasten struct, not its name.
    (should (string= "numerus"
                     (ezeka-kasten-name
                      (ezeka-file-kasten (ezeka-link-file "a-0000")))))
    (should (string= "tempus"
                     (ezeka-kasten-name
                      (ezeka-file-kasten (ezeka-link-file "20160313T2228")))))))

(ert-deftest ezeka-file-link ()
  (ezeka-test-with-zettelkasten
    (let ((numerus "q-8148")
          (tempus "20160313T2228"))
      (should (string= numerus (ezeka-file-link (ezeka-link-file numerus))))
      (should (string= tempus (ezeka-file-link (ezeka-link-file tempus))))
      ;; A file outside any Kasten yields nil rather than an error.
      (should-not (ezeka-file-link (ezeka-test-file "not-a-zettel.txt"))))))

(ert-deftest ezeka-link-file ()
  (ezeka-test-with-zettelkasten
    (should (string= "q-8148 {μ} sample note for link resolution"
                     (file-name-base (ezeka-link-file "q-8148"))))
    (should (string= (ezeka-test-file
                      "tempus/2016/20160313T2228 {Class} sample tempus note.txt")
                     (ezeka-link-file "20160313T2228")))
    ;; A Kasten-qualified link resolves the same as a bare ID.
    (should (string= (ezeka-link-file "20160313T2228")
                     (ezeka-link-file "tempus:20160313T2228")))
    ;; Nothing on disk for this ID.
    (should-not (ezeka-link-file "z-9999"))))

(ert-deftest ezeka-link-kasten ()
  (should (string= (ezeka-link-kasten "a-1234") "numerus"))
  (should (string= (ezeka-link-kasten "20240729T1511") "tempus"))
  (should (string= (ezeka-link-kasten "a-1234~56") "scriptum")))

(ert-deftest ezeka-link-p ()
  (should (ezeka-link-p "a-1234"))
  (should (ezeka-link-p "20221029T1534"))
  (should-not (ezeka-link-p "abc-1234")))

(ert-deftest ezeka-link-regexp-compiled-configuration ()
  "Compiled link matching must use the current Kasten registry."
  (let ((ezeka--kaesten (copy-sequence ezeka--kaesten))
        (matcher (byte-compile
                  '(lambda (link)
                     (string-match-p (ezeka-link-regexp 'match-entire) link)))))
    (should-not (funcall matcher "custom-42"))
    (ezeka-kasten-new "custom"
                      :id-regexp "custom-[0-9]+"
                      :minimal-id "custom-1")
    (should (funcall matcher "custom-42"))
    (should (funcall matcher "custom:custom-42"))
    (should-not (funcall matcher "custom-42-extra"))))

(ert-deftest ezeka-link-path ()
  (ezeka-test-with-zettelkasten
    ;; `ezeka-link-path' computes a path; the file need not exist.
    (should (string=
             (ezeka-test-file
              "numerus/a/a-1234 {ψ} ezeka--create-placeholder test.txt")
             (ezeka-link-path "a-1234"
                              '((link . "a-1234")
                                (label . "ψ")
                                (caption . "ezeka--create-placeholder test")))))))

(ert-deftest ezeka-make-link ()
  (should-error (ezeka-make-link "kasten" "1234"))
  (should-error (ezeka-make-link "numerus" "1234"))
  (should (ezeka-make-link "numerus" "a-1234"))
  (should (ezeka-make-link "tempus" "20230403T1000"))
  (should (ezeka-make-link "scriptum" "a-1234~01")))

(ert-deftest ezeka--directory-files ()
  (ezeka-test-with-zettelkasten
    (let ((all-files (ezeka--directory-files "scriptum"))
          (symlinks (ezeka--directory-files "scriptum"
                                            (lambda (file)
                                              (file-symlink-p file)))))
      (should all-files)
      (should (< (length symlinks) (length all-files))))))

(ert-deftest ezeka--generate-id ()
  (ezeka-test-with-zettelkasten
    ;; Even with BATCH, `ezeka-new-numerus-currens' asks the user to accept
    ;; the candidate it picked, so answer that prompt.
    (should (eq :numerus
                (ezeka-id-type
                 (ert-simulate-keys "y" (ezeka--generate-id "numerus" 'batch)))))
    (should (eq :tempus (ezeka-id-type (ezeka--generate-id "tempus" 'batch))))
    ;; `ezeka--generate-id' has no way to pass a project to the scriptum
    ;; branch, which then prompts for one; call the generator directly.
    (should (eq :scriptum (ezeka-id-type (ezeka-scriptum-id "a-1234"))))))

(ert-deftest ezeka--make-symbolic-link ()
  ;; The fixture is needed for the system log the function writes to.
  (ezeka-test-with-zettelkasten
    (let ((target (ezeka-link-file "a-0000"))
          (linkname (ezeka-test-file "numerus/a/a-0001 {ψ} symlink target.txt")))
      (ezeka--make-symbolic-link target linkname)
      (should (and (file-exists-p linkname) (file-symlink-p linkname)))
      (should-not (when (file-symlink-p linkname)
                    (delete-file linkname)
                    (file-exists-p linkname))))))

(ert-deftest ezeka--pasteurize-file-name ()
  (should (string= (ezeka--pasteurize-file-name "/Mickey 17/ (dir. Bong Joon-ho, 2025)")
                   "_Mickey 17_")))

(provide 'tests-ezeka-file)
;;; tests-ezeka-file.el ends here
