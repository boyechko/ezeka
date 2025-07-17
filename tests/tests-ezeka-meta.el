;;; tests-ezeka-meta.el --- Unit tests for ezeka-meta.el -*- lexical-binding: t -*-

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
(require 'ezeka-meta)

(ert-deftest ezeka-format-metadata ()
  (let* ((file (ezeka-link-file "a-0000"))
         (mdata (ezeka-file-metadata file)))
    (should (string= "a-0000" (ezeka-format-metadata "%i" mdata)))
    (should (string= "2022-08-08 Mon 18:25" (ezeka-format-metadata "%C" mdata)))
    (should (string= (ezeka-format-metadata "%t" mdata)
                     (ezeka-format-metadata "%T" mdata)))
    (should (string= (file-name-base file) (ezeka-format-metadata "%R" mdata)))))

(ert-deftest ezeka--header-yamlify-value ()
  (should (string= (ezeka--header-yamlify-value "one")
                   "one"))
  (should (string= (ezeka--header-yamlify-value '("one" "two" "three"))
                   "[ one, two, three ]")))

(ert-deftest ezeka--header-deyamlify-value ()
  (should (equal (ezeka--header-deyamlify-value "2022-01-01")
                 "2022-01-01"))
  (should (equal (ezeka--header-deyamlify-value " 2022-01-01 ")
                 "2022-01-01"))
  (should (equal (ezeka--header-deyamlify-value "[ 2020-01-01, 2022-01-01 ]")
                 '("2020-01-01" "2022-01-01"))))

(ert-deftest ezeka--metadata-equal-p ()
  (let ((md1 (ezeka-file-metadata (ezeka-link-file "x-1613")))
        (md1a (nreverse (ezeka-file-metadata (ezeka-link-file "x-1613"))))
        (md2 (ezeka-file-metadata (ezeka-link-file "q-8148"))))
    (should (ezeka--metadata-equal-p md1 md1a))
    (should-not (ezeka--metadata-equal-p md1 md2))))

(ert-deftest ezeka--parse-citation-key ()
  (should (equal '(author1 "Horkheimer" author2 "Adorno" date "1989" authors "Horkheimer and Adorno")
                 (ezeka--parse-citation-key "HorkheimerAdorno1989")))
  (should (equal '(author1 "Chiang" author2 nil date "1998" authors "Chiang")
                 (ezeka--parse-citation-key "&Chiang1998"))))

(ert-deftest ezeka--validate-citekey ()
  (should (string= "@Blah1999" (ezeka--validate-citekey "Blah1999")))
  (should (string= "&Blah1999" (ezeka--validate-citekey "&Blah1999")))
  (should (string= "@Blah1999" (ezeka--validate-citekey "@Blah1999")))
  (should-not (ezeka--validate-citekey "Zoomba Blah 1999")))

(provide 'tests-ezeka-meta)
;;; tests-ezeka-meta.el ends here
