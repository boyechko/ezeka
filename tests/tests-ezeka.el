;;; ezeka-tests.el --- Unit tests for ezeka.el -*- lexical-binding: t -*-

;; Copyright (C) 2015-2022 Richard Boyechko

;; Author: Richard Boyechko <code@diachronic.net>
;; Version: 0.1
;; Package-Requires: ((emacs "25.1") (org "9.5"))
;; Keywords: zettelkasten org
;; URL: https://github.com/boyechko/ezeka

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

;; These are unit tests for ezeka.el

;;; Code:

(require 'ezeka)
(require 'ert)

(define-key emacs-lisp-mode-map (kbd "C-c C-e") 'ert)

;;;=============================================================================
;;; Tests
;;;=============================================================================

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

(ert-deftest ezeka-link-kasten ()
  (let ((numerus "q-8148")
        (tempus-with-kasten "os:20160313T2228")
        (tempus-no-kasten "20160313T2228"))
    (should (string= "numerus" (ezeka-link-kasten numerus)))
    (should (string= "os" (ezeka-link-kasten tempus-with-kasten)))
    (should (string= "tempus" (ezeka-link-kasten tempus-no-kasten)))
    (should-not (ezeka-link-kasten tempus-no-kasten t))))

(ert-deftest ezeka-file-link ()
  (let ((numerus "q-8148")
        (tempus "20160313T2228"))
    (should (string= numerus (ezeka-file-link (ezeka-link-file numerus))))
    (should (string= tempus (ezeka-file-link (ezeka-link-file tempus))))
    (should-error (ezeka-file-link buffer-file-name))))

(ert-deftest ezeka--generate-id ()
  (should (eq (ezeka-id-type (ezeka--generate-id "numerus")) :numerus))
  (should (eq (ezeka-id-type (ezeka--generate-id "tempus")) :tempus))
  (should (eq (ezeka-id-type (ezeka--generate-id "scriptum")) :scriptum)))

(ert-deftest ezeka-link-file ()
  (let ((numerus "q-8148")
        (tempus-with-kasten "os:20160313T2228")
        (tempus-no-kasten "20160313T2228"))
    (should
     (string=
      "q-8148 {μ} having everything in one place frees up the mind @Allen2001"
      (file-name-base (ezeka-link-file numerus))))
    (should
     (string=
      "q-8148 random string"
      (file-name-base (ezeka-link-file numerus "random string"))))
    (should
     (string=
      "/Users/richard/Zettelkasten/os/2016/20160313T2228 {Class} Translation, Final Project.txt"
      (ezeka-link-file tempus-with-kasten)))
    (should
     (string=
      "/Users/richard/Zettelkasten/os/2016/20160313T2228 {Class} Translation, Final Project.txt"
      (ezeka-link-file tempus-no-kasten)))
    (should
     (string=
      "/Users/richard/Zettelkasten/omasum/2016/20160313T2228.txt"
      (ezeka-link-file tempus-no-kasten "")))
    (should
     (string=
      "/Users/richard/Zettelkasten/omasum/2016/20160313T2228 testing.txt"
      (ezeka-link-file tempus-no-kasten "testing")))))

(ert-deftest ezeka--header-normalize-readings ()
  (should
   (equal (ezeka--header-normalize-readings
           '("2018-04-14"
             "[2019-05-01 Fri]"
             "[2019-06-27 Thu 18:51]"))
          '("2018-04-14" "2019-05-01" "2019-06-27"))))

(ert-deftest ezeka--decode-header-make-tuple ()
  (should (equal (ezeka--decode-header-make-tuple 'created "2021-01-01")
                 '(created . "2021-01-01")))
  (should (equal (ezeka--decode-header-make-tuple 'readings '("2021-01-01"))
                 '(readings . ("2021-01-01"))))
  (should (equal (ezeka--decode-header-make-tuple 'readings '("2019-01-01" "2021-01-01"))
                 '(readings . ("2019-01-01" "2021-01-01")))))

(ert-deftest ezeka-id-type ()
  (should-error (ezeka-id-type "1234"))
  (should (ezeka-id-type "a-1234"))
  (should (ezeka-id-type "20230403T1000"))
  (should-error (ezeka-id-type "MS-123"))
  (should (eq (ezeka-id-type "a-1234") :numerus))
  (should (eq (ezeka-id-type "20221029T1534") :tempus))
  (should-error (ezeka-id-type "abc-1234")))

(ert-deftest ezeka-file-kasten ()
  (should (ezeka-file-kasten (ezeka-link-file "a-0000"))))

(ert-deftest ezeka-link-p ()
  (should (ezeka-link-p "a-1234"))
  (should (ezeka-link-p "20221029T1534"))
  (should-not (ezeka-link-p "abc-1234")))

(ert-deftest ezeka-make-link ()
  (should-error (ezeka-make-link "kasten" "1234"))
  (should-error (ezeka-make-link "numerus" "1234"))
  (should (ezeka-make-link "numerus" "a-1234"))
  (should (ezeka-make-link "tempus" "20230403T1000"))
  (should (ezeka-make-link "scriptum" "a-1234~01")))

(ert-deftest ezeka--validate-citekey ()
  (should (string= "@Blah1999" (ezeka--validate-citekey "Blah1999")))
  (should (string= "&Blah1999" (ezeka--validate-citekey "&Blah1999")))
  (should (string= "@Blah1999" (ezeka--validate-citekey "@Blah1999")))
  (should-not (ezeka--validate-citekey "Zoomba Blah 1999")))

(ert-deftest adv--octavo-group-function ()
  (let ((file "/Users/richard/Zettelkasten/os/2022/20221030T1409.txt"))
    (should (adv--octavo-group-function file nil))))

(ert-deftest ezeka--create-placeholder ()
  (let* ((mdata (ezeka-metadata "a-1234"
                  'label "ψ"
                  'caption "ezeka--create-placeholder test"))
         (path (ezeka-link-path "a-1234" mdata)))
    (should (and (ezeka--create-placeholder "a-1234"
                                            mdata
                                            'quietly)
                 (file-symlink-p path)
                 (if (y-or-n-p (format "Delete placeholder `%s'?" path))
                     (delete-file path)
                   (message "Placeholder not deleted: %s" path))))))

(provide 'tests-ezeka)
;;; tests-ezeka.el ends here

