;;; github-clone-test.el --- Tests for github-clone  -*- lexical-binding: t; -*-

;; Copyright (C) 2014  Charles Comstock

;; Author: Charles Comstock <clgc@nocturnal>
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(require 'github-clone)

(ert-deftest github-clone-repo-name-matches-repo ()
  (flet ((github-clone-user-name () "login"))
    (should (equal (github-clone-repo-name "repo")
                   (cons "login" "repo")))))

(ert-deftest github-clone-repo-name-matches-user-repo ()
  (should (equal (github-clone-repo-name "user/repo")
                 (cons "user" "repo"))))

(ert-deftest github-clone-repo-name-matches-url ()
  (should (equal (github-clone-repo-name "https://github.com/clojure-emacs/cider")
                 (cons "clojure-emacs" "cider"))))

(ert-deftest github-clone-repo-name-matches-git-url ()
  (should (equal (github-clone-repo-name "git@github.com:clojure-emacs/cider.git")
                 (cons "clojure-emacs" "cider"))))

(provide 'github-clone-test)
;;; github-clone-test.el ends here



