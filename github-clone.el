;;; github-clone.el --- Fork and clone github repos  -*- lexical-binding: t; -*-

;; Copyright (C) 2014  Charles Comstock

;; Author: Charles Comstock <clgc@nocturnal>
;; Created: 2 Aug 2014
;; Version: 0.1
;; Keywords: vc, tools
;; Package-Requires: ((eieio "1.3") (gh "0.7.2") (magit "1.2.0"))

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

(require 'eieio)

(require 'gh)
(require 'gh-repos)
(require 'subr-x)

(defun github-clone-fork (repo)
  (oref (gh-repos-fork (gh-repos-api "api") repo) :data))

(defun github-clone-info (user repo-id)
  (thread-first (gh-repos-api "api")
    (gh-repos-repo-get repo-id user)
    (oref :data)))

(defun github-clone-remotes (user repo-id)
  (let* ((repo (github-clone-info user repo-id))
         (forks (oref (gh-repos-forks-list (gh-repos-api "api") repo) :data)))
    (cl-loop for fork in forks
             collect (cons (oref (oref fork :owner) :login)
                           (oref fork :git-url)))))

(defun github-clone-upstream (repo)
  (if (eq (oref repo :fork) t)
      (cons "upstream" (oref (oref repo :parent) :git-url))
    nil))

(defun github-clone-repo (repo directory)
  (let* ((name (oref repo :name))
         (target (if (file-exists-p directory)
                     (expand-file-name name directory)
                   directory))
         (repo-url (oref repo :git-url)))
    (message "Cloning %s into %s" name target)
    (shell-command (format "git clone %s %s" repo-url target))
    (magit-status target)
    (if-let (upstream (github-clone-upstream repo))
        (progn
          (message "Adding remote %s" upstream)
          (magit-add-remote (car upstream) (cdr upstream))))))

(defun github-clone-repo-name (url)
  (cond ((string-match "\.git$" url)
         (github-clone-repo-name (replace-match "" nil nil url)))
        ((string-match "\\([[:alnum:]\-_.]+\\)/\\([[:alnum:]\-_.]+\\)$" url)
         (cons (match-string 1 url) (match-string 2 url)))
        ((string-match "^\\([[:alnum:]\-_.]+\\)$" url)
         (cons (github-clone-user-name) (match-string 1 url)))
        (t (error "Cannot parse repo name %s" url))))

(defvar github-clone--user nil "Cache for current github login")
(defun github-clone-user-name ()
  (if (null github-clone--user)
      (setq github-clone--user
            (oref (oref (gh-users-get (gh-users-api "api")) :data) :login))
    github-clone--user))

(defun github-clone (user-repo-url directory)
  (interactive
   (list (read-from-minibuffer "Url or User/Repo: ")
         (read-directory-name "Directory: " nil default-directory t)))
  (let* ((name (github-clone-repo-name user-repo-url))
         (repo (github-clone-info (car name) (cdr name))))
    (if (oref repo :git-url)
        (let ((fork (github-clone-fork repo)))
          (if (oref fork :git-url)
              (github-clone-repo fork directory)
            (error "Unable to fork %s" user-repo-url)))
      (error "Repository %s does not exist" user-repo-url))))

(provide 'github-clone)
;;; github-clone.el ends here
