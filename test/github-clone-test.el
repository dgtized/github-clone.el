(require 'noflet)
(require 'github-clone)

(ert-deftest github-clone-repo-name-matches-repo ()
  (noflet ((github-clone-user-name () "login"))
    (should (equal (github-clone-repo-name "repo")
                   (cons "login" "repo")))))

(ert-deftest github-clone-repo-name-matches-user-repo ()
  (should (equal (github-clone-repo-name "user/repo")
                 (cons "user" "repo"))))

(ert-deftest github-clone-repo-name-matches-url ()
  (should (equal (github-clone-repo-name "https://github.com/dgtized/github-clone.el")
                 (cons "dgtized" "github-clone.el"))))

(ert-deftest github-clone-repo-name-works-with-magit ()
  (should (equal (github-clone-repo-name "https://github.com/magit/magit")
                 (cons "magit" "magit"))))

(ert-deftest github-clone-repo-name-matches-git-url ()
  (should (equal (github-clone-repo-name "git@github.com:clojure-emacs/cider.git")
                 (cons "clojure-emacs" "cider"))))



