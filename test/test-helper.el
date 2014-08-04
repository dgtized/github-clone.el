(require 'f)

(defvar github-clone/test-path (f-dirname (f-this-file)))
(defvar github-clone/root-path (f-parent github-clone/test-path))

(add-to-list 'load-path github-clone/root-path)
