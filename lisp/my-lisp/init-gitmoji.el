(push (expand-file-name "lisp/my-lisp/gitmoji" user-emacs-directory) load-path)
(require 'gitmoji)
(eval-after-load 'gitmoji
  (gitmoji-commit-mode))
(provide 'init-gitmoji)
