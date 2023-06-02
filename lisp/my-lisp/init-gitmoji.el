;;; init-gitmoji.el --- config gitmoji
;;; Commentary:
;;; Code:
(push (expand-file-name "lisp/my-lisp/gitmoji" user-emacs-directory) load-path)
(require 'gitmoji)

(with-eval-after-load 'gitmoji
  (setq gitmoji-selection-backend (list 'consult))
  (gitmoji-commit-mode))

(provide 'init-gitmoji)
;;; init-gitmoji.el ends here
