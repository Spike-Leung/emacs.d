;;; init-gitmoji.el --- config gitmoji -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(require 'gitmoji)

(with-eval-after-load 'gitmoji
  (setq gitmoji-selection-backend (list 'consult))
  (setq gitmoji--display-utf8-emoji t)
;; (gitmoji-commit-mode)
)

(provide 'init-gitmoji)
;;; init-gitmoji.el ends here
