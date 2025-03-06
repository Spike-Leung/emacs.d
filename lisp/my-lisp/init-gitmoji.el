;;; init-gitmoji.el --- config gitmoji -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'gitmoji)

;; you can enable `gitmoji-commit-mode' to call gitmoji when commit
(with-eval-after-load 'gitmoji
  (maybe-require-package 'consult)
  (setq gitmoji-selection-backend '(consult))
  (setq gitmoji--display-utf8-emoji t))

(provide 'init-gitmoji)
;;; init-gitmoji.el ends here
