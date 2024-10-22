;;; init-my-dired.el --- Dired customisations -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(when (maybe-require-package 'dired-git-info)
  (with-eval-after-load 'dired
    (add-hook 'dired-mode-hook 'dired-hide-details-mode)
    (define-key dired-mode-map (kbd ")") 'dired-git-info-mode)))

(provide 'init-my-dired)
;;; init-my-dired.el ends here
