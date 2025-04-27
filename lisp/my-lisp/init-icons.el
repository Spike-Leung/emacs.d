;;; init-icons.el --- icons setting -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(when (maybe-require-package 'nerd-icons)
  (require 'nerd-icons)
  (maybe-require-package 'nerd-icons-dired)
  (maybe-require-package 'nerd-icons-ibuffer)
  (add-hook 'dired-mode-hook #'nerd-icons-dired-mode)
  (add-hook 'ibuffer-mode-hook #'nerd-icons-ibuffer-mode))

(setq magit-format-file-function 'magit-format-file-nerd-icons)

(provide 'init-icons)
;;; init-icons.el ends here
