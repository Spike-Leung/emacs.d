;;; init-writing.el --- writing related -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(maybe-require-package 'olivetti)
(setq
 olivetti-style 'fancy
 olivetti-body-width 100
 olivetti-margin-width 5)
(with-eval-after-load 'init-my-keybindings
  (define-key spike-leung/meta-o-keymap (kbd "o") 'olivetti-mode))
(provide 'init-writing)
;;; init-writing.el ends here
