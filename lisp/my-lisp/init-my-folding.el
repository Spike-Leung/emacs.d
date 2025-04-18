;;; init-my-folding.el --- Folding using treesit-fold -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(when (maybe-require-package 'treesit-fold)
  (setq treesit-fold-indicators-mode 1)
  (global-treesit-fold-indicators-mode 1)
  (with-eval-after-load 'treesit-fold
    (define-key treesit-fold-mode-map (kbd "C-c f") #'treesit-fold-toggle)
    (define-key treesit-fold-mode-map (kbd "C-c F") #'treesit-fold-close-all)))

(provide 'init-my-folding)
;;; init-my-folding.el ends here
