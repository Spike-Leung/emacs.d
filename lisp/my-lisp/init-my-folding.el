;;; init-my-folding.el --- Folding using treesit-fold -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(when (maybe-require-package 'treesit-fold)
  (setq treesit-fold-indicators-mode 1)
  (global-treesit-fold-indicators-mode 1)
  (with-eval-after-load 'treesit-fold
    (define-key treesit-fold-mode-map (kbd "C-c f") #'treesit-fold-toggle)
    (define-key treesit-fold-mode-map (kbd "C-c F") #'treesit-fold-open-recursively)
    (define-key treesit-fold-mode-map (kbd "C-c o") #'treesit-fold-close-all)
    (define-key treesit-fold-mode-map (kbd "C-c O") #'treesit-fold-open-all)))

(provide 'init-my-folding)
;;; init-my-folding.el ends here
