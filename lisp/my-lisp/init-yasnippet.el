;;; init-yasnippet.el --- enable yasnippet global -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(maybe-require-package 'yasnippet)

;; @see: https://github.com/AndreaCrotti/yasnippet-snippets
(maybe-require-package 'yasnippet-snippets)
(yas-global-mode 1)

(provide 'init-yasnippet)
;;; init-yasnippet.el ends here
