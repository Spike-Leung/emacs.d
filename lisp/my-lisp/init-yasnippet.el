;;; init-yasnippet.el --- enable yasnippet global
;;; Commentary:
;;; Code:
(maybe-require-package 'yasnippet)
(with-eval-after-load 'yasnippet (yas-reload-all));
(yas-global-mode 1)

;; see: https://github.com/AndreaCrotti/yasnippet-snippets
(maybe-require-package 'yasnippet-snippets)
(provide 'init-yasnippet)
;;; init-yasnippet.el ends here
