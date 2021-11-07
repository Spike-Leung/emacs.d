;;; Package --- Summary
;; init-yasnippet
;;; Commentary:
;;; Code:
(maybe-require-package 'yasnippet)

(with-eval-after-load 'yasnippet (yas-reload-all));

(add-hook 'beancount-mode-hook 'yas-minor-mode)

(provide 'init-yasnippet)
;;; init-yasnippet.el ends here
