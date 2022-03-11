;;; Package --- Summary
;; init-yasnippet
;;; Commentary:
;;; Code:
(maybe-require-package 'yasnippet)

(with-eval-after-load 'yasnippet (yas-reload-all));

;; (add-hook 'beancount-mode-hook 'yas-minor-mode)

(yas-global-mode 1)
(provide 'init-yasnippet)
;;; init-yasnippet.el ends here
