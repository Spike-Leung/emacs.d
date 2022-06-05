;;; Package --- Summary
;; init-reset-keybinding
;;; Commentary:
;;; Code:
(eval-after-load 'default-text-scale
  '(define-key default-text-scale-mode-map (kbd "C-M--") nil)
  )

(provide 'init-reset-keybinding)
;;; init-beancount.el ends here
