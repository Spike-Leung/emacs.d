;;; init-typescript.el --- typescript support
;; init-typescript
;;; Commentary:
;;; Code:

(add-to-list 'auto-mode-alist '("\.\(ts\|tsx\)" . typescript-mode))

;; omit js2 semi warning
(setq js2-strict-missing-semi-warning nil)

(provide 'init-typescript)
;;; init-typescript.el ends here
