;;; init-typescript.el --- typescript support
;; init-typescript
;;; Commentary:
;;; https://github.com/ananthakumaran/tide/
;;; Code:
(maybe-require-package 'tide)
(maybe-require-package 'web-mode)

(defun setup-tide-mode ()
  "Setup tide mode."
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  ;; company is an optional dependency. You have to
  ;; install it separately via package-install
  ;; `M-x package-install [ret] company`
  (company-mode +1))

;; aligns annotation to the right hand side
(setq company-tooltip-align-annotations t)

;; formats the buffer before saving
(add-hook 'before-save-hook 'tide-format-before-save)
(add-hook 'typescript-mode-hook #'setup-tide-mode)
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
(add-hook 'web-mode-hook
          (lambda ()
            (when
                (and (bound-and-true-p buffer-file-name)
                     (string-equal "tsx" (file-name-extension buffer-file-name)))
              (setup-tide-mode))))

;; enable typescript-tslint checker
(with-eval-after-load 'flycheck (flycheck-add-mode 'typescript-tslint 'web-mode))

(setq typescript-indent-level 2)
(setq tide-server-max-response-length 400240)

(provide 'init-typescript)
;;; init-typescript.el ends here
