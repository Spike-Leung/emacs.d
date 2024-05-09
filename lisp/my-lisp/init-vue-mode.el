;;; init-vue-mode.el --- vue mode config
;;; Commentary:
;;; Code:
(add-hook 'emacs-startup-hook (lambda () (setq gc-cons-threshold (* 100 1024 1024))))
(setq-default eglot-events-buffer-size 0)

(maybe-require-package 'web-mode)

;;; Eglot config for Vue
;;; see: https://emacs-china.org/t/volar-eglot/21255
(define-derived-mode vue-mode web-mode "Vue")
(add-to-list 'auto-mode-alist '("\\.vue\\'" . vue-mode))
(add-hook 'vue-mode-hook 'eglot-ensure)

(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '(vue-mode . ("vue-language-server" "--stdio" :initializationOptions
                             (:typescript
                              (:tsdk "/home/spike/.nvm/versions/node/v16.20.2/lib/node_modules/typescript/lib/"))))))
(provide 'init-vue-mode)
;;; init-vue-mode.el ends here
