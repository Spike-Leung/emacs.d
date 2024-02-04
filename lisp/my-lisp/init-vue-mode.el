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
  (add-to-list 'eglot-server-programs '(vue-mode . (eglot-volar "vue-language-server" "--stdio")))
  (defclass eglot-volar (eglot-lsp-server) () :documentation "volar")
  (cl-defmethod eglot-initialization-options ((server eglot-volar))
    "Passes through required cquery initialization options"
    `(
      ;; Absolute path to node_modules/typescript/lib
      :typescript (:tsdk ,(expand-file-name "~/.nvm/versions/node/v16.18.0/lib/node_modules/typescript/lib/"))
      :fullCompletionList t
      )))
(provide 'init-vue-mode)
;;; init-vue-mode.el ends here
