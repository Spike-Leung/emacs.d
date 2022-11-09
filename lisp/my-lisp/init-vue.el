;;; Package --- Summary
;; init-vue
;;; Commentary:
;;; Code:
(define-derived-mode vue-mode web-mode "Vue")
(add-to-list 'auto-mode-alist '("\\.vue\\'" . vue-mode))
(add-hook 'vue-mode-hook 'eglot-ensure)
(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs '(vue-mode . (eglot-volar "vue-language-server" "--stdio")))
  (defclass eglot-volar (eglot-lsp-server) ()
    :documentation "volar")
  (cl-defmethod eglot-initialization-options ((server eglot-volar))
    "Passes through required cquery initialization options"
    `(
      ;; // Absolute path to node_modules/typescript/lib
      :typescript (:tsdk ,(expand-file-name "~/Library/pnpm/global/5/node_modules/typescript/lib/"))
      )))
;;; init-vue.el ends here
