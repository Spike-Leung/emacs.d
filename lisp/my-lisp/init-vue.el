;;; init-vue.el --- vue support
;;; Commentary:
;;; Code:
;;; https://emacs-china.org/t/volar-eglot/21255
(add-hook 'emacs-startup-hook (lambda () (setq gc-cons-threshold (* 100 1024 1024))))
(setq-default eglot-events-buffer-size 0)
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
      ;; Absolute path to node_modules/typescript/lib
      :typescript (:tsdk ,(expand-file-name "~/Library/pnpm/global/5/node_modules/typescript/lib/"))
      :languageFeatures (
                         :references t
                         :implementation t
                         :definition t
                         :typeDefinition t
                         :rename t
                         :renameFileRefactoring t
                         :signatureHelp t
                         :codeAction t
                         :workspaceSymbol t
                         :completion (
                                      :defaultTagNameCase ""
                                      :defaultAttrNameCase ""
                                      :getDocumentNameCasesRequest :json-false
                                      :getDocumentSelectionRequest :json-false)
                         )
      :documentFeatures (
                         :selectionRange t,
                         :foldingRange :json-false,
                         :linkedEditingRange t,
                         :documentSymbol t,
                         :documentColor t,
                         :documentFormatting (
                                              :defaultPrintWidth 100
                                              :getDocumentPrintWidthRequest :json-false)
                         :defaultPrintWidth 100
                         :getDocumentPrintWidthRequest :json-false
                         ))
    ))
(provide 'init-vue)
;;; init-vue.el ends here
