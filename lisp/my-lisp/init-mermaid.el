;;; init-mermaid.el --- mermaid -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Mermaid: https://mermaid.js.org/
;; mermaid-mode: https://github.com/abrochard/mermaid-mode
;; install mermaid cli: ni -g @mermaid-js/mermaid-cli
;; see: https://github.com/mermaid-js/mermaid-cli

(maybe-require-package 'mermaid-mode)
(add-to-list 'auto-mode-alist '("\\.mmd\\'" . mermaid-mode))
(provide 'init-mermaid)
;;; init-mermaid.el ends here
