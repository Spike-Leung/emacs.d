;;; init-mermaid.el --- mermaid -*- lexical-binding: t -*-
;;; Commentary:
;;;
;;; Mermaid: https://mermaid.js.org/
;;; mermaid-mode: https://github.com/abrochard/mermaid-mode
;;; install mermaid cli: ni -g @mermaid-js/mermaid-cli
;;; see: https://github.com/mermaid-js/mermaid-cli
;;;
;;; Code:

(maybe-require-package 'mermaid-mode)
(add-to-list 'auto-mode-alist '("\\.mmd\\'" . mermaid-mode))

(when *is-wsl*
  (defun spike-leung/preview-mermaid-png ()
    "Prompt for a PNG file and open it with 'feh --reload 1'."
    (interactive)
    (let* ((file (read-file-name "Select PNG file: " nil nil t nil
                                 (lambda (f) (string-match-p "\\.png\\'" f)))))
      (when (and file (file-exists-p file) (string-match-p "\\.png\\'" file))
        (message "file: %s" file)
        (start-process "preview-mermaid" "*preview-mermaid*" "feh" "--reload" "1" (expand-file-name file))))))

(provide 'init-mermaid)
;;; init-mermaid.el ends here
