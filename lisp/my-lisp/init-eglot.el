;;; Package --- Summary
;; init-eglot
;;; Commentary:
;;; Code:
(add-to-list 'eglot-server-programs '((js-mode typescript-mode) . (eglot-deno "deno" "lsp")))

(defclass eglot-deno (eglot-lsp-server) ()
  :documentation "A custom class for deno lsp.")

(cl-defmethod eglot-initialization-options ((server eglot-deno))
  "Passes through required deno initialization options"
  (list :enable t
        :lint t))
(provide 'init-eglot)
;;; init-eglot.el ends here
