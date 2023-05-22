;;; init-deno.el --- deno eglot support
;;; Commentary:
;;; Code:
(when (maybe-require-package 'eglot)
  (eval-after-load 'eglot
    (add-to-list 'eglot-server-programs '((js-mode typescript-mode) . (eglot-deno "deno" "lsp")))))

(defclass eglot-deno (eglot-lsp-server) ()
  :documentation "A custom class for deno lsp.")

(cl-defmethod eglot-initialization-options ((server eglot-deno))
  "Passes through required deno initialization options."
  (list :enable t
        :lint t))
(provide 'init-deno)
;;; init-deno.el ends here
