;;; init-aider.el --- aider
;;; Commentary:
(push (expand-file-name "lisp/my-lisp/aider" user-emacs-directory) load-path)
(require 'aider)

(with-eval-after-load 'aider
  (setq aider-args '("--no-auto-commits" "--model" "deepseek/deepseek-coder"))
  (setenv "DEEPSEEK_API_KEY" (spike-leung/get-deepseek-api-key))
  (global-set-key (kbd "M-o a") 'aider-transient-menu))

(provide 'init-aider)
;;; init-aider.el ends here
