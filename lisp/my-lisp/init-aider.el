;;; init-aider.el --- aider
;;; Commentary:
(push (expand-file-name "lisp/my-lisp/aider" user-emacs-directory) load-path)
(require 'aider)

(with-eval-after-load 'aider
  ;; (setq aider-args '("--no-auto-commits" "--model" "deepseek/deepseek-chat"))
  ;; (setenv "DEEPSEEK_API_KEY" (spike-leung/get-deepseek-api-key))
  (setq aider-args '("--no-auto-commits" "--model" "openai/Pro/deepseek-ai/DeepSeek-V3"))
  (setenv "OPENAI_API_KEY" (spike-leung/get-siliconflow-api-key))
  (setenv "OPENAI_API_BASE" "https://api.siliconflow.cn")
  (global-set-key (kbd "M-o a") 'aider-transient-menu))

(provide 'init-aider)
;;; init-aider.el ends here
