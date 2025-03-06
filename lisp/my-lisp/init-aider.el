;;; init-aider.el --- aider
;;; Commentary:
(push (expand-file-name "lisp/my-lisp/aider" user-emacs-directory) load-path)
(require 'aider)

(with-eval-after-load 'init-auth
  (with-eval-after-load 'aider
    (setenv "DEEPSEEK_API_KEY" (spike-leung/get-deepseek-api-key))
    (setenv "OPENAI_API_KEY" (spike-leung/get-siliconflow-api-key))
    (setenv "OPENAI_API_BASE" "https://api.siliconflow.cn")
    (setenv "OPENROUTER_API_KEY" (spike-leung/get-openrouter-api-key))
    (setq aider-args '("--no-auto-commits" "--model" "deepseek/deepseek-chat"))
    (setq aider-popular-models '(
                                 "openrouter/anthropic/claude-3.7-sonnet"
                                 "deepseek/deepseek-chat"
                                 "openai/Pro/deepseek-ai/DeepSeek-V3" ;; provide by siliconflow))
                                 ))
    (global-set-key (kbd "M-o a") 'aider-transient-menu)))


(provide 'init-aider)
;;; init-aider.el ends here
