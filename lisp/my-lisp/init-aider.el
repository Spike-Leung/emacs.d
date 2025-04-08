;;; init-aider.el --- aider -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(eval-after-load 'aider (message "aider"))

(when (maybe-require-package 'aider)
  (with-eval-after-load 'init-auth
    ;; `with-eval-after-load' 是首次加载时才会执行 body，如果已经加载过了，就不会执行 body
    ;; `eval-after-load' 无关是否首次加载
    (eval-after-load 'aider
      (progn
        (setenv "DEEPSEEK_API_KEY" (spike-leung/get-deepseek-api-key))
        (setenv "OPENAI_API_KEY" (spike-leung/get-siliconflow-api-key))
        (setenv "OPENAI_API_BASE" "https://api.siliconflow.cn")
        (setenv "OPENROUTER_API_KEY" (spike-leung/get-openrouter-api-key))
        ;; (setq aider-args '("--no-auto-commits" "--model" "openrouter/anthropic/claude-3.7-sonnet"))
        (setq aider-args '("--no-auto-commits" "--model" "openrouter/openrouter/quasar-alpha"))
        (setq aider-popular-models '("openrouter/anthropic/claude-3.7-sonnet"
                                     "openrouter/deepseek/deepseek-r1"
                                     "openrouter/deepseek/deepseek-chat"
                                     "openrouter/deepseek/deepseek-chat-v3-0324"
                                     "openrouter/google/gemini-2.0-flash-001"
                                     "openrouter/google/gemini-2.5-pro-exp-03-25:free"
                                     "openrouter/openrouter/quasar-alpha"
                                     "openrouter/qwen/qwq-32b"
                                     ;; deepseek official
                                     "deepseek/deepseek-chat"
                                     ;; provide by siliconflow
                                     "openai/Pro/deepseek-ai/DeepSeek-V3"))
        (global-set-key (kbd "M-o a") 'aider-transient-menu)))))

(add-hook 'aider-comint-mode-hook (lambda ()
                                    (set-face-attribute 'comint-highlight-prompt nil :foreground (modus-themes-get-color-value 'green-cooler))
                                    (set-face-attribute 'comint-highlight-input nil :weight 'thin :foreground (modus-themes-get-color-value 'yellow-warmer))))

(provide 'init-aider)
;;; init-aider.el ends here
