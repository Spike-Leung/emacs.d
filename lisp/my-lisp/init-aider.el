;;; init-aider.el --- aider -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(require 'aider)

(with-eval-after-load 'init-auth
  (with-eval-after-load 'aider
    (setenv "DEEPSEEK_API_KEY" (spike-leung/get-deepseek-api-key))
    (setenv "OPENAI_API_KEY" (spike-leung/get-siliconflow-api-key))
    (setenv "OPENAI_API_BASE" "https://api.siliconflow.cn")
    (setenv "OPENROUTER_API_KEY" (spike-leung/get-openrouter-api-key))
    (setq aider-args '("--no-auto-commits" "--model" "openrouter/qwen/qwq-32b"))
    (setq aider-popular-models '("openrouter/anthropic/claude-3.7-sonnet"
                                 "openrouter/deepseek/deepseek-r1"
                                 "openrouter/openai/gpt-4o"
                                 "openrouter/openai/gpt-4o-mini"
                                 "openrouter/google/gemini-2.0-flash-001"
                                 "openrouter/qwen/qwq-32b"
                                 ;; deepseek official
                                 "deepseek/deepseek-chat"
                                 ;; provide by siliconflow
                                 "openai/Pro/deepseek-ai/DeepSeek-V3"))
    (global-set-key (kbd "M-o a") 'aider-transient-menu)))


(provide 'init-aider)
;;; init-aider.el ends here
