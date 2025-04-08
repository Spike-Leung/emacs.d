;;; init-gptel.el --- gptel -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(maybe-require-package 'gptel)

(defvar spike-leung/openrouter-models
  '(;; google
    google/gemini-2.0-flash-001
    google/gemma-3-27b-it:free
    google/gemini-2.5-pro-exp-03-25:free
    ;; openai
    openai/gpt-4o-mini
    openai/gpt-4o
    ;; anthropic
    anthropic/claude-3.7-sonnet
    ;; deepseek
    deepseek/deepseek-r1
    deepseek/deepseek-chat
    deepseek/deepseek-chat-v3-0324
    ;; Mistral
    mistralai/mistral-small-3.1-24b-instruct
    ;; openrouter
    openrouter/quasar-alpha
    ;; qwen
    qwen/qwen-2.5-72b-instruct
    qwen/qwq-32b)
  "List of available models for OpenRouter API.")

(defvar spike-leung/siliconflow-models
  '(Pro/deepseek-ai/DeepSeek-R1
    Pro/deepseek-ai/DeepSeek-V3
    deepseek-ai/DeepSeek-R1-Distill-Qwen-32B
    deepseek-ai/DeepSeek-R1-Distill-Llama-70B)
  "List of available models for SiliconFlow API.")


(with-eval-after-load 'init-auth
  (with-eval-after-load 'gptel
    ;; define provider
    (gptel-make-openai "DeepSeek"
      :host "api.deepseek.com"
      :endpoint "/chat/completions"
      :stream t
      :key (spike-leung/get-deepseek-api-key)
      :models '(deepseek-chat deepseek-coder))
    (gptel-make-openai "SiliconFlow"
      :host "api.siliconflow.cn"
      :endpoint "/chat/completions"
      :stream t
      :key (spike-leung/get-siliconflow-api-key)
      :models spike-leung/siliconflow-models)
    (gptel-make-openai "OpenRouter"
      :host "openrouter.ai"
      :endpoint "/api/v1/chat/completions"
      :stream t
      :key (spike-leung/get-openrouter-api-key)
      :models spike-leung/openrouter-models)
    (gptel-make-gemini "Gemini" :key (spike-leung/get-gemini-api-key) :stream t)
    ;; set default
    (setq gptel-model   'openrouter/quasar-alpha
          gptel-backend
          (gptel-make-openai "OpenRouter"
            :host "openrouter.ai"
            :endpoint "/api/v1/chat/completions"
            :stream t
            :key (spike-leung/get-openrouter-api-key)
            :models spike-leung/openrouter-models))))


(global-set-key (kbd "M-o g") 'gptel-menu)

(provide 'init-gptel)
;;; init-gptel.el ends here
