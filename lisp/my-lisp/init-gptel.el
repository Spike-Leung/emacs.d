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
    openai/gpt-4.1
    openai/gpt-4.1-mini
    openai/gpt-4.1-nano
    ;; anthropic
    anthropic/claude-3.7-sonnet
    ;; deepseek
    deepseek/deepseek-r1
    deepseek/deepseek-chat
    deepseek/deepseek-chat-v3-0324
    ;; Mistral
    mistralai/mistral-small-3.1-24b-instruct
    ;; openrouter
    openrouter/optimus-alpha
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
    (setq gptel-model   'openai/gpt-4.1-nano
          gptel-backend
          (gptel-make-openai "OpenRouter"
            :host "openrouter.ai"
            :endpoint "/api/v1/chat/completions"
            :stream t
            :key (spike-leung/get-openrouter-api-key)
            :models spike-leung/openrouter-models))))

(global-set-key (kbd "M-o g") 'gptel-menu)


;;; some helpful utils use gptel

(defun spike-leung/translate-region-to-english ()
  "Translate the selected region to English using gptel and replace it in the buffer."
  (interactive)
  (if (use-region-p)
      (let ((text (buffer-substring-no-properties (region-beginning) (region-end))))
        (gptel-request
            (format "Translate the following text to English:\n\n%s" text)
          :callback (lambda (response _)
                      (when response
                        (let ((start (region-beginning))
                              (end (region-end)))
                          (save-excursion
                            (delete-region start end)
                            (goto-char start)
                            (insert response)))))))
    (user-error "No region selected")))

;;; keybindings
(defvar spike-leung/my-gptel-utils (make-sparse-keymap)
  "Keymap for gptel utils commands.")

(define-key spike-leung/my-gptel-utils (kbd "t") 'spike-leung/translate-region-to-english)
(global-set-key (kbd "M-o u") spike-leung/my-gptel-utils)

(provide 'init-gptel)
;;; init-gptel.el ends here
