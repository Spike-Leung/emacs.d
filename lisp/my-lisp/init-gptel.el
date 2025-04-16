;;; init-gptel.el --- gptel -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(maybe-require-package 'gptel)

(defvar spike-leung/openrouter-models
  '(;; google
    google/gemini-2.0-flash-001
    google/gemma-3-27b-it:free
    google/gemini-2.5-pro-preview-03-25
    ;; openai
    openai/gpt-4o
    openai/gpt-4o-mini
    openai/gpt-4.1
    openai/gpt-4.1-mini
    openai/gpt-4.1-nano
    ;; anthropic
    anthropic/claude-3.7-sonnet
    anthropic/claude-3.7-sonnet:thinking
    ;; deepseek
    deepseek/deepseek-r1
    deepseek/deepseek-chat
    deepseek/deepseek-chat-v3-0324
    ;; Mistral
    mistralai/mistral-small-3.1-24b-instruct
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
    (setq gptel-model   'openai/gpt-4.1
          gptel-backend
          (gptel-make-openai "OpenRouter"
            :host "openrouter.ai"
            :endpoint "/api/v1/chat/completions"
            :stream t
            :key (spike-leung/get-openrouter-api-key)
            :models spike-leung/openrouter-models))))

(global-set-key (kbd "M-o g") 'gptel-menu)


;;; some helpful utils use gptel
(defun spike-leung/translate-region (prompt)
  "Translate the selected region (default to English) replace it.
If no region is active, try to guess the sentence or paragraph at point.
With prefix argument, PROMPT is used as the translation prompt."
  (interactive
   (list (when current-prefix-arg
           (read-string "Translation prompt: " "Translate the following text to English:"))))
  (require 'gptel)
  (let* ((has-region (use-region-p))
         (bounds
          (cond
           (has-region
            (cons (region-beginning) (region-end)))
           ((use-region-p)
            (cons (region-beginning) (region-end)))
           ((and (fboundp 'bounds-of-thing-at-point))
            (or (bounds-of-thing-at-point 'sentence)
                (bounds-of-thing-at-point 'paragraph)
                (bounds-of-thing-at-point 'line)
                (cons (point) (point))))
           (t (cons (point) (point)))))
         (start (car bounds))
         (end (cdr bounds))
         (text (buffer-substring-no-properties start end))
         (prompt-text (or prompt "Translate the following text to English:")))
    (if (string-blank-p text)
        (user-error "No text to translate")
      (let ((openrouter-backend (gptel-make-openai "OpenRouter"
                                  :host "openrouter.ai"
                                  :endpoint "/api/v1/chat/completions"
                                  :stream t
                                  :key (spike-leung/get-openrouter-api-key)
                                  :models spike-leung/openrouter-models))
            (primary-model 'openai/gpt-4.1-nano)
            (fallback-model 'google/gemini-2.0-flash-001))
        (cl-labels
            ((do-translate
               (model)
               (let ((gptel-backend openrouter-backend)
                     (gptel-model model)
                     (gptel-use-tools nil)
                     (gptel-use-context nil))
                 (gptel-request
                     (format "%s\n\n%s" prompt-text text)
                   :callback
                   (lambda (response _)
                     (if (and response (not (string-blank-p response)))
                         (save-excursion
                           (delete-region start end)
                           (goto-char start)
                           (insert response))
                       (if (eq model primary-model)
                           (progn
                             (message "Primary model failed, retrying with fallback model...")
                             (do-translate fallback-model))
                         (message "Translation failed with both models."))))))))
          (do-translate primary-model))))))

;;; keybindings
(defvar spike-leung/my-gptel-utils (make-sparse-keymap)
  "Keymap for gptel utils commands.")

(define-key spike-leung/my-gptel-utils (kbd "t") 'spike-leung/translate-region)
(global-set-key (kbd "M-o u") spike-leung/my-gptel-utils)

(provide 'init-gptel)
;;; init-gptel.el ends here
