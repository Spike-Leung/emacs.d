;;; init-gptel.el --- gptel -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(maybe-require-package 'gptel)

(defvar spike-leung/openrouter-models
  '(;; google
    google/gemini-2.0-flash-001
    google/gemini-2.5-pro-preview-03-25
    google/gemini-2.5-flash-preview
    google/gemini-2.5-flash-preview:thinking
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
    deepseek/deepseek-chat-v3-0324)
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
    (setq gptel-model   'google/gemini-2.5-flash-preview:thinking
          gptel-backend
          (gptel-make-openai "OpenRouter"
            :host "openrouter.ai"
            :endpoint "/api/v1/chat/completions"
            :stream t
            :key (spike-leung/get-openrouter-api-key)
            :models spike-leung/openrouter-models))))

(global-set-key (kbd "M-o g") 'gptel-menu)



;;; some helpful utils use gptel

(defcustom spike-leung/custom-rewrite-prompts
  '(("Translate" . "Translate the following text to English:")
    ("Translate to Chinese" . "将以下文本翻译成中文：")
    ("Format quotes" .
     "按照以下要求，格式化内容:
1.移除英文
2.在每个中文句号(。)换行，行与行之间需要添加一行空行
3.如果存在中英文混合，中文和英文/数字之间需要保留一个空格
4.如果涉及到人名，使用英文的名字
5.如果涉及到缩写，需要在中文附近补充英文缩写和完整的英文，如最低合格读者 (MQR, Minimum Qualified Reader)
"))
  "Alist of translation prompt options for `spike-leung/gptel-rewrite'.
Each entry is (DISPLAY . PROMPT).The first entry is the default."
  :type '(alist :key-type string :value-type string)
  :group 'spike-leung)

(defun spike-leung/gptel-rewrite (&optional prompt)
  "Rewrite the selected region using a customizable PROMPT.
Always prompt the user to select or enter a prompt."
  (interactive
   (list
    (let* ((choices (append (mapcar #'car spike-leung/custom-rewrite-prompts)
                            '("Custom...")))
           (choice (completing-read "Choose rewrite prompt: " choices nil t)))
      (if (string-equal choice "Custom...")
          (read-string "Custom rewrite prompt: ")
        (cdr (assoc choice spike-leung/custom-rewrite-prompts))))))
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
         (prompt-text prompt))
    (if (string-blank-p text)
        (user-error "No text to translate")
      (let ((openrouter-backend (gptel-make-openai "OpenRouter"
                                  :host "openrouter.ai"
                                  :endpoint "/api/v1/chat/completions"
                                  :stream t
                                  :key (spike-leung/get-openrouter-api-key)
                                  :models spike-leung/openrouter-models))
            (primary-model 'openai/gpt-4.1-nano)
            (fallback-model 'google/gemini-2.5-flash-preview))
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

(define-key spike-leung/my-gptel-utils (kbd "t") 'spike-leung/gptel-rewrite)
(global-set-key (kbd "M-o u") spike-leung/my-gptel-utils)

(provide 'init-gptel)
;;; init-gptel.el ends here
