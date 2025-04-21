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
    ("Translate to Chinese" . "翻译成中文。对于翻译后的内容，中文和英文/数字之间要保留一个空格。需要翻译的文本: ")
    ("Format quotes" .
     "按照以下要求，格式化内容:
- 如果存在英文和中文翻译，移除英文
- 每行一个句子，在每个中文句号(。)换行
- 行与行之间需要有一行空行，且最多一行空行，如果存在多行空行，请移除
- 如果存在中英文混合，中文和英文/数字之间需要保留一个空格
- 如果涉及到人名，使用英文的名字
- 如果涉及到缩写，需要在中文附近补充英文缩写和完整的英文，如最低合格读者 (MQR, Minimum Qualified Reader)
"))
  "Alist of translation prompt options for `spike-leung/gptel-rewrite'.
Each entry is (DISPLAY . PROMPT).The first entry is the default."
  :type '(alist :key-type string :value-type string)
  :group 'spike-leung)

(defvar spike-leung/gptel-rewrite-last-model 'google/gemini-2.5-flash-preview
  "Last model used for `spike-leung/gptel-rewrite'.")

(defun spike-leung/gptel-rewrite (&optional prompt model)
  "Rewrite the selected region using a customizable PROMPT.
Always prompt the user to select or enter a prompt.
With optional MODEL (prefix arg), prompt for model, default is 'google/gemini-2.5-flash-preview.
If a model is selected, it is memorized for next use."
  (interactive
   (let* ((seperator " - ")
          (key-face `(:foreground ,(modus-themes-get-color-value 'green-cooler)))
          (choices
           (append
            (mapcar (lambda (entry)
                      (let ((key (car entry))
                            (val (cdr entry)))
                        (format "%s%s%s" (propertize key 'face key-face) seperator val)))
                    spike-leung/custom-rewrite-prompts)
            (list (propertize "Custom..." 'face key-face))))
          (choice (completing-read "Choose rewrite prompt: " choices nil t))
          (prompt-text (if (string-equal choice "Custom...")
                           (read-string "Custom rewrite prompt: ")
                         (let* ((key (car (split-string choice seperator)))
                                (found (assoc key spike-leung/custom-rewrite-prompts)))
                           (cdr found))))
          (model (when current-prefix-arg
                   (intern
                    (completing-read
                     (format "Choose model (default %s): "
                             (symbol-name spike-leung/gptel-rewrite-last-model))
                     (mapcar #'symbol-name spike-leung/openrouter-models)
                     nil t
                     nil nil
                     (symbol-name spike-leung/gptel-rewrite-last-model))))))
     (list prompt-text (or model spike-leung/gptel-rewrite-last-model))))
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
         (text (buffer-substring-no-properties start end)))
    (when model
      (setq spike-leung/gptel-rewrite-last-model model))
    (if (string-blank-p text)
        (user-error "No text to translate")
      (let ((openrouter-backend (gptel-make-openai "OpenRouter"
                                  :host "openrouter.ai"
                                  :endpoint "/api/v1/chat/completions"
                                  :stream t
                                  :key (spike-leung/get-openrouter-api-key)
                                  :models spike-leung/openrouter-models)))
        (let ((gptel-backend openrouter-backend)
              (gptel-model (or model spike-leung/gptel-rewrite-last-model))
              (gptel-use-tools nil)
              (gptel-use-context nil)
              (gptel-log-level 'debug))
          (gptel-request
              (format "%s\n\n%s" prompt text)
            :fsm (gptel-make-fsm :handlers gptel-send--handlers)
            :callback
            (lambda (response _)
              (if (and response (not (string-blank-p response)))
                  (save-excursion
                    (delete-region start end)
                    (goto-char start)
                    (insert response))
                (message "Translation failed.")))))))))

;;; keybindings
(global-set-key (kbd "M-o u") 'spike-leung/gptel-rewrite)

(provide 'init-gptel)
;;; init-gptel.el ends here
