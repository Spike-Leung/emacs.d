;;; init-gptel.el --- gptel -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(maybe-require-package 'gptel)

(require 'init-openrouter-models)

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
      :models #'spike-leung/get-openrouter-models)
    (gptel-make-gemini "Gemini" :key (spike-leung/get-gemini-api-key) :stream t)
    ;; set default
    (setq gptel-model   'google/gemini-2.5-flash-preview:thinking
          gptel-backend
          (gptel-make-openai "OpenRouter"
            :host "openrouter.ai"
            :endpoint "/api/v1/chat/completions"
            :stream t
            :key (spike-leung/get-openrouter-api-key)
            :models #'spike-leung/get-openrouter-models))))

(defun spike-leung/gptel-ensure-openrouter-models (&rest _args)
  (unless spike-leung/openrouter-models
    (spike-leung/get-openrouter-models
     nil
     (lambda (models)
       (setq spike-leung/openrouter-models models)))))

(advice-add 'spike-leung/gptel-rewrite :before #'spike-leung/gptel-ensure-openrouter-models)

(global-set-key (kbd "M-o g") 'gptel-menu)



;;; some helpful utils use gptel

(require 'init-gptel-prompts)

(defvar spike-leung/gptel-rewrite-last-model 'google/gemini-2.5-flash-preview
  "Last model used for `spike-leung/gptel-rewrite'.")

(defun spike-leung/gptel-rewrite (model &optional prompt)
  "Rewrite region or `thing-at-point' using a customizable PROMPT and MODEL.
If called without prefix, use the default model
`spike-leung/gptel-rewrite-last-model'.
If a model is selected, it is memorized for next use."
  (interactive
   (let* ((seperator " - ")
          (key-face `(:foreground ,(modus-themes-get-color-value 'green-cooler)))
          (model (if current-prefix-arg
                     (intern
                      (completing-read
                       (format "Choose model (default %s): "
                               (symbol-name spike-leung/gptel-rewrite-last-model))
                       (mapcar #'symbol-name spike-leung/openrouter-models)
                       nil t
                       nil nil
                       (symbol-name spike-leung/gptel-rewrite-last-model)))
                   spike-leung/gptel-rewrite-last-model))
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
          )
     (list model prompt-text)))
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
              (gptel-model model)
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
