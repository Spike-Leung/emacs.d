;;; init-gptel.el --- gptel -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(maybe-require-package 'gptel)

(require 'init-openrouter-models) ; Ensures spike-leung/openrouter-models-cache and hook are defined

(defvar spike-leung/siliconflow-models
  '(Pro/deepseek-ai/DeepSeek-R1
    Pro/deepseek-ai/DeepSeek-V3
    deepseek-ai/DeepSeek-R1-Distill-Qwen-32B
    deepseek-ai/DeepSeek-R1-Distill-Llama-70B)
  "List of available models for SiliconFlow API.")

(defun spike-leung/gptel-refresh-openrouter-provider ()
  "Refresh gptel's OpenRouter provider with the latest models.
This function is intended to be called from `spike-leung/openrouter-models-updated-hook`."
  (when (fboundp 'spike-leung/get-openrouter-api-key) ; From init-auth
    (let ((api-key (spike-leung/get-openrouter-api-key)))
      (if api-key
          (progn
            ;; Update the OpenRouter provider definition in gptel's list
            (gptel-make-openai "OpenRouter"
              :host "openrouter.ai"
              :endpoint "/api/v1/chat/completions"
              :stream t
              :key api-key
              :models spike-leung/openrouter-models-cache)

            ;; If gptel-backend is currently set to the OpenRouter provider,
            ;; update it to use the new model list.
            (when (and (boundp 'gptel-backend)
                       gptel-backend ; Ensure gptel-backend is not nil
                       (listp gptel-backend) ; gptel-backend is a plist
                       (string= (plist-get gptel-backend :name) "OpenRouter"))
              (setq gptel-backend
                    (gptel-make-openai "OpenRouter"
                      :host "openrouter.ai"
                      :endpoint "/api/v1/chat/completions"
                      :stream t
                      :key api-key
                      :models spike-leung/openrouter-models-cache)))
            (message "gptel OpenRouter provider models refreshed via hook."))
        (message "Cannot refresh gptel OpenRouter provider via hook: API key not found.")))))

(with-eval-after-load 'init-auth
  (with-eval-after-load 'gptel
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
      :models spike-leung/openrouter-models-cache)
    (gptel-make-gemini "Gemini" :key (spike-leung/get-gemini-api-key) :stream t)

    (setq gptel-model   'google/gemini-2.5-flash-preview-05-20:thinking
          gptel-backend
          (gptel-make-openai "OpenRouter"
            :host "openrouter.ai"
            :endpoint "/api/v1/chat/completions"
            :stream t
            :key (spike-leung/get-openrouter-api-key)
            :models spike-leung/openrouter-models-cache)))

  ;; Add hook function to refresh OpenRouter provider when models are updated
  (add-hook 'spike-leung/openrouter-models-updated-hook #'spike-leung/gptel-refresh-openrouter-provider)

  ;; If models were fetched before gptel loaded and hook was added,
  ;; explicitly refresh now to ensure consistency if cache is populated.
  ;; This is particularly relevant if init-openrouter-models.el's initial fetch
  ;; populates the cache before this eval-after-load block runs.
  (when (and spike-leung/openrouter-models-cache
             (> (length spike-leung/openrouter-models-cache) 0))
    (spike-leung/gptel-refresh-openrouter-provider)))

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
                       (mapcar #'symbol-name spike-leung/openrouter-models-cache) ; Uses the dynamic cache
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
           ((use-region-p) ; This condition is redundant due to has-region
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
      (let ((gptel-model model)
            (gptel-backend (gptel-make-openai "OpenRouter"
                             :host "openrouter.ai"
                             :endpoint "/api/v1/chat/completions"
                             :stream t
                             :key (spike-leung/get-openrouter-api-key)
                             :models spike-leung/openrouter-models-cache)) ; Otherwise, use the current default
            (gptel-use-tools nil)
            (gptel-use-context nil)
            (gptel-log-level 'debug))
        (unless (and (listp gptel-backend) (string= (plist-get gptel-backend :name) "OpenRouter"))
          ;; If the selected model is an OpenRouter model but the backend isn't OpenRouter,
          ;; we might need to explicitly set it.
          ;; For now, this relies on the user setting gptel-backend appropriately
          ;; or the (gptel-provider "OpenRouter") call above.
          )
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
              (message "Translation failed."))))))))

(with-eval-after-load 'init-my-keybindings
  (define-key spike-leung/meta-o-keymap (kbd "g") 'gptel-menu)
  (define-key spike-leung/meta-o-keymap (kbd "u") 'spike-leung/gptel-rewrite))

(provide 'init-gptel)
;;; init-gptel.el ends here
