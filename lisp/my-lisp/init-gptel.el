;;; init-gptel.el --- gptel -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(maybe-require-package 'gptel)
(require 'init-openrouter-models) ; Ensures spike-leung/openrouter-models-cache and hook are defined

(defconst openrouter-default-model 'google/gemini-3-pro-preview
  "Default model for openrouter.")

(with-eval-after-load 'init-auth
  (with-eval-after-load 'gptel
    (gptel-make-openai "DeepSeek"
      :host "api.deepseek.com"
      :endpoint "/chat/completions"
      :stream t
      :key (spike-leung/get-deepseek-api-key)
      :models '(deepseek-chat deepseek-coder)
      :request-params '(:reasoning (:enable t)))
    (gptel-make-openai "OpenRouter"
      :host "openrouter.ai"
      :endpoint "/api/v1/chat/completions"
      :stream t
      :key (spike-leung/get-openrouter-api-key)
      :models spike-leung/openrouter-models-cache
      :request-params '(:reasoning (:enable t)))
    (setq gptel-model   openrouter-default-model
          gptel-default-mode 'org-mode
          gptel-backend
          (gptel-make-openai "OpenRouter"
            :host "openrouter.ai"
            :endpoint "/api/v1/chat/completions"
            :stream t
            :key (spike-leung/get-openrouter-api-key)
            :models spike-leung/openrouter-models-cache
            :request-params '(:reasoning ( :enable t))))
    (setopt
     gptel--system-message "You are a helpful assistant. Respond concisely."
     gptel-highlight-methods '(fringe face margin))

    (gptel-make-preset 'dict
      :description "英语词典"
      :system "你充当一个字典，将内容翻译成英文，如果结果一个单词，同时给出音标。考虑提供多个可能的含义。"
      :backend "OpenRouter"
      :model 'google/gemini-2.5-flash)

    (gptel-make-preset 'quick
      :description "快速回答"
      :system "You are a helpful assistant. Respond concisely."
      :backend "OpenRouter"
      :model 'deepseek/deepseek-v3.2-exp)

    (add-hook 'spike-leung/openrouter-models-updated-hook #'spike-leung/gptel-refresh-openrouter-provider)
    (add-hook 'gptel-mode-hook 'auto-fill-mode)
    (add-hook 'gptel-mode-hook 'visual-line-mode)
    (add-hook 'gptel-mode-hook 'gptel-highlight-mode)

    (when (and spike-leung/openrouter-models-cache
               (> (length spike-leung/openrouter-models-cache) 0))
      (spike-leung/gptel-refresh-openrouter-provider))))

(defun spike-leung/gptel-refresh-openrouter-provider ()
  "Refresh gptel's OpenRouter provider with the latest models.
This function is intended to be called from `spike-leung/openrouter-models-updated-hook`."
  (when (fboundp 'spike-leung/get-openrouter-api-key) ; From init-auth
    (let ((api-key (spike-leung/get-openrouter-api-key)))
      (if api-key
          (progn
            (gptel-make-openai "OpenRouter"
              :host "openrouter.ai"
              :endpoint "/api/v1/chat/completions"
              :stream t
              :key api-key
              :models spike-leung/openrouter-models-cache)

            (when (and (boundp 'gptel-backend)
                       gptel-backend
                       (listp gptel-backend)
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



;;; some helpful utils use gptel
(require 'init-gptel-prompts)

(defvar spike-leung/gptel-rewrite-last-model 'google/gemini-2.5-flash
  "Last model used for `spike-leung/gptel-rewrite'.")

(defun spike-leung/gptel-rewrite (model &optional prompt)
  "Rewrite region or `thing-at-point' using a customizable PROMPT and MODEL.
If called without prefix, use the default model
`spike-leung/gptel-rewrite-last-model'.
If a model is selected, it is memorized for next use."
  (interactive
   (let* ((seperator " - ")
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
                        (format "%s%s%s" key seperator val)))
                    spike-leung/custom-rewrite-prompts)
            (list "Custom...")))
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
