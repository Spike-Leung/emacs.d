;;; init-aider.el --- aider -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(require 'init-openrouter-models)
(maybe-require-package 'aider)
(require 'aider)

(with-eval-after-load 'init-auth
  ;; `with-eval-after-load' 是首次加载时才会执行 body，如果已经加载过了，就不会执行 body
  ;; `eval-after-load' 无关是否首次加载
  (eval-after-load 'aider
    (progn
      (setenv "DEEPSEEK_API_KEY" (spike-leung/get-deepseek-api-key))
      (setenv "OPENAI_API_KEY" (spike-leung/get-siliconflow-api-key))
      (setenv "OPENAI_API_BASE" "https://api.siliconflow.cn")
      (setenv "OPENROUTER_API_KEY" (spike-leung/get-openrouter-api-key))
      (setq aider-args '("--no-auto-commits"
                         "--model" "openrouter/google/gemini-2.5-pro-preview"
                         "--editor-model" "openrouter/google/gemini-2.5-flash-preview-05-20"
                         "--weak-model" "openrouter/google/gemini-2.5-flash-preview-05-20"))
      ;; Use advice to lazy-load models before menu
      (advice-add 'aider-transient-menu :before #'spike-leung/aider-ensure-models)))

  (with-eval-after-load 'init-my-keybindings
    (define-key spike-leung/meta-o-keymap (kbd "a") 'aider-transient-menu)))

(defun spike-leung/aider-ensure-models (&rest _args)
  "Set `aider-popular-models` from OpenRouter models cache."
  (unless spike-leung/openrouter-models-cache
    (spike-leung/get-openrouter-models))
  (when spike-leung/openrouter-models-cache
    (setq aider-popular-models
          (mapcar (lambda (m) (concat "openrouter/" (symbol-name m)))
                  spike-leung/openrouter-models-cache))))

(add-hook 'aider-comint-mode-hook (lambda ()
                                    (set-face-attribute 'comint-highlight-prompt nil :foreground (modus-themes-get-color-value 'green-cooler))
                                    (set-face-attribute 'comint-highlight-input nil :weight 'thin :foreground (modus-themes-get-color-value 'yellow-warmer))))

(provide 'init-aider)
;;; init-aider.el ends here
