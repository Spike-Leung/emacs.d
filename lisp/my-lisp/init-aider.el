;;; init-aider.el --- aider -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(require 'init-openrouter-models)

(when (maybe-require-package 'aider)
  (with-eval-after-load 'init-auth
    ;; `with-eval-after-load' 是首次加载时才会执行 body，如果已经加载过了，就不会执行 body
    ;; `eval-after-load' 无关是否首次加载
    (eval-after-load 'aider
      (progn
        (setenv "DEEPSEEK_API_KEY" (spike-leung/get-deepseek-api-key))
        (setenv "OPENAI_API_KEY" (spike-leung/get-siliconflow-api-key))
        (setenv "OPENAI_API_BASE" "https://api.siliconflow.cn")
        (setenv "OPENROUTER_API_KEY" (spike-leung/get-openrouter-api-key))
        (setq aider-args '("--no-auto-commits" "--model" "openrouter/openai/gpt-4.1"))
        ;; Use advice to lazy-load models before menu
        (advice-add 'aider-transient-menu :before #'spike-leung/aider-ensure-models)))

    (global-set-key (kbd "M-o a") 'aider-transient-menu)))

(defun spike-leung/aider-ensure-models (&rest _args)
  "Ensure `aider-popular-models` is set from OpenRouter models."
  (unless aider-popular-models
    (spike-leung/get-openrouter-models
     nil
     (lambda (models)
       (setq aider-popular-models
             (mapcar (lambda (m) (concat "openrouter/" (symbol-name m))) models))))))

(add-hook 'aider-comint-mode-hook (lambda ()
                                    (set-face-attribute 'comint-highlight-prompt nil :foreground (modus-themes-get-color-value 'green-cooler))
                                    (set-face-attribute 'comint-highlight-input nil :weight 'thin :foreground (modus-themes-get-color-value 'yellow-warmer))))

(provide 'init-aider)
;;; init-aider.el ends here
