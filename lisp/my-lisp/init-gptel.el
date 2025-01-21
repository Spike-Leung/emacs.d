;;; init-gptel.el --- gptel
;;; Commentary:

(maybe-require-package 'gptel)

(with-eval-after-load 'gptel
  (gptel-make-openai "DeepSeek"
    :host "api.deepseek.com"
    :endpoint "/chat/completions"
    :stream t
    :key (spike-leung/get-deepseek-api-key)
    :models '(deepseek-chat deepseek-coder))
  (gptel-make-gemini "Gemini" :key (spike-leung/get-gemini-api-key) :stream t)
  (setq
   gptel-model 'deepseek-reasoner
   gptel-backend (gptel-make-openai "DeepSeek"
                   :host "api.deepseek.com"
                   :endpoint "/chat/completions"
                   :stream t
                   :key (spike-leung/get-deepseek-api-key)
                   :models '(deepseek-reasoner deepseek-chat deepseek-coder))))

(global-set-key (kbd "M-o g") 'gptel-menu)

(provide 'init-gptel)
;;; init-gptel.el ends here
