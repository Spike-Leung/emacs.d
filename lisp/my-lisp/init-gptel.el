;;; init-gptel.el --- gptel
;;; Commentary:

(maybe-require-package 'gptel)

(defun spike-leung/get-deepseek-api-key ()
  "Retrieve the DeepSeek API key from authinfo."
  (let ((creds (car (auth-source-search :host "api.deepseek.com" :port 443))))
    (if creds
        (let ((api-key (plist-get creds :secret)))
          (if api-key
              api-key
            (error "API key not found for DeepSeek")))
      (error "No credentials found for DeepSeek"))))

(spike-leung/get-deepseek-api-key)

(setq gptel-model   'deepseek-chat
      gptel-backend
      (gptel-make-openai "DeepSeek"
        :host "api.deepseek.com"
        :endpoint "/chat/completions"
        :stream t
        :key (spike-leung/get-deepseek-api-key) ;can be a function that returns the key
        :models '(deepseek-chat deepseek-coder)))

(provide 'init-gptel)
;;; init-gptel.el ends here
