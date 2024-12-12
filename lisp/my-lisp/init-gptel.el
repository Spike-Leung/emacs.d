;;; init-gptel.el --- gptel
;;; Commentary:

(maybe-require-package 'gptel)

(defun spike-leung/get-api-key (service)
  "Retrieve the API key for SERVICE from authinfo."
  (let* ((host (format "api.%s.com" service))
         (creds (car (auth-source-search :host host :port 443))))
    (if creds
        (let ((api-key (plist-get creds :secret)))
          (if api-key
              api-key
            (error "API key not found for %s" service)))
      (error "No credentials found for %s" service))))

(defun spike-leung/get-deepseek-api-key ()
  "Retrieve the DeepSeek API key from authinfo."
  (spike-leung/get-api-key "deepseek"))

(defun spike-leung/get-gemini-api-key ()
  "Retrieve the Gemini API key from authinfo."
  (spike-leung/get-api-key "gemini"))

(with-eval-after-load 'gptel
  (gptel-make-gemini "Gemini" :key (spike-leung/get-gemini-api-key) :stream t)
  (setq gptel-model   'deepseek-chat
        gptel-backend
        (gptel-make-openai "DeepSeek"
          :host "api.deepseek.com"
          :endpoint "/chat/completions"
          :stream t
          :key (spike-leung/get-deepseek-api-key) ;can be a function that returns the key
          :models '(deepseek-chat deepseek-coder))))

(global-set-key (kbd "M-o g") 'gptel-menu)

(provide 'init-gptel)
;;; init-gptel.el ends here
