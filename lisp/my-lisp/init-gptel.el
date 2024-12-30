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
  (gptel-make-gemini "Deepseek" :key (spike-leung/get-deepseek-api-key) :stream t)
  (setq
   gptel-model 'gemini-1.5-pro-latest
   gptel-backend (gptel-make-gemini "Gemini"
                   :key (spike-leung/get-gemini-api-key)
                   :stream t)))

(global-set-key (kbd "M-o g") 'gptel-menu)

(provide 'init-gptel)
;;; init-gptel.el ends here
