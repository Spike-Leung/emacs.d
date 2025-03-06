;;; init-auth --- config for auth related -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(setq epa-pinentry-mode 'loopback) ;; input pass in minibuffer

;; (auth-source-pass-enable) ;;; use pass to manage auth-source

;;; function to get api-key from authinfo
(defun spike-leung/get-api-key (service &optional host)
  "Retrieve the API key for SERVICE from authinfo.  if HOST exist, retrieve by HOST."
  (let* ((host (or host (format "api.%s.com" service)))
         (creds (car (auth-source-search :host host :port 443))))
    (if creds
        (let ((api-key (plist-get creds :secret
                                  )))
          (if (functionp api-key)
              (funcall api-key)
            api-key
            (error "API key not found for %s" service)))
      (error "No credentials found for %s" service))))

(defun spike-leung/get-deepseek-api-key ()
  "Retrieve the DeepSeek API key from authinfo."
  (spike-leung/get-api-key "deepseek"))

(defun spike-leung/get-siliconflow-api-key ()
  "Retrieve the SiliconFlow API key from authinfo."
  (spike-leung/get-api-key "siliconflow" "api.siliconflow.cn"))

(defun spike-leung/get-openrouter-api-key ()
  "Retrieve the OpenRouter API key from authinfo."
  (spike-leung/get-api-key "openrouter" "openrouter.ai"))

(defun spike-leung/get-gemini-api-key ()
  "Retrieve the Gemini API key from authinfo."
  (spike-leung/get-api-key "gemini"))

(provide 'init-auth)
;;; init-auth.el ends here.
