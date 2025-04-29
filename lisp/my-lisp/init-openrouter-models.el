;;; init-openrouter-models.el --- Fetch and cache OpenRouter models -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'json)
(require 'url)

(defvar spike-leung/openrouter-models-cache nil
  "Cached list of OpenRouter models as symbols.")

(defvar spike-leung/openrouter-models-cache-timestamp nil
  "Timestamp (float) of last OpenRouter models cache.")

(defcustom spike-leung/openrouter-models-cache-ttl 3600
  "Time-to-live for OpenRouter models cache, in seconds."
  :type 'integer)

(defun spike-leung/fetch-openrouter-models (&optional callback)
  "Fetch the list of models from OpenRouter API and cache them.
If CALLBACK is non-nil, call it with the models list."
  (let ((url-request-method "GET")
        (url "https://openrouter.ai/api/v1/models"))
    (url-retrieve
     url
     (lambda (_status)
       (goto-char url-http-end-of-headers)
       (let* ((json-object-type 'alist)
              (json-array-type 'list)
              (json-key-type 'symbol)
              (data (json-read))
              (models (mapcar (lambda (m) (intern (alist-get 'id m)))
                              (alist-get 'data data))))
         (setq spike-leung/openrouter-models-cache models)
         (setq spike-leung/openrouter-models-cache-timestamp (float-time))
         (when callback (funcall callback models))
         (message "OpenRouter models updated: %s" (length models)))))))

(defun spike-leung/get-openrouter-models (&optional force-refresh callback)
  "Get cached OpenRouter models, refresh if cache is old or FORCE-REFRESH.
If CALLBACK is non-nil, call it with the models list (async)."
  (interactive)
  (if (or force-refresh
          (null spike-leung/openrouter-models-cache)
          (null spike-leung/openrouter-models-cache-timestamp)
          (> (- (float-time) spike-leung/openrouter-models-cache-timestamp)
             spike-leung/openrouter-models-cache-ttl))
      (spike-leung/fetch-openrouter-models callback)
    (when callback (funcall callback spike-leung/openrouter-models-cache))
    spike-leung/openrouter-models-cache))

;; init
(spike-leung/get-openrouter-models nil)

(provide 'init-openrouter-models)
;;; init-openrouter-models.el ends here
