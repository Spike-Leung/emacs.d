;;; init-openrouter-models.el --- Fetch and cache OpenRouter models -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(maybe-require-package 'plz)
(require 'plz)

(defvar spike-leung/openrouter-models-cache '()
  "Cached list of OpenRouter models as symbols. Initialized to an empty list.")

(defvar spike-leung/openrouter-models-cache-timestamp nil
  "Timestamp (float) of last OpenRouter models cache.")

(defcustom spike-leung/openrouter-models-cache-ttl 3600
  "Time-to-live for OpenRouter models cache, in seconds."
  :type 'integer)

(defvar spike-leung/openrouter-models-updated-hook nil
  "Hook run after OpenRouter models are fetched and cache is updated.")

(defun spike-leung/fetch-openrouter-models (&optional callback)
  "Fetch the list of models from OpenRouter API and cache them.
If CALLBACK is non-nil, call it with the models list.
Runs `spike-leung/openrouter-models-updated-hook` after updating cache."
  (plz 'get "https://openrouter.ai/api/v1/models"
    :headers '(("Content-Type" . "application/json"))
    :as #'json-read
    :then (lambda (response)
            (let ((models (mapcar (lambda (m) (intern (alist-get 'id m)))
                                  (alist-get 'data response))))
              (setq spike-leung/openrouter-models-cache models)
              (setq spike-leung/openrouter-models-cache-timestamp (float-time))
              (run-hooks 'spike-leung/openrouter-models-updated-hook)
              (when callback (funcall callback models))
              (message "OpenRouter models updated: %s" (length models))))))

(defun spike-leung/get-openrouter-models (&optional force-refresh callback)
  "Get cached OpenRouter models, refresh if cache is old or FORCE-REFRESH.
If CALLBACK is non-nil, call it with the models list (async)."
  (interactive)
  (if (or force-refresh
          (null spike-leung/openrouter-models-cache) ; This is true if cache is '()
          (null spike-leung/openrouter-models-cache-timestamp)
          (> (- (float-time) spike-leung/openrouter-models-cache-timestamp)
             spike-leung/openrouter-models-cache-ttl))
      (spike-leung/fetch-openrouter-models callback)
    (progn
      (when callback (funcall callback spike-leung/openrouter-models-cache))
      ;; If cache is valid and callback exists, still run the hook
      ;; in case the consumer wants to react even if no fetch occurred.
      ;; However, the primary use case is after a fetch.
      ;; For now, let's only run hook after actual fetch/update.
      ;; If needed, this can be revisited.
      spike-leung/openrouter-models-cache)))

;; init
(spike-leung/get-openrouter-models nil)

(provide 'init-openrouter-models)
;;; init-openrouter-models.el ends here
