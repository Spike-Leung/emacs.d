;;; init-openrouter-models.el --- Fetch and cache OpenRouter models -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(maybe-require-package 'plz)
(require 'plz)

(defvar spike-leung/openrouter-models-cache '()
  "Cached list of OpenRouter models as symbols. For completion.")

(defvar spike-leung/openrouter-models-data-cache (make-hash-table :test 'equal)
  "Cached hash table of OpenRouter models data.
Key is model ID string, value is an alist of model properties.")

(defvar spike-leung/openrouter-models-cache-timestamp nil
  "Timestamp (float) of last OpenRouter models cache.")

(defcustom spike-leung/openrouter-models-cache-ttl 3600
  "Time-to-live for OpenRouter models cache, in seconds."
  :type 'integer)

(defvar spike-leung/openrouter-models-updated-hook nil
  "Hook run after OpenRouter models are fetched and cache is updated.")

(defun spike-leung/fetch-openrouter-models (&optional callback)
  "Fetch model list from OpenRouter, cache IDs and pricing data.
If CALLBACK is non-nil, call it with the list of model ID symbols.
Runs `spike-leung/openrouter-models-updated-hook` after updating cache."
  (plz 'get "https://openrouter.ai/api/v1/models"
    :headers '(("Content-Type" . "application/json"))
    :as #'json-read
    :then (lambda (response)
            (let ((models-data (alist-get 'data response))
                  (new-models-list '())
                  (new-data-cache (make-hash-table :test 'equal)))
              (seq-map (lambda (model-alist)
                         (let* ((id (alist-get 'id model-alist))
                                (pricing (alist-get 'pricing model-alist)))
                           (when (and id pricing)
                             (push (intern id) new-models-list)
                             (puthash id
                                      `((input . ,(alist-get 'prompt pricing))
                                        (output . ,(alist-get 'completion pricing)))
                                      new-data-cache)))) models-data)
              ;; Update caches
              (setq spike-leung/openrouter-models-cache (nreverse new-models-list)
                    spike-leung/openrouter-models-data-cache new-data-cache
                    spike-leung/openrouter-models-cache-timestamp (float-time))
              ;; Run hooks and callback
              (run-hooks 'spike-leung/openrouter-models-updated-hook)
              (when callback (funcall callback spike-leung/openrouter-models-cache))
              (message "OpenRouter models updated: %s" (length spike-leung/openrouter-models-cache))))))

(defun spike-leung/get-openrouter-models (&optional force-refresh callback)
  "Get cached OpenRouter models, refresh if cache is old or FORCE-REFRESH.
If CALLBACK is non-nil, call it with the models list (async).
This function returns the cached list of model ID symbols synchronously
if the cache is valid."
  (interactive "P")
  (if (or force-refresh
          (null spike-leung/openrouter-models-cache)
          (null spike-leung/openrouter-models-cache-timestamp)
          (> (- (float-time) spike-leung/openrouter-models-cache-timestamp)
             spike-leung/openrouter-models-cache-ttl))
      (spike-leung/fetch-openrouter-models callback)
    (progn
      (when callback (funcall callback spike-leung/openrouter-models-cache))
      spike-leung/openrouter-models-cache)))

(defun spike-leung/list-openrouter-models (&optional force-refresh)
  "Display all OpenRouter models and pricing in an org-mode table.
With a prefix argument FORCE-REFRESH, force a refresh of the model list."
  (interactive "P")
  (let ((show-table-fn
         (lambda (&optional _)
           (let ((buffer-name "*OpenRouter Models*"))
             (with-current-buffer (get-buffer-create buffer-name)
               (let ((inhibit-read-only t))
                 (erase-buffer)
                 (insert "| Model ID | Input Cost (/M) | Output Cost (/M) |\n")
                 (insert "|-|\n")
                 (let ((format-price
                        (lambda (price-str)
                          (if (and price-str (> (length price-str) 0))
                              (format "$%.2f" (* (read price-str) 1000000))
                            "N/A"))))
                   (dolist (model-sym spike-leung/openrouter-models-cache)
                     (let* ((model-id (symbol-name model-sym))
                            (data (gethash model-id spike-leung/openrouter-models-data-cache)))
                       (insert (format "| %s | %s | %s |\n"
                                       model-id
                                       (funcall format-price (cdr (assoc 'input data)))
                                       (funcall format-price (cdr (assoc 'output data))))))))
                 (org-mode)
                 (org-table-align)
                 (goto-char (point-min)))
               (pop-to-buffer buffer-name))))))
    (if (and (not force-refresh) spike-leung/openrouter-models-cache)
        (funcall show-table-fn)
      (message "Fetching OpenRouter models...")
      (spike-leung/get-openrouter-models t show-table-fn))))

;; init
(spike-leung/get-openrouter-models nil)

(provide 'init-openrouter-models)
;;; init-openrouter-models.el ends here
