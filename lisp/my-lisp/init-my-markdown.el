;;; Package --- Summary
;; init-my-markdown
;;; Commentary:
;;; - https://stackoverflow.com/questions/36183071/how-can-i-preview-markdown-in-emacs-in-real-time/36189456?noredirect=1#comment104784050_36189456
;;; - https://wikemacs.org/wiki/Markdown#Live_preview_as_you_type
;;; Code:

(maybe-require-package 'impatient-mode)

(defun spike-leung/imp-markdown-filter (buffer)
  (princ (with-current-buffer buffer
           (format "<!DOCTYPE html><html><title>Impatient Markdown</title><xmp theme=\"united\" style=\"display:none;\"> %s  </xmp><script src=\"https://cdn.jsdelivr.net/gh/Naereen/StrapDown.js@master/strapdown.min.js\"></script></html>"
                   (buffer-substring-no-properties (point-min) (point-max))))
         (current-buffer)))

(defun spike-leung/preview-markdown () (interactive)
  (progn
    (httpd-start)
    (impatient-mode nil)
    (imp-set-user-filter 'spike-leung/imp-markdown-filter)))

(defun spike-leung/disable-preview-markdown () (interactive)
  (progn
    (httpd-stop)
    (impatient-mode -1)
    (imp-remove-user-filter)))


(provide 'init-my-markdown)

;;; init-my-markdown.el ends here
