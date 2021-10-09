;;; Package --- Summary
;; init-my-markdown
;;; Commentary:
;;; - https://stackoverflow.com/questions/36183071/how-can-i-preview-markdown-in-emacs-in-real-time/36189456?noredirect=1#comment104784050_36189456
;;; - https://wikemacs.org/wiki/Markdown#Live_preview_as_you_type
;;; TODO: 写一个函数，完成启动服务器，启动impatient-mode, 设置filter的步骤
;;; Code:

(defun my-markdown-html (buffer)
  (princ (with-current-buffer buffer
           (format "<!DOCTYPE html><html><title>Impatient Markdown</title><xmp theme=\"united\" style=\"display:none;\"> %s  </xmp><script src=\"http://strapdownjs.com/v/0.2/strapdown.js\"></script></html>"
                   (buffer-substring-no-properties (point-min) (point-max))))
         (current-buffer)))

(provide 'init-my-markdown)

;;; init-my-markdown.el ends here