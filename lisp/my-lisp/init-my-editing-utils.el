;;; init-my-editing-utils.el --- Day-to-day editing helpers -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(maybe-require-package 'string-inflection)
(maybe-require-package 'vundo)

(global-set-key (kbd "C-x u") 'vundo)

;; see: https://arialdomartini.github.io/consult-line-at-point
(defun spike-leung/consult-line-symbol-at-point ()
  "Search for a line matching the symbol found near point."
  (interactive)
  (consult-line
   (or (thing-at-point 'symbol))))

(global-set-key (kbd "M-s .") #'spike-leung/consult-line-symbol-at-point)
(global-set-key (kbd "M-s M-s .") #'isearch-forward-symbol-at-point)

(provide 'init-my-editing-utils)
;;; init-my-editing-utils.el ends here
