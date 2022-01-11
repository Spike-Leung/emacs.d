;;; Package --- Summary
;; init-beancount
;;; Commentary:
;;; Code:

(push (expand-file-name "lisp/my-lisp/beancount-mode" user-emacs-directory) load-path)

(setq beancount-accounts-files
      (directory-files "~/Dropbox/beancount/accounts/" 'full (rx ".beancount" eos)))
(require 'beancount)

(push '("\\.\\(beancount\\|bean\\)\\'" . beancount-mode) auto-mode-alist)

(add-hook 'beancount-mode-hook #'outline-minor-mode)
(define-key beancount-mode-map (kbd "C-c C-n") #'outline-next-visible-heading)
(define-key beancount-mode-map (kbd "C-c C-p") #'outline-previous-visible-heading)
(provide 'init-beancount)
;;; init-beancount.el ends here
