;;; Package --- Summary
;; init-beancount
;;; Commentary:
;;; Code:

(push (expand-file-name "lisp/my-lisp/beancount-mode" user-emacs-directory) load-path)

(require 'beancount)

(push '("\\.beancount\\'" . beancount-mode) auto-mode-alist)

(provide 'init-beancount)
;;; init-beancount.el ends here
