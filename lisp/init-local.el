;;; Package --- Summary
;; init-local
;; https://github.com/purcell/emacs.d


;;; Commentary:
;;; Code:
(add-to-list 'load-path (expand-file-name "lisp/my-lisp" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "lisp/my-lisp/beancount-mode" user-emacs-directory))

(require 'init-modus-theme)
(require 'init-mu4e)
(require 'init-my-org)
(require 'init-org-roam)
(require 'init-org-download)
(require 'init-my-markdown)
(require 'beancount)
(require 'init-easy-hugo)
(require 'init-extension-mode)
(require 'init-elfeed)

(provide 'init-local)
;;; init-local.el ends here
