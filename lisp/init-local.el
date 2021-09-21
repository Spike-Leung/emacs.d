;;; Package --- Summary
;; init-local
;; https://github.com/purcell/emacs.d


;;; Commentary:
;;; Code:
(add-to-list 'load-path (expand-file-name "lisp/my-lisp" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "lisp/my-lisp/beancount-mode" user-emacs-directory))

;; (require 'init-my-themes)
(require 'init-modus-theme)
(require 'init-extension-mode)
(require 'init-my-org)
(require 'init-mu4e)
(require 'init-org-roam)
(require 'init-org-download)
(require 'init-deft)
(require 'init-easy-hugo)
(require 'init-my-markdown)
(require 'beancount)

(provide 'init-local)
;;; init-local.el ends here
