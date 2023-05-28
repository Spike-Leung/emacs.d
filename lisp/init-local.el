;;; init-local.el --- load local custom configs
;; https://github.com/purcell/emacs.d
;;; Commentary:
;;; Code:
(add-to-list 'load-path (expand-file-name "lisp/my-lisp" user-emacs-directory))

(require 'init-auto-save)
(require 'init-beancount)
(require 'init-breadcrumb)
;; (require 'init-codeium)
(require 'init-company)
(require 'init-easy-hugo)
(require 'init-elfeed)
(require 'init-gitmoji)
(require 'init-mu4e)
(require 'init-my-eglot)
(require 'init-my-markdown)
(require 'init-my-org)
(require 'init-modus-theme)
(require 'init-org-roam)
(require 'init-org-roam-ui)
(require 'init-projectile)
(require 'init-proxy)
(require 'init-typescript)
(require 'init-yasnippet)
(provide 'init-local)
;;; init-local.el ends here
