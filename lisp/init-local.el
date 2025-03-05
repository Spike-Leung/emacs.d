;;; init-local.el --- load local custom configs
;; https://github.com/purcell/emacs.d
;;; Commentary:
;;; Code:
(add-to-list 'load-path (expand-file-name "lisp/my-lisp" user-emacs-directory))

(require 'init-aider)
(require 'init-auth)
(require 'init-auto-save)
(require 'init-citre)
(require 'init-company)
(require 'init-denote)
(require 'init-elfeed)
(require 'init-gitmoji)
(require 'init-gptel)
(require 'init-helpful)
(require 'init-icons)
(require 'init-magit)
(require 'init-modus-theme)
(require 'init-my-dired)
(require 'init-my-editing-utils)
(require 'init-my-javascript)
(require 'init-my-markdown)
(require 'init-my-misc)
(require 'init-my-org)
(require 'init-org-publish)
(require 'init-projectile)
(require 'init-translate)
(require 'init-vue-mode)
(require 'init-yasnippet)

(when *is-a-mac*
  (require 'init-beancount)
  (require 'init-mu4e)
  (require 'init-proxy))

(when *is-wsl*
  (require 'init-wsl))

(provide 'init-local)
;;; init-local.el ends here
