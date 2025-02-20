;;; init-local.el --- load local custom configs
;; https://github.com/purcell/emacs.d
;;; Commentary:
;;; Code:
(add-to-list 'load-path (expand-file-name "lisp/my-lisp" user-emacs-directory))

(require 'init-auto-save)
(require 'init-citre)
(require 'init-company)
(require 'init-denote)
(require 'init-my-dired)
(require 'init-elfeed)
(require 'init-gitmoji)
(require 'init-helpful)
(require 'init-my-javascript)
(require 'init-vue-mode)
(require 'init-my-editing-utils)
(require 'init-my-markdown)
(require 'init-my-misc)
(require 'init-my-org)
(require 'init-modus-theme)
(require 'init-projectile)
(require 'init-wsl)
(require 'init-yasnippet)
(require 'init-magit)
(require 'init-translate)
(require 'init-org-publish)
(require 'init-gptel)
(require 'init-aider)
(require 'init-icons)

(provide 'init-local)
;;; init-local.el ends here
