;;; init-local.el --- load local custom configs
;; https://github.com/purcell/emacs.d
;;; Commentary:
;;; Code:

;; Setup necessary load-path.
(defconst my-lisp-dir (expand-file-name "lisp/my-lisp" user-emacs-directory) "Path of my custom Lisp.")
(add-to-list 'load-path my-lisp-dir)
(dolist (dir (directory-files my-lisp-dir t "^[^.]"))
  (when (file-directory-p dir)
    (add-to-list 'load-path dir)))

(require 'init-aider)
(require 'init-auth)
(require 'init-auto-save)
(require 'init-citre)
(require 'init-denote)
(require 'init-elfeed)
(require 'init-gitmoji)
(require 'init-gptel)
(require 'init-helpful)
(require 'init-icons)
(require 'init-kaomoji)
(require 'init-magit)
(require 'init-mermaid)
(require 'init-my-theme)
(require 'init-my-dired)
(require 'init-my-editing-utils)
(require 'init-my-folding)
(require 'init-my-javascript)
(require 'init-my-markdown)
(require 'init-my-misc)
(require 'init-my-org)
(require 'init-org-publish)
(require 'init-translate)
(require 'init-vue-mode)
(require 'init-yasnippet)

(when *is-a-mac*
  (require 'init-beancount)
  (require 'init-face)
  (require 'init-proxy)
  ;; (require 'init-mu4e)
  )

(when *is-wsl*
  (require 'init-wsl))

(provide 'init-local)
;;; init-local.el ends here
