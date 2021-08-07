;;; Package --- Summary
;; init-local
;; https://github.com/purcell/emacs.d


;;; Commentary:
;;; Code:
(add-to-list 'load-path (expand-file-name "lisp/my-lisp" user-emacs-directory))

(require 'init-vue)
(require 'init-my-org)
(require 'init-mu4e)
(require 'init-org-roam)

(provide 'init-local)
;;; init-local.el ends here
