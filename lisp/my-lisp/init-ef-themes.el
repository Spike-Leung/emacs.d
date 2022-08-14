;;; Package --- Summary
;; init-ef-themes
;;; Commentary:
;;; Code:
(push (expand-file-name "lisp/my-lisp/ef-themes" user-emacs-directory) load-path)
(require 'ef-themes)

(load-theme 'ef-summer)

(provide 'init-ef-themes)
;;; init-ef-themes.el ends here
