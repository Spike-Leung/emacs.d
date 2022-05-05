;;; Package --- Summary
;; init-sort-tab
;;; Commentary:
;;; Code:
(push (expand-file-name "lisp/my-lisp/sort-tab" user-emacs-directory) load-path)
(require 'sort-tab)
(sort-tab-mode 1)
(provide 'init-sort-tab)
;;; init-sort-tab.el ends here
