;;; Package --- Summary
;; init-sort-tab
;;; Commentary:
;;; Code:
(push (expand-file-name "lisp/my-lisp/sort-tab" user-emacs-directory) load-path)
(require 'sort-tab)
(sort-tab-mode 1)
(global-set-key (kbd "C-x t 1") 'sort-tab-select-visible-tab)
(global-set-key (kbd "C-x t 2") 'sort-tab-select-visible-tab)
(global-set-key (kbd "C-x t 3") 'sort-tab-select-visible-tab)
(global-set-key (kbd "C-x t 4") 'sort-tab-select-visible-tab)
(global-set-key (kbd "C-x t 5") 'sort-tab-select-visible-tab)
(global-set-key (kbd "C-x t 6") 'sort-tab-select-visible-tab)
(global-set-key (kbd "C-x t 7") 'sort-tab-select-visible-tab)
(global-set-key (kbd "C-x t 8") 'sort-tab-select-visible-tab)
(global-set-key (kbd "C-x t 9") 'sort-tab-select-visible-tab)
(global-set-key (kbd "C-x t 0") 'sort-tab-select-visible-tab)
(global-set-key (kbd "C-x t O") 'sort-tab-select-prev-tab)
(global-set-key (kbd "C-x t o") 'sort-tab-select-next-tab)
(global-set-key (kbd "C-x t Q") 'sort-tab-close-all-tabs)
(global-set-key (kbd "C-x t m") 'sort-tab-close-mode-tabs)
(global-set-key (kbd "C-x t ;") 'sort-tab-close-current-tab)
(provide 'init-sort-tab)
;;; init-sort-tab.el ends here
