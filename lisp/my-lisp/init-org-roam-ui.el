;;; Package --- Summary
;; init-org-roam-ui

;;; Commentary:
;;; org-roam-ui
;;; Code:
(push (expand-file-name "lisp/my-lisp/org-roam-ui" user-emacs-directory) load-path)

(when (maybe-require-package 'websocket)
  (when (maybe-require-package 'simple-httpd)
    (load-library "org-roam-ui")))

(setq org-roam-ui-open-on-start nil
      org-roam-ui-sync-theme t
      org-roam-ui-follow t
      org-roam-ui-update-on-save t)

;; Not save org-roam-ui-mode in desktop to avoid error
(push '(org-roam-ui-mode nil) desktop-minor-mode-table)

(provide 'init-org-roam-ui)
;;; init-org-roam-ui.el ends here
