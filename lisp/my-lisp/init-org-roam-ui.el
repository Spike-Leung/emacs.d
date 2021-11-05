;;; Package --- Summary
;; init-org-roam-ui

;;; Commentary:
;;; org-roam-ui
;;; Code:
(push (expand-file-name "lisp/my-lisp/org-roam-ui" user-emacs-directory) load-path)

(when (maybe-require-package 'websocket)
  (when (maybe-require-package 'simple-httpd)
    (require 'org-roam-ui)
    (setq org-roam-ui-sync-theme t
          org-roam-ui-follow t
          org-roam-ui-update-on-save t
          org-roam-ui-open-on-start t)))

(provide 'init-org-roam-ui)
;;; init-org-roam-ui.el ends here
