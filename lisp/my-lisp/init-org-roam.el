;;; Package --- Summary
;; init-org-roam

;;; Commentary:
;;; org-roam
;;; Code:
(setq
 org-roam-v2-ack t
 org-roam-directory (file-truename "~/org-roam")
 org-roam-completion-everywhere t
 )

(when
    (maybe-require-package 'org-roam)
  (with-eval-after-load 'org-roam

    (org-roam-db-autosync-mode)

    (global-set-key (kbd "C-c n  l") 'org-roam-buffer-toggle)
    (global-set-key (kbd "C-c n  f") 'org-roam-node-find)
    (global-set-key (kbd "C-c n  g") 'org-roam-graph)
    (global-set-key (kbd "C-c n  i") 'org-roam-node-insert)
    (global-set-key (kbd "C-c n  c") 'org-roam-capture)
    (global-set-key (kbd "C-c n  j") 'org-roam-dailies-capture-today)
    ))

(require 'org-roam-protocol)

(provide 'init-org-roam)
;;; init-org-roam.el ends here