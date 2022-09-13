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
(setq completion-ignore-case t)

(setq org-roam-dailies-directory "daily/")
(setq org-roam-capture-templates
      '(("d" "Default" plain (file "~/org-roam/template/default.txt")
         :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title} ")
         :empty-lines-before 2
         :unnarrowed t)))

(setq org-roam-dailies-capture-templates
      '(("d" "Default" entry
         "* %?"
         :target (file+head "%<%Y-%m-%d>.org"
                            "#+title: %<%Y-%m-%d>\n"))
        ("m" "Meditation" entry
         "* 冥想"
         :target (file+head "%<%Y-%m-%d>.org"
                            "#+title: %<%Y-%m-%d>\n"))))

(when
    (maybe-require-package 'org-roam)
  (with-eval-after-load 'org-roam
    (org-roam-db-autosync-mode)))

(global-set-key (kbd "C-c n  l") 'org-roam-buffer-toggle)
(global-set-key (kbd "C-c n  f") 'org-roam-node-find)
(global-set-key (kbd "C-c n  g") 'org-roam-graph)
(global-set-key (kbd "C-c n  i") 'org-roam-node-insert)
(global-set-key (kbd "C-c n  c") 'org-roam-capture)
(global-set-key (kbd "C-c n  j") 'org-roam-dailies-capture-today)
(global-set-key (kbd "C-c n  t") (lambda () (interactive) (org-roam-dailies-goto-today "d")))
(global-set-key (kbd "C-c n  d") (lambda () (interactive) (org-roam-dailies-goto-date nil "d")))
(add-hook 'org-roam-find-file-hook 'turn-on-auto-fill)

;; (maybe-require-package 'org-roam-protocol)

(provide 'init-org-roam)
;;; init-org-roam.el ends here
