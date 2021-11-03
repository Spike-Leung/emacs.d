;;; Package --- Summary
;; init-org-download
;;; Commentary:
;;; https://github.com/abo-abo/org-download
;;; Code:
(when (maybe-require-package 'org-download)
  (with-eval-after-load 'org-download
    (setq-default org-download-image-dir "~/org/picture")
    (global-set-key (kbd "C-c i c") 'org-download-clipboard)
    (global-set-key (kbd "C-c i d") 'org-download-delete)
    (global-set-key (kbd "C-c i e") 'org-download-edit)
    (global-set-key (kbd "C-c i r") 'org-download-rename-at-point)
    (global-set-key (kbd "C-c i s") 'org-download-screenshot)))

(provide 'init-org-download)
;;; init-org-download.el ends here
