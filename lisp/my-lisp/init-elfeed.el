;;; Package --- Summary
;; init-elfeed

;;; Commentary:
;;; Code:
(when (maybe-require-package 'elfeed)
  (when (maybe-require-package 'elfeed-org)
    (with-eval-after-load 'elfeed-org
      (setq rmh-elfeed-org-files (list "~/org/elfeed.org"))
      (elfeed-org))))

(provide 'init-elfeed)
;;; init-elfeed.el ends here
