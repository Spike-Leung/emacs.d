;;; init-elfeed.el --- elfeed config -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(when (maybe-require-package 'elfeed)
  (when (maybe-require-package 'elfeed-org)
    (setq rmh-elfeed-org-files (list "~/.emacs.d/elfeed.org"))
    (elfeed-org)))

(provide 'init-elfeed)
;;; init-elfeed.el ends here
