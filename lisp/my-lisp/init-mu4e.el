;;; Package --- Summary
;; init-mu43

;;; Commentary:

;;; Code:
(add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu/mu4e")
(require 'mu4e)
(require 'pinentry)

(setq
 mue4e-headers-skip-duplicates  t
 mu4e-view-show-images t
 mu4e-view-show-addresses t
 mu4e-compose-format-flowed nil
 mu4e-date-format "%y/%m/%d"
 mu4e-headers-date-format "%Y/%m/%d"
 mu4e-change-filenames-when-moving t
 mu4e-attachment-dir "~/Downloads"

 mu4e-maildir       "~/Maildir" ;; top-level Maildir
 ;; note that these folders below must start with /
 ;; the paths are relative to maildir root
 mu4e-refile-folder "/Archive"
 mu4e-sent-folder   "/Sent"
 mu4e-drafts-folder "/Drafts"
 mu4e-trash-folder  "/Trash"
 mu4e-update-interval 600
 mu4e-headers-auto-update t
 )

;; this setting allows to re-sync and re-index mail
;; by pressing U
(setq mu4e-get-mail-command (format "INSIDE_EMACS=%s mbsync -a" emacs-version)
      epa-pinentry-mode 'ask)
(pinentry-start)

(provide 'init-mu4e)                  ;
;;; init-mu4e.el ends here
