;;; Package --- Summary
;; init-mu4e

;;; Commentary:

;;; Code:
(add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu/mu4e")
;; (add-to-list 'load-path "/usr/share/emacs/site-lisp/mu4e")
;; (maybe-require-package 'mu4e)
(require 'mu4e)
;; for sending mails
(require 'smtpmail)
(require 'epa-file)

;; https://pengpengxp.github.io/archive/before-2018-11-10/2017-08-24-emacs-use-mu4e.html
;; https://github.com/djcb/mu/issues/544
(setenv "XAPIAN_CJK_NGRAM" "1")
(setq
 mue4e-headers-skip-duplicates  t
 mu4e-view-show-images t
 mu4e-view-show-addresses t
 mu4e-compose-format-flowed nil
 mu4e-date-format "%y/%m/%d"
 mu4e-headers-date-format "%Y/%m/%d"
 mu4e-change-filenames-when-moving t
 mu4e-attachment-dir "~/Downloads"
 mu4e-maildir       "~/.Maildir" ;; top-level Maildir
 ;; note that these folders below must start with /
 ;; the paths are relative to maildir root
 mu4e-refile-folder "/Archive"
 mu4e-sent-folder   "/Sent"
 mu4e-drafts-folder "/Drafts"
 mu4e-trash-folder  "/Trash"
 mu4e-update-interval 600
 mu4e-headers-auto-update t
 mu4e-user-mail-address-list '("l-yanlei@hotmail.com")
 )

;; this setting allows to re-sync and re-index mail
;; by pressing U
(setq mu4e-get-mail-command (format "INSIDE_EMACS=%s mbsync -a" emacs-version)
      epa-pinentry-mode 'ask)

(when (maybe-require-package 'pinentry)
  (with-eval-after-load 'pinentry
    (pinentry-start)))

;; ---------------------------------------------------------------------------------------

;; gpg encryptiom & decryption:
;; this can be left alone
(epa-file-enable)
(setq epa-pinentry-mode 'loopback)
(auth-source-forget-all-cached)

;; don't keep message compose buffers around after sending:
(setq message-kill-buffer-on-exit t)

;; send function:
(setq send-mail-function 'sendmail-send-it
      message-send-mail-function 'sendmail-send-it)

;; send program:
;; this is exeranal. remember we installed it before.
(setq sendmail-program (executable-find "msmtp"))

;; select the right sender email from the context.
(setq message-sendmail-envelope-from 'header)



(provide 'init-mu4e)
;;; init-mu4e.el ends here
