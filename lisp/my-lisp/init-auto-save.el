;;; init-auto-save.el --- auto save file -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(push (expand-file-name "lisp/my-lisp/auto-save" user-emacs-directory) load-path)
(require 'auto-save)

(setq auto-save-idle 10)
(setq auto-save-silent t)   ; quietly save
(setq auto-save-delete-trailing-whitespace t)  ; automatically delete spaces at the end of the line when saving

(auto-save-enable)
(provide 'init-auto-save)
;;; init-auto-save.el ends here
