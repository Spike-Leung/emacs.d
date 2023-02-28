;;; Package --- Summary
;; init-translate
;;; Commentary:
;;; See: https://github.com/manateelazycat/insert-translated-name
;;; Code:
(push (expand-file-name "lisp/my-lisp/insert-translated-name" user-emacs-directory) load-path)
(require 'insert-translated-name)
(setq insert-translated-name-crow-engine "lingva")
(defun spike-leung/translate-ruiyun-key ()
  "Translate key of ruiyun."
  (interactive)
  (progn
    (yas-exit-all-snippets)
    (save-excursion
      (back-to-indentation)
      (push-mark))
    (insert-translated-name-replace-with-underline)
    (end-of-line)
    (newline)))
(global-set-key (kbd "C-c C-l") 'spike-leung/translate-ruiyun-key)
(provide 'init-translate)
;;; init-translate.el ends here
