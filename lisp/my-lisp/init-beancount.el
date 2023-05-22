;;; init-beancount.el --- beancount support
;;; Commentary:
;;; Code:

(push (expand-file-name "lisp/my-lisp/beancount-mode" user-emacs-directory) load-path)
(require 'beancount)

(push '("\\.\\(beancount\\|bean\\)\\'" . beancount-mode) auto-mode-alist)
(add-hook 'beancount-mode-hook #'outline-minor-mode)
(define-key beancount-mode-map (kbd "C-c C-n") #'outline-next-visible-heading)
(define-key beancount-mode-map (kbd "C-c C-p") #'outline-previous-visible-heading)

;;; https://whatacold.io/blog/2022-09-10-emacs-beancount-accout-files/
(defvar beancount-accounts-files nil "List of account files")
(setq beancount-accounts-files
      (directory-files "~/Dropbox/beancount/accounts/" 'full (rx ".beancount" eos)))
(defun w/beancount--collect-accounts-from-files (oldfun regex n)
  (let ((keys (funcall oldfun regex n))
        (hash (make-hash-table :test 'equal)))
    (dolist (key keys)
      (puthash key nil hash))
    ;; collect accounts from files
    (save-excursion
      (dolist (f beancount-accounts-files)
        (with-current-buffer (find-file-noselect f)
          (goto-char (point-min))
          (while (re-search-forward beancount-account-regexp nil t)
            (puthash (match-string-no-properties n) nil hash)))))
    (hash-table-keys hash)))

(advice-add #'beancount-collect
            :around #'w/beancount--collect-accounts-from-files
            '((name . "collect accounts from files as well")))

(provide 'init-beancount)
;;; init-beancount.el ends here
