;;; init-wsl.el --- config for wsl -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
;;; https://adam.kruszewski.name/2017-09-16-emacs-in-wsl-and-opening-links.html

(let ((cmd-exe "/mnt/c/Windows/System32/cmd.exe")
      (cmd-args '("/c" "start")))
  (when (file-exists-p cmd-exe)
    (setq browse-url-generic-program  cmd-exe
          browse-url-generic-args     cmd-args
          browse-url-browser-function 'browse-url-generic)))

(defun spike-leung/wsl-copy ()
  "Copy region to windows clipboard."
  (interactive)
  (progn
    (write-region (mark) (point) "~/copy.txt" nil nil nil nil)
    (shell-command "cat ~/copy.txt | clip.exe")))

(defun my-kill-ring-save-function (&rest args)
  "A function to run after `whole-line-or-region-kill-ring-save`."
  (let ((cmd (format "echo %s | clip.exe" (shell-quote-argument (string-trim-right (substring-no-properties (car kill-ring)))))))
    (message "%s" cmd)
    (shell-command-to-string cmd)
    (message "Copied to clipboard.")))

(advice-add 'whole-line-or-region-kill-ring-save :after #'my-kill-ring-save-function)
(advice-add 'magit-copy-section-value :after #'my-kill-ring-save-function)

(provide 'init-wsl)
;;; init-wsl.el ends here
