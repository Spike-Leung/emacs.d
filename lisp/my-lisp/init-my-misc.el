;;; init-my-misc.el --- miscellaneous config
;;; Commentary:
;;; Code:
(with-eval-after-load 'magit
  (setq magit-repository-directories
        '(("~/" . 1)
          ("~/git" . 1)
          )))

(defun spike-leung/open-jira-issue (issue-id)
  "Open the JIRA issue with ISSUE-ID in a web browser."
  (interactive "sEnter JIRA issue ID (e.g., DD-1234): ")
  (let* ((jira-base-url "https://jira.gyenno.com")
         (full-url (concat jira-base-url "/browse/" issue-id)))
    (browse-url full-url)))

(defvar spike-leung/js-related-modes '(vue-mode typescript-mode web-mode js-mode js2-mode js-ts-mode)
  "List of modes to add node_modules path.")

;;; enabled `add-node-modules-path` in FrontEnd related mode
(when (maybe-require-package 'add-node-modules-path)
  (with-eval-after-load 'flymake-flycheck
    (with-eval-after-load 'add-node-modules-path
      (dolist (mode spike-leung/js-related-modes)
        (let ((mode-hook (intern (concat (symbol-name mode) "-hook"))))
          (add-hook mode-hook 'add-node-modules-path)
          (add-hook mode-hook (lambda ()
                                (setq-local completion-at-point-functions '(codeium-completion-at-point))))
          )))))

(provide 'init-my-misc)
;;; init-my-misc.el ends here
