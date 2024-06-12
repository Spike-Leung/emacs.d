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

(defun spike-leung/enable-javascript-eslint-checker ()
  "Enable javascript-eslint checker."
  (setq-local flycheck--automatically-enabled-checkers '(javascript-eslint))
  (flycheck--toggle-checker 'javascript-eslint t)
  (flymake-mode-off)
  (flymake-mode-on))

(defun spike-leung/add-codeium-completion ()
  "Add codeium completion to completion-at-point-functions."
  (setq-local completion-at-point-functions '(codeium-completion-at-point)))

;;; enabled `add-node-modules-path` in FrontEnd related mode
(when (maybe-require-package 'add-node-modules-path)
  (with-eval-after-load 'flymake-flycheck
    (with-eval-after-load 'add-node-modules-path
      (dolist (mode spike-leung/js-related-modes)
        (let ((mode-hook (intern (concat (symbol-name mode) "-hook"))))
          (add-hook mode-hook (lambda ()
                                (add-node-modules-path)
                                (spike-leung/enable-javascript-eslint-checker))))))))

(provide 'init-my-misc)
;;; init-my-misc.el ends here
