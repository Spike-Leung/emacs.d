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


;;; write by ChatGPT-4o
;;; https://chatgpt.com/share/fd2bfa45-fbfd-492a-aa6b-6b3023f76772
(defun spike-leung/process-next-html-src-block ()
  "Process the next HTML block from the current position, escape it, compress it, and generate a #+begin_export html block with an iframe, replacing any existing export block."
  (interactive)
  (let (html-content escaped-html srcdoc start end export-start export-end)
    ;; Search for the next #+begin_src html block from the current position
    (save-excursion
      (when (re-search-forward "^#\\+begin_src html" nil t)
        (setq start (match-end 0))
        (when (re-search-forward "^#\\+end_src" nil t)
          (setq end (match-beginning 0))
          (setq html-content (buffer-substring-no-properties start end))
          ;; Process the HTML content
          (when html-content
            ;; Escape HTML characters using sgml-quote in a temporary buffer
            (setq escaped-html
                  (with-temp-buffer
                    (insert html-content)
                    (sgml-mode) ;; Switch to sgml-mode to enable sgml-quote
                    (sgml-quote (point-min) (point-max))
                    (buffer-string)))

            ;; Compress into a single line and remove extra spaces
            (setq srcdoc (replace-regexp-in-string "[\n\r]+" " " escaped-html))
            (setq srcdoc (replace-regexp-in-string "[ \t]+" " " srcdoc))

            ;; Move to the end of the block and check for existing export block
            (goto-char end)
            (forward-line 1) ;; Move to the line after #+end_src

            ;; Check if there's an existing #+begin_export html block
            (when (looking-at "^#\\+begin_export html")
              (setq export-start (point))
              (when (re-search-forward "^#\\+end_export" nil t)
                (setq export-end (point))
                ;; Delete the existing export block
                (delete-region export-start export-end)))

            ;; Insert the new #+begin_export html block
            (insert (format "#+begin_export html\n<iframe style=\"width:100%%\" srcdoc=\"%s\"></iframe>\n#+end_export\n" srcdoc))))))))



(provide 'init-my-misc)
;;; init-my-misc.el ends here
