;;; init-magit.el --- config magit
;;; Commentary: see: https://tsdh.org/posts/2022-08-01-difftastic-diffing-with-magit.html
;;; Code:

(defun th/magit--with-difftastic (buffer command)
  "Run COMMAND with GIT_EXTERNAL_DIFF=difft then show result in BUFFER."
  (let ((process-environment
         (cons (concat "GIT_EXTERNAL_DIFF=difft --width="
                       (number-to-string (frame-width)))
               process-environment)))
    ;; Clear the result buffer (we might regenerate a diff, e.g., for
    ;; the current changes in our working directory).
    (with-current-buffer buffer
      (setq buffer-read-only nil)
      (erase-buffer))
    ;; Now spawn a process calling the git COMMAND.
    (make-process
     :name (buffer-name buffer)
     :buffer buffer
     :command command
     ;; Don't query for running processes when emacs is quit.
     :noquery t
     ;; Show the result buffer once the process has finished.
     :sentinel (lambda (proc event)
                 (when (eq (process-status proc) 'exit)
                   (with-current-buffer (process-buffer proc)
                     (goto-char (point-min))
                     (ansi-color-apply-on-region (point-min) (point-max))
                     (setq buffer-read-only t)
                     (view-mode)
                     (end-of-line)
                     ;; difftastic diffs are usually 2-column side-by-side,
                     ;; so ensure our window is wide enough.
                     (let ((width (current-column)))
                       (while (zerop (forward-line 1))
                         (end-of-line)
                         (setq width (max (current-column) width)))
                       ;; Add column size of fringes
                       (setq width (+ width
                                      (fringe-columns 'left)
                                      (fringe-columns 'right)))
                       (goto-char (point-min))
                       (pop-to-buffer
                        (current-buffer)
                        `(;; If the buffer is that wide that splitting the frame in
                          ;; two side-by-side windows would result in less than
                          ;; 80 columns left, ensure it's shown at the bottom.
                          ,(when (> 80 (- (frame-width) width))
                             #'display-buffer-at-bottom)
                          (window-width
                           . ,(min width (frame-width))))))))))))


(defun th/magit-show-with-difftastic (rev)
  "Show the result of \"git show REV\" with GIT_EXTERNAL_DIFF=difft."
  (interactive
   (list (or
          ;; If REV is given, just use it.
          (when (boundp 'rev) rev)
          ;; If not invoked with prefix arg, try to guess the REV from
          ;; point's position.
          (and (not current-prefix-arg)
               (or (magit-thing-at-point 'git-revision t)
                   (magit-branch-or-commit-at-point)))
          ;; Otherwise, query the user.
          (magit-read-branch-or-commit "Revision"))))
  (if (not rev)
      (error "No revision specified")
    (th/magit--with-difftastic
     (get-buffer-create (concat "*git show difftastic " rev "*"))
     (list "git" "--no-pager" "show" "--ext-diff" rev))))

(defun th/magit-diff-with-difftastic (arg)
  "Show the result of \"git diff ARG\" with GIT_EXTERNAL_DIFF=difft."
  (interactive
   (list (or
          ;; If RANGE is given, just use it.
          (when (boundp 'range) range)
          ;; If prefix arg is given, query the user.
          (and current-prefix-arg
               (magit-diff-read-range-or-commit "Range"))
          ;; Otherwise, auto-guess based on position of point, e.g., based on
          ;; if we are in the Staged or Unstaged section.
          (pcase (magit-diff--dwim)
            ('unmerged (error "unmerged is not yet implemented"))
            ('unstaged nil)
            ('staged "--cached")
            (`(stash . ,value) (error "stash is not yet implemented"))
            (`(commit . ,value) (format "%s^..%s" value value))
            ((and range (pred stringp)) range)
            (_ (magit-diff-read-range-or-commit "Range/Commit"))))))
  (let ((name (concat "*git diff difftastic"
                      (if arg (concat " " arg) "")
                      "*")))
    (th/magit--with-difftastic
     (get-buffer-create name)
     `("git" "--no-pager" "diff" "--ext-diff" ,@(when arg (list arg))))))

(with-eval-after-load 'magit
  (transient-append-suffix 'magit-dispatch "!"
    '("#" "My Magit Cmds" th/magit-aux-commands)
    )

  (transient-define-prefix th/magit-aux-commands ()
    "My personal auxiliary magit commands."
    ["Auxiliary commands"
     ("d" "Difftastic Diff (dwim)" th/magit-diff-with-difftastic)
     ("s" "Difftastic Show" th/magit-show-with-difftastic)])

  (define-key magit-status-mode-map (kbd "#") #'th/magit-aux-commands))

(with-eval-after-load 'magit-todos
  (setq magit-todos-keywords
        '("TODO" "FIXME" "HACK" "OPTIMIZE" "BUG" "CHECKME" "REVIEW" "DOCME" "TESTME"))

  (setq hl-todo-keyword-faces
        '(("TODO" . "#ff5f59")
          ("FIXME" . "#fec43f")
          ("HACK" . "#d0bc00")
          ("OPTIMIZE" . "#44bc44")
          ("BUG" . "#2fafff")
          ("CHECKME" . "#9099d9")
          ("REVIEW" . "#f78fe7")
          ("DOCME" . "#feacd0")
          ("TESTME" . "#00d3d0"))))

(defconst spike-leung/magit-todo-keywords
  '(("TODO" . "Task that needs to be done")
    ("FIXME" . "Something is broken")
    ("HACK" . "The code is suboptimal and should be refactored")
    ("OPTIMIZE" . "The code is suboptimal and should be refactored")
    ("BUG" . "There is a bug in the code")
    ("CHECKME" . "The code needs to be reviewed")
    ("REVIEW" . "The code needs to be reviewed")
    ("DOCME" . "The code needs to be documented (either in codebase or external documentation)")
    ("TESTME" . "Tests need to be written for that code selection")))

(defun spike-leung/magit-todo-keyword-insert ()
  "Use `consult` to select a keyword from `magit-todos-keywords` with descriptions and insert it into the buffer."
  (interactive)
  (let* ((keyword (consult--read
                   (mapcar (lambda (pair)
                             (concat (car pair) " - " (cdr pair)))
                           spike-leung/magit-todo-keywords)
                   :prompt "Select keyword: "
                   :category 'consult-candidate)))
    (insert (concat "// " (car (split-string keyword " - ")) ": "))))

(provide 'init-magit)
;;; init-magit.el ends here
