;;; init-my-denote.el --- denote relavive config
;;; Commentary:
;;; Code:
(maybe-require-package 'denote)

;; Remember to check the doc strings of those variables.
(setq denote-directory (expand-file-name "~/notes/"))
(setq denote-known-keywords '("emacs" "frontend"))
(setq denote-infer-keywords t)
(setq denote-sort-keywords t)
(setq denote-file-type nil) ; Org is the default, set others here
(setq denote-prompts '(title keywords))
(setq denote-excluded-directories-regexp nil)
(setq denote-excluded-keywords-regexp nil)

;; Pick dates, where relevant, with Org's advanced interface:
(setq denote-date-prompt-use-org-read-date t)

;; Read manual for how to specify `denote-templates'.  We do not
;; include an example here to avoid potential confusion.

;; We do not allow multi-word keywords by default.  The author's
;; personal preference is for single-word keywords for a more rigid
;; workflow.
(setq denote-allow-multi-word-keywords t)

(setq denote-date-format nil) ; read doc string

;; By default, we do not show the context of links.  We just display
;; file names.  This provides a more informative view.
(setq denote-backlinks-show-context t)

;; Also see `denote-link-backlinks-display-buffer-action' which is a bit
;; advanced.

;; If you use Markdown or plain text files (Org renders links as buttons
;; right away)
(add-hook 'find-file-hook #'denote-link-buttonize-buffer)

;; We use different ways to specify a path for demo purposes.
;; (setq denote-dired-directories
;;       (list denote-directory
;;             (thread-last denote-directory (expand-file-name "attachments"))
;;             (expand-file-name "~/Documents/books")))

;; Generic (great if you rename files Denote-style in lots of places):
;; (add-hook 'dired-mode-hook #'denote-dired-mode)
;;
;; OR if only want it in `denote-dired-directories':
(add-hook 'dired-mode-hook #'denote-dired-mode-in-directories)

(setq xref-search-program
      (cond
       ((or (executable-find "ripgrep")
            (executable-find "rg"))
        'ripgrep)
       ((executable-find "ugrep")
        'ugrep)
       (t
        'grep)))

;; (defvar my-denote-silo-directories
;;   `("~/notes/.hidden"
;;     ;; You don't actually need to include the `denote-directory' here
;;     ;; if you use the regular commands in their global context.  I am
;;     ;; including it for completeness.
;;     ,denote-directory)
;;   "List of file paths pointing to my Denote silos.
;; This is a list of strings.")

(defvar my-denote-silo-directories
  `("~/notes/.hidden")
  "List of file paths pointing to my Denote silos.
This is a list of strings.")

(defvar my-denote-commands-for-silos
  '(denote-open-or-create)
  "List of Denote commands to call after selecting a silo.
This is a list of symbols that specify the note-creating
interactive functions that Denote provides.")

(defun my-denote-pick-silo-then-command (silo command)
  "Select SILO and run Denote COMMAND in it.
SILO is a file path from `my-denote-silo-directories', while
COMMAND is one among `my-denote-commands-for-silos'."
  (interactive
   (list (completing-read "Select a silo: " my-denote-silo-directories nil t)
         (intern (completing-read
                  "Run command in silo: "
                  my-denote-commands-for-silos nil t))))
  (let ((denote-user-enforced-denote-directory silo))
    (call-interactively command)))

;; Denote DOES NOT define any key bindings.  This is for the user to
;; decide.  For example:
(let ((map global-map))
  (define-key map (kbd "C-c n n") #'denote-open-or-create)
  (define-key map (kbd "C-c n N") #'my-denote-pick-silo-then-command)
  (define-key map (kbd "C-c n c") #'denote-region) ; "contents" mnemonic
  (define-key map (kbd "C-c n s") #'denote-subdirectory)
  (define-key map (kbd "C-c n t") #'denote-template)
  (define-key map (kbd "C-c n k a") #'denote-keywords-add)
  (define-key map (kbd "C-c n k k") #'denote-keywords-remove)

  ;; If you intend to use Denote with a variety of file types, it is
  ;; easier to bind the link-related commands to the `global-map', as
  ;; shown here.  Otherwise follow the same pattern for `org-mode-map',
  ;; `markdown-mode-map', and/or `text-mode-map'.
  (define-key map (kbd "C-c n i") #'denote-link-or-create) ; "insert" mnemonic
  (define-key map (kbd "C-c n I") #'denote-add-links)
  (define-key map (kbd "C-c n b") #'denote-backlinks)
  (define-key map (kbd "C-c n f f") #'denote-find-link)
  (define-key map (kbd "C-c n f b") #'denote-find-backlink)
  ;; Note that `denote-rename-file' can work from any context, not just
  ;; Dired bufffers.  That is why we bind it here to the `global-map'.
  (define-key map (kbd "C-c n r") #'denote-rename-file)
  (define-key map (kbd "C-c n R") #'denote-rename-file-using-front-matter))

;; Key bindings specifically for Dired.
(let ((map dired-mode-map))
  (define-key map (kbd "C-c C-d C-i") #'denote-link-dired-marked-notes)
  (define-key map (kbd "C-c C-d C-r") #'denote-dired-rename-marked-files)
  (define-key map (kbd "C-c C-d C-R") #'denote-dired-rename-marked-files-using-front-matter))

;; (with-eval-after-load 'org-capture
;;   (setq denote-org-capture-specifiers "%l\n%i\n%?")
;;   (add-to-list 'org-capture-templates
;;                '("n" "New note (with denote.el)" plain
;;                  (file denote-last-path)
;;                  #'denote-org-capture
;;                  :no-save t
;;                  :immediate-finish nil
;;                  :kill-buffer t
;;                  :jump-to-captured t)))

;; Also check the commands `denote-link-after-creating',
;; `denote-link-or-create'.  You may want to bind them to keys as well.

;; If you want to have Denote commands available via a right click
;; context menu, use the following and then enable
;; `context-menu-mode'.
;; (add-hook 'context-menu-functions #'denote-context-menu)

(provide 'init-denote)
;;; init-my-denote.el ends here
