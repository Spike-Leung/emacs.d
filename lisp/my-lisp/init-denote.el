;;; init-my-denote.el --- denote relavive config
;;; Commentary:
;;; Code:

(maybe-require-package 'denote)

(when (maybe-require-package 'denote)
  ;; Remember to check the doc strings of those variables.
  (setq denote-directory (expand-file-name "~/notes/"))
  (setq denote-known-keywords '("emacs" "frontend"))
  (setq denote-infer-keywords t)
  (setq denote-sort-keywords t)
  (setq denote-file-type nil)    ; Org is the default, set others here
  (setq denote-prompts '(template title keywords))
  (setq denote-excluded-directories-regexp nil)
  (setq denote-excluded-keywords-regexp nil)

  ;; Pick dates, where relevant, with Org's advanced interface:
  (setq denote-date-prompt-use-org-read-date t)

  ;; Read manual for how to specify `denote-templates'.  We do not
  ;; include an example here to avoid potential confusion.
  (setq denote-templates
        '((memo . "* Refs")))

  ;; We do not allow multi-word keywords by default.  The author's
  ;; personal preference is for single-word keywords for a more rigid
  ;; workflow.
  (setq denote-allow-multi-word-keywords t)

  (setq denote-date-format nil)         ; read doc string

  ;; By default, we do not show the context of links.  We just display
  ;; file names.  This provides a more informative view.
  (setq denote-backlinks-show-context t)

  (setq xref-search-program
        (cond
         ((or (executable-find "ripgrep")
              (executable-find "rg"))
          'ripgrep)
         ((executable-find "ugrep")
          'ugrep)
         (t
          'grep)))

  (defvar my-denote-silo-directories
    `("~/notes/.private"
      "~/notes/gptel")
    "List of file paths pointing to my Denote silos.
This is a list of strings.")

  (defun spike-leung/denote-open-or-create-silo (silo command)
    "Select SILO and run Denote `denote-open-or-create' in it.
SILO is a file path from `my-denote-silo-directories'"
    (interactive
     (list (completing-read "Select a silo: " my-denote-silo-directories nil t)
           'denote-open-or-create))
    (let ((denote-directory silo))
      (call-interactively command)))

  ;;; Denote key bindings.
  (let ((map global-map))
    (define-key map (kbd "C-c n n") #'denote-open-or-create)
    (define-key map (kbd "C-c n N") #'spike-leung/denote-open-or-create-silo)
    (define-key map (kbd "C-c n c") #'denote-region) ; "contents" mnemonic
    (define-key map (kbd "C-c n s") #'denote-subdirectory)
    (define-key map (kbd "C-c n t") #'denote-template)
    (define-key map (kbd "C-c n k a") #'denote-keywords-add)
    (define-key map (kbd "C-c n k k") #'denote-keywords-remove)
    (define-key map (kbd "C-c n i") #'denote-link-or-create) ; "insert" mnemonic
    (define-key map (kbd "C-c n I") #'denote-add-links)
    (define-key map (kbd "C-c n b") #'denote-backlinks)
    (define-key map (kbd "C-c n f f") #'denote-find-link)
    (define-key map (kbd "C-c n f b") #'denote-find-backlink)
    (define-key map (kbd "C-c n r") #'denote-rename-file)
    (define-key map (kbd "C-c n R") #'denote-rename-file-using-front-matter))

  ;; Key bindings specifically for Dired.
  (with-eval-after-load 'dired
    (let ((map dired-mode-map))
      (define-key map (kbd "C-c C-d C-i") #'denote-link-dired-marked-notes)
      (define-key map (kbd "C-c C-d C-r") #'denote-dired-rename-marked-files)
      (define-key map (kbd "C-c C-d C-R") #'denote-dired-rename-marked-files-using-front-matter)))

  (with-eval-after-load 'denote
    (add-hook 'find-file-hook #'denote-fontify-links-mode-maybe)
    (add-hook 'dired-mode-hook #'denote-dired-mode-in-directories)))

(provide 'init-denote)
;;; init-my-denote.el ends here
