;;; init-denote.el --- denote relavive config
;;; Commentary:
;;; Code:

(maybe-require-package 'denote)
(maybe-require-package 'denote-silo)

(when (maybe-require-package 'denote)
  ;; Remember to check the doc strings of those variables.
  (setq denote-directory (expand-file-name "~/notes/"))
  (setq denote-silo-directories
        (list denote-directory
              "~/git/taxodium/posts"))
  (setq denote-known-keywords '("emacs" "frontend" "writing" "blog" "draft"))
  (setq denote-infer-keywords t)
  (setq denote-sort-keywords t)
  (setq denote-file-type nil)    ; Org is the default, set others here
  (setq denote-prompts '(template title keywords))
  (setq denote-excluded-directories-regexp nil)
  (setq denote-excluded-keywords-regexp nil)
  (setq denote-excluded-files-regexp "hidden")

  ;; Pick dates, where relevant, with Org's advanced interface:
  (setq denote-date-prompt-use-org-read-date t)

  ;; We do not allow multi-word keywords by default.  The author's
  ;; personal preference is for single-word keywords for a more rigid
  ;; workflow.
  (setq denote-allow-multi-word-keywords t)
  (setq denote-date-format nil)         ; read doc string

  ;; By default, we do not show the context of links.  We just display
  ;; file names.  This provides a more informative view.
  (setq denote-backlinks-show-context t)

  ;; When this user option is set to a non-nil value, Denote will use minibuffer history entries as completion candidates in all of the `denote-prompts-with-history-as-completion'.
  (setq denote-history-completion-in-prompts t)

  ;; Read manual for how to specify `denote-templates'.  We do not
  ;; include an example here to avoid potential confusion.
  (setq denote-templates
        '((memo . "* Refs")))

  (setq xref-search-program
        (cond
         ((or (executable-find "ripgrep")
              (executable-find "rg"))
          'ripgrep)
         ((executable-find "ugrep")
          'ugrep)
         (t
          'grep)))

  ;;; Denote key bindings.
  (let ((map global-map))
    (define-key map (kbd "C-c n n") #'denote-open-or-create)
    (define-key map (kbd "C-c n N") #'denote-silo-open-or-create)
    (define-key map (kbd "C-c n i") #'denote-link-or-create) ; "insert" mnemonic
    (define-key map (kbd "C-c n I") #'denote-add-links)
    (define-key map (kbd "C-c n b") #'denote-backlinks)
    (define-key map (kbd "C-c n g") #'denote-grep)
    (define-key map (kbd "C-c n c") #'denote-region) ; "contents" mnemonic
    (define-key map (kbd "C-c n s") #'denote-subdirectory)
    (define-key map (kbd "C-c n t") #'denote-template)
    (define-key map (kbd "C-c n k a") #'denote-keywords-add)
    (define-key map (kbd "C-c n k k") #'denote-keywords-remove)
    (define-key map (kbd "C-c n f f") #'denote-find-link)
    (define-key map (kbd "C-c n f b") #'denote-find-backlink)
    (define-key map (kbd "C-c n q c") #'denote-query-contents-link)
    (define-key map (kbd "C-c n q f") #'denote-query-filenames-link)
    (define-key map (kbd "C-c n r") #'denote-rename-file)
    (define-key map (kbd "C-c n R") #'denote-rename-file-using-front-matter)
    (define-key map (kbd "C-c n d") #'denote-rename-file-date))

  ;; Key bindings specifically for Dired.
  (with-eval-after-load 'dired
    (let ((map dired-mode-map))
      (define-key map (kbd "C-c C-d C-i") #'denote-link-dired-marked-notes)
      (define-key map (kbd "C-c C-d C-r") #'denote-dired-rename-marked-files)
      (define-key map (kbd "C-c C-d C-R") #'denote-dired-rename-marked-files-using-front-matter)))

  (with-eval-after-load 'denote
    (add-hook 'find-file-hook #'denote-fontify-links-mode-maybe)
    (add-hook 'dired-mode-hook #'denote-dired-mode-in-directories)))


;;; https://protesilaos.com/emacs/denote#h:fed09992-7c43-4237-b48f-f654bc29d1d8
(setq org-export-allow-bind-keywords t)

;;; make denote-link-ol-export support #+export_file_name
;; see also: https://jiewawa.me/2024/03/blogging-with-denote-and-hugo/
(defun spike-leung/my-denote--get-export-file-name (file)
  "Find #+export_file_name in FILE and return its value.
Return nil if not found or FILE does not exist."
  (when (and file (file-exists-p file))
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (when (re-search-forward "^#\\+export_file_name: \\(.*\\)$" nil t)
        (string-trim (match-string-no-properties 1))))))

(defun spike-leung/denote-link-ol-export (link description format)
  "Export a `denote:' link from Org files.
The LINK, DESCRIPTION, and FORMAT are handled by the export
backend."
  (pcase-let* ((`(,path ,query ,file-search) (denote-link--ol-resolve-link-to-target link :full-data))
               (export-file-name (when path (spike-leung/my-denote--get-export-file-name path)))
               (anchor (if export-file-name
                           export-file-name
                           (when path (file-relative-name (file-name-sans-extension path)))))
               (desc (cond
                       (description)
                       (file-search (format "denote:%s::%s" query file-search))
                       (t (concat "denote:" query)))))
    (if path
        (pcase format
          ('html (if file-search
                     (format "<a href=\"%s.html%s\">%s</a>" anchor file-search desc)
                     (format "<a href=\"%s.html\">%s</a>" anchor desc)))
          ('latex (format "\\href{%s}{%s}" (replace-regexp-in-string "[\\{}$%&_#~^]" "\\\\\\&" path) desc))
          ('texinfo (format "@uref{%s,%s}" path desc))
          ('ascii (format "[%s] <denote:%s>" desc path))
          ('md (format "[%s](%s)" desc path))
          (_ path))
        (format-message "[[Denote query for `%s']]" query))))

(org-link-set-parameters "denote" :export #'spike-leung/denote-link-ol-export)

(provide 'init-denote)
;;; init-denote.el ends here
