;;; init-citre.el --- citre
;;; Commentary:
;;; Code:
;;; See: https://github.com/universal-ctags/citre
(maybe-require-package 'citre)

(with-eval-after-load 'citre
  ;; Bind your frequently used commands.  Alternatively, you can define them
  ;; in `citre-mode-map' so you can only use them when `citre-mode' is enabled.
  (require 'citre-config)
  (global-set-key (kbd "C-x c j") 'citre-jump)
  (global-set-key (kbd "C-x c J") 'citre-jump-back)
  (global-set-key (kbd "C-x c p") 'citre-peek)
  (global-set-key (kbd "C-x c a") 'citre-ace-peek)
  (global-set-key (kbd "C-x c u") 'citre-update-this-tags-file)
  (global-set-key (kbd "C-x c e") 'citre-edit-tags-file-recipe)
  (setq
   ;; Set these if readtags/ctags is not in your PATH.
   ;; citre-readtags-program "/path/to/readtags"
   ;; citre-ctags-program "/path/to/ctags"
   ;; Set these if gtags/global is not in your PATH (and you want to use the
   ;; global backend)
   ;; citre-gtags-program "/path/to/gtags"
   ;; citre-global-program "/path/to/global"
   ;; Set this if you use project management plugin like projectile.  It's
   ;; used for things like displaying paths relatively, see its docstring.
   citre-project-root-function #'projectile-project-root
   ;; Set this if you want to always use one location to create a tags file.
   citre-default-create-tags-file-location 'global-cache
   ;; See the "Create tags file" section above to know these options
   citre-use-project-root-when-creating-tags t
   citre-prompt-language-for-ctags-command t
   ;; By default, when you open any file, and a tags file can be found for it,
   ;; `citre-mode' is automatically enabled.  If you only want this to work for
   ;; certain modes (like `prog-mode'), set it like this.
   ;; citre-auto-enable-citre-mode-modes '(prog-mode)
   citre-ctags-cmd-buf-default-cmd "ctags
-o
%TAGSFILE%
;; programming languages to be scanned, or \"all\" for all supported languages
--languages=all
--kinds-all=*
--fields=*
--extras=*
--exclude=node_modules
-R
;; add dirs/files to scan here, one line per dir/file
"
   ))

(provide 'init-citre)
;;; init-citre.el ends here
