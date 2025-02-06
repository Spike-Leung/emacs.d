;;; init-my-dired.el --- Dired customisations -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(when (maybe-require-package 'dired-git-info)
  (with-eval-after-load 'dired
    (add-hook 'dired-mode-hook 'dired-hide-details-mode)
    (define-key dired-mode-map (kbd ")") 'dired-git-info-mode)

    ;; When you first call `find-file' (C-x C-f by default), you do not
    ;; need to clear the existing file path before adding the new one.
    ;; Just start typing the whole path and Emacs will "shadow" the
    ;; current one.  For example, you are at ~/Documents/notes/file.txt
    ;; and you want to go to ~/.emacs.d/init.el: type the latter directly
    ;; and Emacs will take you there.
    (file-name-shadow-mode 1)

    ;; This works with `file-name-shadow-mode' enabled.  When you are in
    ;; a sub-directory and use, say, `find-file' to go to your home '~/'
    ;; or root '/' directory, Vertico will clear the old path to keep
    ;; only your current input.
    (add-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy)

    ;; Teach Dired to use a specific external program with either the
    ;; `dired-do-shell-command' or `dired-do-async-shell-command' command
    ;; (with the default keys, those are bound to `!' `&', respectively).
    ;; The first string is a pattern match against file names.  The
    ;; remaining strings are external programs that Dired will provide as
    ;; suggestions.  Of course, you can always type an arbitrary program
    ;; despite these defaults.
    (setq dired-guess-shell-alist-user
          '(("\\.\\(png\\|jpe?g\\|tiff\\)" "feh" "xdg-open" )
            ("\\.\\(mp[34]\\|m4a\\|ogg\\|flac\\|webm\\|mkv\\)" "mpv" "xdg-open")
            (".*" "xdg-open")))
    ;; 由于 init-corfu.el 中将 `completion-category-overrides' 设置为 nil
    ;; 导致 `find-file' 无法使用缩写(~/g/t/p/1.org -> ~/git/taxodium/post/1.org)
    ;; 所以这里重新覆盖配置中的值，使其生效
    (setq completion-category-overrides '((file (styles basic partial-completion))))))

(provide 'init-my-dired)
;;; init-my-dired.el ends here
