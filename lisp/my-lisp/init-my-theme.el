;;; init-my-theme.el --- my-theme customisations -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(maybe-require-package 'modus-themes)
(maybe-require-package 'ef-themes)
(maybe-require-package 'gruvbox-theme)
(maybe-require-package 'monokai-pro-theme)

;;; spacious-padding
;; (maybe-require-package 'spacious-padding)
;; (setq spacious-padding-subtle-mode-line t)
;; (spacious-padding-mode 1)

(setq modus-themes-headings
      '((1 . (ultrabold 1.1)))

      ;; modus-themes-org-blocks 'gray-background

      modus-themes-common-palette-overrides
      '(
        ;; Make line numbers less intense, but use a shade of red for the current line number
        (fg-line-number-inactive "gray50")
        (fg-line-number-active red-cooler)
        (bg-line-number-inactive unspecified)
        (bg-line-number-active unspecified)

        ;; Diffs with only foreground colours.  Word-wise ("refined") diffs
        (bg-added           unspecified)
        (bg-added-faint     unspecified)
        (bg-added-refine    bg-inactive)
        (fg-added           green)
        (fg-added-intense   green-intense)

        (bg-changed         unspecified)
        (bg-changed-faint   unspecified)
        (bg-changed-refine  bg-inactive)
        (fg-changed         yellow)
        (fg-changed-intense yellow-intense)

        (bg-removed         unspecified)
        (bg-removed-faint   unspecified)
        (bg-removed-refine  bg-inactive)
        (fg-removed         red)
        (fg-removed-intense red-intense)

        ;; Highlight comments to make them more prominent. Hope it will not rust :P
        (comment rust)

        (bg-diff-context    unspecified)))

;; Load the theme of your choice:
(load-theme 'modus-vivendi :no-confirm)
(load-theme 'ef-dream :no-confirm)
(enable-theme 'ef-dream)

;;; theme related
;; @see: https://emacsredux.com/blog/2025/02/03/clean-unloading-of-emacs-themes/
(defun spike-leung/disable-all-active-themes ()
  "Disable all currently active themes."
  (interactive)
  (dolist (theme custom-enabled-themes)
    (disable-theme theme)))

(provide 'init-my-theme)
;;; init-my-theme.el ends here
