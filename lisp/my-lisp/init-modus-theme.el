;;; init-modus-theme.el --- modus-theme customisations
;;; Commentary:
;;; Code:
(require-package 'modus-themes)

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

        (bg-diff-context    unspecified)))

;; Load the theme of your choice:
(load-theme 'modus-vivendi-tritanopia t t)
(enable-theme 'modus-vivendi-tritanopia)

(provide 'init-modus-theme)
;;; init-modus-theme.el ends here
