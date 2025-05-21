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

;; --- Random theme cycling ---
(defvar spike-leung/candidate-themes
  '(modus-operandi
    modus-operandi-tinted
    modus-vivendi
    modus-vivendi-tinted
    ef-arbutus
    ef-autumn
    ef-bio
    ef-cherie
    ef-cyprus
    ef-dark
    ef-deuteranopia-dark
    ef-deuteranopia-light
    ef-dream
    ef-duo-dark
    ef-duo-light
    ef-eagle
    ef-elea-dark
    ef-elea-light
    ef-frost
    ef-kassio
    ef-light
    ef-maris-dark
    ef-maris-light
    ef-melissa-dark
    ef-melissa-light
    ef-night
    ef-owl
    ef-reverie
    ef-rosa
    ef-spring
    ef-summer
    ef-symbiosis
    ef-trio-dark
    ef-trio-light
    ef-tritanopia-dark
    ef-tritanopia-light
    ef-winter)
  "A list of themes to randomly cycle through.")

;;; theme related
;; @see: https://emacsredux.com/blog/2025/02/03/clean-unloading-of-emacs-themes/
(defun spike-leung/disable-all-active-themes ()
  "Disable all currently active themes."
  (interactive)
  (dolist (theme custom-enabled-themes)
    (disable-theme theme)))

(defun spike-leung/apply-random-theme ()
  "Disable current themes and apply a random theme from `spike-leung/candidate-themes`."
  (interactive)
  (when (boundp 'spike-leung/candidate-themes)
    (spike-leung/disable-all-active-themes)
    (let ((theme (nth (random (length spike-leung/candidate-themes)) spike-leung/candidate-themes)))
      (condition-case err
          (progn
            ;; Load theme to ensure its specific settings/customizations are applied
            (load-theme theme :no-confirm)
            ;; Enable the theme (this actually applies it and adds to custom-enabled-themes)
            (enable-theme theme)
            (message "Applied random theme: %s" theme))
        (error (message "Error applying theme %s: %s" theme err))))))

(run-with-timer (* 15 60) (* 15 60) 'spike-leung/apply-random-theme)

(provide 'init-my-theme)
;;; init-my-theme.el ends here
