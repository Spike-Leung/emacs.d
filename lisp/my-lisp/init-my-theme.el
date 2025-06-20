;;; init-my-theme.el --- my-theme customisations -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(maybe-require-package 'modus-themes)
(maybe-require-package 'ef-themes)
(maybe-require-package 'doric-themes)
(maybe-require-package 'gruvbox-theme)
(maybe-require-package 'monokai-pro-theme)
(maybe-require-package 'timu-caribbean-theme)

(require 'modus-themes)
(require 'ef-themes)
(require 'doric-themes)
(require 'gruvbox-theme)
(require 'monokai-pro-theme)
(require 'timu-caribbean-theme)

;; nyan-mode config
;; (maybe-require-package 'nyan-mode)
;; (setq nyan-cat-face-number 4)
;; (nyan-mode 1)
;; (if *is-a-mac*
;;     (progn
;;       (setq
;;        nyan-animate-nyancat t
;;        nyan-wavy-trail t)
;;       (nyan-start-animation)))

;; spacious-padding
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
    ;; ef-melissa-dark
    ;; ef-melissa-light
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
    ef-winter
    doric-cherry
    doric-dark
    doric-earth
    doric-fire
    doric-light
    doric-marble
    doric-obsidian
    doric-plum
    doric-water
    doric-wind
    sanityinc-solarized-dark
    sanityinc-solarized-light
    sanityinc-tomorrow-day
    sanityinc-tomorrow-night
    sanityinc-tomorrow-bright
    sanityinc-tomorrow-eighties
    gruvbox
    monokai
    timu-caribbean)
  "A list of themes to randomly cycle through.")

(defvar spike-leung/theme-cycle-timer nil
  "Timer for cycling themes.")

(defvar spike-leung/shuffled-themes-queue nil
  "A queue of shuffled themes to apply.")
(defvar spike-leung/last-applied-theme nil
  "The last theme that was applied.")

(defun spike-leung/shuffle-list (list)
  "Return a shuffled copy of LIST using the Fisher-Yates algorithm."
  (let ((list (copy-sequence list))
        (len (length list)))
    (dotimes (i len list)
      (let* ((j (+ i (random (- len i))))
             (tmp (elt list i)))
        (setf (elt list i) (elt list j))
        (setf (elt list j) tmp)))))

;;; theme related
;; @see: https://emacsredux.com/blog/2025/02/03/clean-unloading-of-emacs-themes/
(defun spike-leung/disable-all-active-themes ()
  "Disable all currently active themes."
  (interactive)
  (dolist (theme custom-enabled-themes)
    (disable-theme theme)))

(defun spike-leung/apply-random-theme ()
  "Apply a theme from a shuffled list, ensuring all are used before repeating."
  (interactive)
  (when (boundp 'spike-leung/candidate-themes)
    (spike-leung/disable-all-active-themes)
    ;; Refill the queue if it's empty
    (when (not spike-leung/shuffled-themes-queue)
      (setq spike-leung/shuffled-themes-queue (spike-leung/shuffle-list spike-leung/candidate-themes))
      ;; Avoid immediate repetition from the previous cycle
      (when (and spike-leung/last-applied-theme
                 (eq (car spike-leung/shuffled-themes-queue) spike-leung/last-applied-theme))
        (setq spike-leung/shuffled-themes-queue
              (append (cdr spike-leung/shuffled-themes-queue)
                      (list (car spike-leung/shuffled-themes-queue))))))

    (let ((theme (pop spike-leung/shuffled-themes-queue)))
      (setq spike-leung/last-applied-theme theme)
      (condition-case err
          (progn
            (load-theme theme :no-confirm)
            (enable-theme theme)
            (message "｡:.ﾟヽ(*´∀`)ﾉﾟ.:｡ Applied random theme: %s" theme))
        (error (message "(╯°□°）╯︵ ┻━┻ Error applying theme %s: %s" theme err))))))

(defun spike-leung/toggle-random-theme-cycling ()
  "Toggle the random theme cycling timer."
  (interactive)
  (if (timerp spike-leung/theme-cycle-timer)
      (progn
        (cancel-timer spike-leung/theme-cycle-timer)
        (setq spike-leung/theme-cycle-timer nil)
        (message "Random theme cycling stopped."))
    (progn
      (setq spike-leung/theme-cycle-timer
            (run-with-timer (* 15 60) (* 15 60) 'spike-leung/apply-random-theme))
      (message "Random theme cycling started. Next change in ~15 minutes. Timer: %s" spike-leung/theme-cycle-timer))))

;; Start theme cycling by default, ensuring only one instance from this init file
(if (timerp spike-leung/theme-cycle-timer)
    (message "Random theme cycling timer already active: %s. Use 'M-x spike-leung/toggle-random-theme-cycling' to manage." spike-leung/theme-cycle-timer)
  (progn
    (setq spike-leung/theme-cycle-timer
          (run-with-timer (* 15 60) (* 15 60) 'spike-leung/apply-random-theme))
    (message "Random theme cycling started automatically. Next change in ~15 minutes. Timer: %s. Use 'M-x spike-leung/toggle-random-theme-cycling' to stop/start." spike-leung/theme-cycle-timer)))

;; pick a random them when init
(spike-leung/apply-random-theme)

(provide 'init-my-theme)
;;; init-my-theme.el ends here
