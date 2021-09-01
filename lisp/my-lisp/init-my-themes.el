;;; init-my-themes.el --- Defaults for themes -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require-package 'modus-themes)

;; Don't prompt to confirm theme safety. This avoids problems with
;; first-time startup on Emacs > 26.3.
(setq custom-safe-themes t)

;; If you don't customize it, this is the theme you get.
(setq-default custom-enabled-themes '(modus-operandi))

;; Ensure that themes will be applied even if they have not been customized
(defun reapply-themes ()
  "Forcibly load the themes listed in `custom-enabled-themes'."
  (dolist (theme custom-enabled-themes)
    (unless (custom-theme-p theme)
      (load-theme theme)))
  (custom-set-variables `(custom-enabled-themes (quote ,custom-enabled-themes))))

(add-hook 'after-init-hook 'reapply-themes)



;; Toggle between light and dark

(defun my-light ()
  "Activate a light color theme."
  (interactive)
  (setq custom-enabled-themes '(modus-operandi))
  (reapply-themes))

(defun my-dark ()
  "Activate a dark color theme."
  (interactive)
  (setq custom-enabled-themes '(modus-vivendi))
  (reapply-themes))

(provide 'init-my-themes)
;;; init-my-themes.el ends here
