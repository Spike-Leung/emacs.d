;;; Package --- Summary
;; init-face
;;; Commentary:
;;; Code:
(set-face-attribute 'default nil :height 150 :family "Hack Nerd Font")
;; set transparent background
(setq frame-background-mode "light")
(set-face-background 'default "unspecified-bg")

(defun spike-leung/toggle-wsl-theme ()
  "Toggle dark theme and set background transparent."
  (interactive)
  (modus-themes-select 'modus-vivendi-tritanopia)
  (spike-leung/set-background-transparent))


(defun spike-leung/set-background-transparent ()
  "Set background transparent."
  (interactive)
  (set-face-background 'default "unspecified-bg"))

(provide 'init-face)
;;; init-face.el ends here
