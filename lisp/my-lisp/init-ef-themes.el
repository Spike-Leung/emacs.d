;;; Package --- Summary
;; init-ef-themes
;;; Commentary:
;;; Code:
(push (expand-file-name "lisp/my-lisp/ef-themes" user-emacs-directory) load-path)
(require 'ef-themes)

;; Disable all other themes to avoid awkward blending:
(mapc #'disable-theme custom-enabled-themes)

;; Load the theme of choice:
;; The themes we provide:
;;
;; Light: `ef-day', `ef-light', `ef-spring', `ef-summer'.
;; Dark:  `ef-autumn', `ef-dark', `ef-night', `ef-winter'.
(load-theme 'ef-spring :no-confirm)
(load-theme 'ef-summer :no-confirm)
(load-theme 'ef-winter :no-confirm)
(load-theme 'ef-autumn :no-confirm)

;; We also provide these commands, but do not assign them to any key:
;;
;; - `ef-themes-select'
;; - `ef-themes-load-random'
;; - `ef-themes-preview-colors'
;; - `ef-themes-preview-colors-current'

(provide 'init-ef-themes)
;;; init-ef-themes.el ends here
