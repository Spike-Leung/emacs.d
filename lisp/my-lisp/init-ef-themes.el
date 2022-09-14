;;; Package --- Summary
;; init-ef-themes
;;; Commentary:
;;; Code:
;;; Official manual https://protesilaos.com/emacs/ef-themes
(require-package 'ef-themes)

;; Disable all other themes to avoid awkward blending:
(mapc #'disable-theme custom-enabled-themes)

;; Load the themes
(load-theme 'ef-spring t t)
(load-theme 'ef-summer t t)
(load-theme 'ef-winter t t)
(load-theme 'ef-autumn t t)
(load-theme 'ef-day t )
(load-theme 'ef-night t t)
(load-theme 'ef-day t t)
(load-theme 'ef-night t t)

(provide 'init-ef-themes)
;;; init-ef-themes.el ends here
