;;; Package --- Summary
;; init-ef-themes
;;; Commentary:
;;; Code:
(push (expand-file-name "lisp/my-lisp/ef-themes" user-emacs-directory) load-path)
(require 'ef-themes)

;; Disable all other themes to avoid awkward blending:
(mapc #'disable-theme custom-enabled-themes)

;; Load the themes
(load-theme 'ef-spring t t)
(load-theme 'ef-summer t)
(load-theme 'ef-winter t t)
(load-theme 'ef-autumn t t)
(load-theme 'ef-day t t)
(load-theme 'ef-night t t)
(load-theme 'ef-day t t)
(load-theme 'ef-night t t)

(provide 'init-ef-themes)
;;; init-ef-themes.el ends here
