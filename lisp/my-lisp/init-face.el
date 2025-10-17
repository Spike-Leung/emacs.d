;;; Package --- init-face -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(set-face-attribute 'default nil :height 200 :font "Iosevka" :font "LXGW WenKai Mono")
(set-face-attribute 'bold nil :font "Iosevka" :weight 'heavy)
;;; make background transparent
;; (setq frame-background-mode "light")
;; (set-face-background 'default "unspecified-bg")
(provide 'init-face)
;;; init-face.el ends here
