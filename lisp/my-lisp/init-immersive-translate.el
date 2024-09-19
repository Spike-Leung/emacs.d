;;; init-immersive-translate.el
;;; Commentary:
;;; Code:
;;; need to install https://github.com/soimort/translate-shell first.
;;; because `translate-shell` use google translate, emacs should able to connect to google.
(maybe-require-package 'immersive-translate)
;; use translate-shell
(setq immersive-translate-backend 'trans)
(provide 'init-immersive-translate)
;;; init-immersive-translate.el ends here