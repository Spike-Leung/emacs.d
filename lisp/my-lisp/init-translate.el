;;; init-translate.el --- translate related
;;; Commentary:



;;; need to install https://github.com/soimort/translate-shell first.
;;; because `translate-shell` use google translate, emacs should able to connect to google.
(push (expand-file-name "lisp/my-lisp/emacs-immersive-translate" user-emacs-directory) load-path)
(require 'immersive-translate)
;; use translate-shell
(setq immersive-translate-backend 'trans)



;; @see: https://github.com/condy0919/fanyi.el
(maybe-require-package 'fanyi)



;; @see: https://github.com/lorniu/go-translate
(maybe-require-package 'go-translate)
(maybe-require-package 'plz)
(with-eval-after-load 'go-translate
  (setq gt-langs '(en zh))
  (setq gt-default-translator (gt-translator
                               :engines (list (gt-google-engine)))))



(provide 'init-translate)
;;; init-translate.el ends here
