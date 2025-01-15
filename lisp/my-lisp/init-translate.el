;;; init-translate.el --- translate related
;;; Commentary:



;;; need to install https://github.com/soimort/translate-shell first.
;;; because `translate-shell` use google translate, emacs should able to connect to google.
(push (expand-file-name "lisp/my-lisp/emacs-immersive-translate" user-emacs-directory) load-path)
(require 'immersive-translate)
;; use translate-shell
;; (setq immersive-translate-backend 'trans)
(setq immersive-translate-backend 'chatgpt
      immersive-translate-chatgpt-host "api.deepseek.com")



;; @see: https://github.com/condy0919/fanyi.el
(maybe-require-package 'fanyi)
(global-set-key (kbd "M-o f") 'fanyi-dwim2)


;; @see: https://github.com/lorniu/go-translate
(maybe-require-package 'go-translate)
(maybe-require-package 'plz)
(with-eval-after-load 'go-translate
  (setq gt-langs '(en zh))
  (setq gt-chatgpt-key (spike-leung/get-deepseek-api-key))
  (setq gt-chatgpt-host "https://api.deepseek.com")
  (setq gt-chatgpt-model "deepseek-chat")
  (setq gt-default-translator (gt-translator
                               :engines (list
                                         (gt-chatgpt-engine))
                               :render (gt-buffer-render
                                        :buffer-name "gt-translator"
                                        :window-config '((display-buffer-at-bottom))
                                        :then (lambda (_) (pop-to-buffer "gt-translator")))
                               )))
(global-set-key (kbd "M-o t") 'gt-do-translate)


(provide 'init-translate)
;;; init-translate.el ends here
