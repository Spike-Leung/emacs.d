;;; init-translate.el --- translate related -*- lexical-binding: t -*-
;;; Commentary:



;;; need to install https://github.com/soimort/translate-shell first.
;;; because `translate-shell` use google translate, emacs should able to connect to google.
(require 'immersive-translate)
;; need to api key with user `apikey` in `.authinfo`
(setq immersive-translate-backend 'chatgpt
      immersive-translate-chatgpt-host "openrouter.ai/api"
      immersive-translate-chatgpt-model "google/gemini-2.0-flash-001"
      immersive-translate-pending-message "(≖ᴗ≖๑)"
      immersive-translate-failed-message "(つд⊂) ")



;; @see: https://github.com/condy0919/fanyi.el
(maybe-require-package 'fanyi)



;; @see: https://github.com/lorniu/go-translate
(maybe-require-package 'go-translate)
(maybe-require-package 'plz)
(with-eval-after-load 'init-auth
  (with-eval-after-load 'go-translate
    (setq gt-langs '(en zh)
          gt-chatgpt-key (spike-leung/get-deepseek-api-key)
          gt-chatgpt-host "https://api.deepseek.com"
          gt-chatgpt-model "deepseek-chat"
          gt-default-translator (gt-translator
                                 :engines (list
                                           (gt-chatgpt-engine))
                                 :render (gt-buffer-render
                                          :buffer-name "gt-translator"
                                          :window-config '((display-buffer-at-bottom))
                                          :then (lambda (_) (pop-to-buffer "gt-translator")))))))


(defvar spike-leung/my-translate-keymap (make-sparse-keymap)
  "Keymap for translation commands.")

(define-key spike-leung/my-translate-keymap (kbd "i") 'immersive-translate-buffer)
(define-key spike-leung/my-translate-keymap (kbd "f") 'fanyi-dwim2)
(define-key spike-leung/my-translate-keymap (kbd "g") 'gt-do-translate)

(global-set-key (kbd "M-o t") spike-leung/my-translate-keymap)


(provide 'init-translate)
;;; init-translate.el ends here
