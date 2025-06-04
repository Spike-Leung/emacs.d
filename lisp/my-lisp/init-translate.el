;;; init-translate.el --- translate related -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;; immersive-translate
(require 'immersive-translate)
(add-hook 'elfeed-show-mode-hook #'immersive-translate-setup)
;; need to api key with user `apikey` in `.authinfo`
(setq immersive-translate-backend 'chatgpt
      immersive-translate-chatgpt-host "openrouter.ai/api"
      immersive-translate-chatgpt-model "google/gemini-2.5-flash-preview-05-20"
      immersive-translate-pending-message "(≖ᴗ≖๑)"
      immersive-translate-failed-message "(つд⊂) ")


;;; fanyi
;; @see: https://github.com/condy0919/fanyi.el
(maybe-require-package 'fanyi)
(with-eval-after-load 'fanyi
  (custom-set-variables '(fanyi-providers '(
                                            fanyi-haici-provider
                                            fanyi-longman-provider))))



;;; go-translate
;; @see: https://github.com/lorniu/go-translate
(maybe-require-package 'go-translate)
(maybe-require-package 'plz)
(with-eval-after-load 'init-auth
  (with-eval-after-load 'go-translate
    (setq gt-langs '(en zh)
          gt-chatgpt-key (spike-leung/get-openrouter-api-key)
          gt-chatgpt-host "https://openrouter.ai/api"
          gt-chatgpt-model "google/gemini-2.5-flash-preview-05-20"
          gt-default-translator (gt-translator
                                 :engines (list
                                           ;; FIXME: stream nil will pass stream: null in request, which is not support by openrouter, report a issue sometime
                                           (gt-chatgpt-engine :stream nil))
                                 :render (gt-buffer-render
                                          :buffer-name "gt-translator"
                                          :window-config '((display-buffer-at-bottom))
                                          :then (lambda (_) (pop-to-buffer "gt-translator")))))))


;;; keybindings
(defvar spike-leung/my-translate-keymap (make-sparse-keymap)
  "Keymap for translation commands.")

(define-key spike-leung/my-translate-keymap (kbd "i") 'immersive-translate-buffer)
(define-key spike-leung/my-translate-keymap (kbd "f") 'fanyi-dwim2)
(define-key spike-leung/my-translate-keymap (kbd "g") 'gt-do-translate)

(global-set-key (kbd "M-o t") spike-leung/my-translate-keymap)


(provide 'init-translate)
;;; init-translate.el ends here
