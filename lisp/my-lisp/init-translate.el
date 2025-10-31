;;; init-translate.el --- translate related -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;; immersive-translate
(require 'immersive-translate)
(add-hook 'elfeed-show-mode-hook #'immersive-translate-setup)
;; need to api key with user `apikey` in `.authinfo`
(setq immersive-translate-backend 'chatgpt
      immersive-translate-chatgpt-host "openrouter.ai/api"
      immersive-translate-chatgpt-model "google/gemini-2.5-flash"
      immersive-translate-pending-message "(≖ᴗ≖๑)"
      immersive-translate-failed-message "(つд⊂) ")


;;; fanyi
;; @see: https://github.com/condy0919/fanyi.el
(maybe-require-package 'fanyi)
(with-eval-after-load 'fanyi
  (custom-set-variables '(fanyi-providers '(
                                            fanyi-haici-provider
                                            fanyi-longman-provider))))



;; @see: https://github.com/lorniu/gt.el
(maybe-require-package 'gt)
(maybe-require-package 'plz)
(with-eval-after-load 'init-auth
  (with-eval-after-load 'gt
    (setq gt-langs '(en zh)
          gt-chatgpt-key (spike-leung/get-openrouter-api-key)
          gt-chatgpt-host "https://openrouter.ai/api"
          gt-chatgpt-model "google/gemini-2.5-flash"
          gt-default-translator (gt-translator
                                 :taker (gt-taker :pick nil :prompt t)
                                 :engines (gt-chatgpt-engine :stream t)
                                 :render (gt-buffer-render
                                          :name "gt-translator"
                                          :window-config '((display-buffer-at-bottom))
                                          :then (lambda (_) (pop-to-buffer "gt-translator")))))))


;;; keybindings
(defvar spike-leung/my-translate-keymap (make-sparse-keymap)
  "Keymap for translation commands.")

(define-key spike-leung/my-translate-keymap (kbd "i") 'immersive-translate-buffer)
(define-key spike-leung/my-translate-keymap (kbd "p") 'immersive-translate-paragraph)
(define-key spike-leung/my-translate-keymap (kbd "c") 'immersive-translate-clear)
(define-key spike-leung/my-translate-keymap (kbd "f") 'fanyi-dwim2)
(define-key spike-leung/my-translate-keymap (kbd "g") 'gt-translate)

(with-eval-after-load 'init-my-keybindings
  (define-key spike-leung/meta-o-keymap (kbd "t") spike-leung/my-translate-keymap))


(provide 'init-translate)
;;; init-translate.el ends here
