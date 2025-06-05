;;; init-my-keybindings.el --- Centralized user keybindings -*- lexical-binding: t -*-
;;; Commentary:
;;; Defines common prefix keymaps and assigns global keys.
;;; Code:

(defvar spike-leung/meta-o-keymap (make-sparse-keymap)
  "Keymap for custom M-o commands.
This keymap is bound to M-o globally.")
(global-set-key (kbd "M-o") spike-leung/meta-o-keymap)

(when (maybe-require-package 'avy)
  (with-eval-after-load 'avy
    (define-key spike-leung/meta-o-keymap (kbd "M-o") 'avy-goto-char-timer)))

(add-hook 'html-ts-mode-hook
          (lambda ()
            (define-key html-ts-mode-map (kbd "M-o") spike-leung/meta-o-keymap)))
(add-hook 'html-mode-hook
          (lambda ()
            (define-key html-mode-map (kbd "M-o") spike-leung/meta-o-keymap)))
(add-hook 'mhtml-mode-hook
          (lambda ()
            (define-key mhtml-mode-map (kbd "M-o") spike-leung/meta-o-keymap)))

(when (maybe-require-package 'avy)
  (define-key spike-leung/meta-o-keymap (kbd "M-o") 'avy-goto-char-timer))

(provide 'init-my-keybindings)
;;; init-my-keybindings.el ends here
