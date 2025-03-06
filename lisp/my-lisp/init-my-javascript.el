;;; init-my-javascript.el --- javascript related config -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(maybe-require-package 'jsdoc)

(defvar spike-leung/js-related-modes '(vue-mode typescript-mode web-mode js-mode js2-mode js-ts-mode)
  "List of modes to add node_modules path.")

(defun spike-leung/enable-javascript-eslint-checker ()
  "Enable javascript-eslint checker."
  (setq-local flycheck--automatically-enabled-checkers '(javascript-eslint))
  (flycheck--toggle-checker 'javascript-eslint t)
  (flymake-mode-off)
  (flymake-mode-on))

;;; enabled `add-node-modules-path` in FrontEnd related mode
(when (maybe-require-package 'add-node-modules-path)
  (with-eval-after-load 'flymake-flycheck
    (with-eval-after-load 'add-node-modules-path
      (dolist (mode spike-leung/js-related-modes)
        (let ((mode-hook (intern (concat (symbol-name mode) "-hook"))))
          (add-hook mode-hook (lambda ()
                                (add-node-modules-path)
                                (spike-leung/enable-javascript-eslint-checker))))))))

(provide 'init-my-javascript)
;;; init-my-javascript.el ends here
