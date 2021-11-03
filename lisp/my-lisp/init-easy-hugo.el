;;; Package --- Summary
;; init-easy-hugo

;;; Commentary:
;;; Code:
(when (maybe-require-package 'easy-hugo)
  (with-eval-after-load 'easy-hugo
    (setq
     easy-hugo-basedir "~/Documents/private-git/taxodium"
     easy-hugo-default-ext ".org"
     easy-hugo-url "https://spike-leung.github.io/taxodium/"
     easy-hugo-preview-url "http://localhost:1313/taxodium")
    (global-set-key (kbd "C-c b") 'easy-hugo)
    ))

(provide 'init-easy-hugo)
;;; init-easy-hugo.el ends here
