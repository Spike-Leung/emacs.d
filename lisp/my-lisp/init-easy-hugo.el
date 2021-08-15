;;; Package --- Summary
;; init-easy-hugo

;;; Commentary:
;;; Code:
(when (executable-find "hugo")
  (when (maybe-require-package 'easy-hugo)
    (global-set-key (kbd "C-c b") 'easy-hugo)

    (with-eval-after-load 'easy-hugo
      (setq
       easy-hugo-basedir "~/Documents/private-git/taxodium"
       easy-hugo-default-ext ".org"
       easy-hugo-url "https://spike-leung.github.io/taxodium/"
       ))))

(provide 'init-easy-hugo)
;;; init-easy-hugo.el ends here
