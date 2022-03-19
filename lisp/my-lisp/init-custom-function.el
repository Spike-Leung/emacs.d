;;; Package --- Summary
;; init-custom-function
;;; Commentary:
;;; Code:
(defun spike-leung/uninstall-magit-deps ()
  "Remove magit dependences."
  (interactive)
  (let ((packages '(magit-todos github-clone forge magit)))
    (while packages
      (let ((package (car packages)))
        (if (assq package package-alist)
            (package-delete (package-get-descriptor package))))
      (setq packages (cdr packages)))))

(defun spike-leung/install-magit-deps ()
  "Install magit dependences."
  (let ((packages '(magit magit-todos github-clone forge)))
    (while packages
      (package-install (car packages))
      (load-library (car packages))
      (setq packages (cdr packages)))))

(defun spike-leung/unload-magit-feature ()
  "Unload magit related features."
  (let ((feats '(magit forge)))
    (while feats
      (let ((feat (car feats)))
        (if (featurep feat) (unload-feature feat t)))
      (setq feats (cdr feats)))))

(spike-leung/unload-magit-feature)

(defun spike-leung/reinstall-magit ()
  "Remove magit dependences and reinstall magit."
  (interactive)
  (progn
    (spike-leung/uninstall-magit-deps)
    (spike-leung/unload-magit-feature)
    (spike-leung/install-magit-deps)))

(provide 'init-custom-function)
;;; init-custom-function.el ends here
