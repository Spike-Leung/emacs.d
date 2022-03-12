;;; Package --- Summary
;; init-custom-function
;;; Commentary:
;;; Code:
(defun spike-leung/reinstall-magit ()
  "Remove magit dependences and reinstall magit."
  (interactive)
  (let ((remove-packages '(magit-todos github-clone forge magit)))
    (progn
      (while remove-packages
        (let ((package (car remove-packages)))
          (if (assq package package-alist)
              (progn
                (package-delete (package-get-descriptor package))
                )))
        (setq remove-packages (cdr remove-packages)))
      (if (featurep 'magit) (unload-feature 'magit t))
      (let ((install-packages '(magit magit-todos github-clone forge)))
        (while install-packages
          (package-install (car install-packages))
          (setq install-packages (cdr install-packages)) ;
          ))
      )))

(provide 'init-custom-function)
;;; init-custom-function.el ends here
