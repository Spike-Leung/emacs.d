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
              (package-delete (package-get-descriptor package))))
        (setq remove-packages (cdr remove-packages)))
      )))

;; (let ((install-packages) '(magit github-clone forge))
;;   (while install-packages
;;     (package-install (car install-packages))
;;     (setq install-packages (cdr install-packages));
;;     ))

(provide 'init-custom-function)
;;; init-custom-function.el ends here
