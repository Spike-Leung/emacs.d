;;; Package --- Summary
;; init-local
;; https://github.com/purcell/emacs.d


;;; Commentary:
;; setup files ending in “.vue” to open in mhtml-mode
;; http://ergoemacs.org/emacs/emacs_auto-activate_a_major-mode.html
;; https://www.emacswiki.org/emacs/AutoModeAlist



;;; Code:
(add-to-list 'auto-mode-alist '("\\.vue\\'" . mhtml-mode))
(add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu/mu4e")


;;; links: https://koenig-haunstetten.de/2016/07/09/code-snippet-for-orgmode-e05s02/
(defun my/org-add-ids-to-headlines-in-file ()
  "Add ID properties to all headlines in the current file which do not already have one."
  (interactive)
  (org-map-entries 'org-id-get-create))

(add-hook 'org-mode-hook
          (lambda ()
            (add-hook 'before-save-hook 'my/org-add-ids-to-headlines-in-file nil 'local)))

(defun my/copy-id-to-clipboard()
  "Copy the ID property value to killring,
if no ID is there then create a new unique ID.
This function works only in org-mode buffers.

The purpose of this function is to easily construct id:-links to
org-mode items. If its assigned to a key it saves you marking the
text and copying to the killring."
  (interactive)
  (when (eq major-mode 'org-mode) ; do this only in org-mode buffers
    (setq mytmpid (funcall 'org-id-get-create))
    (kill-new mytmpid)
    (message "Copied %s to killring (clipboard)" mytmpid)
    ))

(global-set-key (kbd "<f5>") 'my/copy-id-to-clipboard)


;;; Agenda views
(let ((active-project-match "-INBOX/PROJECT"))

  (setq org-stuck-projects
        `(,active-project-match ("NEXT")))

  (setq org-agenda-compact-blocks t
        org-agenda-sticky t
        org-agenda-start-on-weekday nil
        org-agenda-span 'day
        org-agenda-include-diary nil
        org-agenda-sorting-strategy
        '((agenda habit-down time-up user-defined-up effort-up category-keep)
          (todo category-up effort-up)
          (tags category-up effort-up)
          (search category-up))
        org-agenda-window-setup 'current-window
        org-agenda-custom-commands
        `(("g" "GTD"
           ((agenda "" nil)
            (tags "INBOX"
                  ((org-agenda-overriding-header "Inbox")
                   (org-tags-match-list-sublevels nil)))
            (stuck ""
                   ((org-agenda-overriding-header "Stuck Projects")
                    (org-agenda-tags-todo-honor-ignore-options t)
                    (org-tags-match-list-sublevels t)
                    (org-agenda-todo-ignore-scheduled 'future)))
            (tags-todo "-INBOX"
                       ((org-agenda-overriding-header "Next Actions")
                        (org-agenda-tags-todo-honor-ignore-options t)
                        (org-agenda-todo-ignore-scheduled 'future)
                        (org-agenda-skip-function
                         '(lambda ()
                            (or (org-agenda-skip-subtree-if 'todo '("HOLD" "WAITING"))
                                (org-agenda-skip-entry-if 'nottodo '("NEXT")))))
                        (org-tags-match-list-sublevels t)
                        (org-agenda-sorting-strategy
                         '(todo-state-down effort-up category-keep))))
            (tags-todo ,active-project-match
                       ((org-agenda-overriding-header "Projects")
                        (org-tags-match-list-sublevels t)
                        (org-agenda-sorting-strategy
                         '(category-keep))))
            (tags-todo "-INBOX/-NEXT"
                       ((org-agenda-overriding-header "Orphaned Tasks")
                        (org-agenda-tags-todo-honor-ignore-options t)
                        (org-agenda-todo-ignore-scheduled 'future)
                        (org-agenda-skip-function
                         '(lambda ()
                            (or (org-agenda-skip-subtree-if 'todo '("PROJECT" "HOLD" "WAITING" "DELEGATED" "SOMEDAY" "GOAL"))
                                (org-agenda-skip-subtree-if 'nottododo '("TODO")))))
                        (org-tags-match-list-sublevels t)
                        (org-agenda-sorting-strategy
                         '(category-keep))))
            (tags-todo "/WAITING"
                       ((org-agenda-overriding-header "Waiting")
                        (org-agenda-tags-todo-honor-ignore-options t)
                        (org-agenda-todo-ignore-scheduled 'future)
                        (org-agenda-sorting-strategy
                         '(category-keep))))
            (tags-todo "/GOAL"
                       ((org-agenda-overriding-header "Goal")
                        (org-agenda-tags-todo-honor-ignore-options t)
                        (org-agenda-todo-ignore-scheduled 'future)
                        (org-agenda-sorting-strategy
                         '(category-keep))))
            (tags "/DONE|CANCELED"
                  ((org-agenda-overriding-header "Done")
                   (org-agenda-tags-todo-honor-ignore-options t)
                   (org-agenda-todo-ignore-scheduled 'future)
                   (org-agenda-sorting-strategy
                    '(category-keep))))
            (tags-todo "/SOMEDAY"
                       ((org-agenda-overriding-header "Someday")
                        (org-agenda-tags-todo-honor-ignore-options t)
                        (org-agenda-todo-ignore-scheduled 'future)
                        (org-agenda-sorting-strategy
                         '(category-keep))))
            (tags-todo "/DELEGATED"
                       ((org-agenda-overriding-header "Delegated")
                        (org-agenda-tags-todo-honor-ignore-options t)
                        (org-agenda-todo-ignore-scheduled 'future)
                        (org-agenda-sorting-strategy
                         '(category-keep))))
            (tags-todo "-INBOX"
                       ((org-agenda-overriding-header "On Hold")
                        (org-agenda-skip-function
                         '(lambda ()
                            (or (org-agenda-skip-subtree-if 'todo '("WAITING"))
                                (org-agenda-skip-entry-if 'nottodo '("HOLD")))))
                        (org-tags-match-list-sublevels nil)
                        (org-agenda-sorting-strategy
                         '(category-keep))))
            ;; (tags-todo "-NEXT"
            ;;            ((org-agenda-overriding-header "All other TODOs")
            ;;             (org-match-list-sublevels t)))
            )))))


;; (add-hook 'org-agenda-mode-hook 'hl-line-mode)


;;; mu4e
(add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu/mu4e")
(require 'mu4e)

(setq
 mue4e-headers-skip-duplicates  t
 mu4e-view-show-images t
 mu4e-view-show-addresses t
 mu4e-compose-format-flowed nil
 mu4e-date-format "%y/%m/%d"
 mu4e-headers-date-format "%Y/%m/%d"
 mu4e-change-filenames-when-moving t
 mu4e-attachments-dir "~/Downloads"

 mu4e-maildir       "~/Maildir"   ;; top-level Maildir
 ;; note that these folders below must start with /
 ;; the paths are relative to maildir root
 mu4e-refile-folder "/Archive"
 mu4e-sent-folder   "/Sent"
 mu4e-drafts-folder "/Drafts"
 mu4e-trash-folder  "/Trash")

;; this setting allows to re-sync and re-index mail
;; by pressing U
(setq mu4e-get-mail-command  "mbsync -a")




;;; @see https://docs.google.com/document/d/1P5At-z1sP8rgwYLHso5sEy3u4rMnIUDDgob9Y_BYuWE/edit
;; add beancount minor mode
;; (add-to-list 'load-path "/Library/Frameworks/Python.framework/Versions/3.8/elisp")
;; (require 'beancount)
;; (add-to-list 'auto-mode-alist '("\\.beancount\\'" . beancount-mode))
;; (add-hook 'beancount-mode-hook #'outline-minor-mode)
;; (define-key beancount-mode-map (key "C-c C-n")
;;   #'outline-next-visible-heading)
;; (define-key beancount-mode-map (key "C-c C-p")
;;   #'outline-previous-visible-heading)
(provide 'init-local)
;;; init-local.el ends here
