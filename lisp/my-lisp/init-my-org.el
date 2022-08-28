;;; Package --- Summary
;; init-my-org

;;; Commentary:
;; links: https://koenig-haunstetten.de/2016/07/09/code-snippet-for-orgmode-e05s02/

;;; Code:
(defun my/org-add-ids-to-headlines-in-file ()
  "Add ID properties to all headlines in the current file which do not already have one."
  (interactive)
  (org-map-entries 'org-id-get-create))

;; (add-hook 'org-mode-hook
;;           (lambda ()
;;             (add-hook 'before-save-hook 'my/org-add-ids-to-headlines-in-file nil 'local)))

(defun my/copy-id-to-clipboard()
  "Copy the ID property value to killring,
if no ID is there then create a new unique ID.
This function works only in org-mode buffers.

The purpose of this function is to easily construct id:-links to
Org-mode items. If its assigned to a key it saves you marking the
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
                         '(todo-state-down effort-up category-keep priority-down))))
            (tags-todo ,active-project-match
                       ((org-agenda-overriding-header "Projects")
                        (org-tags-match-list-sublevels t)
                        (org-agenda-sorting-strategy
                         '(category-keep priority-down))))
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
                         '(category-keep priority-down))))
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
                         '(category-keep priority-down))))
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


;;; Agenda files
(setq org-agenda-files (list "~/org/mylife.org" "~/org/mywork.org" "~/org/birthday.org" "~/org/reference.org" "~/org/goals.org"))


;;; Capture templates
;;; Private

(setq org-capture-templates '(
                              ;; private
                              ("p" "Private")
                              ("pa" "Anime" entry (file "~/org/anime.org") (file "~/org/template/tpl-anime.txt"))
                              ("pb" "Book" entry (file "~/org/book.org") (file "~/org/template/tpl-book.txt"))
                              ("pd" "Diary" plain (file+olp+datetree "~/org/diary.org") (file "~/org/template/tpl-diary.txt") :jump-to-captured t)
                              ("pm" "Movie" entry (file "~/org/movie.org") (file "~/org/template/tpl-movie.txt"))
                              ("ps" "Shopping" entry (file+headline "~/org/mylife.org" "Shopping-list") (file "~/org/template/tpl-shopping.txt"))
                              ("pw" "Weekly review" entry
                               (file+olp+datetree "~/org/weekly-review.org")
                               (file "~/org/template/tpl-weekly-review.txt")
                               :immediate-finish t
                               :jump-to-captured t)
                              ;; goal
                              ("g" "Goals")
                              ("gs" "Short term goals (next 6 month)" entry (file+olp "~/org/goals.org" "Short term goals") (file "~/org/template/tpl-goal.txt"))
                              ("gm" "Medium term goals (6 month up to 2 years)" entry (file+olp "~/org/goals.org" "Medium term goals") (file "~/org/template/tpl-goal.txt"))
                              ("gl" "Long term goals (2 - 5 years from now)" entry (file+olp "~/org/goals.org" "Long term goals") (file "~/org/template/tpl-goal.txt"))
                              ;; toto
                              ("w" "Works")
                              ("wd" "Daily Tasks" plain (file+olp+datetree "~/org/daily.org") (file "~/org/template/tpl-daily.txt"))
                              ("t" "Todo" entry (file "~/org/inbox.org") (file "~/org/template/tpl-todo.txt"))
                              ))

(setq
 org-latex-listings 'minted
 org-latex-packages-alist '(("" "minted"))
 org-latex-to-pdf-process
 '("xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"
   "xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"
   "xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"))


(setq org-startup-folded nil)

;;; Mobile Org Settings
;;; https://mobileorg.github.io/documentation/#syncing-with-mobileorg
;; Set to the location of your Org files on your local system
(setq org-directory "~/org-roam")
;; Set to the name of the file where new notes will be stored
(setq org-mobile-inbox-for-pull "~/org/inbox.org")
;; Set to <your Dropbox root directory>/MobileOrg.
(setq org-mobile-directory "~/Dropbox/Apps/MobileOrg")

(provide 'init-my-org)                  ;
;;; init-my-org.el ends here
