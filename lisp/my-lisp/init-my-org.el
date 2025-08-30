;;; init-my-org.el --- org config to overwrite init-org.el -*- lexical-binding: t -*-
;;; Commentary:
;; links: https://koenig-haunstetten.de/2016/07/09/code-snippet-for-orgmode-e05s02/
;;; Code:

(defun spike-leung/org-add-ids-to-headlines-in-file ()
  "Add ID properties to all headlines in the current file which do not already have one."
  (interactive)
  (org-map-entries 'org-id-get-create))

(defun spike-leung/org-add-custom-id-to-headings-in-files ()
  "Add a CUSTOM_ID property to all headings in the current buffer, if it does not already exist."
  (interactive)
  (org-map-entries
   (progn
     (lambda ()
       (unless (org-entry-get nil "CUSTOM_ID")
         (let ((custom-id (org-id-new)))
           (org-set-property "CUSTOM_ID" custom-id))))
     (org-id-get-create))))

(add-hook 'org-mode-hook
          (lambda ()
            (add-hook 'before-save-hook 'spike-leung/org-add-custom-id-to-headings-in-files nil 'local)))

(defun my/copy-id-to-clipboard()
  "Copy the ID property value to killring.
If no ID is there then create a new unique ID.
This function works only in `org-mode' buffers.

The purpose of this function is to easily construct id:-links to Org-mode items.
If its assigned to a key it saves you marking the text and copying to the killring."
  (interactive)
  (when (eq major-mode 'org-mode) ; do this only in org-mode buffers
    (setq mytmpid (funcall 'org-id-get-create))
    (kill-new mytmpid)
    (message "Copied %s to killring (clipboard)" mytmpid)
    ))

;;; https://www.youtube.com/watch?v=be8TC-i-NpE&list=PLVtKhBrRV_ZkPnBtt_TD1Cs9PJlU0IIdE&index=40&t=111s
;;; https://koenig-haunstetten.de/2019/01/06/changes-to-my-orgmode-system/
;;; https://koenig-haunstetten.de/2018/02/17/improving-my-orgmode-workflow/
(defun my/copy-idlink-to-clipboard()
  "Copy an ID link with the headline to `kill-ring'.
If no ID is there then create a new unique ID.
This function works only in `org-mode' or `org-agenda' buffers.

The purpose of this function is to easily construct id:-links to `org-mode' items.
If its assigned to a key it saves you marking the text and copying to the killring."
  (interactive)
  (when (eq major-mode 'org-agenda-mode) ;switch to orgmode
    (org-agenda-show)
    (org-agenda-goto))
  (when (eq major-mode 'org-mode) ; do this only in org-mode buffers
    (setq mytmphead (nth 4 (org-heading-components)))
    (setq mytmpid (funcall 'org-id-get-create))
    (setq mytmplink (format "[[id:%s][%s]]" mytmpid mytmphead))
    (kill-new mytmplink)
    (message "Copied %s to killring (clipboard)" mytmplink)
    ))

(global-set-key (kbd "<f5>") 'my/copy-idlink-to-clipboard)


;;; Agenda views
(let ((active-project-match "-INBOX/PROJECT"))

  (setq org-stuck-projects
        `(,active-project-match ("NEXT" "TODO")))

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
            (tags "/DONE|CANCELLED"
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
(setq org-agenda-files (list "~/org/mylife.org"
                             "~/org/mywork.org"
                             "~/org/birthday.org"
                             "~/org/reference.org"
                             "~/org/goals.org"
                             "~/org/dead.org"))

(when *is-wsl*
  (setq org-agenda-files (list "~/spike-docs/org/work.org")))


;;; Capture templates
;;; Private

(setq org-capture-templates
      '(
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
        ("w" "Web site" entry
         (file "")
         "* %a :website:\n\n%U %?\n\n%:initial")
        ("wd" "Daily Tasks" plain (file+olp+datetree "~/org/daily.org") (file "~/org/template/tpl-daily.txt") :jump-to-captured t :immediate-finish t)
        ("t" "Todo" entry (file "~/org/inbox.org") (file "~/org/template/tpl-todo.txt"))
        ("c" "Clue" entry (file "~/org/clue.org") "* %f %^{Things that you track}\n%?"
         :jump-to-captured t
         :empty-lines-after 1
         :prepend t
         :unnarrowed t)
        ("x" "Reading List" item
         (file+headline "~/notes/20241112T202642--reading-list__collection_read.org" "Refs")
         "[[%:link][%:description]]\n%i\n\n"
         :jump-to-captured t
         :immediate-finish t)
        ))

(when *is-wsl*
  (setq org-capture-templates
        '(("d"
           "Daily Tasks"
           plain
           (file+olp+datetree "~/daily.org")
           (file "~/spike-docs/org/template/daily.txt")
           ("w" "Web site" entry
            (file "")
            "* %a :website:\n\n%U %?\n\n%:initial")
           :jump-to-captured t
           :immediate-finish t)
          ("x" "Reading List" item
           (file+headline "~/notes/20241112T202642--reading-list__collection_read.org" "Refs")
           "[[%:link][%:description]]\n%i\n\n"
           :jump-to-captured t
           :immediate-finish t)
          )))

(setq
 org-latex-listings 'minted
 org-latex-packages-alist '(("" "minted"))
 org-latex-to-pdf-process
 '("xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"
   "xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"
   "xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"))


(setq org-startup-folded nil)

;;; org-structure-template
(setq org-structure-template-alist '(("e" . "example")
                                     ("s" . "src")
                                     ("se" . "src emacs-lisp")
                                     ("sc" . "src css")
                                     ("sb" . "src bash")
                                     ("ss" . "src sh")
                                     ("sh" . "src html")
                                     ("sj" . "src javascript")
                                     ("st" . "src typescript")
                                     ("q" . "quote")
                                     ("h" . "export html")
                                     ("v" . "verse")))

;;; To-do settings
(setq org-todo-keywords
      (quote ((sequence
               "TODO(t)"
               "NEXT(n)"
               "REPEAT(r)"
               "PROJECT(p)"
               "SOMEDAY(s!)"
               "WAITING(w@/!)"
               "DELEGATED(e!)"
               "HOLD(h)"
               "|"
               "DONE(d!/!)"
               "CANCELLED(c@/!)")))
      org-todo-repeat-to-state "REPEAT")

(setq org-todo-keyword-faces
      `(("NEXT" . (:foreground ,(modus-themes-get-color-value `yellow-warmer)))
        ("TODO" . (:foreground ,(modus-themes-get-color-value `red-intense)))
        ("DONE" . (:foreground ,(modus-themes-get-color-value `green-cooler)))
        ("PROJECT" . (:foreground ,(modus-themes-get-color-value `magenta-cooler)))
        ("SOMEDAY" . (:foreground ,(modus-themes-get-color-value `rust)))))

;;; Mobile Org Settings
;;; https://mobileorg.github.io/documentation/#syncing-with-mobileorg
;; Set to the location of your Org files on your local system
(setq org-directory "~/org-roam")
;; Set to the name of the file where new notes will be stored
(setq org-mobile-inbox-for-pull "~/org/inbox.org")
;; Set to <your Dropbox root directory>/MobileOrg.
(setq org-mobile-directory "~/Dropbox/Apps/MobileOrg")


;;; setup org-modern
;; (maybe-require-package 'org-modern)
;; (with-eval-after-load 'org (global-org-modern-mode))
;; (setq org-modern-block-name '(("src" . t) ("quote" "[" "]") ("example" . t)))
;; (setq org-modern-timestamp nil)
;; (setq org-modern-todo nil)
;; (setq org-modern-checkbox nil)
;; (setq org-modern-tag nil)
;; (setq org-modern-progress nil)


(setq org-html-html5-fancy t)
(setq org-html-doctype "html5")

(with-eval-after-load 'org
  (require 'org-menu)
  (define-key org-mode-map (kbd "C-c m") 'org-menu))

;; enable org-protocol
(server-start)
;; for wsl2
(add-to-list 'load-path "/snap/emacs/current/usr/share/emacs/29.4/lisp/org/")
(require 'org-protocol)
(require 'org-protocol-capture-html)

;; 让中文也可以不加空格就使用行内格式, see: https://emacs-china.org/t/orgmode/9740/14
;; 虽然在 eamcs 的 org-mode 里能用，但是导出成 HTML 的时候会有一些问题
;; (setcar (nthcdr 0 org-emphasis-regexp-components) " \t('\"{[:nonascii:]")
;; (setcar (nthcdr 1 org-emphasis-regexp-components) "- \t.,:!?;'\")}\\[[:nonascii:]")
;; (org-set-emph-re 'org-emphasis-regexp-components org-emphasis-regexp-components)
;; (org-element-update-syntax)

(provide 'init-my-org)                  ;
;;; init-my-org.el ends here
