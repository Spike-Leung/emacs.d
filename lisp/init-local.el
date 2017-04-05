;;; Code:

;;; Stop littering my project with autosaves and backups
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;;; Setup line numbers for code
;;; code and modes-to-hook-with-linum pilfered from https://gist.github.com/dbrady/3988571
(require 'linum)

(defun enable-linum-mode () (linum-mode t))

(defun hook-linum-mode (mode)
  (add-hook mode 'enable-linum-mode))

(setq modes-to-hook-with-linum '(c-mode-hook
                                 csv-mode-hook
                                 emacs-lisp-mode-hook
                                 coffee-mode-hook
                                 feature-mode-hook
                                 java-mode-hook
                                 js-mode-hook
                                 javascript-mode-hook
                                 espresso-mode-hook
                                 haml-mode-hook
                                 lisp-mode-hook
                                 nxml-mode-hook
                                 php-mode-hook
                                 ruby-mode-hook
                                 sass-mode-hook
                                 scss-mode-hook
                                 sh-mode-hook
                                 text-mode-hook
                                 textile-mode-hook
                                 xml-mode-hook
                                 yaml-mode-hook))

(mapc 'hook-linum-mode modes-to-hook-with-linum)

;; Use flx fuzzy finding to in ido
(require-package 'flx-ido)
(flx-ido-mode 1)
(setq ido-use-faces nil) ;; disable ido faces to see flx highlights.


;;; Pick a colour scheme
(color-theme-sanityinc-tomorrow-eighties)

;;; Use ag for searching all the things
(require-package 'ag)

;;; Powerline
(require-package 'powerline)
(require-package 'powerline-evil)
(powerline-evil-vim-color-theme)

;;; Evil mode config
(setq evil-emacs-state-cursor '("red" box))
(setq evil-motion-state-cursor '("orange" box))
(setq evil-normal-state-cursor '("green" box))
(setq evil-visual-state-cursor '("orange" box))
(setq evil-insert-state-cursor '("red" bar))
(setq evil-replace-state-cursor '("red" bar))
(setq evil-operator-state-cursor '("red" hollow))

;; User leader
(require-package 'evil-leader)
(global-evil-leader-mode)
(evil-leader/set-leader ",")

;; Turn on Evil
(require-package 'evil)
(evil-mode 1)

;; Evil key configurations
(evil-set-initial-state 'org-agenda-mode 'normal)
(defvar org-agenda-mode-map)
(evil-define-key 'normal org-agenda-mode-map
  "l" 'org-agenda-later
  "h" 'org-agenda-earlier
  "j" 'org-agenda-next-line
  "k" 'org-agenda-previous-line
  (kbd "RET") 'org-agenda-switch-to
  [escape] 'org-agenda-quit
  "q" 'org-agenda-quit
  "s" 'org-agenda-save-all-org-buffers
  "t" 'org-agenda-todo
  (kbd "SPC") 'org-agenda-show-and-scroll-up
  )

(evil-leader/set-key-for-mode 'org-agenda-mode
  "i" 'org-agenda-clock-in
  "o" 'org-agenda-clock-out
  "k" 'org-agenda-kill
  "/" 'org-agenda-filter-by-tag
  )

(evil-leader/set-key-for-mode 'org-mode
  "i" 'org-clock-in
  "o" 'org-clock-out
  )

(evil-leader/set-key
  "t" 'projectile-find-file
  "d" 'evil-delete-buffer
  "v" 'split-window-horizontally
  "o" 'org-agenda
  "c" 'delete-window
  "a" 'ag
  "," 'other-window
  "w" 'toggle-truncate-lines
  "g" 'magit-status
  )

;; Treat wrapped line scrolling as single lines
(define-key evil-normal-state-map (kbd "j") 'evil-next-visual-line)
(define-key evil-normal-state-map (kbd "k") 'evil-previous-visual-line)

;;; esc quits pretty much anything (like pending prompts in the minibuffer)
(define-key evil-normal-state-map [escape] 'keyboard-quit)
(define-key evil-visual-state-map [escape] 'keyboard-quit)

(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)

;;Shift space breaks out of insert
(define-key evil-insert-state-map (kbd "S-SPC") `evil-normal-state)


;;; Org mode setup
(setq
 org-default-notes-file "~/org/inbox.org"
 )

(provide 'init-local)
;;; init-local.el ends here
