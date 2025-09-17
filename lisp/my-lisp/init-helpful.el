;;; init-helpful.el --- an alternative to the built-in Emacs help -*- lexical-binding: t -*-
;;; Commentary:
;;; https://github.com/Wilfred/helpful
;;; Code:

(when (maybe-require-package 'helpful)
  ;; Note that the built-in `describe-function' includes both functions
  ;; and macros. `helpful-function' is functions only, so we provide
  ;; `helpful-callable' as a drop-in replacement.
  (global-set-key (kbd "C-h f") #'helpful-callable)
  (global-set-key (kbd "C-h v") #'helpful-variable)
  (global-set-key (kbd "C-h k") #'helpful-key)
  (global-set-key (kbd "C-h x") #'helpful-command)
  ;; Lookup the current symbol at point. C-c C-d is a common keybinding
  ;; for this in lisp modes.
  (global-set-key (kbd "C-c C-d") #'helpful-at-point)

  ;; Look up *F*unctions (excludes macros).
  ;;
  ;; By default, C-h F is bound to `Info-goto-emacs-command-node'. Helpful
  ;; already links to the manual, if a function is referenced there.
  (global-set-key (kbd "C-h F") #'helpful-function))

;;; https://www.reddit.com/r/emacs/comments/1mdi8vu/a_transient_for_help/
(with-eval-after-load 'transient
  (transient-define-prefix hrm-help-transient ()
    "Help commands that I use. A subset of C-h with others thrown in."
    ["Help Commands"
     ["Mode & Bindings"
      ("m" "Mode" describe-mode)
      ("M" "Minor Modes" consult-minor-mode-menu)
      ("b" "Major Bindings" which-key-show-full-major-mode)
      ("B" "Minor Bindings" which-key-show-full-minor-mode-keymap)
      ("d" "Descbinds" describe-bindings) ; or embark-bindings
      ("t" "Top Bindings  " which-key-show-top-level)
      ]
     ["Describe"
      ("C" "Command" helpful-command)
      ("f" "Function" helpful-callable)
      ("v" "Variable " helpful-variable)
      ("k" "Key" helpful-key)
      ("s" "Symbol" helpful-symbol)
      ("l" "Library" apropos-library)
      ]
     ["Info on"
      ("C-c" "Command" Info-goto-emacs-command-node)
      ("C-f" "Function" info-lookup-symbol)
      ("C-v" "Variable" info-lookup-symbol) ; fails if transient-detect-key-conflicts
      ("C-k" "Key" Info-goto-emacs-key-command-node)
      ("C-s" "Symbol" info-lookup-symbol)
      ]
     ["Goto Source"
      ""
      ("F" "Function" find-function-other-frame)
      ("V" "Variable" find-variable-other-frame)
      ("K" "Key" find-function-on-key-other-frame)
      ""
      ("L" "Library" find-library-other-frame)
      ]
     ["Apropos"
      ("ac" "Command" apropos-command)
      ("af" "Function" apropos-function)
      ("av" "Variable" apropos-variable)
      ("aV" "Value" apropos-value)
      ("aL" "Local Value" apropos-local-value)
      ("ad" "Documentation" apropos-documentation)
      ]
     ]
    [
     ["Internals"
      ("I" "Input Method" describe-input-method)
      ("G" "Language Env" describe-language-environment)
      ("S" "Syntax" describe-syntax)
      ("T" "Categories" describe-categories)
      ("O" "Coding System" describe-coding-system)
      ("o" "Coding Briefly" describe-current-coding-system-briefly)
      ("T" "Display Table" describe-current-display-table)
      ("e" "Echo Messages" view-echo-area-messages)
      ("H" "Lossage" view-lossage)
      ]
     ["Describe"
      ("." "At Point" helpful-at-point)
      ("c" "Key Short" describe-key-briefly)
      ("p" "Key Map" describe-keymap)
      ("A" "Face" describe-face)
      ("i" "Icon" describe-icon)
      ("w" "Where Is" where-is)
      ("=" "Position" what-cursor-position)
      ("g" "Shortdoc" shortdoc-display-group)
      ]
     ["Info Manuals"
      ("C-i" "Info" info)
      ("C-4" "Other Window" info-other-window)
      ("C-e" "Emacs" info-emacs-manual)
      ("C-l" "Elisp" info-elisp-manual)
      ("C-r" "Pick Manual" info-display-manual)
      ]
     ["External"
      ("N" "Man" consult-man)
      ;; ("W" "Dictionary" lookup-word-at-point)
      ;; ("D" "Dash" dash-at-point)
      ]
     ])
;;; A Help Transient on C-S-h
  (global-set-key (kbd "C-S-h") 'hrm-help-transient))

(defun info-elisp-manual ()
  "Display the Emacs Lisp Reference manual in Info mode."
  (interactive)
  (info "elisp"))

(provide 'init-helpful)
;;; init-helpful.el ends here
