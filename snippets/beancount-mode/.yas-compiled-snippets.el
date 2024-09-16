;;; Compiled snippets and support files for `beancount-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'beancount-mode
                     '(("ts" "`(format-time-string \"%Y-%m\")`-$1 * \"$2\"\n  $0" "transaction" nil nil nil "/Users/spike/.emacs.d/snippets/beancount-mode/ts" nil nil)
                       ("ls" "`(format-time-string \"%Y-%m\" (let* ((now (current-time))\n       (month (nth 4 (decode-time now)))\n       (year (nth 5 (decode-time now))))\n  (if (> month 1)\n      (encode-time 0 0 0 1 (1- month) year)\n    (encode-time 0 0 0 1 12 (1- year)))))`-$1 * \"$2\"\n  $0" "transaction with last month" nil nil nil "/Users/spike/.emacs.d/snippets/beancount-mode/ls" nil nil)
                       ("c" "$1.${2:00} CNY\n   $0" "CNY" nil nil nil "/Users/spike/.emacs.d/snippets/beancount-mode/CNY" nil nil)))


;;; Do not edit! File generated at Mon Sep 16 14:39:35 2024
