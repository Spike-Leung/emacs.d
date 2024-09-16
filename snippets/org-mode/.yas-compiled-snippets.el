;;; Compiled snippets and support files for `org-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'org-mode
                     '(("fontc" "#+ATTR_LATEX: :options fontsize=\\scriptsize\n#+begin_src ${1:bash}\n  $0\n#+end_src\n" "fontc" nil nil nil "/Users/spike/.emacs.d/snippets/org-mode/fontc" nil nil)
                       ("code" "#+BEGIN_SRC ${1:bash}\n  $0\n#+END_SRC\n" "#+begin_src ... #+end_src" nil nil nil "/Users/spike/.emacs.d/snippets/org-mode/code" nil nil)
                       ("blog" "* News | Article\n$0\n* Tutorial\n\n* Code\n\n* Cool Bit\n\n* Tool | Library\n\n* Music" "blog" nil nil nil "/Users/spike/.emacs.d/snippets/org-mode/blog" nil nil)))


;;; Do not edit! File generated at Mon Sep 16 14:39:35 2024
