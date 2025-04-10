;;; init-kaomoji.el --- kaomoji -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defun spike-leung/kaomoji--search (query)
  "Search `spike-leung/kaomoji-table' for QUERY with fuzzy matching."
  (let ((results '()))
    (dolist (entry spike-leung/kaomoji-table)
      (when (cl-some (lambda (keyword)
                       (string-match-p (regexp-quote (downcase query))
                                       (downcase keyword)))
                     (car entry))
        (push (cons (string-join (car entry) ", ") (cdr entry)) results)))
    (nreverse results)))

(defun spike-leung/kaomoji--annotate (cand)
  "Annotate candidate CAND with its kaomoji in a separate column with styling."
  (when-let ((entry (assoc cand spike-leung/kaomoji--candidates)))
    (concat
     (propertize " " 'display '(space :align-to 50))
     (propertize
      (cdr entry)
      'face
      (cond
       ((fboundp 'modus-themes-get-color-value)
        `(:foreground ,(modus-themes-get-color-value `rust)))
       ((fboundp 'ef-themes-get-color-value)
        `(:foreground ,(ef-themes-get-color-value `rust)))
       (t
        nil))))))

(defvar spike-leung/kaomoji--candidates nil
  "List of candidates for consult.")

(defun spike-leung/kaomoji--select (prompt choices)
  "PROMPT user to select from CHOICES with consult support."
  (setq spike-leung/kaomoji--candidates choices)
  (if (fboundp 'consult--read)
      (consult--read
       (mapcar #'car choices)
       :prompt prompt
       :sort nil
       :require-match t
       :history 'spike-leung/kaomoji--history
       :annotate #'spike-leung/kaomoji--annotate
       :category 'spike-leung/kaomoji)
    (completing-read prompt choices nil t)))

;;;###autoload
(defun spike-leung/kaomoji (query)
  "Interactively select and insert a kaomoji.
With prefix arg or when QUERY is provided, search kaomoji by name."
  (interactive
   (list (when current-prefix-arg
           (read-string "Search kaomoji: "))))
  (let* ((choices (if query
                      (spike-leung/kaomoji--search query)
                    (mapcar (lambda (entry)
                              (cons (string-join (car entry) ", ") (cdr entry)))
                            spike-leung/kaomoji-table)))
         (selected (spike-leung/kaomoji--select "Select kaomoji: " choices)))
    (insert (alist-get selected choices nil nil #'equal))))

(defvar spike-leung/kaomoji-table
  '((("angry" "table" "生气" "翻桌" "干")       . "(╯°□°）╯︵ ┻━┻")
    (("bleeding" "吐血")                        . "_​:(´ཀ`」∠):_​")
    (("cry" "泣き")                             . "・ﾟ・｡･ﾟ･(つД｀)")
    (("cry" "泣き" "哭哭")                      . "(´;ω;`)")
    (("dandin" "淡定")                          . "ˊ_​>ˋ")
    (("heart" "ハート" "爱心")                   . "♥")
    (("ehehe" "えへへ" "エヘヘ")                . "へ(゜∇、°)へ")
    (("lazy" "懶")                              . "_​(:3 」∠)_​")
    (("uwu")                                    . "( ˘ω˘ )")
    (("sad")                                    . "(´･_​･`)")
    (("Huh?")                                   . "(ﾟ⊿ﾟ)")
    (("WTF" "什么啦")                           . "(｡ŏ_​ŏ)")
    (("owo")                                    . "( ˘•ω•˘ )")
    (("relax")                                  . "(´-ω-｀)")
    (("happy")                                  . ">w<")
    (("yeah" ">w<")                             . "(ﾉ>ω<)ﾉ")
    (("rock")                                   . "\\m/ >_​< \\m/")
    (("cheers" "欢呼")                          . "｡:.ﾟヽ(*´∀`)ﾉﾟ.:｡")
    (("cheers" "欢呼")                          . "ヾ(*´∀ ˋ*)ﾉ")
    (("owo")                                    . "(´・ω・`)")
    (("don't see")                              . "(つд⊂)")
    (("alas" "无奈")                            . "╮(╯_╰)╭")
    (("me?" "我吗?")                            . "σ(´∀｀*)")
    (("i would be angry" "我要生气了")          . "(・`ω´・)")
    (("really?")                                . "(≖ᴗ≖๑)")
    (("shocked" "震惊" "惊讶")                  . "(ﾟдﾟ)")
    (("come here")                              . "ლ(´ڡ`ლ)")
    (("angry" "furious")                        . "(／‵Д′)／~ ╧╧")))

(provide 'init-kaomoji)
;;; init-kaomoji.el ends here
