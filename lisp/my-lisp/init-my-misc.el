;;; init-my-misc.el --- miscellaneous config -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;; Jira related
(defun spike-leung/open-jira-issue (issue-id)
  "Open the JIRA issue with ISSUE-ID in a web browser."
  (interactive "sEnter JIRA issue ID (e.g., DD-1234): ")
  (let* ((jira-base-url "https://jira.gyenno.com")
         (full-url (concat jira-base-url "/browse/" issue-id)))
    (browse-url full-url)))

(defun spike-leung/open-jira-issue-from-magit-log ()
  "Open the JIRA issue(s) from the current line in a `magit-log' buffer."
  (interactive)
  (let* ((current-line (thing-at-point 'line t))
         (jira-id-regexp "\\([A-Za-z0-9]+-[0-9]+\\)")
         (jira-base-url "https://jira.gyenno.com")
         (start 0)
         jira-ids)
    ;; Extract all JIRA IDs from the current line
    (while (string-match jira-id-regexp current-line start)
      (push (match-string 0 current-line) jira-ids)
      (setq start (match-end 0)))
    (if jira-ids
        ;; Open each JIRA issue in a browser
        (dolist (issue-id (reverse jira-ids))
          (let ((full-url (concat jira-base-url "/browse/" issue-id)))
            (browse-url full-url)))
      (message "No JIRA issue ID found on the current line."))))

(with-eval-after-load 'magit
  (define-key magit-log-mode-map (kbd "]") 'spike-leung/open-jira-issue-from-magit-log))



;;;
;; write by ChatGPT-4o
;; https://chatgpt.com/share/fd2bfa45-fbfd-492a-aa6b-6b3023f76772
(defun spike-leung/process-next-html-src-block ()
  "Process the next HTML block from the current position, escape it, compress it.
Then generate a #+begin_export html block with an iframe, replacing any existing export block."
  (interactive)
  (let (html-content escaped-html srcdoc start end export-start export-end)
    ;; Search for the next #+begin_src html block from the current position
    (save-excursion
      (when (re-search-forward "^#\\+begin_src html" nil t)
        (setq start (match-end 0))
        (when (re-search-forward "^#\\+end_src" nil t)
          (setq end (match-beginning 0))
          (setq html-content (buffer-substring-no-properties start end))
          ;; Process the HTML content
          (when html-content
            ;; Escape HTML characters using sgml-quote in a temporary buffer
            (setq escaped-html
                  (with-temp-buffer
                    (insert html-content)
                    (sgml-mode) ;; Switch to sgml-mode to enable sgml-quote
                    (sgml-quote (point-min) (point-max))
                    (buffer-string)))

            ;; Compress into a single line and remove extra spaces
            (setq srcdoc (replace-regexp-in-string "[\n\r]+" " " escaped-html))
            (setq srcdoc (replace-regexp-in-string "[ \t]+" " " srcdoc))

            ;; Move to the end of the block and check for existing export block
            (goto-char end)
            (forward-line 1) ;; Move to the line after #+end_src

            ;; Check if there's an existing #+begin_export html block
            (when (looking-at "^#\\+begin_export html")
              (setq export-start (point))
              (when (re-search-forward "^#\\+end_export" nil t)
                (setq export-end (point))
                ;; Delete the existing export block
                (delete-region export-start export-end)))

            ;; Insert the new #+begin_export html block
            (insert (format "#+begin_export html\n<iframe style=\"width:100%%\" srcdoc=\"%s\"></iframe>\n#+end_export\n" srcdoc))))))))



;;; compress image
(defun pngcrush-compress-directory (directory)
  "Compress all PNG, JPG, and JPEG files in DIRECTORY to the maximum extent using ImageMagick and pngcrush."
  (interactive "DSelect directory: ")
  (let ((image-files (directory-files directory t "\(\.png\|\.jpg\|\.jpeg\)$")))
    (dolist (image-file image-files)
      (let ((output-file (concat (file-name-sans-extension image-file) "-crushed.png")))
        (cond
         ((string-match-p "\.png$" image-file)
          (call-process "pngcrush" nil nil nil "-brute" image-file output-file)
          (rename-file output-file image-file t))
         ((or (string-match-p "\.jpg$" image-file)
              (string-match-p "\.jpeg$" image-file))
          (let ((png-temp-file (concat (file-name-sans-extension image-file) "-temp.png")))
            (call-process "magick" nil nil nil image-file png-temp-file)
            (call-process "pngcrush" nil nil nil "-brute" png-temp-file output-file)
            (rename-file output-file png-temp-file t)
            (call-process "magick" nil nil nil "convert" png-temp-file image-file)
            )))))))


;;; Yapi related
(defun spike-leung/describe-yapi-api ()
  "获取选中的 [url]，携带 Cookie 请求 YAPI 接口并打开对应的链接."
  (interactive)
  (let* ((url (if (use-region-p)
                  (buffer-substring-no-properties (region-beginning) (region-end))
                (read-string "请输入 URL: "))) ; 如果没有选中 URL，手动输入
         (yapi-url (concat "https://yapi.gyenno.com/api/project/search?q=" url))
         (cookie (or (getenv "YAPI_COOKIE")
                     (let ((input (read-string "请输入 Cookie: "))) ; 提示用户输入 Cookie
                       (setenv "YAPI_COOKIE" input)
                       input)))       ; 从环境变量读取或手动输入并保存
         (url-request-extra-headers `(("Cookie" . ,cookie))) ; 设置请求头中的 Cookie
         (response (with-current-buffer (url-retrieve-synchronously yapi-url)
                     (goto-char (point-min))
                     (if (re-search-forward "^$" nil t)
                         (json-read-from-string (buffer-substring (point) (point-max)))
                       (error "请求失败: 响应格式无效")))))
    (if (and (eq (alist-get 'errcode response) 0)
             (> (length (alist-get 'interface (alist-get 'data response))) 0))
        (let* ((interface-list (alist-get 'interface (alist-get 'data response))) ; 获取接口列表
               (interface-titles (mapcar (lambda (i) (decode-coding-string (alist-get 'title i) 'utf-8)) interface-list)) ; 解码标题
               (selected-title (if (= (length interface-titles) 1)
                                   (car interface-titles) ; 如果只有一个接口，直接选择它
                                 (completing-read "请选择一个接口: " interface-titles))) ; 否则让用户选择
               (interface (cl-find-if (lambda (i) (string= (decode-coding-string (alist-get 'title i) 'utf-8) selected-title))
                                      interface-list)) ; 根据标题查找对应的接口
               (project-id (alist-get 'projectId interface)) ; 获取 projectId
               (interface-id (alist-get '_id interface)) ; 获取 _id
               (final-url (format "https://yapi.gyenno.com/project/%s/interface/api/%s"
                                  (if (stringp project-id) project-id (number-to-string project-id)) ; 确保 projectId 是字符串
                                  (if (stringp interface-id) interface-id (number-to-string interface-id))))) ; 确保 _id 是字符串
          (browse-url final-url))       ; 打开拼接后的链接
      (message "未找到对应的接口信息"))))


;;; outline-mode
(setq outline-minor-mode-cycle t)
(setq text-mode-ispell-word-completion nil)

(provide 'init-my-misc)
;;; init-my-misc.el ends here
