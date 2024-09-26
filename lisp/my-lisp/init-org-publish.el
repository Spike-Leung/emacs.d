;;; init-org-publish.el --- org publish config for my blog -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defun spike-leung/apply-theme-when-publish (&rest args)
  "Switch theme when do `org-publish'.ARGS will pass to `org-publish'."
  (let ((current-theme (car custom-enabled-themes)))
    (load-theme 'modus-operandi-tinted t)
    (apply args)
    (when current-theme
      (disable-theme 'modus-operandi-tinted)
      (enable-theme current-theme)
      (load-theme current-theme :no-confirm))))

(advice-add 'org-publish :around #'spike-leung/apply-theme-when-publish)


(defun spike-leung/get-org-keyword (keyword)
  "Get the value of the given KEYWORD in the current Org file."
  (let ((keywords (org-collect-keywords (list keyword))))
    (if-let ((value (car (cdr (assoc keyword keywords)))))
        value
      (format "No %s found" keyword))))


(defun spike-leung/org-publish-find-date (file project)
  "从 org 文件中提取 `#+date` 属性。"
  (with-temp-buffer
    (insert-file-contents file)
    (org-mode)
    (spike-leung/get-org-keyword "DATE")))

(defun spike-leung/sitemap-format-entry (entry style project)
  "自定义网站地图条目格式，添加日期信息。"
  (let* ((filename (org-publish--expand-file-name entry project))
         (date (spike-leung/org-publish-find-date filename project)))
    (format "%s %s"
            (org-publish-sitemap-default-entry entry style project)
            (if date
                date
              "long time ago..."))))


(setq org-publish-project-alist
      '(("orgfiles"
         :base-directory "~/git/taxodium/post"
         :base-extension "org"
         :publishing-directory "~/git/taxodium/publish"
         :publishing-function org-html-publish-to-html
         :section-numbers nil
         :with-toc t
         :with-tags t
         :with-broken-links marks
         ;; TODO: 封装到变量
         :html-head "
<link rel=\"stylesheet\" href=\"../styles/style.css\" type=\"text/css\"/>
<link rel=\"icon\" href=\"/favicon.ico\" type=\"image/x-icon\">
"
         :html-preamble "
 <nav>
  <ul>
    <li><a href=\"/index.html\">Home</a></li>
    <li><a href=\"/about.html\">About</a></li>
    <li><a href=\"/rss.xml\">RSS</a></li>
    <li><a href=\"https://github.com/Spike-Leung\">GitHub</a></li>
  </ul>
</nav>
"
         :html-postamble "
<p class=\"author\">Author: %a (%e)</p>
<p class=\"date\">Date: %d</p>
"
         :auto-sitemap t
         :sitemap-filename "index.org"
         :sitemap-title "Index"
         :sitemap-format-entry spike-leung/sitemap-format-entry
         :author "Spike Leung"
         :email "l-yanlei@hotmail.com")

        ("sitemap"
         :base-directory "~/git/taxodium/post"
         :base-extension "org"
         :publishing-directory "~/git/taxodium/publish"
         :publishing-function org-html-publish-to-html
         :html-head "
<link rel=\"stylesheet\" href=\"../styles/style.css\" type=\"text/css\"/>
<link rel=\"icon\" href=\"/favicon.ico\" type=\"image/x-icon\">
"
         :html-preamble "
 <nav>
  <ul>
    <li><a href=\"/index.html\">Home</a></li>
    <li><a href=\"/about.html\">About</a></li>
    <li><a href=\"/rss.xml\">RSS</a></li>
    <li><a href=\"https://github.com/Spike-Leung\">GitHub</a></li>
  </ul>
</nav>
"
         :include ("index.org")
         :exclude ".*"
         :html-postamble nil)

        ("public"
         :base-directory "~/git/taxodium/public/"
         :base-extension any
         :publishing-directory "~/git/taxodium/publish/"
         :recursive t
         :publishing-function org-publish-attachment)

        ("website" :components ("orgfiles" "public" "sitemap"))))

(provide 'init-org-publish)
;;; init-org-publish.el ends here