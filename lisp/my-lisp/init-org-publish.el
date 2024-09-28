;;; init-org-publish.el --- org publish config for my blog -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(maybe-require-package 'ox-rss)

(defun spike-leung/apply-theme-when-publish (&rest args)
  "Switch theme when do `org-publish'.
ARGS will pass to `org-publish'."
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
  "Extract `#+date` form org file.
FILE is org file name.
PROJECT is the current project.
"
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


;; @see: https://writepermission.com/org-blogging-rss-feed.html
(defun rw/org-rss-publish-to-rss (plist filename pub-dir)
  "Publish RSS with PLIST, only when FILENAME is 'rss.org'.
PUB-DIR is when the output will be placed."
  (if (equal "rss.org" (file-name-nondirectory filename))
      (org-rss-publish-to-rss plist filename pub-dir)))


(defun rw/format-rss-feed (title list)
  "Generate RSS feed as a string.
TITLE is the RSS feed title and LIST contains files to include."
  (concat "#+TITLE: " title "\n\n" (org-list-to-subtree list)))


(defun rw/format-rss-feed-entry (entry style project)
  "Format ENTRY for the RSS feed.
ENTRY is a file name.  STYLE is either 'list' or 'tree'.
PROJECT is the current project."
  (cond ((not (directory-name-p entry))
         (let* ((title (org-publish-find-title entry project))
                (date (format-time-string "%Y-%m-%d" (org-publish-find-date entry project)))
                (link (concat (file-name-sans-extension entry) ".html")))
           (with-temp-buffer
             (insert (format "%s\n" title))
             (insert ":PROPERTIES:\n:RSS_PERMALINK: " link "\n:PUBDATE: " date "\n:END:\n")
             (insert (format "%s" title))
             (buffer-string))
           ))
        ((eq style 'tree)
         ;; Return only last subdir.
         (file-name-nondirectory (directory-file-name entry)))
        (t entry)))


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
         :time-stamp-file nil
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
    <li><a href=\"https://github.com/Spike-Leung/taxodium/tree/org-publish\">GitHub</a></li>
  </ul>
</nav>
"
         :html-postamble "
<p class=\"author\">Author: <a href=\"mailto:l-yanlei@hotmail.com\">%a</a></p>
<p class=\"date\">Date: %d</p>
<p class=\"license\">License: <a href=\"https://www.creativecommons.org/licenses/by-nc/4.0/deed.zh-hans\">CC BY-NC 4.0</a></p>
<script src=\"https://utteranc.es/client.js\" repo=\"Spike-Leung/taxodium\" issue-term=\"pathname\" theme=\"github-light\" crossorigin=\"anonymous\" async></script>
"
         :exclude "rss.org"
         :auto-sitemap t
         :sitemap-filename "index.org"
         :sitemap-title "Taxodium"
         :sitemap-format-entry spike-leung/sitemap-format-entry
         :sitemap-sort-files anti-chronologically
         :author "Spike Leung"
         :email "l-yanlei@hotmail.com")

        ("sitemap"
         :base-directory "~/git/taxodium/post"
         :base-extension "org"
         :publishing-directory "~/git/taxodium/publish"
         :publishing-function org-html-publish-to-html
         :time-stamp-file nil
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
    <li><a href=\"https://github.com/Spike-Leung/taxodium/tree/org-publish\">GitHub</a></li>
  </ul>
</nav>
"
         :include ("index.org")
         :exclude ".*"
         :html-postamble nil)

        ("rss"
         :base-directory "~/git/taxodium/post"
         :base-extension "org"
         :exclude "about\\|index"
         :publishing-directory "~/git/taxodium/publish"
         :publishing-function rw/org-rss-publish-to-rss
         :html-postamble nil
         :section-numbers nil
         :with-toc nil
         :rss-extension "xml"
         :html-link-home "https://taxodium.ink"
         :html-link-use-abs-url t
         :auto-sitemap t
         :sitemap-filename "rss.org"
         :sitemap-title "Taxodium"
         :sitemap-sort-files anti-chronologically
         :sitemap-function rw/format-rss-feed
         :sitemap-format-entry rw/format-rss-feed-entry
         :author "Spike Leung"
         :email "l-yanlei@hotmail.com")

        ("website" :components ("orgfiles" "sitemap" "rss"))))

(provide 'init-org-publish)
;;; init-org-publish.el ends here
