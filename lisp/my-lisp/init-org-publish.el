;;; init-org-publish.el --- org publish config for my blog -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(maybe-require-package 'ox-rss)

(defconst spike-leung/html-head "
<link rel=\"stylesheet\" href=\"https://chinese-fonts-cdn.deno.dev/packages/lxgwwenkai/dist/LXGWWenKai-Bold/result.css\" />
<link rel=\"stylesheet\" href=\"https://chinese-fonts-cdn.deno.dev/packages/lxgwwenkai/dist/LXGWWenKai-Regular/result.css\" />
<link rel=\"stylesheet\" href=\"/styles/style.css\" type=\"text/css\"/>
<link rel=\"icon\" href=\"/favicon.ico\" type=\"image/x-icon\">
"
  "`:html-head' for `org-publish'.")

(defconst spike-leung/html-head-sitemap (concat
                                         spike-leung/html-head
                                         "<link rel=\"stylesheet\" href=\"/styles/sitemap.css\" type=\"text/css\"/>")
  "`:html-head' for `org-publish'.Customize for index.org.")

(defconst spike-leung/html-preamble "
<nav>
  <ul>
    <li><a href=\"/index.html\">Home</a></li>
    <li><a href=\"/about.html\">About</a></li>
    <li><a href=\"/rss.xml\">RSS</a></li>
    <li><a href=\"https://music.163.com/#/playlist?id=12531191848\">Playlist</a></li>
    <li><a href=\"/_blank.html\">_blank</a></li>
    <li><a href=\"/search.html\">Search</a></li>
  </ul>
</nav>
"
  "`:html-preamble' for `org-publish'." )

(defconst spike-leung/html-preamble-content (concat "
  <ul class=\"ally-nav\">
    <li>
      <a id=\"skip-content\" href=\"#content\">Skip to main content</a>
    </li>
    <li>
      <a id=\"skip-postamble\" href=\"#postamble\">Skip to comments</a>
    </li>
  </ul>
" spike-leung/html-preamble)
  "`:html-preamble' for `org-publish'.Customize for content." )

(defconst spike-lenng/html-postamble "
<p class=\"author\">Author: <a href=\"mailto:l-yanlei@hotmail.com\">%a</a></p>
<p class=\"date\">Date: %d</p>
<p class=\"license\">License: <a href=\"https://www.creativecommons.org/licenses/by-nc/4.0/deed.zh-hans\">CC BY-NC 4.0</a></p>
<script src=\"https://giscus.app/client.js\"
        data-repo=\"Spike-Leung/taxodium\"
        data-repo-id=\"MDEwOlJlcG9zaXRvcnkzOTYyNDQwMzk=\"
        data-category=\"Announcements\"
        data-category-id=\"DIC_kwDOF540R84Ci61D\"
        data-mapping=\"pathname\"
        data-strict=\"0\"
        data-reactions-enabled=\"1\"
        data-emit-metadata=\"0\"
        data-input-position=\"top\"
        data-theme=\"light_high_contrast\"
        data-lang=\"en\"
        data-loading=\"lazy\"
        crossorigin=\"anonymous\"
        async>
</script>
"
  "`:html-postamble' for `org-publish'.")

(defconst spike-leung/follow-claim-description
  "feedId:63132271001948160+userId:72185894417953792"
  "Follow claim description.")

(defun spike-leung/add-custom-id-to-all-headings ()
  "Add a CUSTOM_ID property to all headings in the current buffer."
  (interactive)
  (org-map-entries
   (lambda ()
     (let ((custom-id (org-id-new)))
       (org-set-property "CUSTOM_ID" custom-id)))))

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

(defun spike-leung/sitemap-function (title list)
  "Generate sitemap as a string.
TITLE is the sitemap title and LIST contains files to include."
  (concat
   "#+TITLE: " title
   "\n"
   "#+DESCRIPTION: That the powerful play goes on, and you may contribute a verse."
   "\n\n"
   (org-list-to-org list)))

;; @see: https://writepermission.com/org-blogging-rss-feed.html
(defun rw/org-rss-publish-to-rss (plist filename pub-dir)
  "Publish RSS with PLIST, only when FILENAME is 'rss.org'.
PUB-DIR is when the output will be placed."
  (if (equal "rss.org" (file-name-nondirectory filename))
      (org-rss-publish-to-rss plist filename pub-dir)))


(defun rw/format-rss-feed (title list)
  "Generate RSS feed as a string.
TITLE is the RSS feed title and LIST contains files to include."
  (concat
   "#+TITLE: " title
   "\n"
   "#+DESCRIPTION: " (format
                      "That the powerful play goes on, and you may contribute a verse. (%s)"
                      spike-leung/follow-claim-description)
   "\n"
   "#+RSS_IMAGE_URL:" " https://taxodium.ink/favicon.ico"
   "\n\n"
   (org-list-to-subtree list)))


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
      `(("orgfiles"
         :base-directory "~/git/taxodium/post"
         :base-extension "org"
         :publishing-directory "~/git/taxodium/publish"
         :publishing-function org-html-publish-to-html
         :section-numbers nil
         :with-toc t
         :with-tags t
         :time-stamp-file nil
         :html-head ,spike-leung/html-head
         :html-preamble ,spike-leung/html-preamble-content
         :html-postamble ,spike-lenng/html-postamble
         :exclude "rss.org"
         :auto-sitemap t
         :sitemap-filename "index.org"
         :sitemap-title "Taxodium"
         :sitemap-format-entry spike-leung/sitemap-format-entry
         :sitemap-sort-files anti-chronologically
         :sitemap-function spike-leung/sitemap-function
         :author "Spike Leung"
         :email "l-yanlei@hotmail.com")

        ("sitemap"
         :base-directory "~/git/taxodium/post"
         :base-extension "org"
         :publishing-directory "~/git/taxodium/publish"
         :publishing-function org-html-publish-to-html
         :time-stamp-file nil
         :html-head ,spike-leung/html-head-sitemap
         :html-preamble ,spike-leung/html-preamble
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

        ("website" :components ("orgfiles" "sitemap"))))

(provide 'init-org-publish)
;;; init-org-publish.el ends here
