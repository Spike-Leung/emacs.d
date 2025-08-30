;;; init-org-publish.el --- org publish config for my blog -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(maybe-require-package 'olivetti)
(require 'rx)
(require 'cl-lib)
(require 'denote)

;; <link rel=\"preload\" href=\"/fonts/Atkinson-Hyperlegible/Atkinson-Hyperlegible-Regular-102a.woff2\" as=\"font\" type=\"font/woff2\" crossorigin>
;; <link rel=\"preload\" href=\"/fonts/Atkinson-Hyperlegible/Atkinson-Hyperlegible-Bold-102a.woff2\" as=\"font\" type=\"font/woff2\" crossorigin>
;; <link rel=\"preload\" href=\"/fonts/Atkinson-Hyperlegible/Atkinson-Hyperlegible-Italic-102a.woff2\" as=\"font\" type=\"font/woff2\" crossorigin>
;; <link rel=\"preload\" href=\"/fonts/Atkinson-Hyperlegible/Atkinson-Hyperlegible-BoldItalic-102a.woff2\" as=\"font\" type=\"font/woff2\" crossorigin>

(defconst spike-leung/html-head "
<meta name=\"color-scheme\" content=\"light dark\" />
<script src=\"/js/color-scheme.js\"></script>
<link rel=\"preload\" href=\"/images/background/xv.png\" as=\"image\" type=\"image/png\" />
<link rel=\"stylesheet\" href=\"/fonts/LXGWWenKai/LXGWWenKai-Regular/result.css\" />
<link rel=\"stylesheet\" href=\"/fonts/LXGWWenKai/LXGWWenKai-Medium/result.css\" />
<link rel=\"stylesheet\" href=\"/styles/style.css\" type=\"text/css\"/>
<link rel=\"icon\" href=\"/favicon.ico\" type=\"image/x-icon\">
"
  "`:html-head' for `org-publish'.")

(defconst spike-leung/html-head-sitemap (concat
                                         spike-leung/html-head
                                         "<link rel=\"stylesheet\" href=\"/styles/sitemap.css\" type=\"text/css\"/>")
  "`:html-head' for `org-publish'.Customize for index.org.")

(defconst spike-leung/html-preamble
  "
<nav>
  <ul>
    <li><a href=\"/index.html\">主页</a></li>
    <li><a href=\"/inside-black-hole.html\">黑洞里</a></li>
    <li><a href=\"/rss.xml\">订阅</a></li>
    <li><a href=\"/search.html\">搜索</a></li>
  </ul>
  <select onchange=\"switchMode(this.value)\" id=\"lightdark\" class=\"js-required\">
    <option value=\"auto\">Auto</option>
    <option value=\"light\">Light</option>
    <option value=\"dark\">Dark</option>
  </select>
</nav>
"
  "`:html-preamble' for `org-publish'.")

(defconst spike-leung/html-preamble-content (concat "
  <ul class=\"ally-nav\">
    <li>
      <a id=\"skip-content\" href=\"#content\">Skip to main content</a>
    </li>
  </ul>
" spike-leung/html-preamble)
  "`:html-preamble' for `org-publish'.Customize for content." )

(defconst spike-leung/html-postamble "
<p class=\"author\">作 者： <a href=\"mailto:l-yanlei@hotmail.com\">%a</a></p>
<p class=\"date\">创建于： %d</p>
<p class=\"date\">修改于： %C</p>
<p class=\"license\">许可证： <a href=\"https://www.creativecommons.org/licenses/by-nc/4.0/deed.zh-hans\">CC BY-NC 4.0</a></p>
<p class=\"support-me\">支持我： <a href=\"https://taxodium.ink/support-me.html\">用你喜欢的方式</a></p>
<script src=\"/js/sidenote.js\" defer></script>
<script src=\"/js/code-enhanced.js\" defer></script>
<script src=\"/js/image-enhanced.js\" defer></script>
<script src=\"/js/backtop.js\" defer></script>
<noscript>
  <style>
    .js-required {
       display: none;
     }
  </style>
</noscript>
"
  "`:html-postamble' for `org-publish'.")

(defconst spike-leung/html-postamble-sitemap "
<script src=\"/js/backtop.js\" defer></script>
<noscript>
  <style>
    .js-required {
       display: none;
     }
  </style>
</noscript>
"
  "sitemap `:html-postamble' for `org-publish'.")

(defun spike-leung/apply-theme-when-publish (&rest args)
  "Switch theme when do `org-publish'.
ARGS will pass to `org-publish'."
  (let ((current-theme (car custom-enabled-themes)))
    (load-theme 'modus-vivendi t)
    (apply args)
    (when current-theme
      (disable-theme 'modus-vivendi)
      (enable-theme current-theme)
      (load-theme current-theme :no-confirm))))

(advice-add 'org-publish :around #'spike-leung/apply-theme-when-publish)


(defun spike-leung/org-publish-get-org-keyword (entry project keyword)
  "Get the value of the given KEYWORD in the current Org file.
KEYWORD is case-insensitive."
  (let ((file (org-publish--expand-file-name entry project)))
    (when (and (file-readable-p file) (not (directory-name-p file)))
      (org-with-file-buffer file
        (let* ((normalized-keyword (s-upcase keyword))
               (keywords (org-collect-keywords (list normalized-keyword))))
          (car (cdr (assoc normalized-keyword keywords))))))))

(defun spike-leung/sitemap-format-entry (entry style project)
  "Custom format for site map ENTRY, as a string.
ENTRY is a file name.  STYLE is the style of the sitemap.
PROJECT is the current project."
  (let* ((export-file-name (spike-leung/org-publish-get-org-keyword entry project "export_file_name"))
         (date (spike-leung/org-publish-get-org-keyword entry project "date")))
    (cond ((not (directory-name-p entry))
           (format "[[file:%s][%s]] %s"
                   (or
                    (if export-file-name
                        (format "%s.org" export-file-name)
                      nil)
                    entry)
                   (org-publish-find-title entry project) date))
          ((eq style 'tree)
           ;; Return only last subdir.
           (file-name-nondirectory (directory-file-name entry)))
          (t entry))))

(defun spike-leung/sitemap-function (title list)
  "Generate sitemap as a string.
TITLE is the sitemap title and LIST contains files to include."
  (concat
   "#+TITLE: " title
   "\n"
   "#+DESCRIPTION: SpikeLeung's blog."
   "\n\n"
   "That the powerful play goes on, and you may contribute a verse."
   "\n\n"
   (org-list-to-org list)))

(defun spike-leung/org-html-wrap-table (table backend info)
  "Wrap tables in a div when exporting to HTML."
  (when (org-export-derived-backend-p backend 'html)
    (concat "<div class=\"table-wrapper\"> " table " </div>")))

(with-eval-after-load 'ox
  (add-to-list 'org-export-filter-table-functions 'spike-leung/org-html-wrap-table))


(defun spike-leung/org-html-publish-common (plist filename pub-dir)
  "Common HTML publishing logic for org files and sitemap.
PLIST is the property list for the given project.
FILENAME is the org file being published.
PUB-DIR is the publishing directory.
Returns the path to the generated HTML file."
  (let ((org-html-link-org-files-as-html t))
    ;; First publish to HTML using org-mode's standard publishing function
    (org-html-publish-to-html plist filename pub-dir)
    ;; Return the path to the generated HTML file
    (concat pub-dir "/" (file-name-nondirectory (file-name-sans-extension filename)) ".html")))

(defun spike-leung/get-view-transition-name (filename)
  "Generate consistent view-transition-name based on FILENAME.
Strips directory and .org extension, and converts to valid CSS identifier."
  (let ((basename (file-name-base filename)))
    (replace-regexp-in-string "[^a-zA-Z0-9-]" "-" basename)))

(defun spike-leung/org-html-publish-to-html-orgfiles (plist filename pub-dir)
  "Publish FILENAME to HTML with view-transition-name added to title.
PLIST is the property list for the given project.
FILENAME is the org file being published.
PUB-DIR is the publishing directory."
  (let ((html-file (spike-leung/org-html-publish-common plist filename pub-dir)))
    (when (file-exists-p html-file)
      (with-temp-buffer
        (insert-file-contents html-file)
        (goto-char (point-min))
        ;; Add view-transition-name to <h1> title tags for smooth page transitions
        (while (re-search-forward "<h1[^>]*>\\([^<]+\\)</h1>" nil t)
          (let ((title (match-string 1)))
            (replace-match (format "<h1 style=\"view-transition-name: spike-%s\" class=\"title\">%s</h1>"
                                   (spike-leung/get-view-transition-name filename)
                                   title))))
        (write-region (point-min) (point-max) html-file)))))

(defun spike-leung/org-html-publish-to-html-sitemap (plist filename pub-dir)
  "Publish FILENAME to HTML with view-transition-name added to links.
PLIST is the property list for the given project.
FILENAME is the org file being published (typically index.org).
PUB-DIR is the publishing directory."
  (let ((html-file (spike-leung/org-html-publish-common plist filename pub-dir)))
    (when (file-exists-p html-file)
      (with-temp-buffer
        (insert-file-contents html-file)
        (goto-char (point-min))
        ;; Add view-transition-name to <a> tags linking to content pages
        ;; This enables smooth transitions when navigating from sitemap to content
        (while (re-search-forward "<a href=\"\\([^\"]+\\)\"" nil t)
          (let* ((href (match-string 1))
                 (transition-name (spike-leung/get-view-transition-name href)))
            (replace-match (format "<a href=\"%s\" style=\"view-transition-name: spike-%s\""
                                   href
                                   transition-name))))
        (write-region (point-min) (point-max) html-file)))))

;; thanks https://jiewawa.me/2024/03/blogging-with-denote-and-hugo/
(defun spike-leung/sluggify-denote-title-as-export-file-name ()
  "Add metadata to current `org-mode' file containing export file name.
Export File Name is returned by `denote-retrieve-title-value'."
  (interactive)
  (save-excursion
    (goto-char 0)
    (search-forward "title")
    (end-of-line)
    (insert (format
             "\n#+export_file_name: %s"
             (denote-sluggify-title
              (denote-retrieve-title-value buffer-file-name 'org))))))

(defun spike-leung/get-file-list-from-denote-silo (silos tag)
  "Return files in SILOS match TAG.
SILO is a file path from `denote-silo-directories'.
TAG is string."
  (cl-letf ((denote-directory (expand-file-name silos)))
    (denote-directory-files tag)))

;; see: https://tusharhero.codeberg.page/creating_a_blog.html
;; (add-hook 'org-export-before-processing-hook
;;           #'(lambda (backend)
;;               (insert "#+INCLUDE: \"./setup.org\"\n")))
;; (setq org-confirm-babel-evaluate nil) ; Don't ask permission for evaluating source blocks

(defun spike-leung/setup-org-publish-project-alist (&rest _args)
  "Setup `org-publish-project-alist'."
  (message "setup org-publish-project-alist")
  (setq org-publish-project-alist
        `(("orgfiles"
           :base-directory "~/git/taxodium/posts"
           :base-extension "org"
           :exclude ".*"
           :include  ,(spike-leung/get-file-list-from-denote-silo "~/git/taxodium/posts" "_published")
           :publishing-directory "~/git/taxodium/publish"
           :publishing-function spike-leung/org-html-publish-to-html-orgfiles
           :section-numbers nil
           :with-toc t
           :with-tags t
           :time-stamp-file nil
           :html-head ,spike-leung/html-head
           :html-preamble ,spike-leung/html-preamble-content
           :html-postamble ,spike-leung/html-postamble
           :auto-sitemap t
           :sitemap-filename "index.org"
           :sitemap-title "Taxodium"
           :sitemap-format-entry spike-leung/sitemap-format-entry
           :sitemap-sort-files anti-chronologically
           :sitemap-function spike-leung/sitemap-function
           :author "Spike Leung"
           :email "l-yanlei@hotmail.com")

          ("draft"
           :base-directory "~/git/taxodium/posts"
           :base-extension "org"
           :exclude ".*"
           :include  ,(spike-leung/get-file-list-from-denote-silo "~/git/taxodium/posts" "_draft")
           :publishing-directory "~/git/taxodium/publish"
           :publishing-function spike-leung/org-html-publish-to-html-orgfiles
           :section-numbers nil
           :with-toc t
           :with-tags t
           :time-stamp-file nil
           :auto-sitemap nil
           :html-head ,spike-leung/html-head
           :html-postamble ,spike-leung/html-postamble
           :html-preamble ,spike-leung/html-preamble-content
           :author "Spike Leung"
           :email "l-yanlei@hotmail.com")

          ("black-hole"
           :base-directory "~/git/taxodium/posts"
           :base-extension "org"
           :exclude ".*"
           :include  ,(spike-leung/get-file-list-from-denote-silo "~/git/taxodium/posts" "_blackhole")
           :publishing-directory "~/git/taxodium/publish"
           :publishing-function spike-leung/org-html-publish-to-html-orgfiles
           :section-numbers nil
           :with-toc t
           :with-tags t
           :time-stamp-file nil
           :auto-sitemap nil
           :html-head ,spike-leung/html-head
           :html-preamble ,spike-leung/html-preamble-content
           :html-postamble ,spike-leung/html-postamble
           :author "Spike Leung"
           :email "l-yanlei@hotmail.com")

          ("sitemap"
           :base-directory "~/git/taxodium/posts"
           :base-extension "org"
           :include ("index.org")
           :exclude ".*"
           :publishing-directory "~/git/taxodium/publish"
           :publishing-function spike-leung/org-html-publish-to-html-sitemap
           :time-stamp-file nil
           :html-head ,spike-leung/html-head-sitemap
           :html-preamble ,spike-leung/html-preamble
           :html-postamble ,spike-leung/html-postamble-sitemap
           :author "Spike Leung"
           :email "l-yanlei@hotmail.com")

          ("pages"
           :base-directory "~/git/taxodium/pages"
           :base-extension any
           :recursive t
           :publishing-directory "~/git/taxodium/publish"
           :publishing-function org-publish-attachment)

          ;; copy static fisrt
          ("website" :components ("pages" "orgfiles" "black-hole" "draft" "sitemap")))))

(spike-leung/setup-org-publish-project-alist)
(advice-add 'org-publish :before #'spike-leung/setup-org-publish-project-alist)

(provide 'init-org-publish)
;;; init-org-publish.el ends here
