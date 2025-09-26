;;; init-org-publish.el --- org publish config for my blog -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'rx)
(require 'cl-lib)
(require 'denote)

;; <link rel=\"preload\" href=\"/fonts/Atkinson-Hyperlegible/Atkinson-Hyperlegible-Regular-102a.woff2\" as=\"font\" type=\"font/woff2\" crossorigin>
;; <link rel=\"preload\" href=\"/fonts/Atkinson-Hyperlegible/Atkinson-Hyperlegible-Bold-102a.woff2\" as=\"font\" type=\"font/woff2\" crossorigin>
;; <link rel=\"preload\" href=\"/fonts/Atkinson-Hyperlegible/Atkinson-Hyperlegible-Italic-102a.woff2\" as=\"font\" type=\"font/woff2\" crossorigin>
;; <link rel=\"preload\" href=\"/fonts/Atkinson-Hyperlegible/Atkinson-Hyperlegible-BoldItalic-102a.woff2\" as=\"font\" type=\"font/woff2\" crossorigin>

(defconst spike-leung/org-publish-draft-publishing-directory
  "~/git/taxodium/publish/draft"
  "`:publishing-directory' for draft.")

(defconst spike-leung/org-publish-default-publishing-directory
  "~/git/taxodium/publish"
  "`:publishing-directory' for draft.")

(defconst spike-leung/html-head "
<meta name=\"color-scheme\" content=\"light dark\" />
<script src=\"/js/color-scheme.js\"></script>
<link rel=\"preload\" href=\"/images/background/xv.png\" as=\"image\" type=\"image/png\" />
<link rel=\"stylesheet\" href=\"/fonts/LXGWWenKai/LXGWWenKai-Regular/result.css\" />
<link rel=\"stylesheet\" href=\"/fonts/LXGWWenKai/LXGWWenKai-Medium/result.css\" />
<link rel=\"stylesheet\" href=\"/styles/style.css\" type=\"text/css\"/>
<link rel=\"icon\" href=\"/favicon.ico\" type=\"image/x-icon\">
<link rel=\"webmention\" href=\"https://webmention.io/taxodium.ink/webmention\" />
"
"`:html-head' for `org-publish'.")

(defconst spike-leung/html-head-sitemap (concat
                                         spike-leung/html-head
                                         "<link rel=\"stylesheet\" href=\"/styles/sitemap.css\" type=\"text/css\"/>"
                                         "<link href=\"https://github.com/Spike-Leung\" rel=\"me\">")
  "`:html-head' for `org-publish'.Customize for index.org.")

(defconst spike-leung/html-preamble
  "
<nav>
  <ul>
    <li><a href=\"/index.html\">主页</a></li>
    <li><a href=\"/inside-black-hole.html\">黑洞里</a></li>
    <li><a href=\"/rss.xml\">订阅</a></li>
    <li class=\"js-required\"><a href=\"/search.html\">搜索</a></li>
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
<details class=\"webmention-container js-required\">
<summary>Webmentions <span class=\"webmention-count\">(加载中...)</span></summary>
<p class=\"webmention-tip\"><a href=\"https://webmention.io/taxodium.ink/webmention\">Webmention</a> 由 <a href=\"https://webmention.io\">webmention.io</a> 提供服务。</p>
<form action=\"https://webmention.io//taxodium.ink/webmention\" method=\"post\">
<label for=\"source\">Source URL:</label>
<input type=\"url\" name=\"source\" id=\"source\" placeholder=\"发送 Webmention 的页面 URL（一般来说是你的页面 URL）\"/>
<label for=\"target\">Target URL:</label>
<input type=\"url\" name=\"target\" id=\"target\" readonly />
<input type=\"submit\" class=\"button\" value=\"发送 Webmention\"/>
</form>
<hr></hr>
<ul class=\"webmention-content-list\"></ul>
</details>
<p class=\"author h-entry p-author h-card p-name\">作 者： <a href=\"mailto:l-yanlei@hotmail.com\">%a</a></p>
<p class=\"date h-entry dt-published\">创建于： %d</p>
<p class=\"date h-entry dt-updated\">修改于： %C</p>
<p class=\"license\">许可证： <a href=\"https://creativecommons.org/licenses/by-nc-sa/4.0/deed.zh-hans\">署名—非商业性使用—相同方式共享 4.0</a></p>
<p class=\"support-me\">支持我： <a href=\"https://taxodium.ink/support-me.html\">用你喜欢的方式</a></p>
<script src=\"/js/sidenote.js\" defer></script>
<script src=\"/js/code-enhanced.js\" defer></script>
<script src=\"/js/backtop.js\" defer></script>
<script src=\"/js/webmention.js\" defer></script>
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

(defun spike-leung/org-html-wrap-image-with-link (orig-fn source attributes info)
  "Wrap the <img> tag in an <a> tag linking to the image source."
  (let ((href (or (plist-get attributes :data-href)
                  (plist-get attributes :href)))
        (img-tag (funcall orig-fn source attributes info)))
    (if (string-match-p (concat "^" org-preview-latex-image-directory) source)
        img-tag
      (format "<a href=\"%s\">%s</a>"
              (or href source)
              img-tag))))

(advice-add 'org-html--format-image :around #'spike-leung/org-html-wrap-image-with-link)

(defadvice org-html-paragraph (before org-html-paragraph-advice (paragraph contents info) activate)
  "Join consecutive Chinese lines into a single long line without unwanted space when exporting `org-mode' to html."
  (let* ((origin-contents (ad-get-arg 1))
         (fixed-contents
          (replace-regexp-in-string
           (rx
            (group (or (category chinese) "<" ">"))
            (regexp "\n")
            (group (or (category chinese) "<" ">")))
           "\\1\\2"
           origin-contents)))
    (ad-set-arg 1 fixed-contents)))

(defun spike-leung/remove-unnessary-id-from-html (text backend info)
  "Remove unnecessarily id attibute.
These elements's ID will be remove: figure,details,pre ..."
  (when (org-export-derived-backend-p backend 'html)
    (replace-regexp-in-string (rx (seq "<"
                                       (group (or "figure" "details" "pre"))
                                       (group (zero-or-more (not ">")))
                                       (group (seq whitespace "id=" (syntax string-quote) "org" (zero-or-more hex) (syntax string-quote)))
                                       (group (zero-or-more (not ">")))
                                       ">"))
                              (lambda (match)
                                (format "<%s%s%s%s>"
                                        (match-string 1 match) ;; tag
                                        (match-string 2 match) ;; keep other attrs
                                        "" ;; remove id
                                        (match-string 4 match) ;; keep other attrs
                                        ))
                              text)))
(with-eval-after-load 'ox
  (add-to-list 'org-export-filter-final-output-functions
               'spike-leung/remove-unnessary-id-from-html))

(defun spike-leung/org-publish-get-org-keyword (entry project keyword &optional filename)
  "Get the value of KEYWORD from Org file using `rx` for the regexp.
This is a fast version that avoids creating a full Org mode buffer.
KEYWORD is case-insensitive."
  (let ((file (or filename (org-publish--expand-file-name entry project))))
    (when (and (file-readable-p file) (not (directory-name-p file)))
      (with-temp-buffer
        (insert-file-contents file)
        (goto-char (point-min))
        (let ((case-fold-search t))
          (when (re-search-forward
                 (rx line-start
                     "#+"
                     (literal keyword)
                     (seq ":")
                     (zero-or-more blank)
                     (group (zero-or-more any)))
                 nil t)
            (s-trim (match-string 1))))))))

(defun spike-leung/sitemap-format-entry (entry style project)
  "Custom format for site map ENTRY, as a string.
ENTRY is a file name.  STYLE is the style of the sitemap.
PROJECT is the current project."
  (let* ((export-file-name (spike-leung/org-publish-get-org-keyword entry project "export_file_name"))
         (subtitle (spike-leung/org-publish-get-org-keyword entry project "subtitle")))
    (cond ((not (directory-name-p entry))
           (concat (format "[[file:%s][%s]]"
                           (or
                            (if export-file-name
                                (format "%s.org" export-file-name)
                              nil)
                            entry)
                           (org-publish-find-title entry project))
                   "\n"
                   (or
                    (if subtitle
                        (format "@@html: <span class=\"sitemap-subtitle\">%s</span>@@" subtitle)
                      nil)
                    "")))
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
   "#+DESCRIPTION: Spike Leung's blog."
   "\n\n"
   "That the powerful play goes on, and you may contribute a verse."
   "\n\n"
   (org-list-to-org list '(:backend org :raw t))))

(defun spike-leung/org-html-wrap-table (table backend info)
  "Wrap tables in a div when exporting to HTML."
  (when (org-export-derived-backend-p backend 'html)
    (concat "<div class=\"table-wrapper\"> " table " </div>")))

(with-eval-after-load 'ox
  (add-to-list 'org-export-filter-table-functions 'spike-leung/org-html-wrap-table))

(defun spike-leung/org-html-publish-to-html-orgfiles (plist filename pub-dir)
  "Publish FILENAME to HTML with view-transition-name added to title.
PLIST is the property list for the given project.
FILENAME is the org file being published.
PUB-DIR is the publishing directory."
  (let* ((html-file (spike-leung/org-html-publish-common plist filename pub-dir))
         (transition-name (spike-leung/get-view-transition-name (or (spike-leung/org-publish-get-org-keyword nil nil "export_file_name" filename) filename))))
    (when (file-exists-p html-file)
      (with-temp-buffer
        (insert-file-contents html-file)
        (goto-char (point-min))
        ;; Add view-transition-name to <h1> title tags for smooth page transitions
        (while (re-search-forward "<h1[^>]*>\\([^<]+\\)</h1>" nil t)
          (let ((title (match-string 1)))
            (replace-match (format "<h1 style=\"view-transition-name: spike-%s\" class=\"title\">%s</h1>"
                                   transition-name
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

(defun spike-leung/org-html-publish-common (plist filename pub-dir)
  "Common HTML publishing logic for org files and sitemap.
PLIST is the property list for the given project.
FILENAME is the org file being published.
PUB-DIR is the publishing directory.
Returns the path to the generated HTML file."
  (let ((org-html-link-org-files-as-html t)
        ;; file will be exported with export_file_name
        (export-file-name (or (spike-leung/org-publish-get-org-keyword nil nil "export_file_name" filename) filename)))
    (org-html-publish-to-html plist filename pub-dir)
    (concat pub-dir (file-name-nondirectory (file-name-sans-extension export-file-name)) ".html")))

(defun spike-leung/get-view-transition-name (filename)
  "Generate consistent view-transition-name based on FILENAME.
Strips directory and .org extension, and converts to valid CSS identifier."
  (let ((basename (file-name-base filename)))
    (replace-regexp-in-string "[^a-zA-Z0-9-]" "-" basename)))

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

(defun spike-lenug/org-publish-clean-draft (dir)
  "Clean draft files before org publish. DIR is the draft publish dir."
  (dolist (file (directory-files dir t))
    (when (and (file-regular-p file)
               (not (member (file-name-nondirectory file) '("." ".."))))
      (delete-file file))))

(defun spike-leung/insert-blog-images ()
  "Insert images from blog."
  (interactive)
  (let* ((image-dir (expand-file-name "~/git/taxodium/publish/images/"))
         (image-file (read-file-name "Select image: " image-dir))
         (relative-path (file-relative-name image-file image-dir)))
    (insert (format "#+CAPTION: \n[[file:images/%s]]" relative-path))))

(setq org-html-content-class "content h-entry")

(defun spike-leung/setup-org-publish-project-alist (&rest _args)
  "Setup `org-publish-project-alist'."
  (message "setup org-publish-project-alist")
  (setq org-publish-project-alist
        `(("orgfiles"
           :base-directory "~/git/taxodium/posts"
           :base-extension "org"
           :exclude ".*"
           :include  ,(spike-leung/get-file-list-from-denote-silo "~/git/taxodium/posts" "_published")
           :publishing-directory ,spike-leung/org-publish-default-publishing-directory
           ;; :publishing-function spike-leung/org-html-publish-to-html-orgfiles
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
           :sitemap-style list
           :author "Spike Leung"
           :email "l-yanlei@hotmail.com")

          ("draft"
           :base-directory "~/git/taxodium/posts"
           :base-extension "org"
           :exclude ".*"
           :include  ,(spike-leung/get-file-list-from-denote-silo "~/git/taxodium/posts" "_draft")
           :preparation-function ,(spike-lenug/org-publish-clean-draft spike-leung/org-publish-draft-publishing-directory)
           :publishing-directory ,spike-leung/org-publish-draft-publishing-directory
           ;; :publishing-function spike-leung/org-html-publish-to-html-orgfiles
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
           :publishing-directory ,spike-leung/org-publish-default-publishing-directory
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
           :publishing-directory ,spike-leung/org-publish-default-publishing-directory
           ;; :publishing-function spike-leung/org-html-publish-to-html-sitemap
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
           :publishing-directory ,spike-leung/org-publish-default-publishing-directory
           :publishing-function org-publish-attachment)

          ;; copy static fisrt
          ("website" :components ("pages" "orgfiles" "black-hole" "draft" "sitemap")))))

(spike-leung/setup-org-publish-project-alist)
(advice-add 'org-publish :before #'spike-leung/setup-org-publish-project-alist)

(defun spike-leung/org-add-custom-id-to-headings-in-blog-files ()
  "Add a CUSTOM_ID property to all headings in the current buffer, if it does not already exist."
  (interactive)
  (org-map-entries
   (lambda ()
     (unless (org-entry-get nil "CUSTOM_ID")
       (let ((custom-id (org-id-new)))
         (org-set-property "CUSTOM_ID" custom-id))))))

(add-hook 'org-mode-hook
          (lambda ()
            (when (and buffer-file-name
                       (string-match "taxodium" buffer-file-name))
              (add-hook 'before-save-hook 'spike-leung/org-add-custom-id-to-headings-in-blog-files nil 'local))))

(provide 'init-org-publish)
;;; init-org-publish.el ends here
