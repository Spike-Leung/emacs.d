;;; init-org-publish.el --- org publish config for my blog -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(setq org-publish-project-alist
      '(("orgfiles"
         :base-directory "~/git/taxodium/post"
         :base-extension "org"
         :publishing-directory "~/git/taxodium/publish"
         :publishing-function org-html-publish-to-html
         :section-numbers nil
         :with-toc t
         :with-broken-links marks
         :html-head "
<link rel=\"stylesheet\" href=\"../styles/style.css\" type=\"text/css\"/>
<link rel=\"icon\" href=\"/favicon.ico\" type=\"image/x-icon\">
"
         :html-postamble (concat org-html-postamble "
<nav>
  <ul>
    <li><a href=\"/index.html\">Index</a></li>
    <li><a href=\"/about.html\">About</a></li>
  </ul>
</nav>
")
         :html-postamble nil
         :makeindex t
         :auto-sitemap t
         )

        ("fonts"
         :base-directory "~/git/taxodium/fonts/"
         :base-extension any
         :publishing-directory "~/git/taxodium/publish/fonts"
         :recursive t
         :publishing-function org-publish-attachment)

        ("images"
         :base-directory "~/git/taxodium/images/"
         :base-extension any
         :publishing-directory "~/git/taxodium/publish/images"
         :recursive t
         :publishing-function org-publish-attachment)

        ("styles"
         :base-directory "~/git/taxodium/styles/"
         :base-extension "css"
         :publishing-directory "~/git/taxodium/publish/styles"
         :recursive t
         :publishing-function org-publish-attachment)

        ("public"
         :base-directory "~/git/taxodium/public/"
         :base-extension any
         :publishing-directory "~/git/taxodium/publish/"
         :recursive t
         :publishing-function org-publish-attachment)

        ("website" :components ("orgfiles" "fonts" "images" "styles" "public"))))

;;; init-org-publish.el ends here
