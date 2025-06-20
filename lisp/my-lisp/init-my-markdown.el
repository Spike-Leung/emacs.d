;;; init-my-markdown.el --- use impatient-mode to preview markdown
;;; Commentary:
;;; @see:  https://wikemacs.org/wiki/Markdown#Live_preview_as_you_type
;;; Code:

(maybe-require-package 'impatient-mode)

(defun spike-leung/imp-markdown-filter (buffer)
  "Define imp markdown filter.
Wrap BUFFER with HTML, render with https://github.com/markedjs/marked and style with https://github.com/sindresorhus/github-markdown-css"
  (princ (with-current-buffer buffer
(format "<!DOCTYPE html>
<html>
  <title>Markdown Preview</title>
  <head>
    <link
      rel=\"stylesheet\"
      href=\"https://cdnjs.cloudflare.com/ajax/libs/github-markdown-css/5.8.1/github-markdown.min.css\"
      integrity=\"sha512-BrOPA520KmDMqieeM7XFe6a3u3Sb3F1JBaQnrIAmWg3EYrciJ+Qqe6ZcKCdfPv26rGcgTrJnZ/IdQEct8h3Zhw==\"
      crossorigin=\"anonymous\"
      referrerpolicy=\"no-referrer\"
    />
    <style>
          .markdown-body {
                  box-sizing: border-box;
                  min-width: 200px;
                  max-width: 980px;
                  margin: 0 auto;
                  padding: 45px;
          }

          @media (max-width: 767px) {
                  .markdown-body {
                          padding: 15px;
                  }
          }
    </style>
    <script src=\"https://cdn.jsdelivr.net/npm/marked/marked.min.js\"></script>
  </head>
  <body>
    <div id=\"markdown-body\" class=\"markdown-body\"></div>
    <script>
      document.getElementById('markdown-body').innerHTML = marked.parse(%s);
    </script>
  </body>
</html>"
                   (json-encode (buffer-substring-no-properties (point-min) (point-max)))))
         (current-buffer)))

(defun spike-leung/preview-markdown ()
  "Preview markdown."
  (interactive)
  (progn
    (httpd-start)
    (impatient-mode nil)
    (imp-set-user-filter 'spike-leung/imp-markdown-filter)
    (let ((port httpd-port))
      (browse-url (concat "http://localhost:" (number-to-string port) "/imp")))))

(defun spike-leung/disable-preview-markdown ()
  "Disable preview markdown."
  (interactive)
  (progn
    (httpd-stop)
    (impatient-mode -1)
    (imp-remove-user-filter)))

(provide 'init-my-markdown)
;;; init-my-markdown.el ends here
