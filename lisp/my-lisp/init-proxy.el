;;; init-proxy.el --- set proxy -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(custom-set-variables
 '(url-proxy-services '(("http" . "localhost:7890") ("https" . "localhost:7890"))))
(provide 'init-proxy)
;;; init-proxy.el ends here
