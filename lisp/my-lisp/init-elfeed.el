;;; Package --- Summary
;; init-elfeed

;;; Commentary:
;;; Code:
(when (maybe-require-package 'elfeed)
  (with-eval-after-load 'elfeed
    (setq elfeed-feeds '(
                         "https://www.ruanyifeng.com/blog/atom.xml"
                         "https://cprss.s3.amazonaws.com/javascriptweekly.com.xml"
                         "https://weekly.howie6879.cn/rss/rss.xml"))))

(provide 'init-elfeed)
;;; init-elfeed.el ends here
