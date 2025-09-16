;;; init-gptel-prompts.el --- Custom rewrite prompts for gptel -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defcustom spike-leung/custom-rewrite-prompts
  '(("Translate" . "Translate the following text to English:")
    ("Translate to Chinese" . "翻译成中文，仅返回翻译后的内容。需要翻译的文本: ")
    ("Format quotes" .
     "按照以下要求，格式化内容:
- 如果存在英文和中文翻译，移除英文
- 段落之间需要有一行空行，且最多一行空行，如果存在多行空行，请移除
- 如果存在中英文混合，中文和英文/数字之间需要保留一个空格
- 如果涉及到人名、书名、电影名等，使用英文原文，如果是书名、电影名，需要用 // 标记为斜体，例如 /book/
- 如果涉及到缩写，需要在中文附近补充英文缩写和完整的英文，如最低合格读者 (MQR, Minimum Qualified Reader)
- 如果文本中包含破折号，例如 ——、-，需要将他们替换为 ⸺ ，注意，⸺  的前后需要保留一个空格
- 文本中的引号替换为直角引号 「」、『』，如果引号里面还有引号，则采用 「『』」的嵌套形式

现在请处理以下内容：
"))
  "Alist of translation prompt options for `spike-leung/gptel-rewrite'.
Each entry is (DISPLAY . PROMPT).The first entry is the default."
  :type '(alist :key-type string :value-type string)
  :group 'spike-leung)

(provide 'init-gptel-prompts)
;;; init-gptel-prompts.el ends here
