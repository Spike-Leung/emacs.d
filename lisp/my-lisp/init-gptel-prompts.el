;;; init-gptel-prompts.el --- Custom rewrite prompts for gptel -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defcustom spike-leung/custom-rewrite-prompts
  '(("Translate" . "Translate the following text to English:")
    ("Translate to Chinese" . "翻译成中文。对于翻译后的内容，中文和英文/数字之间要保留一个空格。需要翻译的文本: ")
    ("Format quotes" .
     "按照以下要求，格式化内容:
- 如果存在英文和中文翻译，移除英文
- 每行一个句子，在每个中文句号(。)换行
- 行与行之间需要有一行空行，且最多一行空行，如果存在多行空行，请移除
- 如果存在中英文混合，中文和英文/数字之间需要保留一个空格
- 如果涉及到人名，使用英文的名字
- 如果涉及到缩写，需要在中文附近补充英文缩写和完整的英文，如最低合格读者 (MQR, Minimum Qualified Reader)

例子1：

输入：

The fall of critical thinking in OSINT won’t come with a bang. It’ll come quietly. It’ll look like faster reports. Cleaner narratives. Fewer questions asked. It’ll feel efficient. It’ll look like progress.
OSINT 中批判性思维的衰落不会轰然发生。它会悄然来临。它看起来像是更快的报告。更清晰的叙述。更少的质疑。它会让人感觉高效。看起来像是进步。

输出：
OSINT 中批判性思维的衰落不会轰然发生。

它会悄然来临。

它看起来像是更快的报告。更清晰的叙述。更少的质疑。

它会让人感觉高效。

看起来像是进步。


例子 2:

输入：

This is how it starts. It starts with trusting summaries. With accepting citations you didn’t check. With replacing your judgment with something that sounds like judgment. The collapse won’t be obvious. It will feel convenient. That’s what makes it so dangerous. But here’s the part that matters: it’s reversible.
事情就是这样开始的。它始于信任摘要，始于接受你未核实的引用，始于用听起来像判断的东西取代你的判断。崩溃不会显而易见，它会让人感觉方便。这正是它如此危险的原因。但关键是：这是可以逆转的。

输出：

事情就是这样开始的。

它始于信任摘要，始于接受你未核实的引用，始于用听起来像判断的东西取代你的判断。

崩溃不会显而易见，它会让人感觉方便。

这正是它如此危险的原因。

但关键是：这是可以逆转的。

例子 3:

输入：

You don’t need to ditch GenAI. You need to confront it. Challenge it. Break it. Question it. Use it, but never trust it without a fight. You’re not just a user of tools. You’re an investigator.
你不需要抛弃生成式人工智能。你需要面对它，挑战它，打破它，质疑它。使用它，但绝不要毫无抵抗地信任它。你不仅仅是工具的使用者，你是调查员。

输出：

你不需要抛弃生成式人工智能。

你需要面对它，挑战它，打破它，质疑它。

使用它，但绝不要毫无抵抗地信任它。

你不仅仅是工具的使用者，你是调查员。

现在请处理以下内容：
"))
  "Alist of translation prompt options for `spike-leung/gptel-rewrite'.
Each entry is (DISPLAY . PROMPT).The first entry is the default."
  :type '(alist :key-type string :value-type string)
  :group 'spike-leung)

(provide 'init-gptel-prompts)
;;; init-gptel-prompts.el ends here
