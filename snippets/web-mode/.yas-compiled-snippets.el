;;; Compiled snippets and support files for `web-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'web-mode
                     '(("tem" "<script setup lang=\"ts\">\n$0\n</script>\n\n<template>\n\n</template>" "template" nil nil nil "/Users/spike/.emacs.d/snippets/web-mode/vue-template" nil nil)
                       ("ut" "  const { t } = useTranslation()" "useTranslation" nil nil nil "/Users/spike/.emacs.d/snippets/web-mode/useTranslation" nil nil)
                       ("tt" "{t(\"editor$0\")}" "translate with brace" nil nil nil "/Users/spike/.emacs.d/snippets/web-mode/translate-brace" nil nil)
                       ("t" "t(\"editor$0\")" "translate" nil nil nil "/Users/spike/.emacs.d/snippets/web-mode/translate" nil nil)
                       ("ri" "import { useTranslation } from \"react-i18next\"" "import react-i18next" nil nil nil "/Users/spike/.emacs.d/snippets/web-mode/import-react-i18n" nil nil)
                       ("importd" "import { $0 } from \"@illa-design/$1\"" "import from @illa-design" nil nil nil "/Users/spike/.emacs.d/snippets/web-mode/import-illa-design" nil nil)
                       ("importa" "import { $0 } from \"@/page/Editor/components/ActionEditor/$1\"" "import from action editor" nil nil nil "/Users/spike/.emacs.d/snippets/web-mode/import-action-editor" nil nil)
                       ("imp" "import { $0 } from \"$1\"" "import" nil nil nil "/Users/spike/.emacs.d/snippets/web-mode/import" nil nil)
                       ("gc" "\\${globalColor(\\`--\\${illaPrefix\\}-${1:$$(yas-choose-value '(\"white\" \"gray\" \"grayBlue\" \"techPurple\" \"techPink\" \"blackAlpha\" \"blue\" \"red\" \"yellow\" \"orange\" \"cyan\" \"brand\"))}-${2}\\`)};" "globalColor" nil
                        ("illa")
                        nil "/Users/spike/.emacs.d/snippets/web-mode/globalColor" nil nil)
                       ("fc" "import { FC } from \"react\"\n\nexport const ${1:name}: FC = (props) => {\n  return <div></div>\n}\n\n$1.displayName = \"$1\"\n" "function component" nil nil nil "/Users/spike/.emacs.d/snippets/web-mode/function-component" nil nil)
                       ("ecc" "export const $1Style = css\\`\n  $0\n`" "export css const" nil nil nil "/Users/spike/.emacs.d/snippets/web-mode/export-css-const" nil nil)))


;;; Do not edit! File generated at Mon Sep 16 14:39:35 2024
