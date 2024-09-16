;;; Compiled snippets and support files for `typescript-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'typescript-mode
                     '(("sty" "export const $1Style = css\\`\n$0\n`\n" "style" nil nil nil "/Users/spike/.emacs.d/snippets/typescript-mode/style" nil nil)
                       ("mock" "  rest.${1:$$(yas-choose-value '(\"get\" \"post\" \"put\" \"delete\"))}(\\`\\${baseUrl}/$2`, (req, res, ctx) => {\n  $0\n  return res(ctx.status(200))\n}),\n" "mock" nil nil nil "/Users/spike/.emacs.d/snippets/typescript-mode/mock" nil nil)
                       ("illas" "import { css } from \"@emotion/react\"\nimport { globalColor, illaPrefix } from \"@illa-design/theme\"" "illa-style-init" nil nil nil "/Users/spike/.emacs.d/snippets/typescript-mode/illa-style-init" nil nil)
                       ("gc" "\\${globalColor(\\`--\\${illaPrefix\\}-${1:$$(yas-choose-value '(\"white\" \"gray\" \"grayBlue\" \"techPurple\" \"techPink\" \"blackAlpha\" \"blue\" \"red\" \"yellow\" \"orange\" \"cyan\" \"brand\"))}-${2}\\`)};" "globalColor" nil
                        ("illa")
                        nil "/Users/spike/.emacs.d/snippets/typescript-mode/globalColor" nil nil)
                       ("tfc" "/** $0 */" "TypeScript Field Comment" nil nil nil "/Users/spike/.emacs.d/snippets/typescript-mode/+new-snippet+" nil nil)))


;;; Do not edit! File generated at Mon Sep 16 14:39:35 2024
