;;; init-gptel.el --- gptel
;;; Commentary:

(maybe-require-package 'gptel)

(with-eval-after-load 'gptel
  ;; define provider
  (gptel-make-openai "DeepSeek"
    :host "api.deepseek.com"
    :endpoint "/chat/completions"
    :stream t
    :key (spike-leung/get-deepseek-api-key)
    :models '(deepseek-chat deepseek-coder))
  (gptel-make-openai "SiliconFlow"
    :host "api.siliconflow.cn"
    :endpoint "/chat/completions"
    :stream t
    :key (spike-leung/get-siliconflow-api-key)
    :models '(Pro/deepseek-ai/DeepSeek-R1
              Pro/deepseek-ai/DeepSeek-V3
              deepseek-ai/DeepSeek-R1-Distill-Qwen-32B
              deepseek-ai/DeepSeek-R1-Distill-Llama-70B))
  (gptel-make-openai "DeepSeek"
    :host "api.deepseek.com"
    :endpoint "/chat/completions"
    :stream t
    :key (spike-leung/get-deepseek-api-key)
    :models '(deepseek-reasoner deepseek-chat deepseek-coder))
  (gptel-make-gemini "Gemini" :key (spike-leung/get-gemini-api-key) :stream t)
  ;; set default
  (setq
   gptel-model "Pro/deepseek-ai/DeepSeek-V3"
   gptel-backend (gptel-make-openai "SiliconFlow"
                   :host "api.siliconflow.cn"
                   :endpoint "/chat/completions"
                   :stream t
                   :key (spike-leung/get-siliconflow-api-key)
                   :models '(Pro/deepseek-ai/DeepSeek-R1
                             Pro/deepseek-ai/DeepSeek-V3
                             deepseek-ai/DeepSeek-R1-Distill-Qwen-32B
                             deepseek-ai/DeepSeek-R1-Distill-Llama-70B))
   ))

(global-set-key (kbd "M-o g") 'gptel-menu)

(provide 'init-gptel)
;;; init-gptel.el ends here
