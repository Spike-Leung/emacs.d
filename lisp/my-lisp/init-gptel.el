;;; init-gptel.el --- gptel
;;; Commentary:

(maybe-require-package 'gptel)

(with-eval-after-load 'gptel
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
    :models '(deepseek-ai/DeepSeek-R1
              deepseek-ai/DeepSeek-V3
              deepseek-ai/DeepSeek-R1-Distill-Qwen-32B
              deepseek-ai/DeepSeek-R1-Distill-Llama-70B))
  (gptel-make-gemini "Gemini" :key (spike-leung/get-gemini-api-key) :stream t)
  (setq
   gptel-model 'deepseek-reasoner
   gptel-backend (gptel-make-openai "DeepSeek"
                   :host "api.deepseek.com"
                   :endpoint "/chat/completions"
                   :stream t
                   :key (spike-leung/get-deepseek-api-key)
                   :models '(deepseek-reasoner deepseek-chat deepseek-coder))))

(global-set-key (kbd "M-o g") 'gptel-menu)

(provide 'init-gptel)
;;; init-gptel.el ends here
