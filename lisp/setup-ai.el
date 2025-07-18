;;; setup-ai.el --- Org mode and its ecosystem -*- lexical-binding: t; -*-

;;(use-package aidermacs
;;  :bind (("C-c a" . aidermacs-transient-menu))
;;  :config
;;  ; Set API_KEY in .bashrc, that will automatically picked up by aider or in elisp
;;  ;;(setenv "ANTHROPIC_API_KEY" "sk-...")
;;  ; defun my-get-openrouter-api-key yourself elsewhere for security reasons
;;  ;;(setenv "OPENROUTER_API_KEY" (my-get-openrouter-api-key))
;;  (add-hook 'aidermacs-before-run-backend-hook
;;          (lambda ()
;;            (setenv "OPENROUTER_API_KEY" (password-store-get "code/openrouter_api_key"))))
;;  :custom
;;  ; See the Configuration section below
;;  (aidermacs-use-architect-mode t)
;;  (aidermacs-default-model "sonnet"))

;; Add your API keys to ~/.authinfo
(use-package gptel
  :config
  ;;(setq gptel-proxy "http://127.0.0.1:7890")
  (setq gptel-provider 'openrouter)
  (setq gptel-default-mode 'org-mode)
  (setf (alist-get 'org-mode gptel-prompt-prefix-alist) "@user\n")
  (setf (alist-get 'org-mode gptel-response-prefix-alist) "@assistant\n")
  ;; :key can be a function that returns the API key.
  (defun get-authinfo-password (machine)
    "Read the contents of FILE-PATH and return it as a string."
    (let ((auth-info (auth-source-search :host machine)))
      (if auth-info
          (let ((secret (plist-get (car auth-info) :secret)))
            (if (functionp secret)
                (funcall secret)
              secret)
            )
        (message "No matching auth info found."))))
  ;; OpenRouter offers an OpenAI compatible API
  (gptel-make-openai "OpenRouter"               ;Any name you want
  :host "openrouter.ai"
  :endpoint "/api/v1/chat/completions"
  :stream t
  :key (get-authinfo-password "openrouter.ai")                 ;can be a function that returns the key
  :models '(qwen/qwen-2.5-coder-32b-instruct
            deepseek/deepseek-chat-v3-0324
            deepseek/deepseek-r1-0528))

  (setq
   gptel-model 'gemini-2.0-flash
   gptel-backend  (gptel-make-gemini "Gemini" :key (get-authinfo-password "gemini.com") :stream t))
)

(provide 'setup-ai)

;;; setup-ai.el ends here
