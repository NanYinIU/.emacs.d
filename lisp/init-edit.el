;; init-edit.el --- Define functions.	-*- lexical-binding: t -*-
(use-package iedit
  :ensure t
  :init
  (setq iedit-toggle-key-default nil)
  :config
;;  (define-key iedit-mode-keymap (kbd "M-h") 'iedit-restrict-function)
;;  (define-key iedit-mode-keymap (kbd "M-i") 'iedit-restrict-current-line))
)

(use-package expand-region
  :ensure t
  :bind (
         ("C-=" . er/expand-region)
         ("C--" . er/contract-region)
         )
  :config
  )

(use-package avy
  :ensure t
  :bind (;; 使用 M-s (search) 作为前缀 (推荐)
        ;; ("M-s c" . avy-goto-char-timer)   ; 跳转到两个字符指定的位置
         ;;("M-s w" . avy-goto-word-1)       ; 跳转到单词词首 (输入单词首字母)
         ;;("M-s W" . avy-goto-word-0)       ; 跳转到单词内的任意字符 (输入单词首字母)
         ;;("M-s l" . avy-goto-line)         ; 跳转到行
         ;;("M-s SPC" . avy-resume)          ; 重复上一次 avy 跳转

         ;; 风格2：使用 C-c j (jump) 作为前缀
          ("C-'" . avy-goto-char-timer)     ; 另一个流行的快速访问绑定
          ("C-c j j" . avy-goto-char-timer)
          ("C-c j w" . avy-goto-word-1)
          ("C-c j l" . avy-goto-line)
          ("C-c j r" . avy-resume) ; 或 avy-isearch
         )
  :config
  ;; (setq avy-keys '(?f ?j ?d ?k ?s ?l ?a ?\; ?g ?h)) ; F J D K S L A ; G H (HJKL-like)
  ;; (setq avy-timeout-seconds 0.3) ; avy-goto-char-timer 等待第二个字符的超时时间
  ;; (setq avy-highlight-first t)   ; 立即高亮第一个字符的匹配项
  ;; (setq avy-style 'at-full)      ; 'at-full 会替换字符，'pre 在字符前显示，'overlay 用覆盖层
  )

(use-package multiple-cursors
  :ensure t
  :bind(
        ("C->" . mc/mark-next-word-like-this)
        ("C-<" . mc/unmark-next-like-this)
        ("C-c C->" . mc/mark-all-like-this)
        ("C-c C-;" . 'mc/edit-lines)
        )
  )

(global-set-key (kbd "C-c d") #'my-duplicate-line)
(global-set-key (kbd "C-c x") #'my-kill-entire-line)

(use-package smartparens
  :ensure smartparens  ;; install the package
  :hook (prog-mode text-mode markdown-mode org-mode) ;; add `smartparens-mode` to these hooks
  :config
  ;; load default config
  (require 'smartparens-config))


(use-package apheleia
  :ensure t
  :config
  (apheleia-global-mode +1) ; 全局启用，它会根据项目和文件类型自动选择格式化器
  )

(use-package which-key
  :ensure t
  :init
  (setq which-key-idle-delay 0.5)
  (setq which-key-idle-secondary-delay 0.05)
  (setq which-key-popup-type 'minibuffer)
  (setq which-key-sort-order #'which-key-key-order)
  (setq which-key-add-keymap-based-replacements t)
  :config
  (which-key-mode 1)
  ;; 2. 为特定的前缀组合自定义描述
  ;; (add-to-list 'which-key-prefix-override-rules
  ;;                '("C-x r" . "Rectangles/Registers"))
  ;; (add-to-list 'which-key-prefix-override-rules
  ;;                '("C-c n" . "Org Roam"))
  )

;;(use-package youdao-dictionary
;;  :ensure t
;;  :after popup
;;  :init
;;  (setq youdao-dictionary-app-key "5f99312ee6462e1e")
;;  (setq youdao-dictionary-secret-key "g6u7hTZCpv4vYJ9PD5U0Rnr46N8kGZjl"))

(provide 'init-edit)

;;; init-edit.el ends here
