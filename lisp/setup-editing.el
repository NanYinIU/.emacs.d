;;; setup-editing.el --- General text editing enhancements -*- lexical-binding: t; -*-

;; For avy
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
  (setq avy-style 'at-full)
  )

;; For diminish
(use-package diminish
  :ensure t)

;; For editorconfig
(use-package editorconfig
  :ensure t
  :diminish
  :hook (after-init . editorconfig-mode))

;; For expand-region
(use-package expand-region
  :ensure t
  :bind (
         ("C-=" . er/expand-region)
         ("C--" . er/contract-region)
         )
  :config
  )

;; For iedit
(use-package iedit
  :ensure t
  :init
  (setq iedit-toggle-key-default nil)
  :config
;;  (define-key iedit-mode-keymap (kbd "M-h") 'iedit-restrict-function)
;;  (define-key iedit-mode-keymap (kbd "M-i") 'iedit-restrict-current-line))
)

;; For multiple-cursors
(use-package multiple-cursors
  :ensure t
  :bind(
        ("C->" . mc/mark-next-word-like-this)
        ("C-<" . mc/unmark-next-like-this)
        ("C-c C->" . mc/mark-all-like-this)
        ("C-c C-;" . 'mc/edit-lines)
        )
  )

;; For smartparens
(use-package smartparens
  :ensure t  ;; install the package, changed from :ensure smartparens
  :hook (prog-mode text-mode markdown-mode org-mode) ;; add `smartparens-mode` to these hooks
  :config
  ;; load default config
  (require 'smartparens-config))

;; For yasnippet (snippet engine)
(use-package yasnippet
  :ensure t
  :hook ((prog-mode . yas-minor-mode)
         (org-mode . yas-minor-mode))
  :config
  (progn
    (setq hippie-expand-try-functions-list
          '(yas/hippie-try-expand
            try-complete-file-name-partially
            try-expand-all-abbrevs
            try-expand-dabbrev
            try-expand-dabbrev-all-buffers
            try-expand-dabbrev-from-kill
            try-complete-lisp-symbol-partially
            try-complete-lisp-symbol))))

;; For yasnippet-capf
(use-package yasnippet-capf
  :ensure t ; 如果 yasnippet-capf 不是 cape 的依赖，确保它被安装
  :after (cape yasnippet) ; 明确依赖 yasnippet
  :hook (emacs-lisp-mode . (lambda ()
                            (add-to-list 'completion-at-point-functions #'yasnippet-capf))))

;; For yasnippet-snippets
(use-package yasnippet-snippets
  :ensure t
  :after yasnippet)

(provide 'setup-editing)
