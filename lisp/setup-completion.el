;;; setup-completion.el --- Completion frameworks (Vertico, Corfu, etc.) -*- lexical-binding: t; -*-

;; For cape
(use-package cape
  :ensure t
  :defer t
  :init
  ;; 添加 Cape 的各种补全后端到 completion-at-point-functions
  ;; Eglot (LSP) 的补全会自动加入这个列表，并被 Corfu 使用
  (add-hook 'completion-at-point-functions #'cape-dabbrev nil t) ; t 表示添加到末尾
  (add-hook 'completion-at-point-functions #'cape-file nil t)
  (add-hook 'completion-at-point-functions #'cape-keyword nil t)
  ;; (add-hook 'completion-at-point-functions #'cape-abbrev nil t)
  ;; (add-hook 'completion-at-point-functions #'cape-ispell nil t)
  ;; (add-hook 'completion-at-point-functions #'cape-tex nil t)
  (add-hook 'completion-at-point-functions #'cape-dict nil t)
  ;; ...根据你的需要添加更多
  )

;; For consult
(use-package consult
  :ensure t ; 确保安装
  :bind (;; 您的 Consult 快捷键绑定... (可以根据需要精简)
         ;; 例如，保留一些核心的：
         ("C-x b" . consult-buffer)
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ;; ("M-x" . consult-complex-command) ; 或者使用默认的 execute-extended-command，让 Vertico 处理
         ("C-c i" . consult-info)
         ([remap Info-search] . consult-info)

         ("M-s r" . consult-ripgrep)       ; ripgrep 搜索\
         ("M-s d" . consult-fd)
         ("M-s l" . consult-line)          ; 当前 buffer 行搜索
         ;; isearch 集成等
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         :map minibuffer-local-map
         ("M-s" . consult-history)
         ("M-r" . consult-history)
         )
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :init
  ;; (保留您原来的 register 和 xref 设置)
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)
  (advice-add #'register-preview :override #'consult-register-window)
  (with-eval-after-load 'xref
    (setq xref-show-xrefs-function #'consult-xref
          xref-show-definitions-function #'consult-xref))
  :config
  (setq consult-preview-key nil) ; 您的预览设置
  (consult-customize
   consult-line consult-line-multi :preview-key 'any
   consult-buffer consult-recent-file consult-theme :preview-key '(:debounce 1.0 any)
   consult-goto-line :preview-key '(:debounce 0.5 any)
   consult-ripgrep consult-git-grep consult-grep
   :initial (selected-region-or-symbol-at-point)
   :preview-key '(:debounce 0.5 any))
  (setq consult-narrow-key "<")
  (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help))

;; For corfu
(use-package corfu
  :ensure t
  :custom
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-separator ?\s)
  (corfu-quit-at-boundary 'separator)
  (corfu-preview-current nil)
  (corfu-popupinfo-delay 0.25)   ; 取消注释以启用文档弹窗延迟
  (corfu-echo-documentation nil) ; 取消注释，如果使用 popupinfo
  :init
  (global-corfu-mode)
  :config
  ;; 推荐使用 corfu-map 来定义 Corfu 激活时的按键
  ;; 而不是直接修改 global-map 或 completion-in-region-mode-map
  (define-key corfu-map (kbd "TAB") #'corfu-complete)
  (define-key corfu-map (kbd "<tab>") #'corfu-complete) ; 确保 <tab> 也映射
  ;; (define-key corfu-map (kbd "TAB") #'my-corfu-tab-or-indent) ; 如果使用自定义的 tab 函数
  (define-key corfu-map (kbd "S-TAB") #'corfu-complete-previous)
  (define-key corfu-map (kbd "C-j") #'corfu-next)
  (define-key corfu-map (kbd "C-k") #'corfu-previous)
  (define-key corfu-map (kbd "RET") #'corfu-insert)
  (define-key corfu-map (kbd "<return>") #'corfu-insert)
  (corfu-popupinfo-mode) ; 取消注释以自动显示 Eldoc 文档弹窗
  )

;; For embark
(use-package embark
  :ensure t
  :bind
  (("C-." . embark-act)
   ("C-;" . embark-dwim)
   ("C-h B" . embark-bindings))
  :init
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

;; For embark-consult
(use-package embark-consult
  :ensure t
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;; For icomplete (built-in completion mechanism)
(use-package icomplete
  :ensure nil ; Built-in
  :config
  (icomplete-mode 1))

;; For marginalia
(use-package marginalia
  :ensure t
  :config
  (marginalia-mode)) ; marginalia-mode 的参数 t 不是必须的，(marginalia-mode) 即可

;; For orderless
(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

;; For vertico
(use-package vertico
  :ensure t
  :init
  ;; make consult-ripgrep work (保留您原来的编码设置，如果需要)
  (add-to-list 'process-coding-system-alist
               '("[rR][gG]" . (utf-8-dos . windows-1251-dos)))
  (vertico-mode)
  :config
  (define-key vertico-map (kbd "C-j") #'vertico-next)
  (define-key vertico-map (kbd "C-k") #'vertico-previous)
  )

(provide 'setup-completion)
