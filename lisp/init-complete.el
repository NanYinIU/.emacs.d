;; init-complete.el --- Define functions.	-*- lexical-binding: t -*-

;; --------------------------------------------------------------------------
;; Vertico, Orderless, Marginalia, Embark, Consult (VOMEC Stack)
;; --------------------------------------------------------------------------
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

(use-package savehist
  :ensure t ; 确保安装 (如果不是内置的)
  :init
  (savehist-mode))

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package marginalia
  :ensure t
  :config
  (marginalia-mode)) ; marginalia-mode 的参数 t 不是必须的，(marginalia-mode) 即可

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

(use-package embark-consult
  :ensure t
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package consult
  :ensure t ; 确保安装
  :bind (;; 您的 Consult 快捷键绑定... (可以根据需要精简)
         ;; 例如，保留一些核心的：
         ("C-x b" . consult-buffer)
         ;; ("M-x" . consult-complex-command) ; 或者使用默认的 execute-extended-command，让 Vertico 处理
         ("C-c r" . consult-ripgrep)       ; ripgrep 搜索
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

;; --------------------------------------------------------------------------
;; In-Buffer Completion (Corfu, Cape) & LSP (Eglot)
;; --------------------------------------------------------------------------
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

(use-package yasnippet-capf
  :ensure t ; 如果 yasnippet-capf 不是 cape 的依赖，确保它被安装
  :after (cape yasnippet) ; 明确依赖 yasnippet
  :hook (emacs-lisp-mode . (lambda ()
                            (add-to-list 'completion-at-point-functions #'yasnippet-capf))))

(use-package eglot
  :ensure t
  :hook ((python-mode . eglot-ensure)
         (rust-mode . eglot-ensure)
         (java-mode . eglot-ensure)
         ;; (c-mode . eglot-ensure) ; c-mode-hook 等
         ;; (c++-mode . eglot-ensure)
         ;; (typescript-mode . eglot-ensure)
         ;; (js-mode . eglot-ensure) ; js-mode-hook 或 js2-mode-hook
         )
  :config
  ;; (setq eglot-autoreconnect 3) ; 尝试自动重连3次，每次间隔1秒 (可调整)
  ;; (setq eglot-connect-timeout 30) ; 连接超时时间 (秒)
  ;; 明确指定 language server (如果 eglot 自动检测不到或你想用特定的)
  (add-to-list 'eglot-server-programs
               '(rust-ts-mode . ("rust-analyzer"))) ; rust-analyzer for rust-ts-mode (if you use it)
  (add-to-list 'eglot-server-programs
                '(rust-mode . ("rust-analyzer"))) ; and for regular rust-mode
  ;; 对于 Python, eglot 通常能自动发现 pylsp 或 pyright
  ;; 如果需要指定，例如 Pyright (假设已通过 npm i -g pyright 安装):
  (add-to-list 'eglot-server-programs
                '((python-mode python-ts-mode) . ("pyright-langserver" "--stdio")))
  ;; 或者 pylsp (假设已通过 pip install python-lsp-server[all] 安装):
  ;; (add-to-list 'eglot-server-programs
  ;;              '((python-mode python-ts-mode) . ("pylsp")))
  )

(provide 'init-complete)
