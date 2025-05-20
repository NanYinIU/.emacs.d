;; init-proj.el --- Define functions.	-*- lexical-binding: t -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 1. 项目管理 (Projectile)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package projectile
  :diminish projectile-mode
  :init
  (projectile-mode +1) ; 全局启用 projectile-mode
  :config
  ;; 设置 projectile 使用的补全系统 (vertico/consult 时通常用 'default' 或 'ivy'/'helm' 如果你用那些)
  (setq projectile-completion-system 'default)
  ;; 配置 projectile 使用 rg/ag 进行内容搜索 (更快)
  ;; 'alien' 会尝试自动侦测 rg, ag, pt, ack, grep
  (setq projectile-indexing-method 'alien)

  ;; 如果你想强制使用 ripgrep (rg)
  (when (executable-find "rg")
    (setq projectile-grep-command "rg --color=never --smart-case --no-heading --line-number")
    ;; projectile-generic-command 用于查找文件，fd 速度很快
    (when (executable-find "fd")
      (setq projectile-project-find-files-command '("fd" "--type" "f" "--hidden" "--exclude" ".git" "--color=never" ".")))
    )
 ;; --- Projectile Hydra 定义 ---
  ;; 注意：defhydra 是一个宏，所以 hydra 包需要在此之前加载
  (defhydra hydra-projectile (:color 'blue :hint nil :quit-on-error nil)
    "
  Projectile   ^| ^项目管理^                     ^| ^查找操作^                      ^| ^项目动作^                   ^| ^搜索与替换^
  ---------------------------------------------------------------------------------------------------------------------------------------
  [_P_] 切换项目       [_f_] 查找文件 (智能)        [_C_] 编译项目               [_g_] Grep (rg/ag/grep)
  [_r_] 最近项目列表   [_d_] 查找目录               [_T_] 测试项目               [_R_] 替换
  [_a_] 添加已知项目   [_b_] 项目内缓冲区列表       [_S_] Shell 命令             [_X_] 正则替换
  [_D_]移除已知项目   [_o_] 打开对应文件(测试/实现) [_E_] Eshell
  [_i_] 清除缓存       [_k_] 关闭项目所有缓冲区     [_L_] 重复上次命令
  "
    ;; --- 项目管理 (PROJECT Management) ---
    ("P" projectile-switch-project :exit t)
    ("r" projectile-recentf :exit t)
    ("a" projectile-add-known-project :exit t)
    ("D" projectile-remove-known-project :exit t)
    ("i" projectile-invalidate-cache :exit t)
    ("k" projectile-kill-buffers :exit t)

    ;; --- 查找操作 (FIND in Project) ---
    ("f" projectile-find-file-dwim :exit t)
    ("d" projectile-find-dir :exit t)
    ("b" projectile-switch-to-buffer :exit t)
    ("o" projectile-find-other-file :exit t)

    ;; --- 项目动作 (ACTIONS on Project) ---
    ("C" projectile-compile-project :exit t)
    ("T" projectile-test-project :exit t)
    ("S" projectile-run-shell-command-in-root :exit t)
    ("E" projectile-run-eshell :exit t)
    ("L" projectile-repeat-last-command :exit t)

    ;; --- 搜索与替换 (SEARCH & REPLACE) ---
    ("g" (lambda ()
           (interactive)
           (cond
            ((executable-find "rg") (call-interactively #'projectile-ripgrep))
            ((executable-find "ag") (call-interactively #'projectile-ag))
            (t (call-interactively #'projectile-grep))))
          "Grep (rg/ag/grep)" :exit t)
    ("R" projectile-replace :exit t)
    ("X" projectile-replace-regexp :exit t)

    ;; --- Hydra 控制 ---
    ("q" nil "退出" :color 'blue))

  ;; --- 绑定 Hydra 的触发快捷键 ---
  ;; 将 "C-c P" (注意是大写P) 绑定到打开 projectile Hydra
  ;; 你可以选择其他不与 projectile 默认快捷键冲突的按键
;;(global-set-key (kbd "C-c P") #'hydra-projectile/body)
;;:bind (("C-c p f" . projectile-find-file)          ; 在项目中查找文件
;;       ("C-c p d" . projectile-find-dir)           ; 在项目中查找目录
;;       ("C-c p s g" . projectile-grep)             ; 在项目中搜索内容 (grep)
;;       ("C-c p s r" . projectile-replace)          ; 在项目中搜索并替换
;;       ("C-c p b" . projectile-switch-to-buffer)   ; 切换到项目中已打开的 buffer
;;       ("C-c p p" . projectile-switch-project)     ; 切换项目
;;       ("C-c p k" . projectile-kill-buffers)       ; 关闭项目所有 buffer
;;       ("C-c p c" . projectile-compile-project)    ; 编译项目
;;       ("C-c p t" . projectile-run-tests)          ; 运行项目测试
;;       ("C-c p R" . projectile-regenerate-tags)))  ; (如果使用 ctags)

  )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 文件树 (Treemacs)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A tree layout file explorer
(use-package treemacs
  :commands (treemacs-follow-mode
             treemacs-filewatch-mode
             treemacs-git-mode)
  :custom-face
  (cfrs-border-color ((t (:inherit posframe-border))))
  :bind (([f8]        . treemacs)
         ("M-0"       . treemacs-select-window)
         ("C-x t 1"   . treemacs-delete-other-windows)
         ("C-x t t"   . treemacs)
         ("C-x t b"   . treemacs-bookmark)
         ("C-x t C-t" . treemacs-find-file)
         ("C-x t M-t" . treemacs-find-tag)
         :map treemacs-mode-map
         ([mouse-1]   . treemacs-single-click-expand-action))
  :config
  (setq treemacs-collapse-dirs           (if treemacs-python-executable 3 0)
        treemacs-missing-project-action  'remove
        treemacs-sorting                 'alphabetic-asc
        treemacs-follow-after-init       t
        treemacs-width                   30)

  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t)
  (pcase (cons (not (null (executable-find "git")))
               (not (null (executable-find "python3"))))
    (`(t . t)
     (treemacs-git-mode 'deferred))
    (`(t . _)
     (treemacs-git-mode 'simple)))

 (use-package treemacs-nerd-icons
    :demand t
    :when (icons-displayable-p)
    :custom-face
    (treemacs-nerd-icons-root-face ((t (:inherit nerd-icons-green :height 1.3))))
    (treemacs-nerd-icons-file-face ((t (:inherit nerd-icons-dsilver))))
    :config (treemacs-load-theme "nerd-icons"))

(use-package treemacs-projectile
  :after (treemacs projectile)
  :ensure t)

(use-package treemacs-magit
  :after (treemacs magit)
  :ensure t)

(use-package treemacs-persp ;;treemacs-perspective if you use perspective.el vs. persp-mode
  :after (treemacs persp-mode) ;;or perspective vs. persp-mode
  :ensure t
  :config (treemacs-set-scope-type 'Perspectives))

(use-package treemacs-tab-bar ;;treemacs-tab-bar if you use tab-bar-mode
  :after (treemacs)
  :ensure t
  :config (treemacs-set-scope-type 'Tabs))

(use-package treemacs-icons-dired
  :after treemacs
  :config (treemacs-icons-dired-mode))

)

;;(treemacs-start-on-boot)
(use-package transient
  :ensure t)

(use-package magit
  :ensure t
  :config
  (setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1)
  (setq magit-save-repository-buffers 'dontask)
  (setq magit-commit-show-diffstat t)
  (setq git-rebase-autosquash t)
  )


(provide 'init-proj)
