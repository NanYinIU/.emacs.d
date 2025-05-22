;;; setup-project.el --- Project management, version control, file tree -*- lexical-binding: t; -*-

;; For magit
(use-package magit
  :ensure t
  :config
  (setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1)
  (setq magit-save-repository-buffers 'dontask)
  (setq magit-commit-show-diffstat t)
  (setq git-rebase-autosquash t)
  )

;; For mgit
(use-package mgit
  :ensure t)

;; For persp-mode (perspective.el, for treemacs-persp)
(use-package persp-mode
  :ensure t
  :defer t)

;; For projectile (Project Interaction Library)
(use-package projectile
  :ensure t
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
  )

;; For tramp
(use-package tramp
  :ensure t)

;; For transient
(use-package transient
  :ensure t)

;;============ Treemacs Group Start ===============
;; A tree layout file explorer
(use-package treemacs
  :ensure t
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
  (setq treemacs-collapse-dirs           (if treemacs-python-executable 3 0) ; treemacs-python-executable needs to be defined
        treemacs-missing-project-action  'remove
        treemacs-sorting                 'alphabetic-asc
        treemacs-follow-after-init       t
        treemacs-width                   30)

  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t)
  (pcase (cons (not (null (executable-find "git")))
               (not (null (executable-find "python3")))) ; Assuming python3 is the intended check
    (`(t . t)
     (treemacs-git-mode 'deferred))
    (`(t . _)
     (treemacs-git-mode 'simple)))
)

;; For treemacs-icons-dired
(use-package treemacs-icons-dired
  :ensure t
  :after treemacs
  :config (treemacs-icons-dired-mode))

;; For treemacs-magit
(use-package treemacs-magit
  :ensure t
  :after (treemacs magit))

;; For treemacs-nerd-icons
(use-package treemacs-nerd-icons
  :ensure t
  :after treemacs ; Ensure treemacs is loaded first
  :demand t 
  :when (icons-displayable-p) ; icons-displayable-p function needs to be available
  :custom-face
  (treemacs-nerd-icons-root-face ((t (:inherit nerd-icons-green :height 1.3))))
  (treemacs-nerd-icons-file-face ((t (:inherit nerd-icons-dsilver))))
  :config (treemacs-load-theme "nerd-icons"))

;; For treemacs-persp
(use-package treemacs-persp ;;treemacs-perspective if you use perspective.el vs. persp-mode
  :ensure t
  :after (treemacs persp-mode) ;;or perspective vs. persp-mode
  :config (treemacs-set-scope-type 'Perspectives))

;; For treemacs-projectile
(use-package treemacs-projectile
  :ensure t
  :after (treemacs projectile))

;; For treemacs-tab-bar
(use-package treemacs-tab-bar ;;treemacs-tab-bar if you use tab-bar-mode
  :ensure t
  :after (treemacs)
  :config (treemacs-set-scope-type 'Tabs))
;;============ Treemacs Group End ===============

(provide 'setup-project)
