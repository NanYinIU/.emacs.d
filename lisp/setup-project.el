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

(use-package diff-hl
  :config
  (global-diff-hl-mode))

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
  ;; Define key bindings for projectile
  :bind-keymap ("C-c p" . projectile-command-map)
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
