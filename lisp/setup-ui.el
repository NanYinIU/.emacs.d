;;; setup-ui.el --- UI, themes, modeline, icons -*- lexical-binding: t; -*-

;; For alert (Commented out)
;;(use-package alert
;;  :ensure t
;;  :config
;;  ;; 设置 alert 的默认样式，例如使用 child-frame
;;  (setq alert-default-style 'child-frame)
;;
;;  ;; 你可以进一步定制 child-frame 的位置、大小、颜色等
;;  ;; (setq alert-childframe-display-position 'bottom-right)
;;  ;; (setq alert-fade-time 2) ; 2秒后淡出
;;
;;  ;; 很多包会自动检测并使用 alert，有些可能需要你显式设置
;;  ;; 例如，对于 Flycheck:
;;  ;; (with-eval-after-load 'flycheck
;;  ;;   (setq flycheck-display-errors-function #'flycheck-display-error-messages-using-alert))
;;  )

;; For all-the-icons
(use-package all-the-icons
  :ensure t
  :if (display-graphic-p)) ; 通常只在图形界面下启用

;; For composite (built-in ligature support)
(use-package composite
    :ensure nil
    :init (defvar composition-ligature-table (make-char-table nil))
    :hook (((prog-mode
             conf-mode nxml-mode markdown-mode help-mode
             shell-mode eshell-mode term-mode vterm-mode)
            . (lambda () (setq-local composition-function-table composition-ligature-table))))
    :config
    ;; support ligatures, some toned down to prevent hang
    (let ((alist
           '((33  . ".\\(?:\\(==\\|[!=]\\)[!=]?\\)")
             (35  . ".\\(?:\\(###?\\|_(\\|[(:=?[_{]\\)[#(:=?[_{]?\\)")
             (36  . ".\\(?:\\(>\\)>?\\)")
             (37  . ".\\(?:\\(%\\)%?\\)")
             (38  . ".\\(?:\\(&\\)&?\\)")
             (42  . ".\\(?:\\(\\*\\*\\|[*>]\\)[*>]?\\)")
             ;; (42 . ".\\(?:\\(\\*\\*\\|[*/>]\\).?\\)")
             (43  . ".\\(?:\\([>]\\)>?\\)")
             ;; (43 . ".\\(?:\\(\\+\\+\\|[+>]\\).?\\)")
             (45  . ".\\(?:\\(-[->]\\|<<\\|>>\\|[-<>|~]\\)[-<>|~]?\\)")
             ;; (46 . ".\\(?:\\(\\.[.<]\\|[-.=]\\)[-.<=]?\\)")
             (46  . ".\\(?:\\(\\.<\\|[-=]\\)[-<=]?\\)")
             (47  . ".\\(?:\\(//\\|==\\|[=>]\\)[/=>]?\\)")
             ;; (47 . ".\\(?:\\(//\\|==\\|[*/=>]\\).?\\)")
             (48  . ".\\(?:x[a-zA-Z]\\)")
             (58  . ".\\(?:\\(::\\|[:<=>]\\)[:<=>]?\\)")
             (59  . ".\\(?:\\(;\\);?\\)")
             (60  . ".\\(?:\\(!--\\|\\$>\\|\\*>\\|\\+>\\|-[-<>|]\\|/>\\|<[-<=]\\|=[<>|]\\|==>?\\||>\\||||?\\|~[>~]\\|[$*+/:<=>|~-]\\)[$*+/:<=>|~-]?\\)")
             (61  . ".\\(?:\\(!=\\|/=\\|:=\\|<<\\|=[=>]\\|>>\\|[=>]\\)[=<>]?\\)")
             (62  . ".\\(?:\\(->\\|=>\\|>[-=>]\\|[-:=>]\\)[-:=>]?\\)")
             (63  . ".\\(?:\\([.:=?]\\)[.:=?]?\\)")
             (91  . ".\\(?:\\(|\\)[]|]?\\)")
             ;; (92 . ".\\(?:\\([\\n]\\)[\\]?\\)")
             (94  . ".\\(?:\\(=\\)=?\\)")
             (95  . ".\\(?:\\(|_\\|[_]\\)_?\\)")
             (119 . ".\\(?:\\(ww\\)w?\\)")
             (123 . ".\\(?:\\(|\\)[|}]?\\)")
             (124 . ".\\(?:\\(->\\|=>\\||[-=>]\\||||*>\\|[]=>|}-]\\).?\\)")
             (126 . ".\\(?:\\(~>\\|[-=>@~]\\)[-=>@~]?\\)"))))
      (dolist (char-regexp alist)
        (set-char-table-range composition-ligature-table (car char-regexp)
                              `([,(cdr char-regexp) 0 font-shape-gstring]))))
    (set-char-table-parent composition-ligature-table composition-function-table))

;; For doom-modeline
(use-package doom-modeline
  :ensure t
  :init
   (doom-modeline-mode 1) ; 或者使用 :hook (after-init . doom-modeline-mode)
  :custom
  ;; 这里可以添加 doom-modeline 的其他自定义设置
  ;; 例如，调整图标大小 (如果需要)
  (setq doom-modeline-icon-font-size 14) ; This was from init-ui, but might be better in init-custom.el if user-specific
  (setq doom-modeline-icon t)
  (setq doom-modeline-nerd-font t)
  ;; (setq nerd-icons-scale-factor 1.0) ; 也可以通过 nerd-icons 的变量调整
)

;; For doom-themes
(use-package doom-themes
  :ensure t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  ;;(load-theme 'doom-one t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Enable custom neotree theme (nerd-icons must be installed!)
  ;; (doom-themes-neotree-config) ; This is for neotree, not treemacs
  ;; or for treemacs users
  (setq doom-themes-treemacs-theme "doom-atom") ; use "doom-colors" for less minimal icon theme
  (doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config)
  ;; (load-theme 'doom-one :no-confirm) ; This is often a user choice, might be better in init-custom.el
)

;; For mini-frame (used by nano setup)
(use-package mini-frame
  :ensure t)

;; For modus-themes (Commented out)
;;(use-package modus-themes
;;  :ensure t
;;  :config
;;  (setq modus-themes-italic-constructs t
;;        modus-themes-bold-constructs nil
;;        modus-themes-disable-other-themes t
;;        modus-themes-org-blocks 'gray-background)
;;  (setq modus-themes-completions
;;      '((matches . (extrabold underline))
;;        (selection . (semibold italic))))
;;  ;;(setq modus-themes-variable-pitch-ui t)
;;  )
;;
;;(load-theme 'modus-operandi-tinted :no-confirm)

;; For moe-theme
(use-package moe-theme
  :ensure t)

;; For nano-local-system (local N Λ N O system from site-lisp/nano-emacs/)
(use-package nano-local-system ;; This is a conceptual name for the local system
  :ensure nil ; It's not an ELPA package
  :load-path "site-lisp/nano-emacs" ; Add its directory to load-path
  :init
  ;; Custom variables set before loading nano components
  (setq nano-font-family-monospaced "Maple Mono NL")
  (setq nano-font-size 13)
  ;; (setq nano-font-family-proportional (face-attribute 'variable-width :family)) ; Keep commented if original was
  :config
  ;; Require all the nano components
  (require 'nano-base-colors)
  (require 'nano-faces)
  (require 'nano-theme)       ; This is nano-emacs/nano-theme.el
  (require 'nano-theme-dark)
  (require 'nano-theme-light)
  (require 'nano-modeline)
  (when (member "-compact" command-line-args) ; Keep conditional load
    (require 'nano-compact))
  (require 'nano-splash)
  (require 'nano-colors)
  (require 'nano-command)
  ;;(require 'nano-minibuffer) ; Keep commented as in original init-nano.el
  ;;(require 'nano-agenda)   ; Keep commented as in original init-nano.el

  ;; Theme activation
  (nano-theme-set-dark)
  (call-interactively 'nano-refresh-theme)

  ;; Welcome message (can also be part of :config)
  (let ((inhibit-message t))
    (message "Welcome to GNU Emacs / N Λ N O edition")
    (message (format "Initialization time: %s" (emacs-init-time)))))

;; For nano-theme (ELPA version, commented out to prefer local nano-local-system)
;;(use-package nano-theme
;;  :ensure t
;;  :defer t
;;  :custom-face
;;  (mode-line-buffer-id ((t (:foreground "black" :weight bold))))
;;  (mode-line-emphasis ((t (:foreground "black" :weight bold))))
;;  (doom-modeline-buffer-modified ((t (:foreground "black" :weight bold))))
;;  (doom-modeline-project-dir ((t (:foreground "black" :weight bold))))
;;  ;; mode-line coincides with mode-line-inactive
;;  (mode-line ((t (:background "#90A4AE" :foreground "#FFFFFF" :box (:line-width (3 . 3) :color "#90A4AE")))))
;;  (nano-popout ((t (:foreground "#B22222" :weight bold))))
;;  (nano-salient ((t (:foreground "#1E90FF" :weight semibold))))
;;  (vertical-border ((t (:foreground "black"))))
;;  ;; config for ess
;;  (doom-modeline-urgent ((t (:foreground "black" :weight bold))))
;;  (doom-modeline-warning ((t (:foreground "black" :weight bold))))
;;  (doom-modeline-info ((t (:foreground "black" :weight bold))))
;;  ;; config for eshell
;;  (eshell-prompt ((t (:foreground "#B03060"))))
;;  (dired-flagged ((t (:foreground "#FF3030" :weight bold))))
;;
;;  :config
;;  (load-theme 'nano-light t)
;;  )

;; For nerd-icons
(use-package nerd-icons
  :ensure t
  :custom
  ;; 可选：如果您想指定特定的 Nerd Font 字体家族给图标
   (nerd-icons-font-family "Symbols Nerd Font Mono")
  ;; 或者让 Emacs 自动检测
 ;;  (nerd-icons-font-family nil)
  :config
  ;; 第一次安装后，您可能需要手动运行 M-x nerd-icons-install-fonts
  ;; 这个函数会尝试下载并安装推荐的 Nerd Font。
  ;; 在某些系统（如 Windows）上，它可能只下载字体，您需要手动安装。

  ;;(unless (nerd-icons--icon-font-configured-p)
 ;;    (nerd-icons-install-fonts)))
  )

;; For posframe (pop-up frames)
(use-package posframe
  :ensure t ; Assuming it should be installed if used
  :hook (after-load-theme . posframe-delete-all)
  :init
  (defface posframe-border
    `((t (:inherit region)))
    "Face used by the `posframe' border."
    :group 'posframe)
  (defvar posframe-border-width 2
    "Default posframe border width.")
  :config
  (with-no-warnings
    (defun my-posframe--prettify-frame (&rest _)
      (set-face-background 'fringe nil posframe--frame))
    (advice-add #'posframe--create-posframe :after #'my-posframe--prettify-frame)

    (defun posframe-poshandler-frame-center-near-bottom (info)
      (cons (/ (- (plist-get info :parent-frame-width)
                  (plist-get info :posframe-width))
               2)
            (/ (+ (plist-get info :parent-frame-height)
                  (* 2 (plist-get info :font-height)))
               2)))))

;; For ultra-scroll
(use-package ultra-scroll
  :ensure t
  :hook (after-init . ultra-scroll-mode))

;; For which-key
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
  (which-key-add-key-based-replacements "C-c o" "Org/Org Roam")
  (add-to-list 'which-key-replacement-alist '(("TAB" . nil) . ("↹" . nil)))
  (add-to-list 'which-key-replacement-alist '(("RET" . nil) . ("⏎" . nil)))
  (add-to-list 'which-key-replacement-alist '(("DEL" . nil) . ("⇤" . nil)))
  (add-to-list 'which-key-replacement-alist '(("SPC" . nil) . ("␣" . nil)))
  )

;; For window-tool-bar
(use-package window-tool-bar
  :ensure t)

;;; Custom UI Settings from init-ui.el ;;;
;;; ========= FONT SETTING  =========
(defun +my/better-font()
  (interactive)
  ;; (setq line-spacing 0.25)
  ;; english font
  (if (display-graphic-p)
      (progn

        ;;(set-face-attribute 'default nil :font (format "%s:pixelsize=%d" "Sarasa Mono SC" 13)) ;; 11 13 17 19 23
        (set-face-attribute 'default nil :font "Maple Mono NL 13") ;; 11 13 17 19 23
        ;; chinese font
        (dolist
            (charset '(kana han symbol cjk-misc bopomofo))
          ;;  文泉 micro Hei ： WenQuanYi Micro Hei Mono
          ;; 更纱黑体 : Sarasa Mono SC
          (set-fontset-font (frame-parameter nil 'font)
                            charset
                            (font-spec :family "Maple Mono NL" :size 13)
                            ))) ;; 14 16 20 22 28
    ))

(defun +my|init-font(frame)
  (with-selected-frame frame
    (if (display-graphic-p)
        (+my/better-font))))

(if (and (fboundp 'daemonp) (daemonp))
    (add-hook 'after-make-frame-functions #'+my|init-font)
  (+my/better-font))

;; Mouse & Smooth Scroll
;; Scroll one line at a time (less "jumpy" than defaults)
(when (display-graphic-p)
  (setq mouse-wheel-scroll-amount '(1 ((shift) . hscroll))
        mouse-wheel-scroll-amount-horizontal 1
        mouse-wheel-progressive-speed nil))
(setq scroll-step 1
      scroll-margin 0
      scroll-conservatively 100000
      auto-window-vscroll nil
      scroll-preserve-screen-position t)

;; macOS specific settings
(setq ns-use-thin-smoothing t)
;; Don't open a file in a new frame
(setq ns-pop-up-frames nil)

(provide 'setup-ui)
