;;; setup-core.el --- Core Emacs configurations -*- lexical-binding: t; -*-

;;; Commentary:
;; Core functionality and packages that support basic Emacs operation

;;; Code:

(require 'init-const)
(require 'init-funcs)

;; For async operations
(use-package async
  :ensure t
  :functions (async-bytecomp-package-mode dired-async-mode)
  :init
  (async-bytecomp-package-mode 1)
  (dired-async-mode 1))

;; For bind-key (for bind-keys macro)
(use-package bind-key
  :ensure t)

;; For compat (compatibility library)
(use-package compat
  :ensure t
  :demand t)

;; For exec-path-from-shell
(use-package exec-path-from-shell
  :ensure t
  :custom
  (exec-path-from-shell-arguments '("-l"))
  :config
  (when (or (memq window-system '(mac ns x))
            (daemonp))
    (exec-path-from-shell-initialize)))

;; For gcmh - Garbage Collector Magic Hack
(use-package gcmh
  :ensure t
  :diminish
  :hook (emacs-startup . gcmh-mode)
  :init
  (setq gcmh-idle-delay 'auto
        gcmh-auto-idle-delay-factor 10
        gcmh-high-cons-threshold #x1000000)) ; 16MB

;; For hideshow (hs-minor-mode) - Code folding
(use-package hideshow
  :ensure nil ; hideshow is built-in
  :diminish hs-minor-mode
  :hook (prog-mode . hs-minor-mode)
  :config
  (define-key hs-minor-mode-map (kbd "C-x $") #'hs-toggle-hiding))

;; For recentf - Recent files
(use-package recentf
  :bind (("C-x C-r" . recentf-open-files))
  :hook (after-init . recentf-mode)
  :init
  (setq recentf-max-saved-items 300
        recentf-exclude
        '("\.?cache" ".cask" "url" "COMMIT_EDITMSG\'" "bookmarks"
          "\.\(?:gz\|gif\|svg\|png\|jpe?g\|bmp\|xpm\)$"
          "\.?ido\.last$" "\.revive$" "/G?TAGS$" "/.elfeed/"
          "^/tmp/" "^/var/folders/.+$" "^/ssh:" "/persp-confs/"
          (lambda (file) (file-in-directory-p file package-user-dir))))
  :config
  (push (expand-file-name recentf-save-file) recentf-exclude)
  (add-to-list 'recentf-filename-handlers #'abbreviate-file-name))

;; For savehist - Minibuffer history
(use-package savehist
  :ensure nil
  :init
  (setq savehist-file (expand-file-name "savehist" user-emacs-directory))
  (setq savehist-additional-variables '(search-ring regexp-search-ring extended-command-history))
  (setq savehist-autosave-interval 60)
  (savehist-mode))

;; For saveplace - Remember cursor position
(use-package saveplace
  :ensure nil
  :init
  (setq save-place-file (expand-file-name "saveplace" user-emacs-directory))
  (save-place-mode 1))

;; For simple - General editing and UI modes
(use-package simple
  :ensure nil
  :hook ((after-init . size-indication-mode)
         (text-mode . visual-line-mode)
         ((prog-mode markdown-mode conf-mode) . enable-trailing-whitespace))
  :init
  (setq column-number-mode t
        line-number-mode t
        kill-whole-line t               ; Kill line including '\n'
        line-move-visual nil
        track-eol t                     ; Keep cursor at end of lines
        set-mark-command-repeat-pop t)  ; Repeating C-SPC after popping mark pops it again

  ;; Visualize whitespace
  (setq-default show-trailing-whitespace nil) ; Don't show trailing whitespace by default
  (defun enable-trailing-whitespace ()
    "Show trailing spaces and delete on saving."
    (setq show-trailing-whitespace t)
    (add-hook 'before-save-hook #'delete-trailing-whitespace nil t)))

;; For time - Display time in mode line
(use-package time
  :ensure nil
  :init
  (setq display-time-default-load-average nil
        display-time-format "%H:%M"))

;; For tramp - Remote file editing
(use-package tramp
  :ensure nil
  :defer t
  :config
  (setq tramp-default-method "ssh")
  (setq tramp-auto-save-directory
        (expand-file-name "tramp-auto-save" user-emacs-directory)))

;; For uniquify - Better buffer naming for files with same name
(use-package uniquify
  :ensure nil
  :init
  (setq uniquify-buffer-name-style 'forward
        uniquify-separator "/"
        uniquify-after-kill-buffer-p t
        uniquify-ignore-buffers-re "^\\*"))

;; For winner - Window configuration history
(use-package winner
  :ensure nil
  :hook (after-init . winner-mode)
  :init
  (setq winner-boring-buffers '("*Completions*" "*Compile-Log*"
                                "*inferior-lisp*" "*Fuzzy Completions*"
                                "*Apropos*" "*Help*" "*cvs*"
                                "*Buffer List*" "*Ibuffer*")))

;; For xref - Code references and navigation
(use-package xref
  :ensure nil
  :bind (("M-g ." . xref-find-definitions)
         ("M-g ," . xref-go-back))
  :init
  ;; Use faster search tool
  (when (executable-find "rg")
    (setq xref-search-program 'ripgrep))
  ;; Select from xref candidates in minibuffer
  (setq xref-show-definitions-function #'xref-show-definitions-completing-read
        xref-show-xrefs-function #'xref-show-definitions-completing-read))

;; For desktop - Session savin
(use-package desktop
  :ensure nil
  :init
  (setq desktop-path (list user-emacs-directory)
        desktop-auto-save-timeout 600
        desktop-restore-eager 10
        desktop-load-locked-desktop 'ask)
  (desktop-save-mode 1))

(use-package vterm
  :ensure t ; Ensures vterm is installed
  :config
  ;; Set your preferred shell (optional, vterm usually picks up your default shell)
  (setq vterm-shell "zsh") ; or "bash", "fish", etc.

  ;; Set a default directory for new vterm buffers (optional)
  ;; (setq vterm-default-directory "~/")

  ;; You might want vterm buffers to have unique names
  (setq vterm-buffer-name-string "vterm:%s") ; Using %s for a counter or specific name

  ;; Automatically kill the buffer when the shell exits
  (setq vterm-kill-buffer-on-exit t)

  ;; Font settings (vterm tries to use your default font, but you can customize)
  ;;(set-face-attribute 'vterm-font nil :font "JetBrains Mono-13")

  ;; Keybindings
  ;; Global keybinding to open vterm
  (define-key global-map (kbd "C-c t") 'vterm)

  ;; Keybindings specific to vterm-mode
  ;; Note: vterm-mode-map is defined by vterm itself,
  ;; so we can directly use it here within the :config block.
  (define-key vterm-mode-map (kbd "C-c C-k") 'vterm-send-C-c) ; Send C-c to the terminal process
  (define-key vterm-mode-map (kbd "<f1>") 'vterm-other-window) ; Quickly switch to vterm in another window
  (define-key vterm-mode-map (kbd "C-c c") 'vterm-copy-mode) ; Enter copy mode
  (define-key vterm-mode-map (kbd "C-c y") 'vterm-yank)      ; Paste into vterm

  ;; If you use Projectile, you can add the project-specific vterm command
  ;; Ensure this runs after projectile is loaded if projectile is also managed by use-package
  ;; or use (with-eval-after-load 'projectile ...)
  (with-eval-after-load 'projectile
    (defun my-projectile-vterm ()
      "Open vterm in the current projectile project root."
      (interactive)
      (let ((project-root (projectile-project-root)))
        (if project-root
            (vterm project-root)
          (vterm))))
    ;; You might want to bind this within projectile's map or globally
    ;; For example, if projectile-mode-map is active in your project buffers:
    (define-key projectile-mode-map (kbd "C-c p t") #'my-projectile-vterm)
    ;; Or a global keybinding if you prefer
    ;; (define-key global-map (kbd "C-x p t") #'my-projectile-vterm)
    )
  ;; :hook
  ;; Example hook: if you want to do something every time vterm-mode is enabled
  ;; (vterm-mode . (lambda () (message "vterm ready!")))
  )

(add-to-list 'process-coding-system-alist
             '("[rR][gG]" . (utf-8 . utf-8)))


(provide 'setup-core)
;;; setup-core.el ends here
