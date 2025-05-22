;;; setup-core.el --- Core Emacs configurations -*- lexical-binding: t; -*-

;; For async operations
(use-package async
  :ensure t ; Assuming it should be installed if used
  :functions (async-bytecomp-package-mode dired-async-mode)
  :init
  (async-bytecomp-package-mode 1)
  (dired-async-mode 1))

;; For bind-key (assumed for bind-keys macro)
(use-package bind-key
  :ensure t)

;; For compat (compatibility library)
(use-package compat
  :ensure t
  :demand t) ; As originally configured

;; For exec-path-from-shell
(use-package exec-path-from-shell
  :ensure t
  :custom
  (exec-path-from-shell-arguments '("-l"))
  :config
  (when (or (memq window-system '(mac ns x)) ; Combined condition
            (daemonp)) 
    (exec-path-from-shell-initialize)))

;; For gcmh
(use-package gcmh
  :ensure t ; ensure this is present
  :diminish
  :hook (emacs-startup . gcmh-mode)
  :init
  (setq gcmh-idle-delay 'auto
        gcmh-auto-idle-delay-factor 10
        gcmh-high-cons-threshold #x1000000)) ; 16MB

;; For hideshow (hs-minor-mode) keybindings
(use-package hideshow
  :ensure nil ; hideshow is built-in
  :config
  (define-key hs-minor-mode-map (kbd "C-x $") #'hs-toggle-hiding))

;; For recentf (built-in)
(use-package recentf
  :bind (("C-x C-r" . recentf-open-files))
  :hook (after-init . recentf-mode)
  :init (setq recentf-max-saved-items 300
              recentf-exclude
              '("\.?cache" ".cask" "url" "COMMIT_EDITMSG\'" "bookmarks"
                "\.\(?:gz\|gif\|svg\|png\|jpe?g\|bmp\|xpm\)$"
                "\.?ido\.last$" "\.revive$" "/G?TAGS$" "/.elfeed/"
                "^/tmp/" "^/var/folders/.+$" "^/ssh:" "/persp-confs/"
                (lambda (file) (file-in-directory-p file package-user-dir))))
  :config
  (push (expand-file-name recentf-save-file) recentf-exclude)
  (add-to-list 'recentf-filename-handlers #'abbreviate-file-name))

;; For savehist (built-in history saving)
(use-package savehist
  :ensure nil 
  :init (savehist-mode))

;; For simple (built-in general commands and modes)
(use-package simple
  :ensure nil ; Explicitly nil for built-ins
  :hook ((after-init . size-indication-mode)
         (text-mode . visual-line-mode)
         ((prog-mode markdown-mode conf-mode) . enable-trailing-whitespace))
  :init
  (setq column-number-mode t
        line-number-mode t
        kill-whole-line t               ; Kill line including '\n'
        line-move-visual nil
        track-eol t                     ; Keep cursor at end of lines. Require line-move-visual is nil.
        set-mark-command-repeat-pop t)  ; Repeating C-SPC after popping mark pops it again

  ;; Visualize TAB, (HARD) SPACE, NEWLINE
  (setq-default show-trailing-whitespace nil) ; Don't show trailing whitespace by default
  (defun enable-trailing-whitespace ()
    "Show trailing spaces and delete on saving."
    (setq show-trailing-whitespace t)
    (add-hook 'before-save-hook #'delete-trailing-whitespace nil t)))

;; For time (display-time-mode configuration)
(use-package time
  :ensure nil
  :init (setq display-time-default-load-average nil
              display-time-format "%H:%M"))

;; For xref (built-in)
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

(provide 'setup-core)
