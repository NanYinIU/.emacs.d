;;; setup-ui.el --- UI, themes, modeline, icons -*- lexical-binding: t; -*-

;;; Commentary:
;; Configure UI elements, themes, and visual enhancements

;;; Code:

(require 'init-const)
(require 'init-funcs)

;;(setq frame-resize-pixelwise t)
;; Emacs plus 28+ no title bar config
(add-to-list 'default-frame-alist '(undecorated-round . t))

;; cursor type set hbar
(setq-default cursor-type 'bar)

;; For all-the-icons
(use-package all-the-icons
  :ensure t
  :if (display-graphic-p))

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
           (43  . ".\\(?:\\([>]\\)>?\\)")
           (45  . ".\\(?:\\(-[->]\\|<<\\|>>\\|[-<>|~]\\)[-<>|~]?\\)")
           (46  . ".\\(?:\\(\\.<\\|[-=]\\)[-<=]?\\)")
           (47  . ".\\(?:\\(//\\|==\\|[=>]\\)[/=>]?\\)")
           (48  . ".\\(?:x[a-zA-Z]\\)")
           (58  . ".\\(?:\\(::\\|[:<=>]\\)[:<=>]?\\)")
           (59  . ".\\(?:\\(;\\);?\\)")
           (60  . ".\\(?:\\(!--\\|\\$>\\|\\*>\\|\\+>\\|-[-<>|]\\|/>\\|<[-<=]\\|=[<>|]\\|==>?\\||>\\||||?\\|~[>~]\\|[$*+/:<=>|~-]\\)[$*+/:<=>|~-]?\\)")
           (61  . ".\\(?:\\(!=\\|/=\\|:=\\|<<\\|=[=>]\\|>>\\|[=>]\\)[=<>]?\\)")
           (62  . ".\\(?:\\(->\\|=>\\|>[-=>]\\|[-:=>]\\)[-:=>]?\\)")
           (63  . ".\\(?:\\([.:=?]\\)[.:=?]?\\)")
           (91  . ".\\(?:\\(|\\)[]|]?\\)")
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
  (doom-modeline-mode 1)
  :custom
  (doom-modeline-icon-font-size 13)
  (doom-modeline-icon t)
  (doom-modeline-nerd-font t))

(use-package keycast
  :ensure t
  :init
  ;; Option 1: Try 't' to append after the last element of mode-line-format.
  ;;(setq keycast-mode-line-insert-after t)
  (setq keycast-mode-line-insert-after 'doom-modeline-misc-info)
  :config
  (keycast-mode-line-mode 1))


;; For doom-themes
(use-package doom-themes
  :ensure t
  ;;:init
  ;;(load-theme 'doom-one t)
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Treemacs theme
  (setq doom-themes-treemacs-theme "doom-atom") ; use "doom-colors" for less minimal icon theme
  (doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

;; For mini-frame (used by nano setup)
(use-package mini-frame
  :ensure t)

;; For ef-theme
(use-package ef-themes
  :ensure t
  :init
  (load-theme 'ef-dream :no-confirm)
  :config
  (setq ef-themes-mixed-fonts t
        ef-themes-variable-pitch-ui t)

  (setq ef-themes-headings
      '((1 light variable-pitch 1.2)
        (2 regular 1.1)
        (3 1.1)
        (agenda-date 1.3)
        (agenda-structure variable-pitch light 1.5)
        (t variable-pitch)))
  (setq org-modern-timestamp nil)

  )

(use-package modus-themes
  :ensure t
  ;;:init
  ;;(load-theme 'modus-vivendi t)
  :config
  (setq modus-themes-bold-constructs t)
  (setq modus-themes-prompts '(bold italic))
  (setq modus-themes-completions
      '((matches . (extrabold underline))
        (selection . (semibold italic))))

  (setq modus-themes-headings
      '((1 . t)           ; keep the default style
        (2 . (semibold 1.2))
        (t . (rainbow)))) ; style for all other headings
)

;; For nano-local-system (local N Λ N O system from site-lisp/nano-emacs/)
(use-package nano-local-system
  :ensure nil
  :load-path "site-lisp/nano-emacs"
  :init
  ;; Custom variables set before loading nano components
  (setq nano-font-family-monospaced "Maple Mono NL")
  (setq nano-font-size 13)
  :config
  ;; Require all the nano components
  (require 'nano-base-colors)
  (require 'nano-faces)
  (require 'nano-theme)
  (require 'nano-theme-dark)
  (require 'nano-theme-light)
  (require 'nano-modeline)
  (when (member "-compact" command-line-args)
    (require 'nano-compact))
  (require 'nano-splash)
  (require 'nano-colors)
  (require 'nano-command)

  ;; Theme activation
  (nano-theme-set-dark)
  (call-interactively 'nano-refresh-theme)

  ;; Welcome message
  (let ((inhibit-message t))
    (message "Welcome to GNU Emacs / N Λ N O edition")
    (message (format "Initialization time: %s" (emacs-init-time)))))

;; For nerd-icons
(use-package nerd-icons
  :ensure t
  :custom
  (nerd-icons-font-family "Symbols Nerd Font Mono"))

;; For posframe (pop-up frames)
(use-package posframe
  :ensure t
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

;;; Font settings
(defun +my/better-font()
  "Set up fonts for UI."
  (interactive)
  (if (display-graphic-p)
      (progn
        ;; Fixedsys Excelsior is great. Input Serif Condensed 12
        ;; Fantasque Sans Mono
        ;; 中文字体
        (set-face-attribute 'default nil :font "Fantasque Sans Mono 13")
        (dolist
            (charset '(kana han symbol cjk-misc bopomofo))
          (set-fontset-font (frame-parameter nil 'font)
                            charset
                            ;; WenQuanyi Micro Hei
                            (font-spec :family "WenQuanYi Micro Hei Mono" :size 12)))))
  )

(defun +my|init-font(frame)
  "Initialize font for FRAME."
  (with-selected-frame frame
    (if (display-graphic-p)
        (+my/better-font))))

(if (and (fboundp 'daemonp) (daemonp))
    (add-hook 'after-make-frame-functions #'+my|init-font)
  (+my/better-font))

;; Mouse & Smooth Scroll
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
(when sys/macp
  (setq ns-use-thin-smoothing t)
  ;; Don't open a file in a new frame
  (setq ns-pop-up-frames nil))


(provide 'setup-ui)
;;; setup-ui.el ends here
