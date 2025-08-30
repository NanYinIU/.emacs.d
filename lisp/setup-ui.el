;;; setup-ui.el --- UI, themes, modeline, icons -*- lexical-binding: t; -*-

;;; Commentary:
;; Configure UI elements, themes, and visual enhancements

;;; Code:

(require 'init-const)
(require 'init-funcs)

;; UI settings
(setq-default cursor-type 'bar)

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

;; For all-the-icons
(use-package all-the-icons
  :ensure t
  :defer t
  :diminish
  :if (display-graphic-p))

;; For composite (built-in ligature support)
(use-package composite
  :ensure nil
  :defer t
  :diminish
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

;; For keycast - Show current key binding in mode line
(use-package keycast
  :ensure t
  :defer t
  :diminish
  :init
  (setq keycast-mode-line-insert-after 'doom-modeline-misc-info)
  :config
  (keycast-mode-line-mode 1))

;; For mini-frame (used by nano setup)
(use-package mini-frame
  :ensure t
  :defer t
  :diminish)

;; For nerd-icons
(use-package nerd-icons
  :ensure t
  :defer t
  :diminish
  :custom
  (nerd-icons-font-family "Symbols Nerd Font Mono"))

;; For posframe (pop-up frames)
(use-package posframe
  :ensure t
  :defer t
  :diminish
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

;; For beacon - Highlight cursor position after scrolling
(use-package beacon
  :ensure t
  :defer t
  :diminish
  :config (beacon-mode 1))

(provide 'setup-ui)
;;; setup-ui.el ends here