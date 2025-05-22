;;; setup-hydra.el --- Hydra and related configurations -*- lexical-binding: t; -*-

;; For hydra
(use-package hydra
  :ensure t
  :hook (emacs-lisp-mode . hydra-add-imenu)
  :init
    (setq hydra-hint-display-type 'posframe)

    (defun hydra-set-posframe-show-params ()
      "Set hydra-posframe style."
      (setq hydra-posframe-show-params
            `(:left-fringe 8
              :right-fringe 8
              :internal-border-width ,posframe-border-width
              :internal-border-color ,(face-background 'posframe-border nil t)
              :background-color ,(face-background 'tooltip nil t)
              :foreground-color ,(face-foreground 'tooltip nil t)
              :lines-truncate t
              :poshandler posframe-poshandler-frame-center-near-bottom)))
    (hydra-set-posframe-show-params)
    (add-hook 'after-load-theme-hook #'hydra-set-posframe-show-params t))

;; For major-mode-hydra
(use-package major-mode-hydra
  :ensure t
  :bind
  ("M-SPC" . major-mode-hydra))

;; For pretty-hydra
(use-package pretty-hydra
  :ensure t
  :bind ("<f6>" . toggles-hydra/body)
  :hook (emacs-lisp-mode . (lambda ()
                             (add-to-list
                              'imenu-generic-expression
                              '("Hydras"
                                "^.*(\\(pretty-hydra-define\\) \\([a-zA-Z-]+\\)"
                                2))))
  :init
  ;; Global toggles
  ;;(with-no-warnings
  ;;  (pretty-hydra-define toggles-hydra (
  ;;                                :color amaranth :quit-key ("q" "C-g"))
  ;;    ("Basic"
  ;;     (("n" (cond ((fboundp 'display-line-numbers-mode)
  ;;                  (display-line-numbers-mode (if display-line-numbers-mode -1 1)))
  ;;                 ((fboundp 'gblobal-linum-mode)
  ;;                  (global-linum-mode (if global-linum-mode -1 1))))
  ;;       "line number"
  ;;       :toggle (or (bound-and-true-p display-line-numbers-mode)
  ;;                   (bound-and-true-p global-linum-mode)))
  ;;      ("a" global-aggressive-indent-mode "aggressive indent" :toggle t)
  ;;      ("d" global-hungry-delete-mode "hungry delete" :toggle t)
  ;;      ("e" electric-pair-mode "electric pair" :toggle t)
  ;;      ("c" flyspell-mode "spell check" :toggle t)
        ;;("s" prettify-symbols-mode "pretty symbol" :toggle t)
  ;;      ("l" global-page-break-lines-mode "page break lines" :toggle t)
  ;;      ("b" display-battery-mode "battery" :toggle t)
  ;;      ("i" display-time-mode "time" :toggle t)
  ;;      ("m" doom-modeline-mode "modern mode-line" :toggle t))
  ;;     "Highlight"
  ;;     (("h l" global-hl-line-mode "line" :toggle t)
  ;;      ("h p" show-paren-mode "paren" :toggle t)
  ;;      ("h s" symbol-overlay-mode "symbol" :toggle t)
  ;;      ("h r" rainbow-mode "rainbow" :toggle t)
  ;;      ("h w" (setq-default show-trailing-whitespace (not show-trailing-whitespace))
   ;;      "whitespace" :toggle show-trailing-whitespace)
  ;;      ("h d" rainbow-delimiters-mode "delimiter" :toggle t)
  ;;      ("h i" highlight-indent-guides-mode "indent" :toggle t)
  ;;      ("h t" global-hl-todo-mode "todo" :toggle t))
  ;;     "Program"
  ;;     (("f" flymake-mode "flymake" :toggle t)
  ;;      ("O" hs-minor-mode "hideshow" :toggle t)
  ;;      ("u" subword-mode "subword" :toggle t)
  ;;      ("W" which-function-mode "which function" :toggle t)
  ;;      ("E" toggle-debug-on-error "debug on error" :toggle (default-value 'debug-on-error))
  ;;      ("Q" toggle-debug-on-quit "debug on quit" :toggle (default-value 'debug-on-quit))
  ;;      ("v" global-diff-hl-mode "gutter" :toggle t)
  ;;      ("V" diff-hl-flydiff-mode "live gutter" :toggle t)
  ;;      ("M" diff-hl-margin-mode "margin gutter" :toggle t)
  ;;      ("D" diff-hl-dired-mode "dired gutter" :toggle t))
  ;;     )))
  )

(provide 'setup-hydra)
