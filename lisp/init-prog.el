;;; init-prog.el --- Define functions.	-*- lexical-binding: t -*-
(use-package treesit-auto
  :demand t
  :config
  (setq treesit-auto-install 'prompt)
  (setq treesit-font-lock-level 4)
  (global-treesit-auto-mode))
  (add-to-list 'treesit-extra-load-path
               (expand-file-name "tree-sitter" user-emacs-directory))

(use-package yasnippet
  :ensure t
  :hook ((prog-mode . yas-minor-mode)
         (org-mode . yas-minor-mode))
  :init
  :config
  (progn
    (setq hippie-expand-try-functions-list
          '(yas/hippie-try-expand
            try-complete-file-name-partially
            try-expand-all-abbrevs
            try-expand-dabbrev
            try-expand-dabbrev-all-buffers
            try-expand-dabbrev-from-kill
            try-complete-lisp-symbol-partially
            try-complete-lisp-symbol))))

(use-package yasnippet-snippets
  :ensure t
  :after yasnippet)

;; Prettify Symbols (e.g., display “lambda” as “λ”)
(use-package prog-mode
  :ensure nil
  :hook ((prog-mode . prettify-symbols-mode)
         (prog-mode . hs-minor-mode))

  :init
  ;;(setq-default prettify-symbols-alist centaur-prettify-symbols-alist)
  (setq prettify-symbols-unprettify-at-point 'right-edge))

(use-package hideshow
  :ensure nil ; hideshow 是内置的
  :config
  ;; 将 C-x $ 绑定到 hs-toggle-hiding 命令
  (define-key hs-minor-mode-map (kbd "C-x $") #'hs-toggle-hiding)

  ;; (可选) 如果想移除旧的快捷键 C-c @ C-d，可以取消下面这行的注释
  ;; 注意：这只会移除这一个特定的绑定，其他 C-c @ 开头的 hs-mode 快捷键仍然有效
  ;; (define-key hs-minor-mode-map (kbd "C-c @ C-d") nil)
  )

;; Cross-referencing commands
(use-package xref
  :bind (("M-g ." . xref-find-definitions)
         ("M-g ," . xref-go-back))
  :init
  ;; Use faster search tool
  (when (executable-find "rg")
    (setq xref-search-program 'ripgrep))

  ;; Select from xref candidates in minibuffer
  (setq xref-show-definitions-function #'xref-show-definitions-completing-read
        xref-show-xrefs-function #'xref-show-definitions-completing-read))

;; Code styles
(use-package editorconfig
  :diminish
  :hook (after-init . editorconfig-mode))

;; Run commands quickly
(use-package quickrun
  :bind (("C-<f5>" . quickrun)
         ("C-c X"  . quickrun)))

;; Protobuf mode configuration
(use-package protobuf-mode
  :hook (protobuf-mode . (lambda ()
                           "Set up Protobuf's imenu generic expressions."
                           (setq imenu-generic-expression
                                 '((nil "^[[:space:]]*\\(message\\|service\\|enum\\)[[:space:]]+\\([[:alnum:]]+\\)" 2))))))



(provide 'init-prog)

;;; init-prog.el ends here
